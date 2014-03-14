(ns thi.ng.luxor.core
  (:require
   [thi.ng.luxor.config :as conf]
   [thi.ng.luxor.compiler :refer [luxvalues-typed]]
   [thi.ng.luxor.color :as col]
   [thi.ng.luxor.presets :as presets]
   [thi.ng.luxor.version :refer [version]]
   [thi.ng.common.math.core :as m]
   [thi.ng.geom.core :as g :refer [vec3 M44]]
   [thi.ng.geom.plane :as pl]
   [thi.ng.geom.gmesh :as gmesh]
   [thi.ng.geom.meshio :as mio]
   [clojure.java.io :as io])
  (:import
   [java.io File Writer OutputStreamWriter StringWriter]
   [java.util Date]))

(defn kw-or-num? [x] (or (keyword? x) (number? x)))
(defn kw-or-str? [x] (or (keyword? x) (string? x)))
(defn color? [x] (and (sequential? x) (= 3 (count x))))
(defn optional-bool
  [x default] (cond x true (false? x) false :default default))

(defn optional
  ([pred x] (if x (pred x) true))
  ([test pred x] (if (test x) (pred x) true)))

(defn ->radians [theta] (if conf/*degrees* (m/radians theta) theta))
(defn ->degrees [theta] (if conf/*degrees* theta (m/degrees theta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; serialization

(defn- lx-header
  [path & comments]
  (format "# %s\n# generated %s by luxor v%s\n%s\n"
          path (.toString (Date.)) version
          (if (seq comments)
            (reduce #(str % "# " %2 "\n") "#\n# Comments:\n" comments)
            "")))

(defn- path-filename
  [path] (.getName (File. path)))

(defn- include-file*
  [path]
  (format "Include \"%s\"\n\n" path))

(defn- include-partials*
  [partials]
  (prn :partials partials)
  (apply str (map include-file* partials)))

(defn- inject-scene-partial
  [partial path include?]
  (when partial
    (if include? (include-file* path) partial)))

;; TODO rename into export-scene & split out serialize part
(defn- serialize-lxs
  [scene base-path separate-files?]
  (let [base-name (path-filename base-path)
        lxs-path (str base-path ".lxs")
        lxs (str
             (apply lx-header lxs-path (:comments scene))
             (include-partials* (get-in scene [:includes :headers]))
             (:renderer scene)
             (:accel scene)
             (:sampler scene)
             (:integrator scene)
             (:volume-integrator scene)
             (:filter scene)
             (:film scene)
             (:camera scene)
             "WorldBegin\n\n"
             (include-partials* (get-in scene [:includes :partials]))
             (inject-scene-partial (:volumes scene) (str base-name ".lxv") separate-files?)
             (inject-scene-partial (:materials scene) (str base-name ".lxm") separate-files?)
             (inject-scene-partial (:geometry scene) (str base-name ".lxo") separate-files?)
             (:lights scene)
             "\nWorldEnd\n")]
    (spit lxs-path lxs)
    lxs))

(defn- serialize-lxm
  [{:keys [materials]} base-path]
  (when materials
    (let [path (str base-path ".lxm")
          body (str (lx-header path) materials)]
      (spit path body)
      body)))

(defn- serialize-lxv
  [{:keys [volumes]} base-path]
  (when volumes
    (let [path (str base-path ".lxv")
          body (str (lx-header path) volumes)]
      (spit path body)
      body)))

(defn- serialize-lxo
  [{:keys [geometry]} base-path]
  (when geometry
    (let [path (str base-path ".lxo")
          body (str (lx-header path) geometry)]
      (spit path body)
      body)))

(defn serialize-scene
  ([scene base-path]
     (serialize-scene scene base-path true))
  ([scene base-path separate?]
     (let [scene* (reduce
                   (fn [s [k type]]
                     (if-let [ents (k scene)]
                       (assoc s k (luxvalues-typed scene type ents))
                       s))
                   (select-keys scene [:comments :includes])
                   {:renderer :renderer
                    :accel :accelerator
                    :sampler :sampler
                    :integrator :integrator
                    :volume-integrator :volume-integrator
                    :filter :filter
                    :film :film
                    :camera :camera
                    :lights :light
                    :materials :material
                    :volumes :volume
                    :geometry :shape})
           lxs (serialize-lxs scene* base-path separate?)]
       (if separate?
         (-> lxs
             (str (serialize-lxm scene* base-path))
             (str (serialize-lxo scene* base-path))
             (str (serialize-lxv scene* base-path)))
         lxs))))

(defn- make-transform-matrix
  [{:keys [scale rx ry rz axis theta translate] :or {rx 0 ry 0 rz 0} :as tx}]
  (let [mat (if translate
              (g/translate M44 (vec3 translate))
              M44)
        mat (if axis
              (g/rotate-around-axis mat axis theta)
              (if (some (complement zero?) [rx ry rz])
                (-> mat
                    (g/rotate-x (->radians rx))
                    (g/rotate-y (->radians ry))
                    (g/rotate-z (->radians rz)))
                mat))
        mat (if scale (g/scale mat scale) mat)]
    (vals (g/transpose mat))))

(defn- transform-common
  [scene {:keys [matrix] :as tx}]
  {:__transform (if matrix matrix (make-transform-matrix tx))})

(defn- append
  ([scene group id opts]
     (append scene group id opts false))
  ([scene group id {tx :__transform :as opts} singleton?]
     (-> (if singleton? (dissoc scene group) scene)
         (assoc-in
          [group (name id)]
          (merge
           (when tx (transform-common scene tx))
           (dissoc opts :__transform))))))

(defn- append*
  [scene group xs]
  (reduce
   (fn [scene [id opts]] (append scene group id opts))
   scene xs))

(defn- append-singleton
  [scene group id opts] (append scene group id opts true))

(defn scene-comments
  [scene & comments]
  (update-in scene [:comments] (fnil into []) comments))

(defn include-headers
  [scene & paths]
  (update-in scene [:includes :headers] (fnil into []) paths))

(defn include-partials
  [scene & paths]
  (update-in scene [:includes :partials] (fnil into []) paths))

(defn- material-common
  [{:keys [interior exterior]}]
  {:__interior interior
   :__exterior exterior})

(defn material-null
  [scene id]
  {:pre [(kw-or-str? id)]}
  (append
   scene :materials id
   {:type [:string "null"]}))

(defn material-mix
  [scene id m1 m2 blend]
  (append
   scene :materials id
   {:type [:string "mix"]
    :namedmaterial1 [:string (name m1)]
    :namedmaterial2 [:string (name m2)]
    :amount [:float blend]}))

(defn- material-id
  [id alpha]
  (if (< alpha 1.0) (str "__" (name id)) (name id)))

(defn- inject-alpha-mat
  [scene id alpha]
  (if (< alpha 1.0)
    (material-mix
     scene id "__hidden__" (material-id id alpha) alpha)
    scene))

(defn material-matte
  [scene id {:keys [diffuse diffuse-hsb sigma alpha]
             :or {diffuse [1.0 1.0 1.0] sigma 0 alpha 1.0} :as opts}]
  {:pre [(kw-or-str? id)
         (color? diffuse) (optional color? diffuse-hsb)
         (number? sigma) (number? alpha)]}
  (-> scene
      (append
       :materials
       (material-id id alpha)
       (merge
        (material-common opts)
        {:type [:string "matte"]
         :Kd [:color (if diffuse-hsb (col/hsb->rgb diffuse-hsb) diffuse)]
         :sigma [:float sigma]}))
      (inject-alpha-mat id alpha)))

(defn material-matte-translucent
  [scene id {:keys [reflect reflect-hsb transmit transmit-hsb
                    sigma alpha conserve?]
             :or {reflect [0.3 0.3 0.3] transmit [0.65 0.65 0.65]
                  sigma 0 alpha 1.0 conserve? true}
             :as opts}]
  {:pre [(kw-or-str? id)
         (color? reflect) (optional color? reflect-hsb)
         (color? transmit) (optional color? transmit-hsb)
         (number? sigma)]}
  (-> scene
      (append
       :materials
       (material-id id alpha)
       (merge
        (material-common opts)
        {:type [:string "mattetranslucent"]
         :Kr [:color (if reflect-hsb (col/hsb->rgb reflect-hsb) reflect)]
         :Kt [:color (if transmit-hsb (col/hsb->rgb transmit-hsb) transmit)]
         :sigma [:float sigma]
         :energyconserving [:bool conserve?]}))
      (inject-alpha-mat id alpha)))

(defn volume
  [scene id {:keys [type ior absorb absorb-hsb abs-scale abs-depth]
             :or {type :clear absorb [1.0 1.0 1.0]
                  abs-scale 1.0 abs-depth 1.0 ior :air}}]
  {:pre [(kw-or-str? id)
         (conf/volume-types type)
         (kw-or-num? ior) (optional keyword? presets/ior-presets ior)
         (color? absorb) (optional color? absorb-hsb)
         (number? abs-scale) (number? abs-depth)]}
  (append
   scene :volumes id
   {:__type type
    :fresnel [:float (if (keyword ior) (presets/ior-presets ior) ior)]
    :absorption [:log-color [(if absorb-hsb (col/hsb->rgb absorb-hsb) absorb)
                             abs-scale abs-depth]]}))

(defn volume-integrator
  [scene id]
  {:pre [(conf/volume-integrators id)]}
  (append-singleton scene :volume-integrator id {}))

(defn renderer-slg
  [scene {:keys [cpu? gpu?]}]
  (append-singleton
   scene :renderer "slg"
   {:config
    [:string-vec [(str "opencl.cpu.use = " (optional-bool cpu? true))
                  (str "opencl.gpu.use = " (optional-bool gpu? true))]]}))

(defn renderer-sampler
  [scene]
  (append-singleton scene :renderer "sampler" {}))

(defn renderer-sppm
  [scene]
  (append-singleton scene :renderer "sppm" {}))

(defn sampler-ld
  [scene {:keys [samples noise-aware?] :or {samples 4}}]
  (append-singleton
   scene :sampler "lowdiscrepancy"
   {:pixelsampler [:string "lowdiscrepancy"]
    :pixelsamples [:int samples]
    :noiseaware [:bool (optional-bool noise-aware? true)]}))

(defn sampler-sobol
  [scene {:keys [noise-aware?]}]
  (append-singleton
   scene :sampler "sobol"
   {:noiseaware [:bool (optional-bool noise-aware? true)]}))

(defn- integrator-common
  [{:keys [shadow-rays light-strategy] :or {shadow-rays 1 light-strategy :auto}}]
  {:pre [(number? shadow-rays) (conf/light-strategies light-strategy)]}
  {:shadowraycount [:int shadow-rays]
   :lightstrategy [:string (name light-strategy)]})

(defn integrator-bidir
  [scene {:keys [eye-depth light-depth light-rays path-strategy]
          :or {eye-depth 16 light-depth 16 light-rays 1 path-strategy :auto}
          :as opts}]
  {:pre [(number? eye-depth) (number? light-depth) (number? light-rays)
         (conf/light-path-strategies path-strategy)]}
  (append-singleton
   scene :integrator "bidirectional"
   (merge
    (integrator-common opts)
    {:eyedepth [:int eye-depth]
     :lightdepth [:int light-depth]
     :lightraycount [:int light-rays]
     :lightpathstrategy [:string (name path-strategy)]})))

(defn integrator-sppm
  [scene {:keys [max-eye max-photon
                 photons hit-points
                 start-radius alpha
                 env? direct-light? glossy? use-prob?
                 wave-passes accel
                 pixel-sampler photon-sampler]
          :or {max-eye 48 max-photon 16
               photons 2e6 hit-points 0
               start-radius 2 alpha 0.7
               wave-passes 8
               accel :hybridhashgrid
               pixel-sampler :hilbert
               photon-sampler :halton}}]
  {:pre [(every? number? [max-eye max-photon photons
                          hit-points start-radius alpha wave-passes])
         (conf/sppm-accelerators accel)
         (conf/pixel-samplers pixel-sampler)
         (conf/photon-samplers photon-sampler)]}
  (append-singleton
   scene :integrator "sppm"
   {:maxeyedepth [:int max-eye]
    :maxphotondepth [:int max-photon]
    :photonperpass [:int photons]
    :hitpointperpass [:int hit-points]
    :startradius [:float start-radius]
    :alpha [:float alpha]
    :includeenvironment [:bool (optional-bool env? true)]
    :directlightsampling [:bool (optional-bool direct-light? true)]
    :storeglossy [:bool (optional-bool glossy? false)]
    :useproba [:bool (optional-bool use-prob? true)]
    :wavelengthstratificationpasses [:int wave-passes]
    :lookupaccel [:string (name accel)]
    :pixelsampler [:string (name pixel-sampler)]
    :photonsampler [:string (name photon-sampler)]}))

(defn filter-mitchell
  [scene {:keys [coeff coeffB coeffC size w h] :or {coeff 1/3 size 2.0}}]
  (append-singleton
   scene :filter "mitchell"
   {:supersample [:bool true]
    :B [:float (or coeffB coeff)]
    :C [:float (or coeffC coeff)]
    :xwidth [:float (or w size)]
    :ywidth [:float (or h size)]}))

(defn accelerator-qbvh
  [scene {:as opts}]
  (append-singleton scene :accel "qbvh" (or opts {})))

(defn film
  [scene {:keys [width height gamma
                 white red green blue
                 base-path
                 write-flm? restart-flm?
                 write-exr? exr-channels exr-imaging? exr-zbuf?
                 write-png? png-channels png-16bit?
                 write-tga? tga-channels
                 premultiply?
                 ldr-method
                 outlier-rejects
                 write-interval display-interval
                 halt-spp halt-time halt-threshold
                 response]
          :or {width 1280 height 720 gamma 2.2 base-path "out"
               white [0.314275 0.329411] red [0.63 0.34] green [0.31 0.595] blue [0.155 0.07]
               write-flm? true restart-flm? true
               write-exr? false exr-channels "RGBA" exr-imaging? true exr-zbuf? true
               write-png? true png-channels "RGB" png-16bit? false
               write-tga? false tga-channels "RGB"
               premultiply? false
               ldr-method "cut"
               outlier-rejects 2
               write-interval 180 display-interval 12}}]
  (append-singleton
   scene :film "fleximage"
   (merge
    (get-in scene [:film "fleximage"])
    {:__aspect (double (/ height width))
     :xresolution [:int width]
     :yresolution [:int height]
     :gamma [:float gamma]
     :filename [:string base-path]
     :colorspace_white [:float-vec white]
     :colorspace_red [:float-vec red]
     :colorspace_green [:float-vec green]
     :colorspace_blue [:float-vec blue]
     :premultiplyalpha [:bool premultiply?]
     :write_resume_flm [:bool write-flm?]
     :restart_resume_flm [:bool restart-flm?]
     :write_exr [:bool write-exr?]
     :write_exr_channels [:string exr-channels]
     :write_exr_applyimaging [:bool exr-imaging?]
     :write_exr_ZBuf [:bool exr-zbuf?]
     :write_png [:bool write-png?]
     :write_png_channels [:string png-channels]
     :write_png_16bit [:bool png-16bit?]
     :write_tga [:bool write-tga?]
     :write_tga_channels [:string tga-channels]
     :ldr_clamp_method [:string ldr-method]
     :writeinterval [:int write-interval]
     :flmwriteinterval [:int write-interval]
     :displayinterval [:int display-interval]
     :outlierrejection_k [:int outlier-rejects]}
    (when halt-spp {:haltspp [:int halt-spp]})
    (when halt-time {:halttime [:int halt-time]})
    (when halt-threshold {:haltthreshold [:int halt-threshold]})
    (when response
      {:cameraresponse [:string (if (keyword? response)
                                  (response presets/film-response-presets)
                                  response)]}))))

(defn tonemap-linear
  [scene {:keys [iso exposure f-stop gamma]
          :or {iso 100 exposure 1.0 f-stop 4 gamma 2.2}}]
  (update-in
   scene [:film "fleximage"]
   assoc
   :tonemapkernel [:string "linear"]
   :linear_sensitivity [:float iso]
   :linear_exposure [:float exposure]
   :linear_fstop [:float f-stop]
   :linear_gamma [:float gamma]))

(defn camera
  [scene {:keys [type eye target up
                 fov lens-radius focal-dist focal-point blades power distribution
                 auto-focus? shutter-open shutter-close
                 window]
          :or {type "perspective" fov 60
               lens-radius 0 blades 0 power 1 distribution :uniform
               shutter-open 0 shutter-close 1.0
               eye [0 -10 0] target [0 0 0]}}]
  (let [eye (vec3 eye)
        target (vec3 target)
        opts {:fov [:float fov]
              :shutteropen [:float shutter-open]
              :shutterclose [:float shutter-close]
              :lensradius [:float lens-radius]
              :blades [:int blades]
              :distribution [:string (name distribution)]
              :power [:int power]
              :screenwindow [:float-vec
                             (or window
                                 (let [a (get-in scene [:film "fleximage" :__aspect])]
                                   [-1 1 (- a) a]))]
              :__lookat {:eye eye :target target
                         :up (if up
                               (vec3 up)
                               (g/normalize (g/cross (g/- eye target) g/V3X)))}}
        opts (cond
              focal-dist  (assoc opts
                            :focaldistance [:float focal-dist]
                            :autofocus [:bool false])
              focal-point (assoc opts
                            :focaldistance [:float (g/dist eye (vec3 focal-point))]
                            :autofocus [:bool false])
              :default    (assoc opts :autofocus [:bool true]))]
    (append-singleton scene :camera type opts)))

(defn light-group
  [scene id {:keys [gain] :or {gain 1.0}}]
  (append scene :light-groups id {:__gain gain}))

(defn light-groups
  [scene groups]
  (reduce #(apply light-group % %2) scene groups))

(defn- light-common
  [scene {:keys [group color gain power efficacy importance tx material hidden?]
          :or {group "default" color [1.0 1.0 1.0]
               gain 1.0 efficacy 17.0 power 100.0 importance 1.0 hidden? false}}]
  {:__parent (name group)
   :__material (or material (when hidden? "__hidden__"))
   :L [:color color]
   :gain [:float gain]
   :power [:float power]
   :efficacy [:float efficacy]
   :importance [:float importance]})

(defn area-light
  [scene id {:keys [samples mesh mesh-type export-path p n size tx]
             :or {samples 1 size 1.0 mesh-type :inline} :as opts}]
  (let [mesh (if mesh
               mesh
               (g/as-mesh
                (pl/plane-with-p (g/vec3 (or p [0 0 0])) (g/vec3 (or n [0 0 -1])))
                (if (sequential? size)
                  {:width (first size) :height (second size)}
                  {:size size})))]
    (append
     scene :lights id
     (merge
      (light-common scene opts)
      {:__transform tx
       :__type :area-light
       :__shape [(conf/mesh-types mesh-type)
                 {:__mesh mesh
                  :__export-path export-path
                  :__basename (str "light-" (name id))}]
       :nsamples [:int samples]}))))

(defn spot-light
  [scene id {:keys [from to cone-angle cone-delta tx]
             :or {cone-angle 30 cone-delta 5 from [0 0 1] to [0 0 0]}
             :as opts}]
  (append
   scene :lights id
   (merge
    (light-common scene opts)
    {:__transform tx
     :__type :spot-light
     :from [:point-vec [from]]
     :to [:point-vec [to]]
     :coneangle [:float (->degrees cone-angle)]
     :conedeltaangle [:float (->degrees cone-delta)]})))

(def light-types
  {:area area-light
   :spot spot-light})

(defn lights
  [scene lspecs]
  (reduce
   (fn [scene [id {ltype :type :as spec}]]
     (if-let [lfn (light-types ltype)]
       (lfn scene id spec)
       (prn "WARN: unknown light type: " ltype)))
   scene lspecs))

(defn shape-disk
  [scene id {:keys [z radius inner-radius phi tx material]
             :or {z 0 radius 1 inner-radius 0 phi 360}}]
  (append
   scene :geometry id
   {:__transform tx
    :__type :disk
    :__material material
    :name [:string (name id)]
    :height [:float z]
    :radius [:float radius]
    :innerradius [:float inner-radius]
    :phimax [:float phi]}))

(defn ply-mesh
  [scene id {:keys [mesh path export-path tx material smooth]}]
  (append
   scene :geometry id
   {:__transform tx
    :__type :plymesh
    :__material material
    :__mesh mesh
    :__export-path (or export-path path)
    :name [:string (name id)]
    :filename [:string (or path (str (name id) ".ply"))]
    :smooth [:bool smooth]}))

(defn stl-mesh
  [scene id {:keys [mesh path export-path tx material]}]
  (append
   scene :geometry id
   {:__transform tx
    :__type :stlmesh
    :__material material
    :__mesh mesh
    :__export-path (or export-path path)
    :name [:string (name id)]
    :filename [:string (or path (str (name id) ".stl"))]}))

(defn lux-scene
  []
  (-> {}
      (renderer-sppm)
      (sampler-sobol {})
      (integrator-sppm {})
      (filter-mitchell {})
      (volume-integrator :multi)
      (accelerator-qbvh {})
      (film {})
      (light-group "default" {})
      (material-null "__hidden__")))
