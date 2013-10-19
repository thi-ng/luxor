(ns thi.ng.luxor.core
  (:require
   [thi.ng.luxor.presets :as presets]
   [thi.ng.common.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.plane :as pl]
   [thi.ng.geom.mesh :as mesh]
   [thi.ng.geom.meshio :as mio]
   [clojure.java.io :as io])
  (:import
   [java.io File Writer OutputStreamWriter StringWriter]
   [java.util Date]))

;; References:
;; http://www.luxrender.net/wiki/Scene_file_format_dev

(def ^:const version "0.1.0-SNAPSHOT")

(def ^:const mesh-types            {:inline :trimesh :ply :plymesh :stl :stlmesh})
(def ^:const volume-types          #{:clear :homogenous :heterogenous})
(def ^:const volume-integrators    #{:none :single :emission :multi})
(def ^:const sppm-accelerators     #{:hashgrid :hybridhashgrid :kdtree :parallelhashgrid})
(def ^:const pixel-samplers        #{:hilbert :liner :tile :vegas})
(def ^:const photon-samplers       #{:halton :amc})
(def ^:const light-strategies      #{:auto :one :all :importance :powerimp :allpowerimp :logpowerimp})
(def ^:const light-path-strategies #{:auto :one :all :importance :powerimp :allpowerimp :logpowerimp})
(def ^:const rr-strategies         #{:none :efficiency :probability})

(def ^:dynamic *indent*         2)
(def ^:dynamic *degrees*        true)
(def ^:dynamic *float-format*   "%1.8f")

(defn kw-or-num? [x] (or (keyword? x) (number? x)))
(defn kw-or-str? [x] (or (keyword? x) (string? x)))
(defn color? [x] (and (sequential? x) (= 3 (count x))))
(defn optional-bool
  [x default] (cond x true (false? x) false :default default))

(defn optional
  ([pred x] (if x (pred x) true))
  ([test pred x] (if (test x) (pred x) true)))

(defn convert-angle [theta] (if *degrees* (m/radians theta) theta))

;; TODO
(defn hsb->rgb
  [[h s b]] [h s b])

(defn scaled-absorption-at-depth
  [x scale d]
  (* (/ (Math/log (max x 1e-30)) d) scale (if (= x 1.0) 1 -1)))

(defn scaled-absorption-color-at-depth
  [rgb scale d]
  (mapv #(scaled-absorption-at-depth % scale d) rgb))

(defn- material-ref
  [scene id]
  (let [{:keys [__interior __exterior]} (get-in scene [:materials id])]
    (str
     (format "NamedMaterial \"%s\"\n" id)
     (when __interior (format "Interior \"%s\"\n" __interior))
     (when __exterior (format "Exterior \"%s\"\n" __exterior)))))

(declare luxvalue)

(defn- filter-opts
  [xs] (->> xs (filter #(not (.startsWith (name (key %)) "__"))) (into {})))

(defn- luxvalues
  [scene opts]
  (let [indent (apply str (repeat *indent* \space))]
    (->> opts
         (filter-opts)
         (into (sorted-map))
         (reduce-kv
          (fn [s k [type v]] (str s indent (luxvalue type scene k v))) ""))))

(defn- luxvalues-typed
  [scene type coll]
  (reduce (fn [s [k v]] (str s (luxvalue type scene k v))) "" (into (sorted-map) coll)))

(defn- luxentity
  [scene type id opts]
  (format "%s \"%s\"\n%s\n" type (name id) (luxvalues scene opts)))

(defn- luxattrib
  [scene id type {:keys [__transform __material]} body]
  (let [sep (format "# -------- %s: %s --------\n" type id)
        inject (str
                (when __transform (luxtransform __transform))
                (when __material (material-ref scene __material)))]
    (format "%sAttributeBegin\n%s%sAttributeEnd\n%s\n" sep inject body sep)))

(defn- luxtransform
  [tx]
  (format "Transform [%s]\n"
          (apply str (interpose " " (map #(format "%1.8f" %) tx)))))

(defmulti luxvalue (fn [type & _] type))

(defmethod luxvalue :float
  [_ _ id x] (format "\"float %s\" [%1.8f]\n" (name id) (float x)))

(defmethod luxvalue :float-vec
  [_ _ id xs]
  (format "\"float %s\" [%s]\n"
          (name id)
          (apply str (interpose " " (map #(format "%1.8f" (float %)) xs)))))

(defmethod luxvalue :int
  [_ _ id x] (format "\"integer %s\" [%d]\n" (name id) (int x)))

(defmethod luxvalue :int-vec
  [_ _ id xs]
  (format "\"integer %s\" [%s]\n"
          (name id)
          (apply str (interpose " " (map #(format "%d" (int %)) xs)))))

(defmethod luxvalue :point-vec
  [_ _ id xs]
  (format "\"point %s\" [%s]\n"
          (name id)
          (apply str (interpose " " (map #(format "%1.8f" (float %)) (mapcat identity xs))))))

(defmethod luxvalue :bool
  [_ _ id x] (format "\"bool %s\" [\"%b\"]\n" (name id) x))

(defmethod luxvalue :string
  [_ _ id x] (format "\"string %s\" [\"%s\"]\n" (name id) x))

(defmethod luxvalue :string-vec
  [_ _ id xs]
  (format "\"string %s\" [%s]\n"
          (name id)
          (apply str (interpose " " (map #(format "\"%s\"" %) xs)))))

(defmethod luxvalue :color
  [_ _ id [r g b]]
  (format "\"color %s\" [%1.6f %1.6f %1.6f]\n" (name id) (float r) (float g) (float b)))

(defmethod luxvalue :log-color
  [_ _ id [col scale depth]]
  (let [[r g b] (scaled-absorption-color-at-depth col scale depth)]
    (format "\"color %s\" [%1.6f %1.6f %1.6f]\n" (name id) r g b)))

(defmethod luxvalue :renderer
  [_ scene id opts]
  (luxentity scene "Renderer" id opts))

(defmethod luxvalue :sampler
  [_ scene id opts]
  (luxentity scene "Sampler" id opts))

(defmethod luxvalue :integrator
  [_ scene id opts]
  (luxentity scene "SurfaceIntegrator" id opts))

(defmethod luxvalue :volume-integrator
  [_ _ id _]
  (format "VolumeIntegrator \"%s\"\n\n" id))

(defmethod luxvalue :accelerator
  [_ scene id opts]
  (luxentity scene "Accelerator" id opts))

(defmethod luxvalue :film
  [_ scene id opts]
  (luxentity scene "Film" id opts))

(defmethod luxvalue :area-light
  [_ scene id opts]
  (let [[stype sopts] (:__shape opts)]
    (str
     (luxentity scene "AreaLightSource" "area" opts)
     (luxvalue stype scene (str id "-mesh") sopts))))

(defmethod luxvalue :light
  [_ scene id opts]
  (luxattrib
   scene id "light" opts
   (str
    (when-let [g (:__parent opts)] (format "LightGroup \"%s\"\n" g))
    (luxvalue (:__type opts) scene id opts))))

(defmethod luxvalue :camera
  [_ scene id opts]
  (let [{[ex ey ez] :eye [tx ty tz] :target [ux uy uz] :up} (:__lookat opts)]
    (str
     (format "LookAt %1.6f %1.6f %1.6f  %1.6f %1.6f %1.6f  %1.6f %1.6f %1.6f\n\n"
             ex ey ez tx ty tz ux uy uz)
     (luxentity scene "Camera" id opts))))

(defmethod luxvalue :material
  [_ scene id opts]
  (luxentity scene "MakeNamedMaterial" id opts))

(defmethod luxvalue :volume
  [_ scene id opts]
  (str
   (format "MakeNamedVolume \"%s\" \"%s\"\n" (name id) (name (:__type opts)))
   (luxvalues scene opts)
   "\n"))

(defmethod luxvalue :trimesh
  [_ scene id {:keys [__mesh]}]
  (let [verts (vec (keys (:vertices __mesh)))
        vidx (zipmap verts (range))
        indices (mapcat (fn [[a b c]] [(vidx a) (vidx b) (vidx c)]) (:faces __mesh))]
    (luxentity
     scene "Shape" "trianglemesh"
     {:indices [:int-vec (vec indices)]
      :P [:point-vec verts]
      :name [:string id]})))

(defmethod luxvalue :plymesh
  [_ scene id {:keys [__mesh __basename path filename] :as opts}]
  (when __mesh
    (let [path (or (second filename) (str __basename ".ply"))]
      (prn "exporting ply mesh: " id path)
      (with-open [out (io/output-stream path)]
        (mio/write-ply out __mesh))))
  (luxentity scene "Shape" "plymesh" opts))

(defmethod luxvalue :stlmesh
  [_ scene id {:keys [__mesh __basename path filename] :as opts}]
  (when __mesh
    (let [path (or (second filename) (str __basename ".stl"))]
      (prn "exporting stl mesh: " id path)
      (with-open [out (io/output-stream path)]
        (mio/write-stl out __mesh))))
  (luxentity scene "Shape" "stlmesh" opts))

(defmethod luxvalue :shape
  [_ scene id {:keys [__type] :as opts}]
  (luxattrib
   scene id "shape" opts
   (str
    (if ((set (vals mesh-types)) __type)
      (luxvalue __type scene id opts)
      (luxentity scene "Shape" __type opts)))))

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
(defn serialize-lxs
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

(defn serialize-lxm
  [{:keys [materials]} base-path]
  (when materials
    (let [path (str base-path ".lxm")
          body (str (lx-header path) materials)]
      (spit path body)
      body)))

(defn serialize-lxv
  [{:keys [volumes]} base-path]
  (when volumes
    (let [path (str base-path ".lxv")
          body (str (lx-header path) volumes)]
      (spit path body)
      body)))

(defn serialize-lxo
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

(defn- append
  ([scene group id opts]
     (append scene group id opts false))
  ([scene group id opts singleton?]
     (-> (if singleton? (dissoc scene group) scene)
         (assoc-in [group id] (or opts {})))))

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
  [scene id & {:as opts}]
  {:pre [(kw-or-str? id)]}
  (append
   scene :materials (name id)
   (merge
    (material-common opts)
    {:type [:string "null"]})))

(defn material-matte
  [scene id & {:keys [diffuse diffuse-hsb sigma]
               :or {diffuse [1.0 1.0 1.0] sigma 0} :as opts}]
  {:pre [(kw-or-str? id)
         (color? diffuse) (optional color? diffuse-hsb)
         (number? sigma)]}
  (append
   scene :materials (name id)
   (merge
    (material-common opts)
    {:type [:string "matte"]
     :Kd [:color (if diffuse-hsb (hsb->rgb diffuse-hsb) diffuse)]
     :sigma [:float sigma]})))

(defn material-matte-translucent
  [scene id & {:keys [reflect reflect-hsb transmit transmit-hsb sigma conserve?]
               :or {reflect [0.3 0.3 0.3] transmit [0.65 0.65 0.65] sigma 0 conserve? true}
               :as opts}]
  {:pre [(kw-or-str? id)
         (color? reflect) (optional color? reflect-hsb)
         (color? transmit) (optional color? transmit-hsb)
         (number? sigma)]}
  (append
   scene :materials (name id)
   (merge
    (material-common opts)
    {:type [:string "mattetranslucent"]
     :Kr [:color (if reflect-hsb (hsb->rgb reflect-hsb) reflect)]
     :Kt [:color (if transmit-hsb (hsb->rgb transmit-hsb) transmit)]
     :sigma [:float sigma]
     :energyconserving [:bool conserve?]})))

(defn volume
  [scene id & {:keys [type ior absorb absorb-hsb abs-scale abs-depth]
               :or {type :clear absorb [1.0 1.0 1.0]
                    abs-scale 1.0 abs-depth 1.0 ior :air}}]
  {:pre [(kw-or-str? id)
         (volume-types type)
         (kw-or-num? ior) (optional keyword? presets/ior-presets ior)
         (color? absorb) (optional color? absorb-hsb)
         (number? abs-scale) (number? abs-depth)]}
  (append
   scene :volumes (name id)
   {:__type type
    :fresnel [:float (if (keyword ior) (presets/ior-presets ior) ior)]
    :absorption [:log-color [(if absorb-hsb (hsb->rgb absorb-hsb) absorb)
                             abs-scale abs-depth]]}))

(defn volume-integrator
  [scene id]
  {:pre [(volume-integrators id)]}
  (append-singleton scene :volume-integrator (name id) {}))

(defn renderer-slg
  [scene & {:keys [cpu? gpu? ]}]
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

(defn sampler-sobol
  [scene & {:keys [noise-aware]}]
  (append-singleton
   scene :sampler "sobol"
   {:noiseaware [:bool (optional-bool noise-aware true)]}))

(defn- integrator-common
  [{:keys [shadow-rays light-strategy] :or {shadow-rays 1 light-strategy :auto}}]
  {:pre [(number? shadow-rays) (light-strategies light-strategy)]}
  {:shadowraycount [:int shadow-rays]
   :lightstrategy [:string (name light-strategy)]})

(defn integrator-bidir
  [scene & {:keys [eye-depth light-depth light-rays path-strategy]
            :or {eye-depth 16 light-depth 16 light-rays 1 path-strategy :auto}
            :as opts}]
  {:pre [(number? eye-depth) (number? light-depth) (number? light-rays)
         (light-path-strategies path-strategy)]}
  (append-singleton
   scene :integrator "bidirectional"
   (merge
    (integrator-common opts)
    {:eyedepth [:int eye-depth]
     :lightdepth [:int light-depth]
     :lightraycount [:int light-rays]
     :lightpathstrategy [:string (name path-strategy)]})))

(defn integrator-sppm
  [scene & {:keys [max-eye max-photon
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
         (sppm-accelerators accel) (pixel-samplers pixel-sampler)
         (photon-samplers photon-sampler)]}
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

(defn accelerator-qbvh
  [scene & {:as opts}]
  (append-singleton scene :accel "qbvh" (or opts {})))

(defn film
  [scene & {:keys [width height gamma
                   white red green blue
                   suffix
                   write-flm? restart-flm?
                   write-exr? exr-channels exr-imaging? exr-zbuf?
                   write-png? png-channels png-16bit?
                   write-tga? tga-channels
                   premultiply?
                   ldr-method
                   write-interval display-interval
                   halt-spp halt-time halt-threshold
                   response]
            :or {width 1280 height 720 gamma 2.2 suffix "out"
                 white [0.314275 0.329411] red [0.63 0.34] green [0.31 0.595] blue [0.155 0.07]
                 write-flm? true restart-flm? true
                 write-exr? false exr-channels "RGBA" exr-imaging? true exr-zbuf? true
                 write-png? true png-channels "RGB" png-16bit? false
                 write-tga? false tga-channels "RGB"
                 premultiply? false
                 ldr-method "cut"
                 write-interval 180 display-interval 12
                 halt-spp 0 halt-time 0 halt-threshold 0}}]
  (append-singleton
   scene :film "fleximage"
   (merge
    (get-in scene [:film "fleximage"])
    {:xresolution [:int width]
     :yresolution [:int height]
     :gamma [:float gamma]
     :filename [:string suffix]
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
     :haltspp [:int halt-spp]
     :halttime [:int halt-time]
     :haltthreshold [:int halt-threshold]}
    (when response
      {:cameraresponse [:string (if (keyword? response)
                                  (response presets/film-response-presets)
                                  response)]}))))

(defn tonemap-linear
  [scene & {:keys [iso exposure f-stop gamma]
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
  [scene & {:keys [type eye target up
                   fov lens-radius focal-dist blades auto-focus?
                   shutter-open shutter-close]
            :or {type "perspective" fov 60
                 lens-radius 0 blades 0
                 shutter-open 0 shutter-close 0.041666
                 eye [0 -10 0] target [0 0 0]}}]
  (let [opts {:fov [:float fov]
              :shutteropen [:float shutter-open]
              :shutterclose [:float shutter-close]
              :lensradius [:float lens-radius]
              :blades [:int blades]
              :__lookat {:eye (g/vec3 eye) :target (g/vec3 target)
                         :up (if up
                               (g/vec3 up)
                               (g/normalize (g/cross (g/sub (g/vec3 eye) (g/vec3 target)) g/V3_X)))}}
        opts (if focal-dist
               (assoc opts
                 :focaldistance [:float focal-dist]
                 :autofocus [:bool false])
               (if auto-focus?
                 (assoc opts :autofocus [:bool true])
                 opts))]
    (append-singleton scene :camera type opts)))

(defn- make-transform-matrix
  [{:keys [scale rx ry rz axis theta translate] :or {rx 0 ry 0 rz 0} :as tx}]
  (let [mat (if translate
              (g/translate g/IDENTITY44 (g/vec3 translate))
              g/IDENTITY44)
        mat (if axis
              (g/rotate-around-axis mat axis theta)
              (if (some (complement zero?) [rx ry rz])
                (-> mat
                    (g/rotate-x (convert-angle rx))
                    (g/rotate-y (convert-angle ry))
                    (g/rotate-z (convert-angle rz)))
                mat))
        mat (if scale (g/scale mat scale) mat)]
    (vals (g/transpose mat))))

;; TODO is scene needed as arg?
(defn- transform-common
  [scene {:keys [matrix] :as tx}]
  {:__transform (if matrix matrix (make-transform-matrix tx))})

(defn light-group
  [scene id & {:keys [gain] :or {gain 1.0}}]
  (append scene :light-groups id {:__gain [:float gain]}))

(defn- light-common
  [scene {:keys [group color gain power efficacy importance tx material hidden?]
          :or {group "default" color [1.0 1.0 1.0]
               gain 1.0 efficacy 10.0 power 100.0 importance 1.0 hidden? false}}]
  {:__parent group
   :__material (or material (when hidden? "__hidden__"))
   :L [:color color]
   :gain [:float (if group
                   (* (get-in scene [:light-groups group :__gain 1] 1.0) gain)
                   gain)]
   :power [:float power]
   :efficacy [:float efficacy]
   :importance [:float importance]})

(defn area-light
  [scene id & {:keys [samples mesh mesh-type p n size tx]
               :or {samples 1 mesh-type :inline} :as opts}]
  (let [mesh (if mesh
               mesh
               (g/as-mesh (pl/plane (or p [0 0 10]) (or n [0 0 -1])) {:size size}))]
    (append
     scene :lights id
     (merge
      (when tx (transform-common scene tx))
      (light-common scene opts)
      {:__type :area-light
       :__shape [(mesh-types mesh-type) {:__mesh mesh :__basename (str "light-" id)}]
       :nsamples [:int samples]}))))

(defn shape-disk
  [scene id & {:keys [z radius inner-radius phi tx material]
               :or {z 0 radius 1 inner-radius 0 phi 360}}]
  (append
   scene :geometry id
   (merge
    (when tx (transform-common scene tx))
    {:__type :disk
     :__material material
     :name [:string id]
     :height [:float z]
     :radius [:float radius]
     :innerradius [:float inner-radius]
     :phimax [:float phi]})))

(defn ply-mesh
  [scene id & {:keys [mesh path tx material smooth]}]
  (append
   scene :geometry id
   (merge
    (when tx (transform-common scene tx))
    {:__type :plymesh
     :__material material
     :__mesh mesh
     :name [:string id]
     :filename [:string (or path (str id ".ply"))]
     :smooth [:bool smooth]})))

(defn stl-mesh
  [scene id & {:keys [mesh path tx material]}]
  (append
   scene :geometry id
   (merge
    (when tx (transform-common scene tx))
    {:__type :stlmesh
     :__material material
     :__mesh mesh
     :name [:string id]
     :filename [:string (or path (str id ".stl"))]})))

(defn lux-scene
  []
  (-> {}
      (renderer-sppm)
      (sampler-sobol)
      (integrator-sppm)
      (volume-integrator :multi)
      (accelerator-qbvh)
      (film)
      (light-group "default")
      (material-null "__hidden__")))

(comment
  (def lxs (-> (lux-scene)
               (camera :eye [10 -2 10] :target [0 0 0] :up [0 0 1])
               (film :width 640 :height 360 :display-interval 5 :halt-spp 50)
               (volume "inside" :type :clear :absorb [0.972 0.8 0.7] :abs-depth 1 :ior 2.04)
               (area-light "left" :p [0 0 5] :size 1 :gain 1 :tx {:translate [-5 0 0] :ry -20})
               (area-light "top" :p [0 0 7] :size 6 :gain 0.5 :tx {:translate [0 4 0] :rx -45})
               (area-light "right" :p [0 0 5] :size 1 :gain 1 :tx {:translate [5 0 0] :ry 20})
               (shape-disk "floor" :radius 20 :material "white")
               ;;(shape-disk "d1" :radius 3 :inner-radius 2 :material "red" :tx {:rx -30})
               ;;(shape-disk "d2" :radius 3 :inner-radius 2 :material "orange" :tx {:rx 30})
               (stl-mesh "map" :material "orange-trans" :tx {:scale [5 3 1] :translate [0 0 1]} :mesh (g/extrude (r/rect -1 -1 2 2) {:depth 1 :scale 0.8}))
               (material-matte "white" :diffuse [0.8 0.8 0.8])
               (material-matte "red" :diffuse [1.0 0 0])
               (material-matte "orange" :diffuse [1.0 0.3 0])
               (material-matte-translucent "orange-trans" :interior "inside" :transmit [0.9 0.3 0.05] :reflect [0.3 0.3 0.3] :conserve? true)
               ;;(serialize-scene "foo" false)
               ))
  )
