(ns thi.ng.luxor.core
  (:require
   [thi.ng.luxor.presets :as presets]
   [thi.ng.common.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.plane :as pl]
   [thi.ng.geom.mesh :as mesh])
  (:import
   [java.io File Writer OutputStreamWriter StringWriter]
   [java.util Date]))

(def ^:const version "0.1.0-SNAPSHOT")

(def ^:dynamic *indent* 2)

(defn scaled-absorption-at-depth
  [x scale d]
  (* (/ (Math/log (max x 1e-30)) d) scale (if (= x 1.0) 1 -1)))

(defn scaled-absorption-color-at-depth
  [rgb scale d]
  (mapv #(scaled-absorption-at-depth % scale d) rgb))

(declare luxvalue)

(defn- filter-opts
  [xs] (->> xs (filter #(not (.startsWith (name (key %)) "__"))) (into {})))

(defn- luxvalues
  [opts]
  (let [indent (apply str (repeat *indent* \space))]
    (->> opts
         (filter-opts)
         (into (sorted-map))
         (reduce-kv
          (fn [s k [type v]] (str s indent (luxvalue type k v))) ""))))

(defn- luxvalues-typed
  [type coll]
  (reduce (fn [s [k v]] (str s (luxvalue type k v))) "" (into (sorted-map) coll)))

(defn- luxentity
  [type id opts]
  (prn type)
  (format "%s \"%s\"\n%s\n" type (name id) (luxvalues opts)))

(defn- luxattrib
  [id body]
  (format "AttributeBegin # %s\n%sAttributeEnd\n\n" id body))

(defn- luxtransform
  [tx]
  (format "Transform [%s]\n"
          (apply str (interpose " " (map #(format "%1.8f" %) tx)))))

(defmulti luxvalue (fn [type & _] type))

(defmethod luxvalue :float
  [_ id x] (format "\"float %s\" [%1.8f]\n" (name id) (float x)))

(defmethod luxvalue :float-vec
  [_ id xs]
  (format "\"float\" \"%s\" [%s]\n"
          (name id)
          (apply str (interpose " " (map #(format "%1.8f" %) xs)))))

(defmethod luxvalue :int
  [_ id x] (format "\"integer %s\" [%d]\n" (name id) (int x)))

(defmethod luxvalue :bool
  [_ id x] (format "\"bool %s\" [\"%b\"]\n" (name id) x))

(defmethod luxvalue :string
  [_ id x] (format "\"string %s\" [\"%s\"]\n" (name id) x))

(defmethod luxvalue :string-vec
  [_ id xs]
  (format "\"string\" \"%s\" [%s]\n"
          (name id)
          (apply str (interpose " " (map #(format "\"%s\"" %) xs)))))

(defmethod luxvalue :color
  [_ id [r g b]]
  (format "\"color %s\" [%1.6f %1.6f %1.6f]\n" (name id) (float r) (float g) (float b)))

(defmethod luxvalue :log-color
  [_ id [col scale depth]]
  (let [[r g b] (scaled-absorption-color-at-depth col scale depth)]
    (format "\"color %s\" [%1.6f %1.6f %1.6f]\n" (name id) r g b)))

(defmethod luxvalue :renderer
  [_ id opts]
  (luxentity "Renderer" id opts))

(defmethod luxvalue :sampler
  [_ id opts]
  (luxentity "Sampler" id opts))

(defmethod luxvalue :integrator
  [_ id opts]
  (luxentity "SurfaceIntegrator" id opts))

(defmethod luxvalue :volume-integrator
  [_ id _]
  (format "VolumeIntegrator \"%s\"\n\n" id))

(defmethod luxvalue :accelerator
  [_ id opts]
  (luxentity "Accelerator" id opts))

(defmethod luxvalue :film
  [_ id opts]
  (luxentity "Film" id opts))

(defmethod luxvalue :light
  [_ id opts]
  (luxattrib
   id
   (str
    (when (:__transform opts) (luxtransform (:__transform opts)))
    (when-let [g (:__parent opts)]
      (format "LightGroup \"%s\"\n" g))
    (luxentity (:__type opts) id opts))))

(defmethod luxvalue :camera
  [_ id opts]
  (let [{[ex ey ez] :eye [tx ty tz] :target [ux uy uz] :up} (:__lookat opts)]
    (str
     (format "LookAt %1.6f %1.6f %1.6f  %1.6f %1.6f %1.6f  %1.6f %1.6f %1.6f\n\n"
             ex ey ez tx ty tz ux uy uz)
     (luxentity "Camera" id opts))))

(defmethod luxvalue :material
  [_ id opts]
  (luxentity "MakeNamedMaterial" id opts))

(defmethod luxvalue :volume
  [_ id opts]
  (str
   (format "MakeNamedVolume \"%s\" \"%s\"\n" (name id) (name (:__type opts)))
   (luxvalues opts)
   "\n"))

(defn- lx-header
  [path]
  (format "# %s\n# generated %s by luxor v%s\n\n"
          path (.toString (Date.)) version))

(defn- path-filename
  [path] (.getName (File. path)))

(defn- include-file
  [path]
  (format "Include \"%s\"\n\n" path))

;; TODO rename into export-scene & split out serialize part
(defn serialize-lxs
  [scene base-path]
  (let [base-name (path-filename base-path)
        lxs-path (str base-path ".lxs")
        lxs (str
             (lx-header lxs-path)
             (:renderer scene)
             (:accel scene)
             (:sampler scene)
             (:integrator scene)
             (:volume-integrator scene)
             (:film scene)
             (:camera scene)
             "WorldBegin\n\n"
             (when (:volumes scene) (include-file (str base-name ".lxv")))
             (when (:materials scene) (include-file (str base-name ".lxm")))
             (when (:geometry scene) (include-file (str base-name ".lxo")))
             (:lights scene)
             "\nWorldEnd\n")]
    (spit lxs-path lxs)
    lxs))

(defn serialize-lxm
  [{:keys [materials]} base-path]
  (when materials
    (let [lxm-path (str base-path ".lxm")
          lxm (str (lx-header lxm-path) materials)]
      (spit lxm-path lxm)
      lxm)))

(defn serialize-lxv
  [{:keys [volumes]} base-path]
  (when volumes
    (let [lxm-path (str base-path ".lxv")
          lxm (str (lx-header lxm-path) volumes)]
      (spit lxm-path lxm)
      lxm)))

(defn serialize-lxo
  [{:keys [geometry]} base-path]
  (when geometry
    ;; TODO
    ))

(defn serialize-scene
  [scene base-path]
  (let [scene* (reduce
                (fn [s [k type]]
                  (assoc s k (luxvalues-typed type (k scene))))
                {} {:renderer :renderer
                    :accel :accelerator
                    :sampler :sampler
                    :integrator :integrator
                    :volume-integrator :volume-integrator
                    :film :film
                    :camera :camera
                    :lights :light
                    :materials :material
                    :volumes :volume})
        lxs (serialize-lxs scene* base-path)
        lxm (serialize-lxm scene* base-path)
        lxo (serialize-lxo scene* base-path)
        lxv (serialize-lxv scene* base-path)]
    (apply str lxs lxm lxo lxv)))

(defn- append
  ([scene group id opts]
     (append scene group id opts false))
  ([scene group id opts singleton?]
     (-> (if singleton? (dissoc scene group) scene)
         (assoc-in [group id] (or opts {})))))

(defn material-null
  [scene id]
  (append scene :materials (name id) {:type [:string "null"]} true))

(defn material-matte
  [scene id & {:keys [diffuse sigma] :or {diffuse [1.0 1.0 1.0] sigma 0}}]
  (append
   scene :materials (name id)
   {:type [:string "matte"]
    :Kd [:color diffuse]
    :sigma [:float sigma]}))

(defn material-matte-translucent
  [scene id & {:keys [reflect transmit sigma conserve?]
               :or {reflect [0.3 0.3 0.3] transmit [0.65 0.65 0.65] sigma 0 conserve? true}}]
  (append
   scene :materials (name id)
   {:type [:string "mattetranslucent"]
    :Kr [:color reflect]
    :Kt [:color transmit]
    :sigma [:float sigma]
    :energyconserving [:bool conserve?]}))

(defn volume
  [scene id & {:keys [type fresnel absorb abs-scale abs-depth]
               :or {type "clear" fresnel 1.000293 absorb [1.0 1.0 1.0]
                    abs-scale 1.0 abs-depth 1.0}}]
  (append
   scene :volumes (name id)
   {:__type type
    :fresnel [:float fresnel]
    :absorption [:log-color [absorb abs-scale abs-depth]]}))

(defn volume-integrator
  [scene id]
  {:pre [(#{:none :single :emission :multi} id)]}
  (append scene :volume-integrator (name id) {} true))

;; "string config" ["opencl.gpu.use = 1" "opencl.cpu.use = 1"]

(defn renderer-slg
  [scene & {:keys [cpu? gpu?] :or {cpu? true gpu? true}}]
  (append
   scene :renderer "slg"
   {:config [:string-vec [(str "opencl.cpu.use = " cpu?)
                          (str "opencl.gpu.use = " gpu?)]]}
   true))

(defn renderer-sppm
  [scene]
  (append scene :renderer "sppm" {} true))

(defn sampler-sobol
  [scene & {:keys [noise-aware] :or {noise-aware true}}]
  (append scene :sampler "sobol" {:noiseaware [:bool noise-aware]} true))

(defn integrator-bidir
  [scene & {:keys [eye-depth light-depth light-rays light-strategy path-strategy]
            :or {eye-depth 16 light-depth 16 light-rays 1
                 light-strategy "auto" path-strategy "auto"}}]
  (append
   scene :integrator "bidirectional"
   {:eyedepth [:int eye-depth]
    :lightdepth [:int light-depth]
    :lightraycount [:int light-rays]
    :lightpathstrategy [:string path-strategy]
    :lightstrategy [:string light-strategy]}
   true))

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
                 env? true direct-light? true glossy? false use-prob? true
                 wave-passes 8 accel "hybridhashgrid"
                 pixel-sampler "hilbert" photon-sampler "halton"}}]
  (append
   scene :integrator "sppm"
   {:maxeyedepth [:int max-eye]
    :maxphotondepth [:int max-photon]
    :photonperpass [:int photons]
    :hitpointperpass [:int hit-points]
    :startradius [:float start-radius]
    :alpha [:float alpha]
    :includeenvironment [:bool env?]
    :directlightsampling [:bool direct-light?]
    :storeglossy [:bool glossy?]
    :useproba [:bool use-prob?]
    :wavelengthstratificationpasses [:int 8]
    :lookupaccel [:string accel]
    :pixelsampler [:string pixel-sampler]
    :photonsampler [:string photon-sampler]}
   true))

(defn accelerator-qbvh
  [scene & {:as opts}]
  (append scene :accel "qbvh" (or opts {}) true))

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
  (append
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
     :write_resume_film [:bool write-flm?]
     :restart_resume_film [:bool restart-flm?]
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
     :filmwriteinterval [:int write-interval]
     :displayinterval [:int display-interval]
     :haltspp [:int halt-spp]
     :halttime [:int halt-time]
     :haltthreshold [:int halt-threshold]}
    (when response
      {:cameraresponse [:string (if (keyword? response)
                                  (response presets/film-response-presets)
                                  response)]}))
   true))

(defn tonemap-linear
  [scene & {:keys [iso exposure f-stop gamma]
            :or {iso 100 exposure 0.37 f-stop 4 gamma 1.2}}]
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
    (append scene :camera type opts true)))

(defn transform-common
  [scene {:keys [matrix scale rotate translate] :as tx}]
  {:__transform tx})

(defn light-group
  [scene id & {:keys [gain] :or {gain 1.0}}]
  (append scene :light-groups id {:__gain [:float gain]}))

(defn- light-common
  [scene {:keys [group color gain power efficacy importance tx]
          :or {group "default" color [1 1 1]
               gain 1 efficacy 10 power 100 importance 1}}]
  {:__parent group
   :L [:color color]
   :gain [:float (if group
                   (* (get-in scene [:light-groups group :__gain 1] 1.0) gain)
                   gain)]
   :power [:float power]
   :efficacy [:float efficacy]
   :importance [:float importance]})

(defn area-light
  [scene id & {:keys [samples mesh p n size tx] :or {samples 1} :as opts}]
  (let [mesh (if mesh
               mesh
               (g/as-mesh (pl/plane (or p [0 0 10]) (or n [0 0 -1])) size))]
    (append
     scene :lights id
     (merge
      (when tx (transform-common scene tx))
      (light-common scene opts)
      {:__type "AreaLightSource"
       :__shape mesh
       :nsamples [:int samples]}))))

(defn lux-scene
  []
  (-> {}
      (renderer-sppm)
      (sampler-sobol)
      (integrator-sppm)
      (volume-integrator :multi)
      (accelerator-qbvh)
      (film)
      (light-group "default")))
