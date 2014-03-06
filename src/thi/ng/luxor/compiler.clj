(ns thi.ng.luxor.compiler
  (:require
   [thi.ng.luxor.config :as conf]
   [thi.ng.luxor.color :as col]
   [thi.ng.geom.meshio :as mio]
   [clojure.java.io :as io]))

(declare emit)

(defn- material-ref
  [scene id]
  (let [{:keys [__interior __exterior]} (get-in scene [:materials id])]
    (str
     (format "NamedMaterial \"%s\"\n" (name id))
     (when __interior (format "Interior \"%s\"\n" __interior))
     (when __exterior (format "Exterior \"%s\"\n" __exterior)))))

(defn filter-opts
  [xs] (->> xs (filter #(not (.startsWith (name (key %)) "__"))) (into {})))

(defn luxvalues
  [scene opts]
  (let [indent (apply str (repeat conf/*indent* \space))]
    (->> opts
         (filter-opts)
         (into (sorted-map))
         (reduce-kv
          (fn [s k [type v]] (str s indent (emit type scene k v))) ""))))

(defn luxvalues-typed
  [scene type coll]
  (reduce (fn [s [k v]] (str s (emit type scene k v))) "" (into (sorted-map) coll)))

(defn luxentity
  [scene type id opts]
  (format "%s \"%s\"\n%s\n" type (name id) (luxvalues scene opts)))

(defn luxtransform
  [tx]
  (format "Transform [%s]\n"
          (apply str (interpose " " (map #(format conf/*float-format* %) tx)))))

(defn luxattrib
  [scene id type {:keys [__transform __material]} body]
  (let [sep-start (format "# -------- %s-start: %s --------\n" type (name id))
        sep-end (format "# -------- %s-end: %s --------\n" type (name id))
        inject (str
                (when __transform (luxtransform __transform))
                (when __material (material-ref scene __material)))]
    (format "%sAttributeBegin\n%s%sAttributeEnd\n%s\n"
            sep-start inject body sep-end)))

(defmulti emit (fn [type & _] type))

(defmethod emit :float
  [_ _ id x] (format (str "\"float %s\" [" conf/*float-format* "]\n") (name id) (float x)))

(defmethod emit :float-vec
  [_ _ id xs]
  (format "\"float %s\" [%s]\n"
          (name id)
          (apply str (interpose " " (map #(format conf/*float-format* (float %)) xs)))))

(defmethod emit :int
  [_ _ id x] (format "\"integer %s\" [%d]\n" (name id) (int x)))

(defmethod emit :int-vec
  [_ _ id xs]
  (format "\"integer %s\" [%s]\n"
          (name id)
          (apply str (interpose " " (map #(format "%d" (int %)) xs)))))

(defmethod emit :point-vec
  [_ _ id xs]
  (format "\"point %s\" [%s]\n"
          (name id)
          (apply str (interpose " " (map #(format conf/*float-format* (float %))
                                         (mapcat identity xs))))))

(defmethod emit :bool
  [_ _ id x] (format "\"bool %s\" [\"%b\"]\n" (name id) x))

(defmethod emit :string
  [_ _ id x] (format "\"string %s\" [\"%s\"]\n" (name id) x))

(defmethod emit :string-vec
  [_ _ id xs]
  (format "\"string %s\" [%s]\n"
          (name id)
          (apply str (interpose " " (map #(format "\"%s\"" %) xs)))))

(defmethod emit :color
  [_ _ id [r g b]]
  (let [ff conf/*float-format*]
    (format (str "\"color %s\" [" ff " " ff " " ff "]\n")
            (name id) (float r) (float g) (float b))))

(defmethod emit :log-color
  [_ _ id [col scale depth]]
  (let [[r g b] (col/scaled-absorption-color-at-depth col scale depth)
        ff conf/*float-format*]
    (format
     (str "\"color %s\" [" ff " " ff " " ff "]\n") (name id) r g b)))

(defmethod emit :renderer
  [_ scene id opts]
  (luxentity scene "Renderer" id opts))

(defmethod emit :sampler
  [_ scene id opts]
  (luxentity scene "Sampler" id opts))

(defmethod emit :integrator
  [_ scene id opts]
  (luxentity scene "SurfaceIntegrator" id opts))

(defmethod emit :volume-integrator
  [_ _ id _]
  (format "VolumeIntegrator \"%s\"\n\n" id))

(defmethod emit :accelerator
  [_ scene id opts]
  (luxentity scene "Accelerator" id opts))

(defmethod emit :filter
  [_ scene id opts]
  (luxentity scene "PixelFilter" id opts))

(defmethod emit :film
  [_ scene id opts]
  (luxentity scene "Film" id opts))

(defmethod emit :area-light
  [_ scene id opts]
  (let [[stype sopts] (:__shape opts)]
    (str
     (luxentity scene "AreaLightSource" "area" opts)
     (emit stype scene (str id "-mesh") sopts))))

(defmethod emit :light
  [_ scene id {group :__parent :as opts}]
  (let [opts (if group
               (update-in opts [:gain 1] * (get-in scene [:light-groups group :__gain] 1.0))
               opts)]
    (luxattrib
     scene id "light" opts
     (str
      (when group (format "LightGroup \"%s\"\n" group))
      (emit (:__type opts) scene id opts)))))

(defmethod emit :camera
  [_ scene id opts]
  (let [{[ex ey ez] :eye [tx ty tz] :target [ux uy uz] :up} (:__lookat opts)
        ff conf/*float-format*
        ff3 (str ff " " ff " " ff " ")]
    (str
     (format (str "LookAt " ff3 ff3 ff3 "\n\n") ex ey ez tx ty tz ux uy uz)
     (luxentity scene "Camera" id opts))))

(defmethod emit :material
  [_ scene id opts]
  (luxentity scene "MakeNamedMaterial" id opts))

(defmethod emit :volume
  [_ scene id opts]
  (str
   (format "MakeNamedVolume \"%s\" \"%s\"\n" (name id) (name (:__type opts)))
   (luxvalues scene opts)
   "\n"))

(defmethod emit :trimesh
  [_ scene id {:keys [__mesh]}]
  (let [verts (vec (keys (:vertices __mesh)))
        vidx (zipmap verts (range))
        indices (mapcat (fn [[a b c]] [(vidx a) (vidx b) (vidx c)]) (:faces __mesh))]
    (luxentity
     scene "Shape" "trianglemesh"
     {:indices [:int-vec (vec indices)]
      :P [:point-vec verts]
      :name [:string id]})))

(defmethod emit :plymesh
  [_ scene id {:keys [__mesh __basename __export-path filename] :as opts}]
  (when __mesh
    (let [path (or __export-path (second filename) (str __basename ".ply"))]
      (prn "exporting ply mesh: " id path)
      (with-open [out (io/output-stream path)]
        (mio/write-ply out __mesh))))
  (luxentity scene "Shape" "plymesh" opts))

(defmethod emit :stlmesh
  [_ scene id {:keys [__mesh __basename __export-path filename] :as opts}]
  (when __mesh
    (let [path (or __export-path (second filename) (str __basename ".stl"))]
      (prn "exporting stl mesh: " id path)
      (with-open [out (io/output-stream path)]
        (mio/write-stl out __mesh))))
  (luxentity scene "Shape" "stlmesh" opts))

(defmethod emit :shape
  [_ scene id {:keys [__type] :as opts}]
  (luxattrib
   scene id "shape" opts
   (str
    (if ((set (vals conf/mesh-types)) __type)
      (emit __type scene id opts)
      (luxentity scene "Shape" __type opts)))))
