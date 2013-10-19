(ns thi.ng.luxor.compiler
  (:require
   [thi.ng.luxor.config :as conf]
   [thi.ng.luxor.color :as col]
   [thi.ng.geom.meshio :as mio]
   [clojure.java.io :as io]))

(declare luxvalue)

(defn- material-ref
  [scene id]
  (let [{:keys [__interior __exterior]} (get-in scene [:materials id])]
    (str
     (format "NamedMaterial \"%s\"\n" id)
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
          (fn [s k [type v]] (str s indent (luxvalue type scene k v))) ""))))

(defn luxvalues-typed
  [scene type coll]
  (reduce (fn [s [k v]] (str s (luxvalue type scene k v))) "" (into (sorted-map) coll)))

(defn luxentity
  [scene type id opts]
  (format "%s \"%s\"\n%s\n" type (name id) (luxvalues scene opts)))

(defn luxtransform
  [tx]
  (format "Transform [%s]\n"
          (apply str (interpose " " (map #(format "%1.8f" %) tx)))))

(defn luxattrib
  [scene id type {:keys [__transform __material]} body]
  (let [sep (format "# -------- %s: %s --------\n" type id)
        inject (str
                (when __transform (luxtransform __transform))
                (when __material (material-ref scene __material)))]
    (format "%sAttributeBegin\n%s%sAttributeEnd\n%s\n" sep inject body sep)))

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
  (let [[r g b] (col/scaled-absorption-color-at-depth col scale depth)]
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
    (if ((set (vals conf/mesh-types)) __type)
      (luxvalue __type scene id opts)
      (luxentity scene "Shape" __type opts)))))
