(ns thi.ng.luxor.config)

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
