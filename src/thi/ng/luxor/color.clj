(ns thi.ng.luxor.color)

;; TODO
(defn hsb->rgb
  [[h s b]] [h s b])

(defn scaled-absorption-at-depth
  [x scale d]
  (* (/ (Math/log (max x 1e-30)) d) scale (if (= x 1.0) 1 -1)))

(defn scaled-absorption-color-at-depth
  [rgb scale d]
  (mapv #(scaled-absorption-at-depth % scale d) rgb))
