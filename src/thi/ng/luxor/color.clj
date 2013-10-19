(ns thi.ng.luxor.color
  (:import
   [java.awt Color]))

(defn rgbvec
  [rgb]
  (let [i8 (/ 1.0 255)]
    [(* i8 (bit-and (bit-shift-right rgb 16) 0xff))
     (* i8 (bit-and (bit-shift-right rgb 8) 0xff))
     (* i8 (bit-and rgb 0xff))]))

(defn hsb->rgb
  [[h s b]]
  (let [rgb (bit-and (Color/HSBtoRGB (float h) (float s) (float b)) 0xffffff)
        i8 (/ 1.0 255)]
    [(* i8 (bit-and (bit-shift-right rgb 16) 0xff))
     (* i8 (bit-and (bit-shift-right rgb 8) 0xff))
     (* i8 (bit-and rgb 0xff))]))

(defn scaled-absorption-at-depth
  [x scale d]
  (* (/ (Math/log (max x 1e-30)) d) scale (if (= x 1.0) 1 -1)))

(defn scaled-absorption-color-at-depth
  [rgb scale d]
  (mapv #(scaled-absorption-at-depth % scale d) rgb))
