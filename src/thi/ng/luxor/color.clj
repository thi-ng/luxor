(ns thi.ng.luxor.color
  (:import
   [java.awt Color]))

(def ^:const inv8bit (/ 1.0 255))

(defn rgb24->rgb
  [rgb]
  [(* inv8bit (bit-and (bit-shift-right rgb 16) 0xff))
   (* inv8bit (bit-and (bit-shift-right rgb 8) 0xff))
   (* inv8bit (bit-and rgb 0xff))])

(defn hsb->rgb
  [[h s b]]
  (let [rgb (bit-and (Color/HSBtoRGB (float h) (float s) (float b)) 0xffffff)]
    [(* inv8bit (bit-and (bit-shift-right rgb 16) 0xff))
     (* inv8bit (bit-and (bit-shift-right rgb 8) 0xff))
     (* inv8bit (bit-and rgb 0xff))]))

(defn scaled-absorption-at-depth
  [x scale d]
  (* (/ (Math/log (max x 1e-30)) d) (if (= x 1.0) scale (- scale))))

(defn scaled-absorption-color-at-depth
  [rgb scale d]
  (mapv #(scaled-absorption-at-depth % scale d) rgb))
