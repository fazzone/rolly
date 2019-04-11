(ns rolly.wheel
  (:require [clojure.java.io :as io]
            [rolly.videoencoder :as videoencoder])
  (:import [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.awt RenderingHints Font Color Rectangle Graphics2D]
           [java.awt.geom AffineTransform]))

(def wheel-inner-frac
  "the rim of the wheel will have a width equal to (* radius (- 1 wheel-inner-frac))"
  0.8)

(defn draw-circle-centered
  [^Graphics2D g cx cy r]
  (let [diam (+ r r)]
   (.fillOval g (- cx r) (- cy r) diam diam)))

(defn draw-string-centered
  [^Graphics2D g s x y]
  (let [bounds (.getStringBounds (.getFontMetrics g) s g)]
    (.drawString g s (int (- x (/ (.getWidth bounds) 2.0))) y)))

(defn biggest-font
  [^Graphics2D g max-width n]
  (last
   (for [font-size (range 8 100 4)
         :let [_ (.setFont g (Font. "DejaVu Sans Light" Font/PLAIN font-size))
               w (.getWidth (.getStringBounds (.getFontMetrics g) (str n) g))]
         :when (< w max-width)]
     (.getFont g))))

(defn draw-wheel
  [^Graphics2D g ^Rectangle ins ndivs dr]
  (let [radius (quot (.getWidth ins) 2)
        tau (* 2 Math/PI)
        div-angle (/ tau ndivs)
        identity-tx (.getTransform g)
        
        center-size (* 0.2 radius)
        width-of-slice (* wheel-inner-frac radius (Math/sin div-angle))
        _ (.setFont g (biggest-font g (* 0.7 width-of-slice) ndivs))
        font-height (.getHeight (.getStringBounds (.getFontMetrics g) "9" g))]

    (.translate g (.getCenterX ins) (.getCenterY ins))
    (.scale g 1 (/ (double (.getHeight ins)) (.getWidth ins)))
    
    #_(.setColor g Color/white)
    #_(draw-circle-centered g 0 0 radius)

    (.setColor g  Color/white #_(Color/decode "#ae81ff") )
    (.rotate g (/ tau -4))
    (.rotate g dr)
    (draw-circle-centered g 0 0 (int center-size))

    (dotimes [i ndivs]
      (.rotate g (/ div-angle 2))

      (.drawLine g (int center-size) 0 (int radius) 0)
      (.rotate g (/ div-angle -2))

      (let [mtx (.getTransform g)]
        (.translate g (- radius font-height) 0.0)
        (.rotate g (/ tau 4))
        (draw-string-centered g (str (inc i)) 0 0) 
        (.setTransform g mtx))
      (.rotate g div-angle))
    (.setTransform g identity-tx)))




(defn make-wheel-image
  [w h n]
  (let [wheel-image (BufferedImage. w h BufferedImage/TYPE_3BYTE_BGR)
        g (doto (.createGraphics wheel-image)
            (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING
                               RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
            (.setRenderingHint RenderingHints/KEY_RENDERING
                               RenderingHints/VALUE_RENDER_QUALITY)
            (.setRenderingHint RenderingHints/KEY_ANTIALIASING
                               RenderingHints/VALUE_ANTIALIAS_ON))]
    (.setFont g (Font. "DejaVu Sans Light" Font/PLAIN 18))
    (draw-wheel g (Rectangle. 0 0 w h) n 0)
    wheel-image))

(defn spin-video-bytes
  [n]
  (let [w 444
        h 444
        img (BufferedImage. w h BufferedImage/TYPE_3BYTE_BGR)
        g (.createGraphics img)
        clip-wfrac 0.4
        clip-hfrac 0.1
        clip-width (* w clip-wfrac)
        clip-height (* h clip-hfrac)

        orig-font (.getFont g)
        pointer-font-size 20
        _ (.setFont g (Font. "DejaVu Sans Light" Font/PLAIN pointer-font-size))
        pointer-font-height (.getHeight (.getStringBounds (.getFontMetrics g) "1" g))
        fudge 4
        wheel-image (make-wheel-image w h n)]


    (let [subimage (.getSubimage img (int (- (/ w 2) (/ clip-width 2)))
                                 0
                                 clip-width
                                 (+ pointer-font-height (int clip-height)))
          duration 6
          fps 60.0
          nframes (int (* fps duration))
          ending-frames (* fps 1.4)
          dtheta 10

          img-center-x (/ w 2.0)
          img-center-y (+ (/ h 2.0) pointer-font-height clip-height)
          itx (.getTransform g)
          offset (* 2 Math/PI (Math/random))]

      (videoencoder/encode-video
       img
       (fn [i] 
         (when (< i nframes)
           (let [dur-frac (- 1 (/ i (double nframes)))]
             (.setColor g Color/black)
             (.fillRect g 0 0 w h)
             (.setColor g Color/white)
             (.translate g 0. pointer-font-height)
             (.translate g (quot (.getWidth wheel-image) 2) (quot (.getHeight wheel-image) 2))
             (.rotate g offset)
             (.rotate g (Math/max 0.0
                                  (- 150.0 (* dur-frac dur-frac dtheta ))))
             (.translate g (- 0 (quot (.getWidth wheel-image) 2)) (- 0 (quot (.getHeight wheel-image) 2)))
             (.drawImage g wheel-image 0 0 nil)
             (.setTransform g itx)
             (draw-string-centered g "⇓" (int (/ w 2)) (int (- pointer-font-height fudge))))))
       (+ nframes ending-frames)
       "-"))))


(comment
  (with-open [out (io/output-stream (io/file "spinfont.webm"))]
    (io/copy (spin-video-bytes 10) out)))
