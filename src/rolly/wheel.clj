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

(defn draw-wheel
  [^Graphics2D g ^Rectangle ins ndivs dr]
  (let [radius (quot (.getWidth ins) 2)
        tau (* 2 Math/PI)
        div-angle (/ tau ndivs)
        identity-tx (.getTransform g)
        font-height (.getHeight (.getStringBounds (.getFontMetrics g) "1" g))
        center-size (* 0.2 radius)]

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
      #_(.drawLine g 0 0 (int (* wheel-inner-frac radius)) 0)
      (.drawLine g (int center-size) 0 (int radius) 0)
      (.rotate g (/ div-angle -2))
      #_(.rotate g (/ tau 4))
      (let [mtx (.getTransform g)
            c (.getColor g)
            width-of-slice (* wheel-inner-frac radius (Math/sin div-angle))]
        (.translate g (- radius font-height) 0.0)
        (.rotate g (/ tau 4))
        #_(.drawString g (str i) (int 0) (int 0))
        #_(prn 'width-of-slice width-of-slice)
        #_(draw-string-centered g (str i) (/ width-of-slice 2) 0)
        (draw-string-centered g (str (inc i)) 0 0)
        #_(.translate g (/ width-of-slice 2) 0.0)
        #_(.setColor g Color/red)
        #_(.fillOval g -1 -1 2 2)
        
        (.setTransform g mtx)
        (.setColor g c))
      #_(.rotate g (/ tau -4))
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
          duration 10
          fps 60.0
          nframes (int (* fps duration))
          ending-frames (* fps 1.4)
          ;; radians per second
          dtheta 10

          img-center-x (/ w 2.0)
          img-center-y (+ (/ h 2.0) pointer-font-height clip-height)
          itx (.getTransform g)]

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
             (.rotate g (Math/max 0.0
                                  (- 150.0 (* dur-frac dur-frac dtheta ))))
             (.translate g (- 0 (quot (.getWidth wheel-image) 2)) (- 0 (quot (.getHeight wheel-image) 2)))
             (.drawImage g wheel-image 0 0 nil)
             (.setTransform g itx)
             (draw-string-centered g "⇓" (int (/ w 2)) (int (- pointer-font-height fudge))))))
       (+ nframes ending-frames)
       "-"))))

