(ns rolly.videoencoder
  (:require [clojure.java.io :as io])
  (:import [java.io EOFException BufferedReader File BufferedInputStream DataInputStream DataOutputStream ByteArrayOutputStream ByteArrayInputStream]
           [java.util.concurrent BlockingQueue ArrayBlockingQueue ConcurrentLinkedQueue SynchronousQueue]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.awt Color]))

(defn spawn
  ^Process [args]
  (let [pb (ProcessBuilder. (into-array args))
        p (.start pb)]
    p))

(defn spawn*
  [& args]
  (spawn (keep (comp not-empty str) (flatten args))))

(defn encode-video-queue-sync
  "output file is expected as first of `ffmpeg-options`"
  [^BlockingQueue buffer-queue width height framerate output-file & {:keys [input-options output-options]}]
  (let [pix-fmt "rgb24"
        proc (spawn* "ffmpeg"
                     "-f" "rawvideo"
                     "-pix_fmt" pix-fmt
                     "-s:v" (str width "x" height)
                     "-r" (str framerate)
                     input-options
                     "-i" "-"
                     "-c:v" "vp8"
                     "-b:v" "2M"
                     ;; "-crf" "4"
                     "-minrate" "2M"
                     "-maxrate" "2M"
                     "-f" "webm"
                     output-options
                     "-y"

                     output-file)
        err (future
              (try
                (let [^BufferedReader reader (io/reader (io/input-stream (.getErrorStream proc)))]
                  (loop []
                    (let [line (.readLine reader)]
                      (when line
                        (prn 'encoder-err line)
                        (recur)))))
                (catch Throwable t
                  (println 'err-slurper-threw! (.getMessage t))
                  (.printStackTrace t))))
        in (future
             (try
               (let [baos (ByteArrayOutputStream.)]
                 (io/copy (io/input-stream (.getInputStream proc)) (io/output-stream baos))
                 (.toByteArray baos))
               (catch Throwable t
                 (println 'in-slurper-threw! (.getMessage t))
                 (.printStackTrace t))))
        out (.getOutputStream proc)]
    (loop []
      (let [buffer (.take buffer-queue)]
        (when (not= :done buffer)
          (try
            (.write out ^bytes buffer)
            (catch Exception e
              (.printStackTrace e)
              (println @err)))
          (recur))))
    (.close out)
    (.waitFor proc)
    @in))

(defn encode-video
  [^BufferedImage canvas draw-frame-fn nframes output]
  (let [encoder-frame-queue (ArrayBlockingQueue. 4)
        width (.getWidth canvas)
        height (.getHeight canvas)
        nbufs 4
        frame-bufs (vec (repeatedly nbufs #(byte-array (* 3 width height))))
        encoder-thread (future
                         (try (encode-video-queue-sync encoder-frame-queue (.getWidth canvas) (.getHeight canvas) 60 output)
                              (catch Throwable t
                                (println "encoder threw!" (.getMessage t))
                                (.printStackTrace t))))
        frame-pusher (future
                       (dotimes [i nframes]
                         (draw-frame-fn i)
                         (let [buf (nth frame-bufs (rem i nbufs))]
                           (.getDataElements (.getRaster canvas) 0 0 width height buf)
                           (.put encoder-frame-queue buf)))
                       (.put encoder-frame-queue :done))]
    @encoder-thread))

(comment
  (let [img (BufferedImage. 1000 1000 BufferedImage/TYPE_3BYTE_BGR)
        g (.createGraphics img)
        buf (byte-array (* 3 (.getWidth img) (.getHeight img)))]
    (prn '_hash (java.util.Arrays/hashCode buf))
    (.setColor g Color/red)

    (let [video-bytes (encode-video
                       img
                       (fn [i]
                         (.fillRect g 0 0 i i))
                       1000
                       "-"
                       #_"myvideo__.mp4")]
      (with-open [out (io/output-stream (io/file "video-bytes-vp8.webm"))]
        (io/copy video-bytes out)))))

