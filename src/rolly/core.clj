(ns rolly.core
  (:require [gniazdo.core :as ws]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clj-http.client :as client]
            [org.httpkit.server :as httpkit]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [com.hypirion.clj-xchart :as chart])
  (:import [org.eclipse.jetty.util.ssl SslContextFactory]
           [org.eclipse.jetty.websocket.client WebSocketClient]
           [java.math BigInteger]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.awt Graphics2D]
           [java.io ByteArrayOutputStream ByteArrayInputStream]
           [java.util Date Random]
           [java.util.zip Inflater])
  (:gen-class))

(def discord-api-endpoint "https://discordapp.com/api/v6")
(def config (edn/read-string (slurp "config.edn")))

(defn get-token
  []
  (-> (str discord-api-endpoint "/oauth2/token")
      (client/post
       {:basic-auth [(:client-id config) (:client-secret config)]
        :headers {"Content-Type" "application/x-www-form-urlencoded"}
        :form-params {"grant_type" "client_credentials"
                      :scope "identify connections"}
        :as :json})
      :body))

(defonce my-random (Random. (System/nanoTime)))

(defn chart-image-bytes
  [w h chart]
  (let [img (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        out (ByteArrayOutputStream.)]
    (.paint chart (.getGraphics img) w h)
    (ImageIO/write img "png" out)
    (.toByteArray out)))

(defn dice-roll-histogram
  [nrolls dsize]
  (let [the-rolls (repeatedly nrolls #(inc (rand-int dsize)))
        ->freq (frequencies the-rolls)
        possible-rolls (range 1 (inc dsize))
        average (/ (reduce + the-rolls)
                   (double nrolls))]
    (chart/category-chart
     {"Thing" {:x possible-rolls
               :y (map ->freq possible-rolls)}}

     {:title (format "%sd%s distribution, sample mean = %.02f" nrolls dsize average)
      :theme :ggplot2
      :render-style :bar
      :legend {:visible? false}})))

(defn send-message
  [channel-id message]
  (client/post (str "https://discordapp.com/api/channels/" channel-id "/messages")
               {:body (json/generate-string {:content message
                                             :nonce (str (System/currentTimeMillis))
                                             :tts false})
                :headers {"Authorization" (str "Bot " (:bot-token config))}
                :content-type :json
                :accept :json}))

(defn post-message-with-file
  [channel-id message name content]
  (client/post (str "https://discordapp.com/api/channels/" channel-id "/messages")
               {:multipart [{:name "content" :content message}
                            {:name "nonce" :content (str (System/currentTimeMillis))}
                            {:name "tts" :content "false"}
                            {:name name :part_name "file" :content content}]
                :headers {"Authorization" (str "Bot " (:bot-token config))}}))

(defn post-embed
  [channel-id embed]
  (client/post (str "https://discordapp.com/api/channels/" channel-id "/messages")
               {:body (json/generate-string {:content ""
                                             :nonce (str (System/nanoTime))
                                             :tts false
                                             :embed embed})
                :content-type :json
                :headers {"Authorization" (str "Bot " (:bot-token config))}}))

(defn map->embed!
  [channel-id kvs]
  (let [[[title desc] & more] kvs]
    (post-embed channel-id {:title title
                            :description desc
                            :fields (for [[k v] more]
                                      {:name k
                                       :value v
                                       :inline? true})})))

(defonce all-received-messages (atom []))

(defn dispatch-event
  [{:keys [d t s op]}]
  (when (zero? op)
    (case t
      "MESSAGE_CREATE" (let [{:keys [channel_id content]} d]
                         (when-let [[_ ^String maxroll] (re-find #"\!bigroll\s+(\d+)" content)]
                           (let [bound (BigInteger. maxroll)
                                 rolled (-> (BigInteger. (.bitLength bound) ^Random my-random)
                                            (.mod bound)
                                            (.add BigInteger/ONE))]
                             (send-message channel_id (str "rolled " rolled))))
                         (when-let [[_ s_nrolls s_dsize] (re-find #"\!roll\s(\d+)\s*d\s*(\d+)$" content)]
                           (let [nrolls (Integer/parseInt s_nrolls)
                                 dsize (Integer/parseInt s_dsize)
                                 the-rolls (repeatedly nrolls #(inc (rand-int dsize)))]
                             (send-message channel_id
                                           (string/join "\n"
                                                        [(str "rolls: " (string/join " + " the-rolls))
                                                         (str "total: " (reduce + the-rolls))]))))
                         (when-let [[_ nrolls dsize] (re-find #"\!hist\s(\d+)d(\d+)$" content)]
                           (->> (dice-roll-histogram (Integer/parseInt nrolls) (Integer/parseInt dsize))
                                (chart-image-bytes 555 555)
                                (post-message-with-file channel_id "" (format "%sd%s.png" nrolls dsize))))
                         (when-let [[_ s_nrolls s_dsize] (re-find #"\!avgroll\s(\d+)\s*d\s*(\d+)$" content)]
                           (let [nrolls (Integer/parseInt s_nrolls)
                                 dsize (Integer/parseInt s_dsize)
                                 average-single-roll (/ (inc dsize) 2.0)
                                 average-total-roll (* nrolls average-single-roll)]
                             (send-message channel_id (format "true average roll of %sd%s = `%s`" nrolls dsize average-total-roll)))))
      nil)))

(defn get-gateway-url
  []
  (let [{:keys [access_token]} (get-token)
        {gateway :url} (:body (client/get
                               "https://discordapp.com/api/gateway"
                               {:headers {:authorization access_token}
                                :as :json}))]
    (str gateway "?v=6&encoding=json")))

(defn connect
  [dispatch-fn]
  (let [heartbeat-interval (promise)
        msg-seq-num (atom nil)
        last-hb-time (atom nil)
        last-ack-time (atom nil)
        running? (atom true)
        inflate-buffer (byte-array (* 128 1024))
        inflater (Inflater.)
        process-msg (fn [msg]
                      (println (format "[%s] %s" (Date.) (dissoc msg :d)))
                      (flush)
                      (when-let [i (-> msg :d :heartbeat_interval)]
                        (deliver heartbeat-interval i))
                      (when-let [s (-> msg :s)]
                        (reset! msg-seq-num s))
                      (when (= 11 (:op msg))
                        (reset! last-ack-time (System/currentTimeMillis)))
                      (try (dispatch-fn msg)
                           (catch Throwable e (.printStackTrace e))))
        conn (ws/connect (get-gateway-url)
                         :client (doto (WebSocketClient. (SslContextFactory.))
                                   (.start))
                         :on-binary (fn [b off len]
                                      (doto inflater
                                        (.reset)
                                        (.setInput b off len))
                                      (let [nbytes (.inflate inflater inflate-buffer)]
                                        (-> (ByteArrayInputStream. inflate-buffer 0 nbytes)
                                            (io/reader)
                                            (json/parse-stream keyword)
                                            (process-msg))))
                         :on-error (fn [e]
                                     (prn 'error e))
                         :on-close (fn [status reason]
                                     (prn 'closed! {:status status :reason reason}))
                         :on-receive (fn [s]
                                       (process-msg (json/parse-string s keyword))))
        do-close (fn []
                   (reset! running? false)
                   (ws/close conn))
        heartbeat-thread (future
                           @heartbeat-interval
                           (while @running?
                             (ws/send-msg conn (json/generate-string {:op 1 :d @msg-seq-num}))
                             (reset! last-hb-time (System/currentTimeMillis))
                             (Thread/sleep @heartbeat-interval)))
        monitor (future
                  @heartbeat-interval
                  (while (nil? @last-ack-time)
                    (Thread/sleep 111))
                  (while @running?
                    (when (< @last-ack-time @last-hb-time)
                      (println "ack timeout - closing!")
                      (do-close))
                    (Thread/sleep 4444)))]
    @heartbeat-interval
    (ws/send-msg conn
                 (json/generate-string
                  {:op 2
                   :d {"token" (:bot-token config)
                       "compress" true
                       "properties" {"$os" "hehe"
                                     "$browser" "lol"
                                     "$device" "xD"
                                     "$referrer" ""
                                     "$referring_domain" ""}
                       "presence" {"game" {"name" "no way this works"
                                           "type" 0}}}}))
    {:conn conn :running? running? :close do-close}))

(def the-client (atom nil))

(defn -main
  [& args]
  (reset! the-client (connect #'dispatch-event))
  (while true
    (Thread/sleep 999)))

(comment
  (def my-client (connect #'dispatch-event))
  (deref (:running? my-client))
  ((:close my-client)))
