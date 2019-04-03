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
           [java.util Random])
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

(def token (:access_token (get-token)))
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
    #_(prn 'DISPATCHY op t)
    (case t
      "MESSAGE_CREATE" (let [{:keys [channel_id content]} d]
                         (when-let [[_ maxroll] (re-find #"\!bigroll\s+(\d+)" content)]
                           ;; TODO FIX OFF BY ONE LOL
                           (let [bound (BigInteger. maxroll)
                                 rolled (-> (BigInteger. (.bitLength bound) my-random)
                                            (.mod bound))]
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

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [{gateway :url} (:body (client/get
                               "https://discordapp.com/api/gateway"
                               {:headers {:authorization token}
                                :as :json}))

        ws-client (WebSocketClient. (SslContextFactory.))
        heartbeat-interval (promise)
        msg-seq-num (atom nil)
        _ (.setMaxTextMessageSize (.getPolicy ws-client) (* 64 1024))
        _ (.start ws-client)
        conn-url (str gateway "?v=6&encoding=json")
        _ (prn 'connecting-to conn-url)
        conn (ws/connect conn-url
                         :client ws-client
                         :on-receive (fn [r]
                                       (let [data (json/parse-string r keyword)]
                                         #_(println "RECEIVED " (count r))
                                         #_(swap! all-received-messages conj data)
                                         (when-let [i (-> data :d :heartbeat_interval)]
                                           (deliver heartbeat-interval i))
                                         (when-let [s (-> data :s)]
                                           (reset! msg-seq-num s))
                                         (try
                                           (#'dispatch-event data)
                                           (catch Throwable e
                                             (println "!!! exception " (.getMessage e))
                                             (.printStackTrace e))))))]

    (def the-client ws-client)
    (let [heartbeat-thread (future
                             (println "waiting for interval...")
                             (prn 'interval-is @heartbeat-interval)
                             (while true
                               #_(prn 'sending-heartbeat)
                               (ws/send-msg conn (json/generate-string {:op 1 :d @msg-seq-num}))
                               (Thread/sleep @heartbeat-interval)))]
      ;; send identify
      (deref heartbeat-interval)
      (Thread/sleep 111)
      (prn 'identifying!!)
      (ws/send-msg conn
                   (json/generate-string
                    {:op 2
                     :d {"token" (:bot-token config)
                         "properties" {"$os" "hehe"
                                       "$browser" "lol"
                                       "$device" "xD"
                                       "$referrer" ""
                                       "$referring_domain" ""}
                         "presence" {"game" {"name" "no way this works"
                                             "type" 0}}}}
                    {:pretty true})))))
