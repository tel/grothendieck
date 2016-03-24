(ns grothendieck.core
  (:require
    [org.httpkit.server :as httpkit]
    [honeysql.core :as sql]
    [bidi.bidi :as bidi]
    [bidi.ring :as bidi-ring]
    [cheshire.core :as cheshire]
    [liberator.core :as liberator]
    [liberator.representation :as repr]
    [liberator.dev :as liberator-dev]
    [clj-jwt.core  :as jwt]
    [clj-time.core :as t]))

(defn as-sql [it & {:as params}]
  (sql/format
    it
    :params params
    :allow-dashed-names? true
    :quoting :ansi))

(def querymap
  {:select [:a :b :c]
   :from   [:foo]
   :where  [:and
            [:= :f.a (sql/param :a)]
            [:= :f.a (sql/param :a)]]})

; -----------------------------------------------------------------------------
; Fake User database

(defonce +users+ (atom {}))
(defonce +user-id-max+ (atom 0))

(defn reset-users!
  "Reset the User database to where it began."
  []
  (reset! +users+ {})
  (reset! +user-id-max+ 0))

; -----------------------------------------------------------------------------

(defn random-string []
  (apply
    str (map (fn [_] (rand-nth "abcdefghijklmnopqrstuvwxyz"))
             (range 10))))

; BIG HACK!
(declare routes)

(def +secret+ "ALAsetouayspdiotyasoeutnasoientaset")

(defn verify-jwt
  "TODO: This should verify ISS, AUD, IAT!"
  [token]
  (try
    (let [jwt (jwt/str->jwt token)]
      (when (jwt/verify jwt :HS256 +secret+) jwt))
    (catch Exception _
      nil)))

(def default-resource-map
  {:authorized?
   (fn [ctx]
     (if-let [token (get-in ctx [:request :headers "authorization"])]
       (if-let [jwt (verify-jwt token)]
         [true {::claims (:claims jwt)}]
         [false {:message "Cannot verify Authorziation token"
                 :headers {"WWW-Authorization" "AHHHHHH"}}])
       [false {:message "Require Authorization header"}]))})

(defn get-body
  "Returns a string for the body. If there was no request body then the empty
   string is returned."
  [ctx]
  (if-let [body-reader (get-in ctx [:request :body])]
    (slurp body-reader)
    ""))

(def user-set
  (liberator/resource
    default-resource-map
    :available-media-types ["application/json"]
    :allowed-methods [:get :post]

    :valid-content-header?
    (fn [ctx]
      (let [ct (get-in ctx [:request :content-type])]
        (= ct "application/json")))

    :malformed?
    (fn [ctx]
      (let [body (get-body ctx)]
        (try
          [false {::request-body (cheshire/parse-string body)}]
          (catch Exception e
            [true {:message (str e)}]))))

    :exists? (fn [_]
               [false {::data (into []
                                    (map (fn [[id data]] (assoc data :id id)))
                                    @+users+)}])

    :handle-ok (fn [ctx] (cheshire/generate-string (::data ctx)))

    :post! (fn [ctx]
             (let [id (swap! +user-id-max+ inc)]
               (swap!
                 +users+ assoc
                 id {:body (::request-body ctx)
                     :etag (random-string)})
               {::id id}))

    :post-redirect? (fn [ctx]
                      {:location (bidi/path-for
                                   routes :user-single
                                   :user-id (::id ctx))})))

(def user-single
  (liberator/resource
    :available-media-types ["text/plain"]
    :allowed-methods [:get]
    :malformed? (fn [ctx]
                  (try
                    (let [user-id-str (get-in ctx [:request :route-params :user-id])]
                      [false {::user-id (Integer/parseInt user-id-str)}])
                    (catch Exception e
                      [true {:message (str e)}])))

    :exists? (fn [ctx]
               (when-let [user (get @+users+ (::user-id ctx))]
                 [true {::data user}]))
    :etag (fn [ctx] (get-in ctx [::data :etag]))
    :handle-ok (fn [ctx] (get-in ctx [::data :body]))))

; -----------------------------------------------------------------------------

(def routes
  ["/" {"user"             (bidi/tag user-set :user-set)
        ["user/" :user-id] (bidi/tag user-single :user-single)}])

; -----------------------------------------------------------------------------

(def app
  "Regular Ring application generated from Bidi routes returning (sub-)Ring
  applications themselves."
  (-> (bidi-ring/make-handler routes)
      (liberator-dev/wrap-trace :header)))

; -----------------------------------------------------------------------------
; Development code for fast REPL reloading

(defonce +server+
  (atom nil))

(defn go []
  (reset! +server+ (httpkit/run-server #'app {:port 12345})))

(defn stop []
  (when-not (nil? @+server+)
    (@+server+ :timeout 100)
    (reset! +server+ nil)))

(defn restart []
  (print "restarting...")
  (stop)
  (go))

(restart)
