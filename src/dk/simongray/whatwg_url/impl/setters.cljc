(ns ^:no-doc dk.simongray.whatwg-url.impl.setters
  "The setters of the URL class of the URL Standard, section 6.1, which
  change a URL map as the basic URL parser does from a state of its own,
  e.g. the hostname state for the hostname setter."
  (:require [clojure.string :as str]
            [dk.simongray.whatwg-url.impl.chars :as chars]
            [dk.simongray.whatwg-url.impl.parser :as parser]
            [dk.simongray.whatwg-url.params :as params]
            [dk.simongray.whatwg-url.percent :as percent]))

(defn- with-parse
  "The `url` as the basic URL parser changes it for a setter that starts
  in the `state` with the text `s`."
  [url state s]
  (:url (parser/run s {:url url :override state})))

(defn- no-credentials?
  "Whether the `url` cannot have a username, password or port."
  [{:keys [scheme host]}]
  (or (nil? host) (= "" host) (= "file" scheme)))

(defn- with-userinfo
  "The `url` with its username or password `k` set to the text `v`,
  percent-encoded."
  [url k v]
  (if (no-credentials? url)
    url
    (parser/put url k (percent/encode (chars/scalar-values v) percent/userinfo-set))))

(defn- with-part
  "The `url` with its query or fragment `k` set by the text `v`, without
  the `delimiter` that starts it, as the parser reads it in the `state`."
  [url k state delimiter v]
  (if (= "" v)
    (dissoc url k)
    (with-parse (assoc url k "") state (cond-> v (str/starts-with? v delimiter) (subs 1)))))

(defn no-attribute
  "The exception for the attribute `k`, which the URL class doesn't have."
  [k]
  (ex-info (str "The URL class has no attribute " k) {:attribute k}))

(defn set-attribute
  "The `url` with the attribute `k` set to `v`, as its setter in the URL
  class sets it: name-value pairs for :search-params, and the text of `v`
  for the others."
  [url k v]
  (if (= :search-params k)
    (parser/put url :query (not-empty (params/serialize v)))
    (let [v       (str v)
          opaque? (string? (:path url))]
      (case k
        :href     (or (parser/parse v nil) url)
        :protocol (with-parse url :scheme-start (str v ":"))
        :username (with-userinfo url :username v)
        :password (with-userinfo url :password v)
        :host     (if opaque? url (with-parse url :host v))
        :hostname (if opaque? url (with-parse url :hostname v))
        :port     (cond
                    (no-credentials? url) url
                    (= "" v)              (dissoc url :port)
                    :else                 (with-parse url :port v))
        :pathname (if opaque? url (with-parse (assoc url :path []) :path-start v))
        :search   (with-part url :query :query "?" v)
        :hash     (with-part url :fragment :fragment "#" v)
        (throw (no-attribute k))))))

(defn set-attributes
  "The `url` with the attribute `k` set to `v`, and then each attribute of
  the pairs `kvs` in turn, as set-attribute sets them, with its parts in
  the order they're written in."
  [url k v kvs]
  ;; a pair at a time, rather than through the lazy seqs of partition
  (loop [url url k k v v kvs kvs]
    (let [url (set-attribute url k v)]
      (if kvs
        (recur url (first kvs) (second kvs) (nnext kvs))
        (parser/in-order url)))))
