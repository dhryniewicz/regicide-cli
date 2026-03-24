(ns regicide.online.auth
  "Firebase Anonymous Authentication for CLI client.
   Persists credentials to ~/.regicide/auth.edn for reuse across sessions."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [com.google.firebase FirebaseApp FirebaseOptions]
           [com.google.firebase.auth FirebaseAuth]
           [java.io File]))

(def ^:private config-dir
  (str (System/getProperty "user.home") "/.regicide"))

(def ^:private auth-file
  (str config-dir "/auth.edn"))

(defn- ensure-config-dir! []
  (.mkdirs (File. ^String config-dir)))

(defn save-credentials!
  "Save auth credentials to disk for reuse across sessions."
  [creds]
  (ensure-config-dir!)
  (spit auth-file (pr-str creds)))

(defn load-credentials
  "Load saved auth credentials from disk, or nil if not found."
  []
  (let [f (File. ^String auth-file)]
    (when (.exists f)
      (edn/read-string (slurp f)))))

(defn clear-credentials!
  "Remove saved credentials."
  []
  (let [f (File. ^String auth-file)]
    (when (.exists f)
      (.delete f))))
