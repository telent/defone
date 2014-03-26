(ns defone.nrepl
  (:require [clojure.tools.nrepl.server :as nrepl]))

(defonce server (nrepl/start-server :port 9990))

(defn -main []
  (println ["server " server]))

(defn stop []
  (nrepl/stop-server server))
