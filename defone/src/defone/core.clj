(ns defone.core
  (:require [clojure.core.async :as async
             :refer [chan >!! <!! >! <! go ]]
            [clojure.java.io :as io])
  (:import [java.io FileReader FileInputStream]))


(defn chan-from-file [channel name]
  (let [r (FileInputStream. name)]
    (future
      (let [buf (byte-array 80)]
        (loop []
          (if (let [bytes (.read r buf 0 80)]
                (if (> bytes 0) (>!! channel (take bytes (seq buf))))
                (>= bytes 0))
            (recur)))) )
    channel))

;; 4 bytes timestamp secs, 4 bytes usec, u16 type, u16 code, s32 value
;; -7 -27 0 0 / -6 -58 9 0 / 3 0 / 57 0 / -99 8 0 0 /
;; -7 -27 0 0 / 25 -57 9 0 / 3 0 / 48 0 / 14 0 0 0
;; -7 -27 0 0 / 25 -57 9 0 / 3 0 / 53 0 / 21 1 0 0
;; -7 -27 0 0 / 25 -57 9 0 / 3 0 / 54 0 / 126 3 0 0
;; -7 -27 0 0 / 25 -57 9 0 / 0 0 / 0 0  / 0 0 0 0

(defn parse-events [out-chan in-chan]
  (go (loop []
        ;; XXX this does not account for the need to resync
        ;; if we somehow started other than on a packet boundary
        (let [bytess (partition 16 (<! in-chan))]
          ;; XXX we should actually parse the protocol at this point,
          ;; not just copy it onto the output channel
          (async/onto-chan out-chan bytess nil))
        (recur)))
  out-chan)

#_
(let [bytes (chan-from-file (chan) "/dev/input/event1")
      events (parse-events (chan) bytes)]
  (go (while true (println (<! events)))))
