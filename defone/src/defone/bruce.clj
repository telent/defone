(ns defone.bruce
  (:refer-clojure)
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as shell]))

;;;; Welcome to Bruce Groveller

;;; extracts structure layouts from C header files by compiling small
;;; C programs that refer to them and that produce output in some
;;; format that Clojure can understand

;;; TODO

;; clean up temporary files after use
;; cope with unions as well as structs
;; nested structs may some day be necessary
;; write some kinda query api?
;; split up namespaces so the `stat` data isn't here
;; better fn names
;; documentation never killed anyone
;; deal with cflags/libs properly
;; obey the :compiler option
;; implement aliases

(def types-to-grovel (atom {}))

(defn- canonize-member [options m]
  (let [name (if (coll? m) (first m) m)
        aliases (if (coll? m) (conj (rest m) name) [name])]
    (list
     (clojure.string/join [(get options :prefix "") name])
     (vec aliases))))

(defmacro define-c-record [record-name header-files options & members]
  (let [n {
           :record-name (name record-name)
           :header-files header-files
           :members `(quote ~(map (partial canonize-member options) members))
           :options options
           }]
    `(swap! types-to-grovel assoc ~(name record-name) ~n)
    ))

(define-c-record stat ["sys/types.h" "sys/stat.h" "unistd.h"]
  {;; :search-path ["/usr/include"]
   ;; :compiler-flags (:out (shell/sh "pkg-config ftgl --cflags --libs"))
   :compiler "gcc"
   :prefix "st_"
   }
  [dev device] ; for each member, name as known to C then aliases
  [ino inode]
  mode
  nlink
  uid
  gid
  size
  atime
  mtime
  ctime)

(defn headers-for [specs]
  (distinct (mapcat :header-files (vals specs))))

(defn grovel-struct [struct-spec]
  (let [s (:record-name struct-spec)]
    (clojure.string/join
     "\n"
     (map #(str "    printf(\"[" s " %s \" FMT_SIZE \" \" FMT_SIZE \"]\\n\", "
                "\"" (first %) "\""
                ", "
                "offsetof(struct " s ", " (first %) ")"
                ", "
                "member_sizeof(struct " s ", " (first %) ")"
                ");")
          (:members struct-spec)))))

(defn groveller-program [record-specs]
  (concat (map #(str "#include <" % ">")
               (conj (headers-for record-specs) "stdio.h" "stddef.h"))
          [
           "#define member_sizeof(type, member) sizeof(((type *)0)->member)"
           "#define FMT_SIZE \"%lu\""
           "int main() {"
           "    printf(\"[\");"
           ]
          (map grovel-struct (vals record-specs))
          [
           "    printf(\"]\");"
           "    exit(0);"
           "}"]))

(defn grovel [record-specs]
  (let [tmpfile (java.io.File/createTempFile "grovel_" ".c")
        tmpout (str (.getPath tmpfile) ".out")
        cc-line (concat ["/usr/bin/gcc"
                         "-o" tmpout
                         (.getPath tmpfile)]
                        (distinct (filter identity
                                          (map #(get % :compiler-flags)
                                               (vals record-specs)))))]
    (spit tmpfile (clojure.string/join "\n" (groveller-program record-specs)))
    (println cc-line)
    (let [cc (apply shell/sh cc-line)]
      (if (zero? (:exit cc))
        (let [result (shell/sh tmpout)]
          (and (zero? (:exit result))
               (reduce (fn [m [record el size offset]]
                         (assoc-in m [record el] [size offset]))
                       {}
                       (edn/read-string (:out result)))))
        cc))))

(def grovelled-types (grovel @types-to-grovel))
