(ns defone.core
  (:require [clojure.core.async :as async
             :refer [chan >!! <!! >! <! go ]]
            [net.n01se.clojure-jna :as jna]
            [clojure.java.io :as io])
  (:import [java.io FileReader FileInputStream]))

(def finished (atom false))

(defn chan-from-file [channel name]
  (let [r (FileInputStream. name)]
    (future
      (let [buf (byte-array 80)]
        (loop []
          (if (let [bytes (.read r buf 0 80)]
                (if (> bytes 0)
                  (let [out (map #(bit-and % 0xff)
                                 (take bytes buf))]
                    (>!! channel out)))
                (and (>= bytes 0) (not @finished)))
            (recur)))) )
    channel))

;; 4 bytes timestamp secs, 4 bytes usec, u16 type, u16 code, s32 value
;; -7 -27 0 0 / -6 -58 9 0 / 3 0 / 57 0 / -99 8 0 0 /
;; -7 -27 0 0 / 25 -57 9 0 / 3 0 / 48 0 / 14 0 0 0
;; -7 -27 0 0 / 25 -57 9 0 / 3 0 / 53 0 / 21 1 0 0
;; -7 -27 0 0 / 25 -57 9 0 / 3 0 / 54 0 / 126 3 0 0
;; -7 -27 0 0 / 25 -57 9 0 / 0 0 / 0 0  / 0 0 0 0



#_
(parse-event '(249 229 0 0 25 199 9 0 3 0 48 0 14 0 0 0))

(defn bytes->word [bytes]
  (reduce (fn [acc next] (bit-or (bit-shift-left acc 8) next))
          (reverse bytes)))

(def event-types
  (clojure.set/map-invert
   {:syn 0
    :key 1
    :rel 2
    :abs 3
    :msc 4
    :sw 5
    :led 0x11
    :snd 0x12
    :rep 0x14
    :ff 0x15
    :pwr 0x16
    :ff-status 0x17
    :max 0x1f}))

(def event-codes
  (clojure.set/map-invert
   {:slot 47
    :touch-major 48
    :position-x 53
    :position-y 54
    :tracking-id 57
    :pressure 58}))

(defn parse-event [bytes]
  (let [[sec0 sec1 sec2 sec3
         usec0 usec1 usec2 usec3
         type0 type1
         code0 code1
         v0 v1 v2 v3] bytes]
    {:sec (bytes->word [sec0 sec1 sec2 sec3])
     :usec (bytes->word [usec0 usec1 usec2 usec3])
     :type (get event-types (bytes->word [type0 type1]))
     :code (let [code (bytes->word [code0 code1])]
             (get event-codes code code))

     :value (bytes->word [v0 v1 v2 v3])}))

(defn parse-events [out-chan in-chan]
  (go (loop []
        ;; XXX this does not account for the need to resync
        ;; if we somehow started other than on a packet boundary
        (let [bytess (partition 16 (<! in-chan))]
          (async/onto-chan out-chan (map parse-event bytess) nil))
        (recur)))
  out-chan)

(defn read-stuff []
  (let [bytes (chan-from-file (chan) "/dev/input/event1")
        events (parse-events (chan) bytes)
        timeout (async/timeout 6000)]
    (go (loop []
          (and
           (async/alt!
            timeout (do (println "timeut") false)
            events ([v] (or (println v) true)))
           (recur))))))

#_
(read-stuff)

;;;;;;;;;;;;;; some opengl stuff

(defn matrix-rotate-z [angle]
  (let [c (Math/cos angle)
        s (Math/sin angle)]
    [[ c     s    0    0]
     [(- s)  c    0    0]
     [ 0     0    1    0]
     [ 0     0    0    1]]))

(defn matrix-scale [xs ys zs]
  [[xs 0  0  0]
   [ 0 ys 0  0]
   [ 0 0  zs 0]
   [ 0 0  0  1]])

;; matrix multiplication in baby steps

(defn matrix-transpose [a]
  (apply map vector a))

(defn matrix-multiply-cell [row-from-a col-from-b]
  (apply + (map * row-from-a col-from-b)))

(defn matrix-multiply-row [row-from-a b]
  (map #(matrix-multiply-cell row-from-a %) (matrix-transpose b)))

(defn matrix-multiply [a b]
  (map #(matrix-multiply-row % b) a))

(assert (= '((2 2)
             (2 2))
           (matrix-multiply
            [[2 0]
             [0 2]]
            [[1 1]
             [1 1]])))


;;;

(def GL_COMPILE_STATUS (int 0x8b81))
(def GL_LINK_STATUS (int 0x8B82))

(defn gl-make-shader [type text]
  (let [typenum (get {:fragment 0x8B30 :vertex 0x8B31} type)
        shader (int (jna/invoke Integer GLESv2/glCreateShader typenum))
        string (clojure.string/join "\n" text)
        compiled? (int-array [42])]
    (jna/invoke Integer GLESv2/glShaderSource
                shader
                (int 1)
                (into-array String [string])
                (int 0))
    (jna/invoke Integer GLESv2/glCompileShader shader)
    (jna/invoke Integer GLESv2/glGetShaderiv shader GL_COMPILE_STATUS compiled?)
    (println (seq compiled?))
    (and (not (zero? (aget compiled? 0))) shader)))


(defn gl-make-program [shaders]
  (let [program (int (jna/invoke Integer GLESv2/glCreateProgram))
        success? (int-array [0])]
    (doall (map #(jna/invoke Integer GLESv2/glAttachShader program %) shaders))
    (jna/invoke Integer GLESv2/glLinkProgram program)
    (jna/invoke Integer GLESv2/glGetProgramiv program GL_LINK_STATUS success?)
    (println (seq success?))
    (if (zero? (aget success? 0))
      (let [err (char-array 1000)
            len (int-array [0])]
        (jna/invoke Integer GLESv2/glGetProgramInfoLog
                    program (int 1000) len err)
        (println (clojure.string/trim (String. err)))
        nil)
      (do (jna/invoke Integer GLESv2/glUseProgram program) program))))

(defn create-shaders []
  (let [frag (gl-make-shader
              :fragment
              ["precision mediump float;"
               "varying vec4 v_color;"
               "void main() {"
               "gl_FragColor = v_color;"
               "}"])
        vert (gl-make-shader
              :vertex
              ["uniform mat4 modelviewProjection;\n"
               "attribute vec4 pos;\n"
               "attribute vec4 color;\n"
               "varying vec4 v_color;\n"
               "void main() {\n"
               "   gl_Position = modelviewProjection * pos;\n"
               "   v_color = color;\n"
               "}\n"])]
    (and frag vert (gl-make-program [frag vert]))))

(def GL_FLOAT (int 0x1406))
(def GL_COLOR_BUFFER_BIT 0x00004000)
(def GL_DEPTH_BUFFER_BIT 0x00000100)
(def GL_TRIANGLES (int 0x0004))

(defn flat-float-array [matrix]
  (float-array (flatten matrix)))

(defn gl-uniform-matrix [index matrix]
  (jna/invoke Integer GLESv2/glUniformMatrix4fv
              index (int 1) (int 0) (flat-float-array matrix)))

(defn gl-attribute-index [program name]
  (int (jna/invoke Integer GLESv2/glGetAttribLocation program name)))

(defn draw-triangle [frame pos col mvp]
  (let [verts [[-1 -1]
               [1  -1]
               [0   1]]
        cols [[1 0 0 1]
              [0 1 0 1]
              [0 0 1 1]]
        matrix (matrix-multiply (matrix-rotate-z (* frame (/ Math/PI 180)))
                                (matrix-scale 0.5 0.5 0.5))]

    (gl-uniform-matrix mvp matrix)
    (jna/invoke Integer GLESv2/glClear
                (int (bit-or GL_COLOR_BUFFER_BIT  GL_DEPTH_BUFFER_BIT)))
    (jna/invoke Integer GLESv2/glVertexAttribPointer
                pos (int 2) GL_FLOAT (int 0) (int 0)
                (flat-float-array verts))
    (jna/invoke Integer GLESv2/glVertexAttribPointer
                col (int 3) GL_FLOAT (int 0) (int 0)
                (flat-float-array cols))

    (jna/invoke Integer GLESv2/glEnableVertexAttribArray pos)
    (jna/invoke Integer GLESv2/glEnableVertexAttribArray col)

    (jna/invoke Integer GLESv2/glDrawArrays GL_TRIANGLES (int 0) (int 3))

    (jna/invoke Integer GLESv2/glDisableVertexAttribArray pos)
    (jna/invoke Integer GLESv2/glDisableVertexAttribArray col)
    ))


(defn gl-run-demo [keep]
  (let [fb0 (jna/invoke Integer cloglure/cloglure_start "/dev/graphics/fb0")
        program (create-shaders)
        attr-position (gl-attribute-index program "pos")
        attr-color (gl-attribute-index program "color")
        u-matrix
        (int (jna/invoke Integer glGetUniformLocation program "modelviewProjection")
             )]
    (println [fb0 program attr-position attr-color u-matrix])
    (doall
     (map (fn [frame]
            (draw-triangle frame attr-position attr-color u-matrix)
            (jna/invoke Integer cloglure/cloglure_swap_buffers))
          (range 0 60)))
    (or keep
        (jna/invoke Integer cloglure/cloglure_stop fb0))))

#_
(gl-run-demo nil)
