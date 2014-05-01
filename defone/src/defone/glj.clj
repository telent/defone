(ns defone.glj
  (:require [net.n01se.clojure-jna :as jna])
  (:import [java.io File FileInputStream]
           [com.sun.jna Memory]))

(jna/to-ns clogl cloglure [Integer cloglure_start,
                           Integer cloglure_stop,
                           Integer cloglure_swap_buffers,
                           Integer cloglure_get_display])
(jna/to-ns egl EGL [Integer eglGetError])
(jna/to-ns gl GLESv2 [Integer glUniformMatrix4fv,
                      Integer glUniform1i,
                      Integer glUniform4fv,
                      Integer glVertexAttribPointer,
                      Integer glEnableVertexAttribArray,
                      Integer glDrawArrays,
                      Integer glDisableVertexAttribArray,
                      Integer glActiveTexture,
                      Integer glBindTexture,
                      Integer glGetError,
                      Integer glUseProgram,
                      Integer glTexParameteri,
                      Integer glClear])

(def GL_COMPILE_STATUS (int 0x8b81))
(def GL_LINK_STATUS (int 0x8B82))
(def GL_FLOAT (int 0x1406))
(def GL_COLOR_BUFFER_BIT 0x00004000)
(def GL_DEPTH_BUFFER_BIT 0x00000100)

(def GL_POINTS                         (int 0x0000))
(def GL_LINES                          (int 0x0001))
(def GL_LINE_LOOP                      (int 0x0002))
(def GL_LINE_STRIP                     (int 0x0003))
(def GL_TRIANGLES                      (int 0x0004))
(def GL_TRIANGLE_STRIP                 (int 0x0005))
(def GL_TRIANGLE_FAN                   (int 0x0006))

(def GL_TEXTURE_2D (int 0x0DE1))
(def GL_TEXTURE_MAG_FILTER (int 0x2800))
(def GL_TEXTURE_MIN_FILTER (int 0x2801))
(def GL_NEAREST (int 0x2600))
(def GL_RGBA (int 0x1908))
(def GL_UNSIGNED_BYTE (int 0x1401))

(def GL_TEXTURE0 (int 0x84c0))
(def GL_TEXTURE1 (int 0x84c1))
(def GL_TEXTURE2 (int 0x84c2))
(def GL_TEXTURE3 (int 0x84c3))
(def GL_TEXTURE4 (int 0x84c4))

(defn flat-float-array [matrix]
  (let [floats (float-array (flatten matrix))
        num (alength floats)
        m (com.sun.jna.Memory. (* 4 num))]
    (.write m 0 floats 0 num)
    m))


(defn shader-type [name]
  (int (get {:fragment 0x8B30 :vertex 0x8B31} name)))

(defn make-shader [type text]
  (let [typenum (shader-type type)
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
    (if (zero? (aget compiled? 0))
      (let [err (byte-array 1000)
            len (int-array [0])]
        (jna/invoke Integer GLESv2/glGetShaderInfoLog
                    shader (int 1000) len err)
        (println (clojure.string/trim (String. err)))))
    (and (not (zero? (aget compiled? 0))) shader)))

(defn make-program [shaders]
  (let [program (int (jna/invoke Integer GLESv2/glCreateProgram))
        success? (int-array [0])]
    (doall (map #(jna/invoke Integer GLESv2/glAttachShader program %) shaders))
    (jna/invoke Integer GLESv2/glLinkProgram program)
    (jna/invoke Integer GLESv2/glGetProgramiv program GL_LINK_STATUS success?)
    (if (zero? (aget success? 0))
      (let [err (byte-array 1000)
            len (int-array [0])]
        (jna/invoke Integer GLESv2/glGetProgramInfoLog
                    program (int 1000) len err)
        (println (clojure.string/trim (String. err)))
        nil)
      program)))

(defn attribute-index [program name]
  (let [i (jna/invoke Integer GLESv2/glGetAttribLocation
                      (int program) name)]
    (and (>= i 0) (int i))))

(defn uniform-index [program name]
 (let [i (jna/invoke Integer GLESv2/glGetUniformLocation
                     (int program) name)]
   (and (>= i 0) (int i))))

(defn uniform-matrix [index matrix]
  (gl/glUniformMatrix4fv
   (int index) (int 1) (int 0) (flat-float-array matrix)))

(defn uniform4 [index vals]
  (gl/glUniform4fv (int index) (int 1) (flat-float-array vals)))

(defn load-texture [data width height]
  (let [name (int-array 1)]
    ;; get a "name" (a.k.a number)
    (jna/invoke Integer GLESv2/glGenTextures (int 1) name)
    (gl/glActiveTexture GL_TEXTURE0)
    ;; make texture target TEXTURE_2D point to our new name
    (jna/invoke Integer GLESv2/glBindTexture
                GL_TEXTURE_2D (aget name 0))
    ;; "Here we're setting the GL_TEXTURE_MIN_FILTER (the case where
    ;; we have to shrink the texture for far away objects) to
    ;; GL_NEAREST (when drawing a vertex, choose the closest
    ;; corresponding texture pixel)."
    (jna/invoke Integer GLESv2/glTexParameteri
                GL_TEXTURE_2D
                GL_TEXTURE_MIN_FILTER
                GL_NEAREST)
    (jna/invoke Integer GLESv2/glTexParameteri
                GL_TEXTURE_2D
                GL_TEXTURE_MAG_FILTER
                GL_NEAREST)
    ;; send the data across
    (jna/invoke Integer GLESv2/glTexImage2D
                GL_TEXTURE_2D ; target
                (int 0)       ; detail level
                GL_RGBA       ; internal format
                (int width) (int height)
                (int 0)             ; border width
                GL_RGBA             ; data format
                GL_UNSIGNED_BYTE    ; data type
                data)
    (aget name 0)))
