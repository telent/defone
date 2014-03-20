(ns defone.ui
  (:require [defone.matrix :as m]
            [net.n01se.clojure-jna :as jna]))

(jna/to-ns clogl cloglure [Integer cloglure_start,
                           Integer cloglure_stop,
                           Integer cloglure_swap_buffers,
                           Integer cloglure_get_display])
(jna/to-ns egl EGL [Integer eglGetError])
(jna/to-ns gl GLESv2 [Integer glUniformMatrix4fv,
                      Integer glUniform4fv,
                      Integer glVertexAttribPointer,
                      Integer glEnableVertexAttribArray,
                      Integer glDrawArrays,
                      Integer glDisableVertexAttribArray,
                      Integer glClear])

(def GL_COMPILE_STATUS (int 0x8b81))
(def GL_LINK_STATUS (int 0x8B82))
(def GL_FLOAT (int 0x1406))
(def GL_COLOR_BUFFER_BIT 0x00004000)
(def GL_DEPTH_BUFFER_BIT 0x00000100)
(def GL_TRIANGLES (int 0x0004))

(defn gl-shader-type [name]
  (int (get {:fragment 0x8B30 :vertex 0x8B31} name)))

(defn gl-make-shader [type text]
  (let [typenum (gl-shader-type type)
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
      (let [err (char-array 1000)
            len (int-array [0])]
        (jna/invoke Integer GLESv2/glGetShaderInfoLog
                    shader (int 1000) len err)
        (println (clojure.string/trim (String. err)))))
    (and (not (zero? (aget compiled? 0))) shader)))

(defn gl-make-program [shaders]
  (let [program (int (jna/invoke Integer GLESv2/glCreateProgram))
        success? (int-array [0])]
    (doall (map #(jna/invoke Integer GLESv2/glAttachShader program %) shaders))
    (jna/invoke Integer GLESv2/glLinkProgram program)
    (jna/invoke Integer GLESv2/glGetProgramiv program GL_LINK_STATUS success?)
    (println [:success (seq success?)])
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
               "  gl_FragColor = v_color;"
               "}"])
        vert (gl-make-shader
              :vertex
              ["uniform mat4 modelviewProjection;"
               "attribute vec4 pos;"
               "uniform vec4 color;"
               "varying vec4 v_color;"
               "void main() {"
               "   gl_Position = modelviewProjection * pos;"
               "   v_color = color;"
               "}"])]
    (assert frag)
    (assert vert)
    (gl-make-program [frag vert])))


(defn flat-float-array [matrix]
  (float-array (flatten matrix)))

(defn gl-uniform-matrix [index matrix]
  (gl/glUniformMatrix4fv
   (int index) (int 1) (int 0) (flat-float-array matrix)))

(defn gl-uniform4 [index vals]
  (gl/glUniform4fv (int index) (int 1) (flat-float-array vals)))

(defn gl-attribute-index [program name]
  (int (jna/invoke Integer GLESv2/glGetAttribLocation program name)))

(defn gl-uniform-index [program name]
  (int (jna/invoke Integer GLESv2/glGetUniformLocation program name)))

;;;;;;

(defmulti draw-scene (fn [context key & more] key))

(defmethod draw-scene :vertices [context key & vertices]
  ;; we could maybe be more effciient by calling gl-uniform-matrix
  ;; lazily but let's try it the easy way first
  (let [pos (:pos (:indices context))]
    (gl-uniform-matrix (:mvp (:indices context)) (:transform context))
    (gl-uniform4 (:color (:indices context)) (:color context))

    ;; XXX I suspect this only works by accident.  Last arg is
    ;; supposed to be "offset of the first component of the first
    ;; generic vertex attribute in the array in the data store of the
    ;; buffer currently bound to the GL_ARRAY_BUFFER target", but we have
    ;; not bound any buffers, so ... probably only works because we have
    ;; software-only mesa and "gpu" memory is system memory

    (gl/glVertexAttribPointer pos
                              (int (count vertices))
                              GL_FLOAT (int 0) (int 0)
                              (flat-float-array vertices))
    (gl/glEnableVertexAttribArray pos)
    (println [pos (count vertices) vertices  (:mvp (:indices context)) (:transform context)])
    (gl/glDrawArrays GL_TRIANGLES (int 0) (int 3))
    (gl/glDisableVertexAttribArray pos)))

(defmethod draw-scene :translate [context key vector child]
  (let [context (update-in context [:transform]
                           #(m/multiply (apply m/translate vector) %))]
    (apply draw-scene context child)))

(defmethod draw-scene :rotate-z [context key angle child]
  (let [context (update-in context [:transform]
                           #(m/multiply (m/rotate-z angle) %))]
    (apply draw-scene context child)))

(defmethod draw-scene :scale [context key factors child]
  (let [context (update-in context [:transform]
                           #(m/multiply (apply m/scale factors) %))]
    (apply draw-scene context child)))

(defmethod draw-scene :color [context key color child]
  (let [context (update-in context [:color] (fn [x] color))]
    (apply draw-scene context child)))

(defmethod draw-scene :aggregate [context key name & children]
  (doall (map #(apply draw-scene context %) children)))

(defn paint [context scene]
  (let [context (merge
                 {:transform (m/scale 1 1 1)
                  :color [0 1 1 1]}
                 context)]
    (gl/glClear (int (bit-or GL_COLOR_BUFFER_BIT  GL_DEPTH_BUFFER_BIT)))
    (apply draw-scene context scene)
    (clogl/cloglure_swap_buffers)))

(defn run-the-world [scene]
  (let [fb0 (clogl/cloglure_start "/dev/graphics/fb0")
        program (create-shaders)
        attr-position (gl-attribute-index program "pos")
        attr-color (gl-uniform-index program "color")
        u-matrix (gl-uniform-index program "modelviewProjection")
        context {:indices
                 {:position attr-position
                  :color attr-color
                  :mvp u-matrix}}]
    (println attr-position attr-color u-matrix context)
    (paint context scene)
    (clogl/cloglure_stop fb0)))

(def the-scene
  [:scale [0.5 0.5 0.5]
   [:rotate-z (* 30 (/ Math/PI 180))
    [:color [1 0 0 1]
     [:aggregate :triangle
      [:vertices [-1 -1 0] [1  -1 0] [0  1 0]]]]]])
#_
(run-the-world the-scene)
