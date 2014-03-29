(ns defone.ui
  (:require [defone.matrix :as m]
            [clojure.core.async :as async
             :refer [chan >!! <!! >! <! go ]]
            [net.n01se.clojure-jna :as jna])
  (:import [java.io File FileInputStream]))


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
(def GL_TEXTURE_MIN_FILTER (int 0x2801))
(def GL_NEAREST (int 0x2600))
(def GL_RGBA (int 0x1908))
(def GL_UNSIGNED_BYTE (int 0x1401))

(def GL_TEXTURE0 (int 0x84c0))
(def GL_TEXTURE1 (int 0x84c1))
(def GL_TEXTURE2 (int 0x84c2))
(def GL_TEXTURE3 (int 0x84c3))
(def GL_TEXTURE4 (int 0x84c4))

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
      (let [err (byte-array 1000)
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
    (if (zero? (aget success? 0))
      (let [err (byte-array 1000)
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

(defn draw-vertices [context draw-mode vertices]
  ;; we could maybe be more efficient by calling gl-uniform-matrix
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
                              (int 3)
                              GL_FLOAT (int 0) (int 0)
                              (flat-float-array vertices))
    (gl/glEnableVertexAttribArray pos)
    (gl/glDrawArrays draw-mode (int 0) (int (count vertices)))
    (gl/glDisableVertexAttribArray pos)))

(defn draw-kids [context kids]
  (doall (map #(apply draw-scene context %) kids)))

(defmethod draw-scene :triangles [context key vertices]
  (draw-vertices context GL_TRIANGLES vertices))

(defmethod draw-scene :triangle-strip [context key vertices]
  (draw-vertices context GL_TRIANGLE_STRIP vertices))

(defmethod draw-scene :translate [context key vector & children]
  (let [context (update-in context [:transform]
                           #(m/multiply (apply m/translate vector) %))]
    (draw-kids context children)))

(defmethod draw-scene :rotate-z [context key angle & children]
  (let [context (update-in context [:transform]
                           #(m/multiply (m/rotate-z angle) %))]
    (draw-kids context children)))

(defmethod draw-scene :scale [context key factors & children]
  (let [context (update-in context [:transform]
                           #(m/multiply (apply m/scale factors) %))]
    (draw-kids context children)))

(defmethod draw-scene :color [context key color & children]
  (let [context (update-in context [:color] (fn [x] color))]
    (draw-kids context children)))

(defmethod draw-scene :group [context key attributes & children]
  (draw-kids context children))

(defmethod draw-scene :texture [context key texture & children]
  (let [name (:name texture)
        vert (:vertices texture)
        context (merge context
                       {:texture-name name
                        :texture-verts vert})]
    (draw-kids context children)))



(defn paint [context scene]
  (let [context (merge
                 {:transform (m/scale 1 1 1)
                  :color [0 1 1 1]}
                 context)]
    (gl/glClear (int (bit-or GL_COLOR_BUFFER_BIT  GL_DEPTH_BUFFER_BIT)))
    (apply draw-scene context scene)
    (clogl/cloglure_swap_buffers)))

(defn render-loop [chan]
  (let [fb0 (clogl/cloglure_start "/dev/graphics/fb0")
        program (create-shaders)
        attr-position (gl-attribute-index program "pos")
        attr-color (gl-uniform-index program "color")
        u-matrix (gl-uniform-index program "modelviewProjection")
        context {:indices
                 {:position attr-position
                  :color attr-color
                  :mvp u-matrix}}]
    (println context)
    (gl/glClear (int (bit-or GL_COLOR_BUFFER_BIT  GL_DEPTH_BUFFER_BIT)))
    (loop []
      (and
       (let [scene (<!! chan)]
         (if scene
           (do
             (println ["scene " scene])
             (paint context scene)
             true)))
       (recur)))
    (clogl/cloglure_stop fb0)))


(defn start-render-thread [scene-atom]
  (let [c (async/chan)]
    (future (defone.ui/render-loop c))
    (add-watch scene-atom :render
               (fn [key ref old new] (>!! c new)))
    c))
(defn stop-render-thread [chan]
  (async/close! chan))

(defn find-element-named [name tree]
  (let [[type attrs & kids] tree]
    (if (and (= type :group) (= (:name attrs) name))
      tree
      (some #(find-element-named name %) kids))))

(defn new-vert [scene]
  (swap! the-scene update-in [2 2 2 2] (constantly scene)))

(defn read-raw-file [name]
  (let [f (File. name)
        l (. f length)
        r (FileInputStream. f)
        buf (byte-array l)]
    (.read r buf 0 l)
    buf))

(def the-scene (atom
                [:scale [0.1 0.1 0.1]
                 [:rotate-z (* 10 (/ Math/PI 180))
                  [:color [1 1 0 1]
                   [:texture {:name bath-texture
                              :vertices
                              [[0 0] [1 0] [0 1] [1 1]]
                              }
                    [:group {:name :triangle}
                     [:triangles
                      [[-1 -1 0] [1 -1 0] [0 1 0]]
                      ]]]]]]))

#_
(new-vert
 [:triangle-strip [[0 0 0] [5 0 0] [0 5 0] [5 5 0]]])

#_
(swap! the-scene (constantly
                  [:scale [0.1 0.1 0.1]
                   [:rotate-z (* 10 (/ Math/PI 180))
                    [:color [1 1 0 1]
                     [:group {:name :triangle}
                      [:triangles
                       [[-1 -1 0] [1 -1 0] [0 1 0]]
                       ]]]]]))


(defonce render-channel (start-render-thread the-scene))
