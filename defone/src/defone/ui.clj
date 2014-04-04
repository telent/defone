(ns defone.ui
  (:require [defone.matrix :as m]
            [defone.glj :as glj]
            [clojure.core.async :as async
             :refer [chan >!! <!! >! <! go ]]
            [net.n01se.clojure-jna :as jna])
  (:import [java.io File FileInputStream]))


(defn compile-glsl-program [{:keys [attributes uniforms varyings shaders]}]
  (let [format-decls (fn [vtype mapp]
                       (map (fn [[n t]] (print-str vtype (name t) (name n) ";"))
                            mapp))
        fragment-shader-src (concat ["precision mediump float;"]
                                    (format-decls "uniform" uniforms)
                                    (format-decls "varying" varyings)
                                    (:fragment shaders))
        fragment-shader (glj/make-shader :fragment fragment-shader-src)
        vertex-shader-src (concat ["precision mediump float;"]
                                  (format-decls "uniform" uniforms)
                                  (format-decls "varying" varyings)
                                  (format-decls "attribute" attributes)
                                  (:vertex shaders))
        vertex-shader (glj/make-shader :vertex vertex-shader-src)
        program (glj/make-program [fragment-shader vertex-shader])]
    {:index program
     :uniforms (reduce (fn [m n] (assoc m
                                   n (glj/uniform-index program (name n))))
                       {}
                       (keys uniforms))
     :attributes (reduce (fn [m n] (assoc m
                                     n (glj/attribute-index program (name n))))
                         {}
                         (keys attributes))
     }))



(defmulti draw-scene (fn [context key & more] key))


(defn draw-vertices [context draw-mode attributes]
  (let [vars (:program context)]
    (glj/uniform-matrix (:mvp (:uniforms vars)) (:transform context))

    (if-let [col (:color (:uniforms vars))]
      (glj/uniform4 col (:color context)))

    ;; XXX I suspect this only works by accident.  Last arg is
    ;; supposed to be "offset of the first component of the first
    ;; generic vertex attribute in the array in the data store of the
    ;; buffer currently bound to the GL_ARRAY_BUFFER target", but we have
    ;; not bound any buffers, so ... perhaps only works because we have
    ;; software-only mesa and "gpu" memory is system memory
    (doall
     (map (fn [attribute]
            (let [data (get attributes attribute)
                  index (get (:attributes vars) attribute)
                  size (count (first data))]
              (println [:index index
                        :data (seq (glj/flat-float-array data))
                        :size size])
              (gl/glVertexAttribPointer index
                                        (int size)
                                        glj/GL_FLOAT (int 0) (int 0)
                                        (glj/flat-float-array data))
              (gl/glEnableVertexAttribArray index)
              (println (gl/glGetError))))
          (keys attributes)))

    (let [len (count (first (vals attributes)))]
      (println [:len len])
      (gl/glDrawArrays draw-mode (int 0) (int len)))

    (doall
     (map (fn [attribute]
            (let [index (get (:attributes vars) attribute)]
              (gl/glDisableVertexAttribArray index)
              (println [:disable index (gl/glGetError) ])))
          (keys attributes)))

    ))

(defn draw-kids [context kids]
  (doall (map #(apply draw-scene context %) kids)))

(defmethod draw-scene :vertices [context key attr vertices]
  (println [:vertices attr vertices])
  (condp = (:mode attr)
    :triangles (draw-vertices context glj/GL_TRIANGLES vertices)
    :triangle-strip (draw-vertices context glj/GL_TRIANGLE_STRIP vertices)))

(defmethod draw-scene :scene [context key attr & children]
  (println :scene)
  (println ["draw-scene" context children])
  (draw-kids context children))

(defmethod draw-scene :translate [context key vector & children]
  (println :translate)
  (let [context (update-in context [:transform]
                           #(m/multiply (apply m/translate vector) %))]
    (draw-kids context children)))

(defmethod draw-scene :rotate-z [context key angle & children]
  (println :rotate-z)
  (let [context (update-in context [:transform]
                           #(m/multiply (m/rotate-z angle) %))]
    (draw-kids context children)))


(defmethod draw-scene :scale [context key factors & children]
  (println :scale)
  (let [context (update-in context [:transform]
                           #(m/multiply (apply m/scale factors) %))]
    (draw-kids context children)))

(defmethod draw-scene :color [context key color & children]
  (println :color)
  (let [context (update-in context [:color] (fn [x] color))]
    (draw-kids context children)))

(defmethod draw-scene :group [context key attributes & children]
  (println :group)
  (draw-kids context children))

(defmethod draw-scene :program [context key attributes & children]
  (println :program)
  (let [num (:index attributes)
        context (update-in context [:program] (fn [x] attributes))]
    (jna/invoke Integer GLESv2/glUseProgram (int num))
    (draw-kids context children)))


(defmethod draw-scene :texture [context key attributes & children]
  (println [:texture attributes])
  (let [name (:name attributes)
        uniform (:texture (:uniforms (:program context)))]
    ;; tell the shader which texture unit we're using
    (println [:uniform uniform])
    (gl/glUniform1i uniform (int 0))
    (println (gl/glGetError))

    ;; activate TEXTURE0 unit and bind our named texture into it
    (gl/glActiveTexture glj/GL_TEXTURE0)
    (gl/glBindTexture glj/GL_TEXTURE_2D name)

    (jna/invoke Integer GLESv2/glTexParameteri
                glj/GL_TEXTURE_2D
                glj/GL_TEXTURE_MIN_FILTER
                glj/GL_NEAREST)
    (jna/invoke Integer GLESv2/glTexParameteri
                glj/GL_TEXTURE_2D
                glj/GL_TEXTURE_MAG_FILTER
                glj/GL_NEAREST)
    (println (gl/glGetError))

    (draw-kids context children)))

(defn paint [context scene]
  (let [context (merge
                 {:transform (m/scale 1 1 1)
                  :color [0 0.1 0.1 1]}
                 context)]
    (gl/glClear (int (bit-or glj/GL_COLOR_BUFFER_BIT  glj/GL_DEPTH_BUFFER_BIT)))
    (apply draw-scene context scene)
    (clogl/cloglure_swap_buffers)))


(defn compile-glsl-in-graph [tree]
  (let [[key attr & k] tree
        kids #(map compile-glsl-in-graph k)]
    (condp = key
      :program (cons :program
                     (cons (compile-glsl-program attr) (kids)))
      :texture (cons :texture
                     (cons (assoc attr :name
                                  (glj/load-texture (:data attr)
                                                    (:width attr)
                                                    (:height attr)))
                           (kids)))
      :vertices tree
      (cons key (cons attr (kids))))))


(defn render-loop [chan]
  (println [:loop chan])
  (let [fb0 (clogl/cloglure_start "/dev/graphics/fb0")]
    (gl/glClear (int (bit-or glj/GL_COLOR_BUFFER_BIT glj/GL_DEPTH_BUFFER_BIT)))
    (loop [scene [:scene {} ]]
      (when scene
        (println ["paintig scene " scene])
        (paint {} scene)
        (println "done painting")
        (recur
         (let [[keys replacement] (<!! chan)]
           (println ["got update at " keys])
           (and keys
                (update-in scene keys
                           (fn [old] (compile-glsl-in-graph replacement))))))))
    (clogl/cloglure_stop fb0)))

(defn stop-render-thread [chan]
  (async/close! chan))

(println "hi render channe")
(defonce render-channel
  (let [c (async/chan)]
    (future (do
              (println "start render thread")
              (try (defone.ui/render-loop c)
                   (catch Exception e (str "caught " e " in render thread")))))
    c))

(println "lo render channe")
(defn read-raw-file [name]
  (let [f (File. name)
        l (. f length)
        r (FileInputStream. f)
        buf (byte-array l)]
    (.read r buf 0 l)
    buf))

(def bath-texture-data (read-raw-file "/defone/bathtime.raw"))



(def my-program
  {:attributes {:pos :vec4
                :texture_st :vec2}
   :uniforms {:mvp :mat4
              :color :vec4
              :texture :sampler2D}
   :varyings {:v_color :vec4
              :v_texture_st :vec2}
   :shaders {:vertex
             ["void main() {"
              "   gl_Position = mvp * pos;"
              "   v_color = color;"
              "   v_texture_st = texture_st;"
              "}"]
             :fragment
             ["void main() {"
              "  gl_FragColor = v_color * texture2D(texture, v_texture_st);"
              "}"]}})

(def the-scene [:scale [0.1 0.1 0.1]
                [:rotate-z (* 10 (/ Math/PI 180))
                 [:color [0.8 0.8 1.0 1]
                  [:program my-program
                   [:texture {:data bath-texture-data :width 309 :height 341}
                    [:vertices {:mode :triangle-strip}
                     {:pos [[1 1 1] [5 1 1] [1 5 1] [5 5 1]]
                      :texture_st [[0 1] [1 1] [0 0] [1 0]]}]
                    ]]]]])
#_
(>!! render-channel [[2] the-scene])
