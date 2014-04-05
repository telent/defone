(ns defone.ui
  (:require [defone.matrix :as m]
            [defone.glj :as glj]
            [clojure.core.async :as async
             :refer [chan >!! <!! >! <! go ]]
            [net.n01se.clojure-jna :as jna])
  (:import [java.io File FileInputStream]))


(defmacro checked [& args]
  `(let [ret# ~args
         err# (gl/glGetError)]
     #_(print '~(first args) [~@(rest args)] "==>" ret# "\n")
     (if (> err# 0) (println "GL error " err#))
     ret#))

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


(defn draw-vertices [context draw-mode attributes]
  (let [vars (:program context)]
    (checked glj/uniform-matrix (:mvp (:uniforms vars)) (:transform context))

    (if-let [col (:color (:uniforms vars))]
      (checked glj/uniform4 col (:color context)))

    (doall
     (map (fn [attribute]
            (let [data (get attributes attribute)
                  index (int (get (:attributes vars) attribute))
                  size (int (count (first data)))]
              (checked gl/glVertexAttribPointer index
                                        size
                                        glj/GL_FLOAT (int 0) (int 0)
                                        (glj/flat-float-array data))
              (checked gl/glEnableVertexAttribArray index)
              ))
          (keys attributes)))

    (let [len (count (first (vals attributes)))]
      (checked gl/glDrawArrays draw-mode (int 0) (int len)))

    (doall
     (map (fn [attribute]
            (let [index (int (get (:attributes vars) attribute))]
              (checked gl/glDisableVertexAttribArray index)))
          (keys attributes)))

    ))


(defmulti draw-scene (fn [context key & more] key))

(defn draw-kids [context kids]
  (doall (map #(apply draw-scene context %) kids)))


(defmethod draw-scene :vertices [context key attr vertices]
  (condp = (:mode attr)
    :triangles (draw-vertices context glj/GL_TRIANGLES vertices)
    :triangle-strip (draw-vertices context glj/GL_TRIANGLE_STRIP vertices)))

(defmethod draw-scene :scene [context key attr & children]
  (draw-kids context children))

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

(defmethod draw-scene :program [context key attributes & children]
  (let [num (:index attributes)
        context (update-in context [:program] (fn [x] attributes))]
    (checked gl/glUseProgram (int num))
    (draw-kids context children)))


(defmethod draw-scene :texture [context key attributes & children]
  (let [name (:name attributes)
        uniform (:texture (:uniforms (:program context)))]
    ;; tell the shader which texture unit we're using
    (checked gl/glUniform1i (int uniform) (int 0))

    ;; activate TEXTURE0 unit and bind our named texture into it
    (checked gl/glActiveTexture glj/GL_TEXTURE0)
    (checked gl/glBindTexture glj/GL_TEXTURE_2D (int name))

    (checked gl/glTexParameteri
             glj/GL_TEXTURE_2D
             glj/GL_TEXTURE_MIN_FILTER
             glj/GL_NEAREST)
    (checked gl/glTexParameteri
             glj/GL_TEXTURE_2D
             glj/GL_TEXTURE_MAG_FILTER
             glj/GL_NEAREST)

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
    (vec
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
       (cons key (cons attr (kids)))))))

(defn read-raw-file [name]
  (let [f (File. name)
        l (. f length)
        r (FileInputStream. f)
        buf (byte-array l)]
    (.read r buf 0 l)
    buf))

(defonce the-scene (atom [:scene {} ]))

(defn render-loop [chan]
  (let [fb0 (clogl/cloglure_start "/dev/graphics/fb0")]
    (gl/glClear (int (bit-or glj/GL_COLOR_BUFFER_BIT glj/GL_DEPTH_BUFFER_BIT)))
    (loop [running true]
      (when running
        (println ["paintig scene " @the-scene])
        (paint {} @the-scene)
        (println "done painting")
        (recur
         (let [[keys replacement] (<!! chan)
               compiled
               (if (= (last keys) 2)
                 ;; only compile if it's a subtree, not a value on a
                 ;; branch
                 (compile-glsl-in-graph replacement)
                 replacement)]
           (println ["got update at " keys])
           (when keys
             (swap! the-scene update-in keys (fn [old] compiled))
             keys)))))
    (println "render thread quit")
    (clogl/cloglure_stop fb0)))

(defn stop-render-thread [chan]
  (async/close! chan))

(defonce render-channel
  (let [c (async/chan)]
    (future (do
              (println "start render thread")
              (try (defone.ui/render-loop c)
                   (catch Exception e (str "caught " e " in render thread")))))
    c))

(defn replace-tree-at [path replacement]
  (>!! render-channel [path replacement]))




;;;;;;

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

(def example-scene
  [:scale [0.1 0.1 0.1]
   [:rotate-z (* 25 (/ Math/PI 180))
    [:color [0.8 0.8 1.0 1]
     [:program my-program
      [:texture {:data bath-texture-data :width 309 :height 341}
       [:vertices {:mode :triangle-strip}
        {:pos [[-2 -2 -1] [2 -2 -1] [2 2 -1] [2 2 -1]]
         :texture_st [[0 1] [1 1] [0 0] [1 0]]}]
       ]]]]])

#_
(replace-tree-at [2] example-scene)
