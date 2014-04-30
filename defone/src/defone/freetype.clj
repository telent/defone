(ns defone.freetype
  (:require [net.n01se.clojure-jna :as jna])
  (:import [java.io File FileInputStream]
           [com.sun.jna Memory]))

(jna/to-ns ft freetype [Integer FT_Init_FreeType
                        Integer FT_New_Face
                        ])

(defmacro checked [& args]
  `(let [ret# ~args]
     (assert (= ret# 0) (str "FT error " ret# " from " ~(first args)))))

(defn open-freetype []
 (let [x (int-array 2)]
   (checked ft/FT_Init_FreeType x)
   (aget x 0)))

(defonce ft-library (open-freetype))

(defn new-face [name]
  (let [x (int-array 1)]
    (checked ft/FT_New_Face ft-library
             name
             0 x)
    (aget x 0)))

#_
(new-face "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSans.ttf")

(defn set-char-size [face width height hdpi vdpi]
  (checked ft/FT_Set_Char_Size
           face
           (* width 64)
           (* height 64)
           hdpi
           vdpi))

(defn get-char-index [face code]
  (ft/FT_Get_Char_Index face code))
