(ns defone.freetype
  (:require [net.n01se.clojure-jna :as jna])
  (:import [java.io File FileInputStream]
           [com.sun.jna Memory Pointer]))

(jna/to-ns ft freetype [Integer FT_Init_FreeType
                        Integer FT_New_Face
                        Integer FT_Set_Char_Size
                        Integer FT_Get_Char_Index
                        Integer FT_Load_Char
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
             name (int 0) x)
    (Pointer. (aget x 0))))

#_
(def face (new-face "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSans.ttf"))

#_
(seq (let [r (int-array 10)] (.read (.getPointer (.getPointer face 84) 76)w 0 r 0 5) r))




(defn set-char-size [face width height hdpi vdpi]
  (checked ft/FT_Set_Char_Size
           face
           (* width 64)
           (* height 64)
           hdpi
           vdpi))

(defn get-char-index [face code]
  (ft/FT_Get_Char_Index (int face) (int code)))


(def FT_LOAD_RENDER (bit-shift-left 1 2))

(defn load-char [face code]
  (checked ft/FT_Load_Char face (int code) FT_LOAD_RENDER))

(defn bitmap-from-pointer [buf]
  {:rows (.getInt buf 0)
   :width (.getInt buf 4)
   :pitch (.getInt buf 8)
   :data (.getPointer buf 12)
   :num-grays (.getShort buf 16)
   :pixel-mode (.getByte buf 18)
   :palette-mode (.getByte buf 19)
   :palette (.getPointer buf 20)
   })

(def face-glyph-offset 84)
(def glyph-bitmap-offset 76)

(defn render-char [face size char]
  (set-char-size face size 0 316 316)
  (load-char face char)
  (let [glyph-slot (.getPointer face face-glyph-offset)]
    (bitmap-from-pointer (.share glyph-slot glyph-bitmap-offset))))

#_
(defn frog [string bitmap x y]
  (let [face (new-face "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSans.ttf")]
    (set-char-size face 12 0 316 316)))
;#    (doall (map #(get-char-index face %) string)
;#      load-glyph
;#      render-glyph
;#      slot=face->glyph
;#      slot->bitmap_top is distance from baseline to
;#      face->glyph->bitmap, face->glyph->bitmap_left, face->glyph->bitmap_top
;#     glyph-indexes)))
