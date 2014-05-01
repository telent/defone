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

(defn set-char-size [face width height hdpi vdpi]
  (checked ft/FT_Set_Char_Size
           face
           (int (* width 64))
           (int (* height 64))
           (int hdpi)
           (int vdpi)))

(defn get-char-index [face code]
  (ft/FT_Get_Char_Index (int face) (int code)))


(def FT_LOAD_RENDER (bit-shift-left 1 2))

(defn load-char [face code]
  (checked ft/FT_Load_Char face (int code) FT_LOAD_RENDER))

; only works for FT_PIXEL_MODE_GRAY which has one pixel per byte
(defn aart-row [array]
  (clojure.string/join
   (map #(cond (> % 128) \* (= % 0) \space :else \.) array)))

(defn aart-bitmap [bitmap]
  (let [p (:pitch bitmap)
        d (:data bitmap)]
    (clojure.string/join
     "\n"
     (map #(aart-row (.getByteArray d (* % p) p)) (range 0 (:rows bitmap))))))

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
