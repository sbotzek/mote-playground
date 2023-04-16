;;;; Loads assets such as text, images, etc.
;;;;
;;;; This isn't necessarily necessary, except it provides a standard
;;;; interface for loading them - otherwise its a mix of promises and
;;;; callbacks (for images).
(ns mote.assetdb
  (:require
   [clojure.string :as string]
   [cljs.core.async :refer [go chan]]
   [cljs.core.async.interop :refer-macros [<p!]]))

(defn load-asset
  [url]
  (let [extension-idx (string/last-index-of url ".")]
    (when (= -1 extension-idx)
      (throw (ex-info "failed to find file extension" {:url url})))
    (let [extension (subs url extension-idx)]
      (cond
        (#{".png" ".gif" ".jpg" ".jpeg"} extension)
        (let [ch (chan)
              image (js/Image.)]
          (set! (.-onload image)
                (fn image-loaded[]
                  (go (>! ch image))))
          (set! (.-src image) url)
          ch)

        (= ".json" extension)
        (go (<p! (.json (<p! (js/fetch url)))))

        (#{".text" ".txt" ".glsl"} extension)
        (go (<p! (.text (<p! (js/fetch url)))))

        :else
        (throw (ex-info "no handler for extension" {:url url :extension extension}))))))
