(ns mote.sprite
  (:require
   [cljs.core.async :refer [go <!]]
   [mote.gl :as mgl]
   [mote.assetdb :as assetdb]))

(defrecord Sprite [gl buffer shader texture
                   width height
                   model-location view-location projection-location
                   total-frames frame-idx playing animations])
(defrecord SpriteAnimation [frames delay])
(defrecord PlayingAnimation [key animation frame-idx next-frame-at])

(defn- create-buffer
  [gl physical-cols physical-rows width height]
  (let [data (transient [])]
    ;; need to populate the data
    (doseq [row (range (int physical-rows))
            col (range (int physical-cols))]
      (let [lower-u (/ col physical-cols)
            upper-u (/ (inc col) physical-cols)
            lower-v (/ row physical-rows)
            upper-v (/ (inc row) physical-rows)]
        (reduce conj! data [0     height lower-u lower-v
                            0     0      lower-u upper-v
                            width 0      upper-u upper-v
                            0     height lower-u lower-v
                            width height upper-u lower-v
                            width 0      upper-u upper-v])))
    (-> gl
        (mgl/create-buffer :draw-mode :triangles)
        (mgl/add-buffer-attribute 0 2)
        (mgl/add-buffer-attribute 1 2)
        (mgl/add-buffer-data (persistent! data))
        (mgl/sync-buffer-data))))

(defn load-sprite
  [gl path & {:keys [width height frame-width frame-height] :or {width 1 height 1}}]
  (go
    (let [texture (<! (mgl/load-texture gl path :filter :nearest))
          shader-source (<! (assetdb/load-asset "/glsl/sprite.glsl"))
          shader (mgl/load-shader gl shader-source)
          frame-width (or frame-width (.-width texture))
          frame-height (or frame-height (.-height texture))
          physical-cols (/ (.-width texture) frame-width)
          physical-rows (/ (.-height texture) frame-height)
          rows (int (/ (.-width texture) frame-width))
          cols (int (/ (.-height texture) frame-height))
          total-frames (* rows cols)]
      (map->Sprite
        {:gl gl
         :texture texture
         :buffer (create-buffer gl physical-cols physical-rows width height)
         :width width
         :height height
         :shader shader
         :total-frames total-frames
         :frame-idx 0
         :model-location (get-in shader [:uniforms "u_model"])
         :view-location (get-in shader [:uniforms "u_view"])
         :projection-location (get-in shader [:uniforms "u_projection"])
         :animations {}
         :playing nil}))))

(defn add-animation
  [sprite key frames delay]
  (when (some #(>= % (:total-frames sprite)) frames)
    (throw (ex-info "add-animation found frames larger than total frames" {:sprite sprite :key key :frames frames})))
  (assoc-in sprite [:animations key] (map->SpriteAnimation {:frames frames :delay delay})))

(defn current-animation
  [^Sprite sprite key current-time]
  (let [^SpriteAnimation animation (get (.-animations sprite) key)]
    (when-not animation
      (throw (ex-info "could not find animation" {:sprite sprite :animation-key key})))
    (assoc sprite
      :frame-idx ((.-frames animation) 0)
      :playing (map->PlayingAnimation
                 {:key key
                  :animation animation
                  :frame-idx 0
                  :next-frame-at (+ current-time (.-delay animation))}))))

(defn update-sprite
  [^Sprite sprite current-time]
  (if-let [^PlayingAnimation playing (.-playing sprite)]
    (let [next-frame-at (.-next-frame-at playing)]
      (if (< next-frame-at current-time)
        (let [^SpriteAnimation animation (.-animation playing)
              frames (.-frames animation)
              delay (.-delay animation)
              playing-frame-idx (.-frame-idx playing)
              playing-frame-idx' (rem (inc playing-frame-idx) (count frames))
              frame-idx' (frames playing-frame-idx')
              next-frame-at' (max current-time (+ next-frame-at delay))]
          (-> sprite
              (assoc :frame-idx frame-idx')
              (assoc :playing (-> playing
                                  (assoc :frame-idx playing-frame-idx')
                                  (assoc :next-frame-at next-frame-at')))))
        sprite))
    sprite))

(defn draw
  [^Sprite sprite model-matrix view-matrix projection-matrix]
  (let [texture (.-texture sprite)
        buffer (.-buffer sprite)
        shader (.-shader sprite)
        frame-idx (.-frame-idx sprite)]
    (mgl/activate-and-bind-texture texture 0)
    (mgl/use-shader shader)
    (mgl/shader-uniform-matrix4x4-by-location shader (.-model-location sprite) model-matrix)
    (mgl/shader-uniform-matrix4x4-by-location shader (.-view-location sprite) view-matrix)
    (mgl/shader-uniform-matrix4x4-by-location shader (.-projection-location sprite) projection-matrix)

    (mgl/bind-buffer buffer)
    (mgl/draw-buffer buffer (* frame-idx 6) 6)

    (mgl/unbind-buffer buffer)
    (mgl/unbind-texture texture)))