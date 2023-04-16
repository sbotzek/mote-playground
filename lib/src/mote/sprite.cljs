(ns mote.sprite
  (:require
   [cljs.core.async :refer [go <!]]
   [mote.gl :as mgl]
   [mote.assetdb :as assetdb]))

(defrecord Sprite [gl texture buffer shader model-location view-location projection-location])

(defn- create-buffer
  [gl]
  (-> gl
      (mgl/create-buffer :draw-mode :triangles)
      (mgl/add-buffer-attribute 0 2)
      (mgl/add-buffer-attribute 1 2)
      (mgl/add-buffer-data [0 1 0 0
                            0 0 0 1
                            1 0 1 1
                            0 1 0 0
                            1 1 1 0
                            1 0 1 1])
      (mgl/sync-buffer-data)))

(defn load-sprite
  [gl path]
  (go
    (let [texture (<! (mgl/load-texture gl path :filter :nearest))
          shader-source (<! (assetdb/load-asset "/glsl/sprite.glsl"))
          shader (mgl/load-shader gl shader-source)]
      (map->Sprite
       {:gl gl
        :texture texture
        :buffer (create-buffer gl)
        :shader shader
        :model-location (get-in shader [:uniforms "u_model"])
        :view-location (get-in shader [:uniforms "u_view"])
        :projection-location (get-in shader [:uniforms "u_projection"])}))))

(defn draw
  [^Sprite sprite model-matrix view-matrix projection-matrix]
  (let [gl (.-gl sprite)
        texture (.-texture sprite)
        buffer (.-buffer sprite)
        shader (.-shader sprite)]
    (mgl/activate-and-bind-texture texture 0)
    (mgl/use-shader shader)
    (mgl/shader-uniform-matrix4x4-by-location shader (.-model-location sprite) model-matrix)
    (mgl/shader-uniform-matrix4x4-by-location shader (.-view-location sprite) view-matrix)
    (mgl/shader-uniform-matrix4x4-by-location shader (.-projection-location sprite) projection-matrix)

    (mgl/bind-buffer buffer)
    (mgl/draw-buffer buffer)

    (mgl/unbind-buffer buffer)
    (mgl/unbind-texture texture)))
