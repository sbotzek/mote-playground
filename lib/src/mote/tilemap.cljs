;;;; For tilemaps.
;;;;
;;;; Tiled file format:
;;;; https://doc.mapeditor.org/en/stable/reference/json-map-format/
(ns mote.tilemap
  (:require
   [mote.gl :as mgl]
   [clojure.string :as str]
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]))

;;; converting layers
(defmulti json->layer (fn [gl tiles tile-width tile-height layer] (aget layer "type")))

;; object layer
(defn- json->object
  [gl json]
  {:class (aget json "class")
   :name (aget json "name")
   :visible (aget json "visible")
   :rotation (aget json "rotation")
   :x (aget json "x")
   :y (aget json "y")
   :width (aget json "width")
   :height (aget json "height")})

(defn- object-buffer-vec
  "Calculates the buffer vector for the tile at the given x, y position"
  [object tile-width tile-height]
  (let [border-w (/ 0.5 (min tile-height tile-width))
        x (/ (:x object) tile-width)
        y (- (dec (/ (:y object) tile-height)))
        width (/ (:width object) tile-width)
        height (/ (:height object) tile-height)
        lft-x x
        rgt-x (+ x width)
        bot-y (- y height)
        top-y y]
    [lft-x top-y 0.0 0.0 0.0 0.5
     lft-x bot-y 0.0 0.0 0.0 0.5
     rgt-x bot-y 0.0 0.0 0.0 0.5
     lft-x top-y 0.0 0.0 0.0 0.5
     rgt-x top-y 0.0 0.0 0.0 0.5
     rgt-x bot-y 0.0 0.0 0.0 0.5
     ;; top
     lft-x              top-y 0.0 0.0 0.0 0.9
     lft-x (- top-y border-w) 0.0 0.0 0.0 0.9
     rgt-x (- top-y border-w) 0.0 0.0 0.0 0.9
     lft-x              top-y 0.0 0.0 0.0 0.9
     rgt-x              top-y 0.0 0.0 0.0 0.9
     rgt-x (- top-y border-w) 0.0 0.0 0.0 0.9
     ;; bottom
     lft-x              bot-y 0.0 0.0 0.0 0.9
     lft-x (+ bot-y border-w) 0.0 0.0 0.0 0.9
     rgt-x (+ bot-y border-w) 0.0 0.0 0.0 0.9
     lft-x              bot-y 0.0 0.0 0.0 0.9
     rgt-x              bot-y 0.0 0.0 0.0 0.9
     rgt-x (+ bot-y border-w) 0.0 0.0 0.0 0.9
     ;; left
     lft-x              top-y 0.0 0.0 0.0 0.9
     lft-x              bot-y 0.0 0.0 0.0 0.9
     (+ lft-x border-w) bot-y 0.0 0.0 0.0 0.9
     lft-x              top-y 0.0 0.0 0.0 0.9
     (+ lft-x border-w) top-y 0.0 0.0 0.0 0.9
     (+ lft-x border-w) bot-y 0.0 0.0 0.0 0.9
     ;; right
     rgt-x              top-y 0.0 0.0 0.0 0.9
     rgt-x              bot-y 0.0 0.0 0.0 0.9
     (- rgt-x border-w) bot-y 0.0 0.0 0.0 0.9
     rgt-x              top-y 0.0 0.0 0.0 0.9
     (- rgt-x border-w) top-y 0.0 0.0 0.0 0.9
     (- rgt-x border-w) bot-y 0.0 0.0 0.0 0.9
     ]))


(defn- stuff-object-layer-into-buffer
  [buffer objects tile-width tile-height]
  (reduce (fn [buffer object]
            (mgl/add-buffer-data buffer (object-buffer-vec object tile-width tile-height)))
          buffer
          objects))

(defn- create-object-layer-buffer
  [gl objects tile-width tile-height]
  (-> gl
      (mgl/create-buffer :draw-mode :triangles)
      (mgl/add-buffer-attribute 0 2)
      (mgl/add-buffer-attribute 1 4)
      (stuff-object-layer-into-buffer objects tile-width tile-height)
      (mgl/sync-buffer-data)))

(defmethod json->layer "objectgroup"
  [gl tiles tile-width tile-height json]
  (let [objects (mapv #(json->object gl %) (aget json "objects"))
        buffer (create-object-layer-buffer gl objects tile-width tile-height)]
    {:type :objects
     :name (aget json "name")
     :visible (aget json "visible")
     :buffer buffer
     :objects objects}))

;; tile layer
(defn- tileset-has-gid?
  [tileset gid]
  (let [firstgid (:firstgid tileset)
        lastgid (+ firstgid (:count tileset))]
    (and (> gid firstgid)
         (< gid lastgid))))

(defn- tile-buffer-vec
  "Calculates the buffer vector for the tile at the given x, y position"
  [tile x y]
  (let [lx x
        ux (inc x)
        ly y
        uy (inc y)
        lu (:start-u tile)
        uu (:end-u tile)
        lv (:start-v tile)
        uv (:end-v tile)
        tileset-idx (:tileset-idx tile)]
    ;; image coordinates are flipped on the y axiscompared to opengl
    ;; so where we have a high y, we put the low v, and vice versa.
    ;; u stays the same
    [lx uy lu lv (+ 0.2 tileset-idx)
     lx ly lu uv (+ 0.2 tileset-idx)
     ux ly uu uv (+ 0.2 tileset-idx)
     lx uy lu lv (+ 0.2 tileset-idx)
     ux uy uu lv (+ 0.2 tileset-idx)
     ux ly uu uv (+ 0.2 tileset-idx)]))

(defn- stuff-tile-layer-into-buffer
  "Puts layer data into the buffer."
  [buffer data tiles width height]
  (let [buffer-data (transient [])]
    (doseq [x (range width)
            y (range height)]
      (let [idx (+ x (* y width))
            gid (aget data idx)
            tile (get tiles (dec gid))]
        (when (not= 0 gid)
          (reduce conj! buffer-data (tile-buffer-vec tile x (- y))))))
    (assoc buffer :data (persistent! buffer-data))))

(defn- create-tile-layer-buffer
  "Creates the buffer for the layer."
  [gl data tiles width height]
  (-> gl
      (mgl/create-buffer :draw-mode :triangles)
      (mgl/add-buffer-attribute 0 2)
      (mgl/add-buffer-attribute 1 2)
      (mgl/add-buffer-attribute 2 1)
      (stuff-tile-layer-into-buffer data tiles width height)
      (mgl/sync-buffer-data)))

(defmethod json->layer "tilelayer"
  [gl tiles tile-width tile-height json]
  (let [encoding (or (aget json "encoding")
                     "csv")]
    (when (not= encoding "csv")
      (throw (ex-info "Unsupported layer encoding" {:encoding encoding})))
    (let [data (aget json "data")
          width (aget json "width")
          height (aget json "height")
          buffer (create-tile-layer-buffer gl data tiles width height)]
      {:type :tiles
       :visible (aget json "visible")
       :name (aget json "name")
       :buffer buffer
       :width width
       :height height
       :data data})))

(defn- json->tileset
  "Converts json to a tileset."
  [gl base-path tileset]
  (let [firstgid (aget tileset "firstgid")
        tile-properties (reduce (fn [tiles-properties tile]
                                  (let [gid (+ firstgid (aget tile "id"))
                                        prop-list (map (fn js->keyvalue[props]
                                                         [(keyword (aget props "name")) (aget props "value")])
                                                       (aget tile "properties"))]
                                    (assoc tiles-properties gid (into {} prop-list))))
                                {}
                                (aget tileset "tiles"))]
    {:gl gl
     :firstgid firstgid
     :count (aget tileset "tilecount")
     :margin (aget tileset "margin")
     :columns (aget tileset "columns")
     :image-path (aget tileset "image")
     :texture (mgl/load-texture gl (str base-path (aget tileset "image")) :filter :nearest)
     :image-height (aget tileset "imageheight")
     :image-width (aget tileset "imagewidth")
     :tile-height (aget tileset "tileheight")
     :tile-width (aget tileset "tilewidth")
     :tile-properties tile-properties}))

(defn- tileset->tiles
  "Converts a tileset into a list of tiles."
  [tileset tileset-idx]
  (let [{:keys [firstgid image-width image-height tile-width tile-height texture]} tileset
        cols (:columns tileset)
        rows (/ (:count tileset) cols)
        ;; images aren't necessarily exact multiples of the number of pixels in a tile
        physical-rows (/ image-height tile-height)
        physical-cols (/ image-width tile-width)]
    (loop [gid firstgid
           sofar []]
      (let [row (int (/ (- gid firstgid) cols))
            col (int (rem (- gid firstgid) cols))]
        (if (>= row rows)
          sofar
          (recur (inc gid)
                 (conj sofar
                       {:gid gid
                        :row row
                        :col col
                        :tileset-idx tileset-idx
                        :properties (get-in tileset [:tile-properties gid])
                        :texture texture
                        :start-u (/ col physical-cols)
                        :end-u (/ (inc col) physical-cols)
                        :start-v (/ row physical-rows)
                        :end-v (/ (inc row) physical-rows)})))))))

(defn- tilesets->tiles
  "Converts tilesets into a vector of tiles."
  [tilesets]
  (vec (apply concat (map tileset->tiles tilesets (range)))))

(defn- base-path
  "Gets the base path of a path."
  [path]
  (let [slash (str/last-index-of path "/")]
    (if (not= -1 slash)
      (subs path 0 (inc slash))
      "")))

(defn load-tiled
  "Loads a tilemap from a tiled file."
  [gl path]
  (go
    (let [response (<p! (js/fetch path))
          json (<p! (.json response))
          tilesets (mapv #(json->tileset gl (base-path path) %) (aget json "tilesets"))
          tiles (tilesets->tiles tilesets)
          tile-width (aget json "tilewidth")
          tile-height (aget json "tileheight")
          layers (mapv #(json->layer gl tiles tile-width tile-height %) (aget json "layers"))
          tiles-shader-source (<p! (.text (<p! (js/fetch "/glsl/tilemap-tiles.glsl"))))
          tiles-shader (mgl/load-shader gl tiles-shader-source)
          objects-shader-source (<p! (.text (<p! (js/fetch "/glsl/tilemap-objects.glsl"))))
          objects-shader (mgl/load-shader gl objects-shader-source)
          tilemap {:gl gl
                   :width (aget json "width")
                   :height (aget json "height")
                   :render-order (keyword (or (aget json "renderorder") "right-down"))
                   :layers layers
                   :tilesets tilesets
                   :tiles tiles
                   :shaders {:tiles tiles-shader :objects objects-shader}}]
      tilemap)))

(defn tiles-at
  [tilemap x y]
  (->> (:layers tilemap)
       ;; only interested in tile layers
       (filter #(= (:type %) :tiles))
       ;; get the tile gid at the x, y coordinate
       (map (fn find-tile-gid[layer]
              (let [width (:width layer)
                    idx (+ x (* y width))]
                (get (:data layer) idx))))
       ;; zero gid means no tile
       (filter #(not= 0 %))
       ;; convert the gid to the tile
       (map (fn get-tile[gid]
              (get (:tiles tilemap) (dec gid))))))

(defn draw
  [tilemap model-matrix view-matrix projection-matrix & {:keys [tilemap/draw-hidden]}]
  (let [gl (:gl tilemap)
        shaders (:shaders tilemap)
        last-layer-type (atom nil)
        tilesets (:tilesets tilemap)]
    ;; bind the textures
    (doseq [tileset-idx (range (count tilesets))]
      (let [{:keys [texture]} (get tilesets tileset-idx)]
        (mgl/activate-and-bind-texture texture tileset-idx)))

    ;; draw the layers
    (doseq [layer (:layers tilemap)]
      (when (or draw-hidden (:visible layer))
        ;; layer types share shaders, so only re-do setup code when we're switching between layer types
        (when-not (= @last-layer-type (:type layer))
          (reset! last-layer-type (:type layer))
          (let [shader (get shaders (:type layer))]
            (mgl/use-shader shader)
            (mgl/shader-uniform-matrix4x4 shader "u_projection" projection-matrix)
            (mgl/shader-uniform-matrix4x4 shader "u_model" model-matrix)
            (mgl/shader-uniform-matrix4x4 shader "u_view" view-matrix)

            (when (= :tiles (:type layer))
              ;; let the shader know where the textures are
              (doseq [tileset-idx (range (count tilesets))]
                (mgl/shader-uniform-int shader (str "u_tileset" tileset-idx) tileset-idx)))))
        (let [buffer (:buffer layer)]
          (mgl/bind-buffer buffer)
          (mgl/draw-buffer buffer)
          (mgl/unbind-buffer buffer))))

    ;; unbind the textures
    (doseq [{:keys [texture]} (:tilesets tilemap)]
      (mgl/unbind-texture texture))))
