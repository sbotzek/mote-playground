(ns experiments.keyboard-movement
  (:require
   [reagent.dom.client :as rclient]
   [reagent.core :as r]
   [re-frame.core :as rf]
   [mote.gl :as mgl]
   [cljs.core.async :refer [go <!]]
   [mote.tilemap :as tm]
   [mote.input :as input]
   [mote.assetdb :as assetdb]
   [mote.sprite :as sp]
   [mote.transform :refer [transform transform->matrix transform->inverted-matrix]]
   [widgets.help :refer [help-widget]]
   [widgets.fps :refer [fps-widget update-fps]]
   ["gl-matrix" :as m]))

(defn widgets-container
  []
  [:<>
   [fps-widget]
   [help-widget]])

(defn projection-matrix
  [canvas zoom]
  (let [matrix (.create m/mat4)
        aspect (/ (.-clientWidth canvas) (.-clientHeight canvas))
        amount (* 10 zoom)
        -amount (* -10 zoom)]
    (.ortho m/mat4 matrix
            (* -amount aspect) (* amount aspect)
            -amount amount
            -10.0 10.0)
    matrix))

(defn draw-player
  [{:keys [buffer shader]} model-matrix view-matrix projection-matrix]
  (mgl/use-shader shader)
  (mgl/shader-uniform-matrix4x4 shader "u_model" model-matrix)
  (mgl/shader-uniform-matrix4x4 shader "u_view" view-matrix)
  (mgl/shader-uniform-matrix4x4 shader "u_projection" projection-matrix)

  (mgl/bind-buffer buffer)
  (mgl/draw-buffer buffer)
  (mgl/unbind-buffer buffer))

(defn draw-scene
  [state]
  (let [gl (state :gl)]
    (.clearColor gl 0.0 0.0 0.0 1.0)
    (.clearDepth gl 1.0)

    ;; near thinks obscure far things
    (.depthFunc gl gl.LEQUAL)
    (.enable gl gl.DEPTH_TEST)

    ;; enable alpha
    (.blendFunc gl gl.SRC_ALPHA gl.ONE_MINUS_SRC_ALPHA)
    (.enable gl gl.BLEND)

    (.clear gl (bit-or gl.COLOR_BUFFER_BIT gl.DEPTH_BUFFER_BIT))

    (let [projection-matrix (projection-matrix (.-canvas gl) (:zoom state))
          view-matrix (transform->inverted-matrix (:camera state))
          settings (:settings state)]
      ;; draw the tilemap
      (let [tilemap (:tilemap state)
            model-matrix (transform->matrix (:tilemap-transform state))]
        (tm/draw tilemap model-matrix view-matrix projection-matrix settings))
      ;; draw the player
      (let [player (:player state)
            model-matrix (transform->matrix (:player-transform state))]
        (draw-player player model-matrix view-matrix projection-matrix)))))

(defn clamp-position-change-to-tile-boundary
  [value dv]
  (let [new-value (+ value dv)]
    (cond
      (= 0 dv)
      0

      (int? value)
      dv

      (= (int value) (int new-value))
      dv

      (> (abs new-value) (abs value))
      (- (int new-value) value)

      true
      (- (int value) value))))

(defn update-position
  ([position dp tilemap]
   ;; We don't want movement to skip over tile boundaries - so instead we apply movement in multiple steps.
   ;; e.g. if we're at 3.75, and we're moving by .3, move by .25 to 4.0, then .05 to 4.05
   ;;
   ;; This way if someone is moving diagonally and trying to get into a 1 tile corridor, they will slip in.  Without this,
   ;; it's basically impossible to get into one.
   (loop [[dx dy] dp
          pos position]
     (let [[x y] pos
           apply-dx (clamp-position-change-to-tile-boundary x dx)
           apply-dy (clamp-position-change-to-tile-boundary y dy)]
     (if (or (not= 0 apply-dy) (not= 0 apply-dx))
       (let [new-pos (cond-> pos
                       ;; split up movement into each axis - this makes the function a bit simpler
                       (not= 0 apply-dx) (update-position apply-dx 0 tilemap)
                       (not= 0 apply-dy) (update-position apply-dy 1 tilemap))]
         ;; if they're not the same, let's do it again!
         (if (not= new-pos pos)
           (let [leftover-x (- dx (- (get new-pos 0) (get pos 0)))
                 leftover-y (- dy (- (get new-pos 1) (get pos 1)))]
             (recur [leftover-x leftover-y] new-pos))
           new-pos))
       pos))))
  ([position dv idx tilemap]
   (let [old-value (get position idx)
         new-value (+ old-value dv)
         [old-x old-y] position
         [new-x new-y] (assoc position idx new-value)
         in-tile-coords [[(int new-x)           (int new-y)]
                         [(int (+ new-x 0.999)) (int new-y)]
                         [(int new-x)           (int (- new-y 0.999))]
                         [(int (+ new-x 0.999)) (int (- new-y 0.999))]]
         in-tiles (mapcat (fn [[x y]] (tm/tiles-at tilemap x (- y))) in-tile-coords)]
     ;; collide with tiles with the "c" property
     (if (seq (filter #(get-in % [:properties :c]) in-tiles))
       (let [normalized-dv (if (= idx 1) (- dv) dv) ; this is a consequence of the top/left corner being a higher y value, but a lower x value
             clamp-value (if (> normalized-dv 0) (int new-value) (int old-value))]
         (assoc position idx clamp-value))
       (assoc position idx new-value)))))

(defn handle-input-movement
  [state delta-time]
  (let [movement (cond-> [0 0]
                   (input/keydown? 83) (update 1 dec)
                   (input/keydown? 87) (update 1 inc)
                   (input/keydown? 65) (update 0 dec)
                   (input/keydown? 68) (update 0 inc))
        movement-magnitude (js/Math.sqrt (apply + (mapv #(* % %) movement)))]
    (if (> movement-magnitude 0)
      (let [tilemap (state :tilemap)
            move-amount (/ (* delta-time 0.01) movement-magnitude)
            dp (mapv #(* move-amount %) movement) player-transform (state :player-transform)
            player-transform' (update player-transform :position (fn update-pos[position]
                                                                   (update-position position dp tilemap)))]
        (-> state
            (assoc :player-transform player-transform')
            (update :camera assoc :position (:position player-transform'))))
      state)))

(defn handle-input-zoom
  [state delta-time]
  (cond-> state
    (input/keydown? 187)
    (update :zoom (fn key-[zoom] (max 0.01 (- zoom (* delta-time 0.001)))))

    (input/keydown? 189)
    (update :zoom (fn key+[zoom] (+ zoom (* delta-time 0.001))))))

(defn handle-input-settings
  [state]
  (cond-> state
    (input/keydown? 72)
    (do (input/reset-keydown! 72)
        (update-in state [:settings :tilemap/draw-hidden] not))))

(defn handle-input
  [state delta-time]
  (-> state
      (handle-input-zoom delta-time)
      (handle-input-movement delta-time)
      (handle-input-settings)))

(defonce *game-state (atom {}))

(defn main-loop
  [current-time]
  (let [game-state @*game-state
        last-time (or (game-state :last-time) current-time)
        delta-time (- current-time last-time)
        game-state' (-> game-state
                        (handle-input delta-time)
                        (assoc :last-time current-time))]
    (update-fps delta-time)
    (reset! *game-state game-state')
    (draw-scene game-state'))
  (js/requestAnimationFrame main-loop))

(defn create-player
  [gl]
  (go
    (let [buffer (-> gl
                     (mgl/create-buffer :draw-mode :triangles)
                     (mgl/add-buffer-attribute 0 2)
                     (mgl/add-buffer-attribute 1 4)
                     (mgl/add-buffer-data [0.1 0.9 1.0 0.0 0.0 0.8
                                           0.9 0.9 1.0 0.0 0.0 0.8
                                           0.9 0.1 1.0 0.0 0.0 0.8
                                           0.1 0.9 1.0 0.0 0.0 0.8
                                           0.1 0.1 1.0 0.0 0.0 0.8
                                           0.9 0.1 1.0 0.0 0.0 0.8])
                     (mgl/sync-buffer-data))
          shader-source (<! (assetdb/load-asset "/glsl/rectangle.glsl"))
          shader (mgl/load-shader gl shader-source)]
      {:buffer buffer
       :shader shader})))

(defn- start-transform
  [tilemap]
  (let [checkpoints (first (filter #(= "checkpoints" (:name %)) (:layers tilemap)))
        objects (:objects checkpoints)
        start-objects (filter #(= "start" (:class %)) objects)
        start-object (rand-nth start-objects)
        x (-> (:x start-object)
              (+ (rand-int (:width start-object)))
              (/ 16)
              (int))
        y (-> (:y start-object)
              (+ (rand-int (:height start-object)))
              (/ 16)
              (int)
              (-))]
    (assoc (transform) :position [x y 0])))

(defn initial-game-state
  [gl]
  (go
    (let [tilemap (<! (tm/load-tiled gl "/tiled/map.json"))
          ;; remove some zones that make object layers incomprehensible
          tilemap (update tilemap :layers #(filter (fn remove-zones[layer]
                                                     (not (#{"zones" "music"} (:name layer))))
                                                   %))
          start-transform (start-transform tilemap)]
      {:gl gl
       :zoom 1.0
       :camera start-transform
       :tilemap tilemap
       :tilemap-transform (transform)
       :player (<! (create-player gl))
       :player-transform start-transform
       :settings {:tilemap/draw-hidden false}})))

(defonce widgets (atom nil))
(defn ^:dev/after-load run
  []
  (rf/clear-subscription-cache!)
  (rclient/render @widgets [#'widgets-container]))

(defn resize-canvas
  []
  (let [canvas (.getElementById js/document "glcanvas")
        height (-> (.-innerHeight js/window)
                   (- 4)) ; TODO why 4?
        width (.-innerWidth js/window)]
    (set! (.-height canvas) height)
    (set! (.-width canvas) width)))

(defn init!
  []
  (input/init)
  (set! (.-onresize js/window) resize-canvas)
  (resize-canvas)
  (reset! widgets
          (rclient/create-root (.getElementById js/document "widgets")))
  (let [canvas (.getElementById js/document "glcanvas")
        gl (.getContext canvas "webgl")]
    (when-not gl
      (throw (ex-info "Unable to initialize webgl." {})))
    (go
      (reset! *game-state (<! (initial-game-state gl)))
      (js/requestAnimationFrame main-loop)))
  (run))
