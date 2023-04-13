(ns experiments.template
  (:require
   [reagent.dom.client :as rclient]
   [reagent.core :as r]
   [re-frame.core :as rf]
   [cljs.core.async :refer [<! go]]
   ["gl-matrix" :as m]
   [widgets.fps :refer [update-fps fps-widget]]
   [widgets.help :refer [help-widget]]
   [mote.input :as input]
   [mote.sprite :as sp]
   [mote.transform :refer [transform transform->matrix]]))

(defn widgets-container
  []
  [:<>
   [fps-widget]
   [help-widget]])

(defn projection-matrix
  [canvas zoom]
  (let [matrix (.create m/mat4)
        aspect (/ (.-clientWidth canvas) (.-clientHeight canvas))
        amount (* 20 zoom)
        -amount (* -20 zoom)]
    (.ortho m/mat4 matrix
            (* -amount aspect) (* amount aspect)
            -amount amount
            -10.0 10.0)
    matrix))

(defn handle-input
  [state delta-time]
  (cond-> state
    (input/keydown? 187)
    (update :zoom (fn key-[zoom] (max 0.01 (- zoom (* delta-time 0.001)))))

    (input/keydown? 189)
    (update :zoom (fn key+[zoom] (+ zoom (* delta-time 0.001))))))

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

    (let [canvas (.-canvas gl)
          camera (state :camera)
          zoom (state :zoom)
          projection-matrix (projection-matrix canvas zoom)
          view-matrix (transform->matrix camera)]
      ;; draw the sprite
      (let [sprite (state :sprite)
            xform (state :sprite-transform)]
        (sp/draw sprite (transform->matrix xform) view-matrix projection-matrix)))))

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

(defn initial-game-state
  [gl]
  (go
    (let [sprite (<! (sp/load-sprite gl "/images/sword.png"))]
      {:gl gl
       :zoom 1.0
       :camera (transform)
       :sprite sprite
       :sprite-transform (transform)})))

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
