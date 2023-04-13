(ns mote.input)

(defonce keyboard (atom {}))

(defn- on-key-down
  [event]
  (swap! keyboard assoc (.-keyCode event) true))

(defn- on-key-up
  [event]
  (println "keyup" (.-keyCode event))
  (swap! keyboard dissoc (.-keyCode event)))

(defn init
  []
  (.addEventListener js/window "keydown" on-key-down)
  (.addEventListener js/window "keyup" on-key-up))

(defn keydown?
  [keycode]
  (get @keyboard keycode))

(defn reset-keydown!
  [keycode]
  (swap! keyboard dissoc keycode))
