(ns widgets.fps
  (:require
   [reagent.core :as r]))

(defonce time-recordings #js [])
(defonce fps (r/atom 0))

(defn reset-fps
  []
  (set! (.-length time-recordings) 0)
  (reset! fps 0))

(defn update-fps
  [delta-time]
  (when (> (.-length time-recordings) 100)
    (.shift time-recordings))
  (.push time-recordings delta-time)
  (reset! fps (/ (.-length time-recordings) (/ (reduce + 0 time-recordings) 1000))))

(defn fps-widget
  []
  [:div {:style {:margin "5px" :color :white}}
   "Fps:" @fps])
