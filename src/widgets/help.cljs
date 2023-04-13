(ns widgets.help
  (:require
   [reagent.dom.client :as rclient]
   [reagent.core :as r]
   [re-frame.core :as rf]))

(rf/reg-event-db
 :help/open
 (fn [db _]
   (assoc db :help/open? true)))

(rf/reg-event-db
 :help/close
 (fn [db _]
   (assoc db :help/open? false)))

(rf/reg-event-db
 :help/set-content
 (fn [db [_ content]]
   (assoc db :help/content content)))

(rf/reg-sub
 :help/open?
 (fn [db _]
   (:help/open? db)))

(rf/reg-sub
 :help/content
 (fn [db _]
   (:help/content db)))

(defn help-widget
  []
  [:div
   (let [open? @(rf/subscribe [:help/open?])]
     (if open?
       [:div {:style {:background-color "#ccc" :padding "1px" :width "50%"
                      :border-style :solid :border-width "3px" :border-color "#333" :border-radius "5px"}}
        [:button
         {:on-click (fn close-help [e]
                      (rf/dispatch [:help/close]))}
         "x"]
        [:div {:style {:margin "10px"}}
         (when-let [content @(rf/subscribe [:help/content])]
           [:div content])]]
       [:div
        [:button
         {:on-click (fn open-help [e]
                      (rf/dispatch [:help/open]))}
         "?"]]))])
