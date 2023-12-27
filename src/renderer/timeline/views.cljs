(ns renderer.timeline.views
  (:require
   ["@xzdarcy/react-timeline-editor" :refer [Timeline]]
   ["@radix-ui/react-select" :as Select]
   ["react" :as react]
   [re-frame.core :as rf]
   [reagent.core :as ra]
   [renderer.components :as comp]))

(def speed-options
  [{:id :0.25
    :value 0.25
    :label "0.25x"}
   {:id :0.5
    :value 0.5
    :label "0.5x"}
   {:id :normal
    :value 1
    :label "1x"}
   {:id :1.5
    :value 1.5
    :label "1.5x"}
   {:id :2
    :value 2
    :label "2x"}])

(defn speed-select
  [editor-ref]
  (let [speed @(rf/subscribe [:timeline/speed])]
    [:div.inline-flex.items-center
     [:label {:style {:height "auto"
                      :background "transparent"}} "Speed"]
     [:> Select/Root
      {:value speed
       :onValueChange #(.setPlayRate (.-current editor-ref) %)}
      [:> Select/Trigger
       {:class "select-trigger"
        :aria-label "No a11y filter"}
       [:> Select/Value {:placeholder "Filter"}
        [:div.flex.gap-1.justify-between.items-center
         {:style {:min-width "50px"}}
         [:span (str speed "x")]
         [:> Select/Icon {:class "select-icon"}
          [comp/icon "chevron-down" {:class "small"}]]]]]
      [:> Select/Portal
       [:> Select/Content
        {:class "menu-content rounded select-content"
         :style {:min-width "auto"}}
        [:> Select/ScrollUpButton {:class "select-scroll-button"}
         [comp/icon "chevron-up"]]
        [:> Select/Viewport {:class "select-viewport"}
         [:> Select/Group
          (map (fn [{:keys [id value label]}] ^{:key id}
                 [:> Select/Item
                  {:value value
                   :class "menu-item select-item"}
                  [:> Select/ItemText label]]) speed-options)]]
        [:> Select/ScrollDownButton
         {:class "select-scroll-button"}
         [comp/icon "chevron-down"]]]]]]))

(defn snap-controls
  []
  (let [grid-snap? @(rf/subscribe [:timeline/grid-snap?])
        guide-snap? @(rf/subscribe [:timeline/guide-snap?])]
    [:<>
     [comp/switch
      {:id "grid-snap"
       :label "Grid snap"
       :default-checked? grid-snap?
       :on-checked-change #(rf/dispatch [:timeline/set-grid-snap %])}]
     [comp/switch
      {:id "guide-snap"
       :label "Guide snap"
       :default-checked? guide-snap?
       :on-checked-change #(rf/dispatch [:timeline/set-guide-snap %])}]]))

(defn toolbar
  [editor-ref]
  (let [time @(rf/subscribe [:timeline/time])
        time-formatted @(rf/subscribe [:timeline/time-formatted])
        paused? @(rf/subscribe [:timeline/paused?])
        replay? @(rf/subscribe [:timeline/replay?])
        end @(rf/subscribe [:timeline/end])
        timeline? @(rf/subscribe [:panel/visible? :timeline])]
    [:div.toolbar.level-1.mb-px
     [:div.flex-1.flex
      [comp/icon-button "go-to-start"
       {:on-click #(.setTime (.-current editor-ref) 0)
        :disabled (zero? time)}]
      [comp/radio-icon-button
       {:title (if paused? "Play" "Pause")
        :active? (not paused?)
        :icon (if paused? "play" "pause")
        :action #(if paused?
                   (.play (.-current editor-ref) #js {:autoEnd true})
                   (.pause (.-current editor-ref)))}]
      [comp/icon-button "go-to-end"
       {:on-click #(.setTime (.-current editor-ref) end)
        :disabled (>= time end)}]
      [comp/radio-icon-button
       {:title "Replay"
        :active? replay?
        :icon "refresh"
        :action #(rf/dispatch [:timeline/toggle-replay])}]
      [speed-select editor-ref]
      [:span.p-2.font-mono time-formatted]
      (when timeline?
        [:<>
         [:span.v-divider]
         [snap-controls]])]]))

(defn root
  []
  (let [ref (react/createRef)]
    (ra/create-class
     {:component-did-mount
      (fn []
        (rf/dispatch [:timeline/pause])
        (rf/dispatch [:timeline/set-time 0])
        (doseq
         [[e f]
          [["play" #(rf/dispatch-sync [:timeline/play])] ;; Prevent navigation
           ["paused" #(rf/dispatch-sync [:timeline/pause])]
           ["ended" #(when @(rf/subscribe [:timeline/replay?])
                       (.setTime (.-current ref) 0)
                       (.play (.-current ref) #js {:autoEnd true}))]
           ["afterSetTime" #(rf/dispatch-sync [:timeline/set-time (.-time %)])]
           ["setTimeByTick" #(rf/dispatch-sync [:timeline/set-time (.-time %)])]
           ["afterSetPlayRate" #(rf/dispatch [:timeline/set-speed (.-rate %)])]]]
          (.on (.-listener (.-current ref)) e f)))

      :component-will-unmount
      #(.offAll (.-listener (.-current ref)))

      :reagent-render
      (fn []
        (let [data @(rf/subscribe [:timeline/rows])
              effects @(rf/subscribe [:timeline/effects])
              grid-snap? @(rf/subscribe [:timeline/grid-snap?])
              guide-snap? @(rf/subscribe [:timeline/guide-snap?])
              timeline? @(rf/subscribe [:panel/visible? :timeline])]
          [:div
           [toolbar ref]
           [:> Timeline
            {:style {:height (if timeline? "200px" 0)}
             :editor-data data
             :effects effects
             :ref ref
             :grid-snap grid-snap?
             :drag-line guide-snap?
             :auto-scroll true
             :on-click-action #(rf/dispatch [:element/select (keyword (.. %2 -action -id))])}]]))})))
