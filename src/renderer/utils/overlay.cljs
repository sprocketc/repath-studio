(ns renderer.utils.overlay
  "Render functions for canvas overlay objects."
  (:require
   [clojure.core.matrix :as mat]
   [malli.core :as m]
   [re-frame.core :as rf]
   [renderer.app.db :refer [App]]
   [renderer.app.subs :as-alias app.s]
   [renderer.document.subs :as-alias document.s]
   [renderer.element.db :refer [Element]]
   [renderer.element.hierarchy :as element.hierarchy]
   [renderer.frame.subs :as-alias frame.s]
   [renderer.snap.subs :as-alias snap.s]
   [renderer.theme.db :as theme.db]
   [renderer.tool.subs :as-alias tool.s]
   [renderer.utils.bounds :as bounds :refer [Bounds]]
   [renderer.utils.element :as element]
   [renderer.utils.hiccup :refer [Hiccup]]
   [renderer.utils.math :as math :refer [Vec2]]))

(m/=> point-of-interest [:-> Vec2 Hiccup any?])
(defn point-of-interest
  "Simple dot used for debugging purposes."
  [[x y] & children]
  (let [zoom @(rf/subscribe [::document.s/zoom])]
    (into [:circle {:cx x
                    :cy y
                    :stroke-width 0
                    :fill theme.db/accent
                    :r (/ 3 zoom)}] children)))

(m/=> line [:function
            [:-> Vec2 Vec2 any?]
            [:-> Vec2 Vec2 boolean? any?]])
(defn line
  ([[x1 y1] [x2 y2]]
   [line [x1 y1] [x2 y2] true])
  ([[x1 y1] [x2 y2] dashed?]
   (let [zoom @(rf/subscribe [::document.s/zoom])
         stroke-width (/ 1 zoom)
         stroke-dasharray (/ 5 zoom)
         attrs {:x1 x1
                :y1 y1
                :x2 x2
                :y2 y2
                :stroke-width stroke-width
                :shape-rendering (when dashed? "crispEdges")}]
     [:g
      (when dashed? [:line (merge attrs {:stroke theme.db/accent-inverted})])
      [:line (merge attrs
                    {:stroke theme.db/accent
                     :stroke-dasharray (when dashed? stroke-dasharray)})]])))

(m/=> cross [:-> Vec2 any?])
(defn cross
  [[x y]]
  (let [zoom @(rf/subscribe [::document.s/zoom])
        size (/ theme.db/handle-size zoom)]
    [:g
     [line
      [(- x (/ size 2)) y]
      [(+ x (/ size 2)) y]
      false]
     [line
      [x (- y (/ size 2))]
      [x (+ y (/ size 2))]
      false]]))

(m/=> arc [:-> Vec2 number? number? number? any?])
(defn arc
  [[x y] radius start-degrees size-degrees]
  (let [zoom @(rf/subscribe [::document.s/zoom])
        stroke-width (/ 1 zoom)
        radius (/ radius zoom)
        end-degrees (+ start-degrees size-degrees)
        stroke-dasharray (/ theme.db/dash-size zoom)
        x1 (+ x (math/angle-dx start-degrees radius))
        y1 (+ y (math/angle-dy start-degrees radius))
        x2 (+ x (math/angle-dx end-degrees radius))
        y2 (+ y (math/angle-dy end-degrees radius))
        d (str "M" x1 "," y1 " "
               "A" radius "," radius " 0 0,1 " x2 "," y2)
        attrs {:d d
               :fill "transparent"
               :stroke-width stroke-width}]
    [:g
     [:path (merge {:stroke theme.db/accent-inverted} attrs)]
     [:path (merge {:stroke theme.db/accent
                    :stroke-dasharray stroke-dasharray} attrs)]]))

(m/=> times [:-> Vec2 any?])
(defn times
  [[x y]]
  (let [zoom @(rf/subscribe [::document.s/zoom])
        size (/ theme.db/handle-size zoom)
        mid (/ size Math/PI)]
    [:g {:style {:pointer-events "none"}}
     [line
      [(- x mid) (- y mid)]
      [(+ x mid) (+ y mid)]
      false]
     [line
      [(+ x mid) (- y mid)]
      [(- x mid) (+ y mid)]
      false]]))

(m/=> label [:function
             [:-> string? Vec2 any?]
             [:-> string? Vec2 [:enum "start" "middle" "end"] any?]])
(defn label
  ([text position]
   [label text position "middle"])
  ([text position text-anchor]
   (let [zoom @(rf/subscribe [::document.s/zoom])
         [x y] position
         font-size (/ 10 zoom)
         padding (/ 8 zoom)
         font-width (/ 6 zoom)
         label-width (+ (* (count text) font-width)
                        font-size)
         label-height (+ font-size padding)]
     [:g
      [:rect {:x (case text-anchor
                   "start" (- x (/ padding 2))
                   "middle" (- x (/ label-width 2))
                   "end" (- x label-width (/ (- padding) 2)))
              :y (- y  (/ label-height 2))
              :fill theme.db/accent
              :rx (/ 4 zoom)
              :width label-width
              :height label-height} text]
      [:text {:x x
              :y y
              :fill theme.db/accent-inverted
              :dominant-baseline "middle"
              :text-anchor text-anchor
              :width label-width
              :font-family theme.db/font-mono
              :font-size font-size} text]])))

(m/=> bounding-box [:-> Bounds boolean? any?])
(defn bounding-box
  [bounds dashed?]
  (let [zoom @(rf/subscribe [::document.s/zoom])
        [x1 y1 _x2 _y2] bounds
        [width height] (bounds/->dimensions bounds)
        stroke-width (/ 2 zoom)
        stroke-dasharray (/ theme.db/dash-size zoom)
        attrs {:x x1
               :y y1
               :width width
               :height height
               :shape-rendering "crispEdges"
               :stroke-width stroke-width
               :fill "transparent"}]

    [:g {:style {:pointer-events "none"}}
     [:rect (merge attrs {:stroke theme.db/accent})]
     (when dashed?
       [:rect (merge attrs {:stroke theme.db/accent-inverted
                            :stroke-dasharray stroke-dasharray})])]))

(m/=> select-box [:-> App any?])
(defn select-box
  [db]
  (let [zoom (get-in db [:documents (:active-document db) :zoom])
        [pos-x pos-y] (:adjusted-pointer-pos db)
        [offset-x offset-y] (:adjusted-pointer-offset db)]
    {:tag :rect
     :attrs {:x (min pos-x offset-x)
             :y (min pos-y offset-y)
             :width (abs (- pos-x offset-x))
             :height (abs (- pos-y offset-y))
             :shape-rendering "crispEdges"
             :fill-opacity ".1"
             :fill theme.db/accent
             :stroke theme.db/accent
             :stroke-opacity ".5"
             :stroke-width (/ 1 zoom)}}))

(m/=> centroid [:-> Element any?])
(defn centroid
  [el]
  (when-let [pos (element.hierarchy/centroid el)]
    (let [offset (element/offset el)
          pos (mat/add offset pos)]
      [point-of-interest pos
       [:title "Centroid"]])))
