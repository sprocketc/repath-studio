(ns renderer.tools.canvas
  "The main SVG element that hosts all pages."
  (:require
   [clojure.string :as str]
   [re-frame.core :as rf]
   [renderer.filters :as filters]
   [renderer.overlay :as overlay]
   [renderer.rulers.views :as rulers]
   [renderer.tools.base :as tools]
   [renderer.utils.keyboard :as keyb]
   [renderer.utils.pointer :as pointer]))

(derive :canvas ::tools/tool)

(defmethod tools/properties :canvas
  []
  {:description "The canvas is the main SVG element that hosts all pages."
   :attrs [:fill]})

(defmethod tools/render :canvas
  [{:keys [attrs children] :as element}]
  (let [child-elements @(rf/subscribe [:element/filter-visible children])
        elements @(rf/subscribe [:document/elements])
        viewbox @(rf/subscribe [:frame/viewbox])
        {:keys [width height]} @(rf/subscribe [:content-rect])
        hovered-or-selected @(rf/subscribe [:element/hovered-or-selected])
        selected-elements @(rf/subscribe [:element/selected])
        bounds @(rf/subscribe [:element/bounds])
        temp-element @(rf/subscribe [:document/temp-element])
        elements-area @(rf/subscribe [:element/area])
        cursor @(rf/subscribe [:cursor])
        tool @(rf/subscribe [:tool])
        primary-tool @(rf/subscribe [:primary-tool])
        rotate @(rf/subscribe [:document/rotate])
        snapping-points @(rf/subscribe [:snapping-points])
        debug-info? @(rf/subscribe [:debug-info?])
        grid? @(rf/subscribe [:grid?])
        state @(rf/subscribe [:state])
        pointer-handler #(pointer/event-handler % element)
        pivot-point @(rf/subscribe [:pivot-point])
        select? (or (= tool :select)
                    (= primary-tool :select))]
    [:svg {:on-pointer-up pointer-handler
           :on-pointer-down pointer-handler
           :on-double-click pointer-handler
           :on-key-up keyb/event-handler
           :on-key-down keyb/event-handler
           :tab-index 0 ; Enable keyboard events 
           :viewBox (str/join " " viewbox)
           :on-drop pointer-handler
           :on-drag-over #(.preventDefault %)
           :width width
           :height height
           :transform (str "rotate(" rotate ")")
           :cursor cursor
           :id "canvas"
           :style {:background (:fill attrs)
                   :outline "none"}}
     (for [el child-elements]
       ^{:key (str (:key el))} [tools/render el])

     [:defs
      (map (fn [{:keys [id tag attrs]}]
             [:filter {:id id :key id} [tag attrs]])
           filters/accessibility)]

     (when debug-info?
       (into [:g] (map overlay/point-of-interest snapping-points)))

     (when (and select? (contains? #{:default :select :scale} state))
       [:<>
        (for [el hovered-or-selected]
          ^{:key (str (:key el) "-bounds")}
          [overlay/bounding-box (tools/adjusted-bounds el elements)])

        (when (pos? elements-area)
          [overlay/area elements-area bounds])

        (when (not-empty (remove zero? bounds))
          [:<>
           [overlay/size bounds]
           [overlay/bounding-handlers bounds]])

        (when (and select? pivot-point)
          [overlay/times pivot-point])])

     (when (or (= tool :edit)
               (= primary-tool :edit))
       (for [el selected-elements]
         ^{:key (str (:key el) "-edit-points")}
         [:g
          [tools/render-edit el]
          ^{:key (str (:key el) "-centroid")}
          [overlay/centroid el]]))

     [tools/render temp-element]

     (when grid? [rulers/grid])]))
