(ns renderer.tool.container.canvas
  "The main SVG element that hosts all pages."
  (:require
   [re-frame.core :as rf]
   [renderer.app.subs :as-alias app.s]
   [renderer.document.subs :as-alias document.s]
   [renderer.element.subs :as-alias element.s]
   [renderer.frame.subs :as-alias frame.s]
   [renderer.menubar.filters :as filters]
   [renderer.ruler.views :as ruler.v]
   [renderer.snap.subs :as-alias snap.s]
   [renderer.tool.base :as tool]
   [renderer.tool.overlay :as overlay]
   [renderer.utils.keyboard :as keyb]
   [renderer.utils.pointer :as pointer]))

(derive :canvas ::tool/element)

(defmethod tool/properties :canvas
  []
  {:description "The canvas is the main SVG container that hosts all elements."
   :attrs [:fill]})

(defmethod tool/render :canvas
  [{:keys [attrs children] :as element}]
  (let [_ @(rf/subscribe [::snap.s/in-viewport-tree]) ; TODO: Remove this.
        child-elements @(rf/subscribe [::element.s/filter-visible children])
        viewbox-attr @(rf/subscribe [::frame.s/viewbox-attr])
        {:keys [width height]} @(rf/subscribe [::app.s/dom-rect])
        hovered-ids @(rf/subscribe [::element.s/hovered])
        selected-elements @(rf/subscribe [::element.s/selected])
        bounds @(rf/subscribe [::element.s/bounds])
        temp-element @(rf/subscribe [::document.s/temp-element])
        elements-area @(rf/subscribe [::element.s/area])
        read-only? @(rf/subscribe [::document.s/read-only?])
        cursor @(rf/subscribe [::app.s/cursor])
        tool @(rf/subscribe [::app.s/tool])
        primary-tool @(rf/subscribe [::app.s/primary-tool])
        rotate @(rf/subscribe [::document.s/rotate])
        grid? @(rf/subscribe [::app.s/grid-visible?])
        state @(rf/subscribe [::app.s/state])
        pointer-handler #(pointer/event-handler % element)
        pivot-point @(rf/subscribe [::app.s/pivot-point])
        snapping-points @(rf/subscribe [::snap.s/points])
        snap? @(rf/subscribe [::snap.s/enabled?])
        nearest-neighbor @(rf/subscribe [::snap.s/nearest-neighbor])
        debug? @(rf/subscribe [::app.s/debug-info?])
        select? (or (= tool :select) (= primary-tool :select))]
    [:svg#canvas {:on-pointer-up pointer-handler
                  :on-pointer-down pointer-handler
                  :on-double-click pointer-handler
                  :on-key-up keyb/event-handler
                  :on-key-down keyb/event-handler
                  :tab-index 0 ; Enable keyboard events
                  :viewBox viewbox-attr
                  :on-drop pointer-handler
                  :on-drag-over #(.preventDefault %)
                  :width width
                  :height height
                  :transform (str "rotate(" rotate ")")
                  :cursor cursor
                  :style {:outline 0
                          :background (:fill attrs)}}
     (for [el child-elements]
       ^{:key (:id el)} [tool/render el])

     [:defs
      (map (fn [{:keys [id tag attrs]}]
             [:filter {:id id :key id} [tag attrs]])
           filters/accessibility)]

     (when-not read-only?
       [:<>
        (when (and select? (contains? #{:default :select :scale} state))
          [:<>
           (for [el selected-elements]
             (when (:bounds el)
               ^{:key (str (:id el) "-bounds")}
               [overlay/bounding-box (:bounds el) false]))

           (for [el hovered-ids]
             (when (:bounds el)
               ^{:key (str (:id el) "-bounds")}
               [overlay/bounding-box (:bounds el) true]))

           (when (and (pos? elements-area) (= state :scale) (seq bounds))
             [overlay/area-label elements-area bounds])

           (when (seq bounds)
             [:<>
              [overlay/wrapping-bounding-box bounds]
              (when (= state :scale) [overlay/size-label bounds])
              [overlay/bounding-handles bounds]])

           (when (and select? pivot-point)
             [overlay/times pivot-point])])

        (when (or (= tool :edit)
                  (= primary-tool :edit))
          (for [el selected-elements]
            ^{:key (str (:id el) "-edit-points")}
            [:g
             [tool/render-edit el]
             ^{:key (str (:id el) "-centroid")}
             [overlay/centroid el]]))

        [tool/render temp-element]])

     (when debug?
       [into [:g]
        (for [snapping-point snapping-points]
          [overlay/point-of-interest snapping-point])])

     (when (and snap? nearest-neighbor)
       [overlay/times (:point nearest-neighbor)])

     (when grid? [ruler.v/grid])]))

(defmethod tool/render-to-string :canvas
  [{:keys [attrs children]}]
  (let [child-elements @(rf/subscribe [::element.s/filter-visible children])
        attrs (->> (dissoc attrs :fill)
                   (remove #(empty? (str (second %))))
                   (into {}))]
    (->> (doall (map tool/render-to-string (merge child-elements)))
         (into [:svg attrs]))))
