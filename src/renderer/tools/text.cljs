(ns renderer.tools.text
  (:require [renderer.elements.handlers :as elements]
            [renderer.attribute.hierarchy :as hierarchy]
            [renderer.utils.units :as units]
            [clojure.core.matrix :as matrix]
            [renderer.tools.base :as tools]
            [renderer.utils.bounds :as bounds]
            [renderer.handlers :as handlers]
            [re-frame.core :as rf]
            [clojure.string :as str]))

(derive :text ::tools/renderable)

(defmethod tools/properties :text
  []
  {:icon "text"
   :description "The SVG <text> element draws a graphics element consisting 
                 of text. It's possible to apply a gradient, pattern, 
                 clipping path, mask, or filter to <text>, like any other SVG 
                 graphics element."
   :attrs [:font-family
           :font-size
           :font-weight
           :font-style
           :stroke
           :stroke-width
           :opacity]})

(defmethod tools/activate :text
  [db]
  (-> db
      (assoc :cursor "text")
      (handlers/set-message
       [:div
        [:div "Click to enter your text."]])))

(defmethod tools/mouse-up :text
  [{:keys [adjusted-mouse-offset active-document] :as db}]
  (let [fill (get-in db [:documents active-document :fill])
        [offset-x offset-y] adjusted-mouse-offset
        attrs {:x offset-x
               :y offset-y
               :fill fill}]
    (-> db
        (elements/create {:type :element
                          :tag :text
                          :attrs attrs})
        (tools/set-tool :edit)
        (handlers/set-state :create))))

(defmethod tools/drag-end :text
  [db]
  (tools/mouse-up db))

(defmethod tools/translate :text
  [element [x y]] (-> element
                      (hierarchy/update-attr :x + x)
                      (hierarchy/update-attr :y + y)))

(defn set-text-and-select-element
  [event key]
  (rf/dispatch [:elements/set-property
                key
                :content
                (str/replace (.. event -target -value) "  " "\u00a0\u00a0")])
  (rf/dispatch [:set-tool :select]))

(defmethod tools/render-edit :text
  [{:keys [attrs key content] :as element}]
  (let [{:keys [fill font-family font-size font-weight]} attrs
        bounds (tools/bounds element)
        [width height] (bounds/->dimensions bounds)
        [x y] bounds
        active-page @(rf/subscribe [:elements/active-page])
        page-pos (mapv units/unit->px
                       [(-> active-page :attrs :x)
                        (-> active-page :attrs :y)])
        [x y] (if (not= (:tag element) :page)
                (matrix/add page-pos [x y])
                [x y])]
    [:foreignObject {:x (- x 2) 
                     :y (- y 2) 
                     :width (+ width 19)
                     :height (+ height 4)}
     [:input
      {:key key
       :default-value content
       :auto-focus true
       :on-focus #(.. % -target select)
       :on-pointer-down #(.stopPropagation %)
       :on-pointer-up #(.stopPropagation %)
       :on-blur #(set-text-and-select-element % key)
       :on-key-down (fn [event]
                      (.stopPropagation event)
                      (if (or (= 13 (.-keyCode event))
                              (= 27 (.-keyCode event)))
                        (set-text-and-select-element event key)
                        (.requestAnimationFrame
                         js/window
                         #(rf/dispatch-sync [:elements/preview-property
                                             key
                                             :content
                                             (str/replace (.. event -target -value)
                                                          " "
                                                          "\u00a0")]))))
       :style {:color fill
               :display "block"
               :width (+ width 15)
               :height height
               :padding 0
               :background "transparent"
               :font-family (if (empty? font-family) "inherit" font-family)
               :font-size (if (empty? font-size) "inherit" font-size)
               :font-weight (if (empty? font-weight) "inherit" font-weight)}}]]))

(defmethod tools/path :text
  [{:keys [attrs content]}]
  (.textToPath js/window.api
               (.-path (first (.findFonts
                               js/window.api
                               ;; TODO Getting the computed styles might safer
                               #js {:family (:font-family attrs)
                                    :weight (js/parseInt (:font-weight attrs))
                                    :italic (= (:font-style attrs)
                                               "italic")})))
               content
               (js/parseFloat (:x attrs))
               (js/parseFloat (:y attrs))
               (js/parseFloat (or (:font-size attrs) 16))))
    
(defn get-bounds
  "Experimental way of getting the bounds of uknown or complicated elements 
   using the getBBox method.
   https://developer.mozilla.org/en-US/docs/Web/API/SVGGraphicsElement/getBBox"
  [element textLength]
  (let [bounds (.getBBox element)
        x1 (.-x bounds)
        y1 (.-y bounds)
        x2 (+ x1 (if (zero? textLength) (.-width bounds) textLength))
        y2 (+ y1 (.-height bounds))]
    [x1 y1 x2 y2]))

(defmethod tools/bounds :text
  [{:keys [attrs content]}]
  (when-let [frame (.getElementById js/document "frame")]
   (when-let [svg (.getElementById (.. frame -contentWindow -document) "canvas")]
     (let [text-tag (js/document.createElementNS "http://www.w3.org/2000/svg" "text")]
       (doseq [[key value] attrs]
         (.setAttributeNS text-tag nil (name key) value))
       (.appendChild svg text-tag)
       (set! (.-innerHTML text-tag) (if (empty? content) "\u00a0" content))
       (let [{:keys [textLength]} attrs
             textLength (units/unit->px textLength)
             bounds (get-bounds text-tag textLength)]
         (.remove text-tag)
         bounds)))))