(ns renderer.tools.fill
  (:require
   [renderer.element.handlers :as elements]
   [renderer.handlers :as handlers]
   [renderer.history.handlers :as history]
   [renderer.tools.base :as tools]))

(derive :fill ::tools/misc)

(defmethod tools/properties :fill
  []
  {:icon "fill"})

(defmethod tools/activate :fill
  [db]
  (-> db
      (assoc :cursor "crosshair")
      (handlers/set-message  [:div "Click on an element to fill."])))

(defmethod tools/translate :fill [])

(defmethod tools/mouse-up :fill
  [{active-document :active-document :as db} _e el]
  (let [color (get-in db [:documents active-document :fill])]
    (-> db
        (elements/set-attribute (:key el) :fill color)
        (history/finalize (str "Fill " color)))))

(defmethod tools/drag-end :fill
  [db e el]
  (tools/mouse-up db e el))
