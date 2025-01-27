(ns renderer.attribute.impl.href
  "https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/href"
  (:require
   [clojure.string :as str]
   [re-frame.core :as rf]
   [renderer.app.effects :as-alias app.fx]
   [renderer.app.events :as app.e]
   [renderer.attribute.hierarchy :as hierarchy]
   [renderer.attribute.views :as v]
   [renderer.element.events :as-alias element.e]
   [renderer.notification.events :as-alias notification.e]
   [renderer.tool.events :as-alias tool.e]
   [renderer.tool.handlers :as tool.h]
   [renderer.tool.subs :as-alias tool.s]
   [renderer.ui :as ui]))

(defmethod hierarchy/description [:default :href]
  []
  "The href attribute defines a link to a resource as a reference URL.
   The exact meaning of that link depends on the context of each element using it.")

(defmethod hierarchy/form-element [:default :href]
  [_ k v {:keys [disabled]}]
  (let [state-default? (= @(rf/subscribe [::tool.s/state]) :idle)
        data-url? (and v (str/starts-with? v "data:"))]
    [:div.flex.gap-px.w-full
     [v/form-input k (if data-url? "data-url" v)
      {:placeholder (when-not v "multiple")
       :disabled (or disabled
                     data-url?
                     (not v)
                     (not state-default?))}]
     [:button.form-control-button
      {:title "Select file"
       :disabled disabled
       :on-click #(rf/dispatch
                   [::app.e/file-open {:options {:startIn "pictures"
                                                 :types [{:accept {"image/png" [".png"]
                                                                   "image/jpeg" [".jpeg" ".jpg"]
                                                                   "image/bmp" [".fmp"]}}]}
                                       :on-success [::success]}])}
      [ui/icon "folder"]]]))

(rf/reg-event-fx
 ::success
 (fn [{:keys [db]} [_ file]]
   {:db (tool.h/activate db :transform)
    ::app.fx/file-read-as [file
                           :data-url
                           {"load" {:on-fire [::element.e/set-attr :href]}
                            "error" {:on-fire [::notification.e/exception]}}]}))
