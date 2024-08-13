(ns renderer.history.db
  (:require
   [renderer.element.db :as element.db]))

(def state
  [:map
   [:explanation string?]
   [:timestamp double?]
   [:index [:or pos-int? zero?]]
   [:id keyword?]
   [:elements element.db/elements]
   [:parent {:optional true} keyword?]
   [:children [:vector keyword?]]])

(def history
  [:map {:default {}}
   [:zoom {:optional true} [double? {:default 0.5}]]
   [:position {:optional true} keyword?]
   [:states [:map-of {:default {}} keyword? state]]])
