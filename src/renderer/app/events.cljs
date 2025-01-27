(ns renderer.app.events
  (:require
   [config :as config]
   [malli.error :as me]
   [re-frame.core :as rf]
   [renderer.app.db :as db]
   [renderer.app.effects :as-alias fx]
   [renderer.notification.events :as-alias notification.e]
   [renderer.notification.handlers :as notification.h]
   [renderer.notification.views :as notification.v]
   [renderer.utils.i18n :as i18n]
   [renderer.window.effects :as-alias window.fx]))

(def persist
  (rf/->interceptor
   :id ::persist
   :after (fn [context]
            (let [db (rf/get-effect context :db)
                  fx (rf/get-effect context :fx)]
              (cond-> context
                db
                (rf/assoc-effect :fx (conj (or fx []) [::fx/persist])))))))

(rf/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default))

(rf/reg-event-fx
 ::load-local-db
 [(rf/inject-cofx :store)]
 (fn [{:keys [db store]} _]
   (let [app-db (merge db store)]
     (if (db/valid? app-db)
       {:db app-db}
       {::fx/local-storage-clear nil
        :db (cond-> db
              config/debug?
              (notification.h/add (notification.v/spec-failed
                                   "Invalid local configuration"
                                   (-> app-db db/explain me/humanize str))))}))))

(rf/reg-event-db
 ::set-system-fonts
 (fn [db [_ fonts]]
   (assoc db :system-fonts fonts)))

(rf/reg-event-db
 ::set-lang
 (fn [db [_ lang]]
   (cond-> db
     (i18n/lang? lang)
     (assoc :lang lang))))

(rf/reg-event-db
 ::set-repl-mode
 (fn [db [_ mode]]
   (assoc db :repl-mode mode)))

(rf/reg-event-db
 ::toggle-debug-info
 (fn [db [_]]
   (update db :debug-info not)))

(rf/reg-event-db
 ::set-backdrop
 (fn [db [_ visible]]
   (assoc db :backdrop visible)))

(rf/reg-event-db
 ::toggle-grid
 [persist]
 (fn [db [_]]
   (update db :grid not)))

(rf/reg-event-db
 ::toggle-panel
 [persist]
 (fn [db [_ k]]
   (update-in db [:panels k :visible] not)))

(rf/reg-event-fx
 ::focus
 (fn [_ [_ id]]
   {::fx/focus id}))

(defn ->font-map
  [^js/FontData font-data]
  (into {} [[:postscriptName (.-postscriptName font-data)]
            [:fullName (.-fullName font-data)]
            [:family (.-family font-data)]
            [:style (.-style font-data)]]))

(rf/reg-event-fx
 ::load-system-fonts
 (fn [_ _]
   {::window.fx/ipc-invoke {:channel "load-system-fonts"
                            :on-success [::set-system-fonts]
                            :on-error [::notification.e/exception]
                            :formatter #(js->clj % :keywordize-keys true)}}))

(rf/reg-event-fx
 ::query-local-fonts
 (fn [_ _]
   {::fx/query-local-fonts {:on-success [::set-system-fonts]
                            :on-error [::notification.e/exception]
                            :formatter #(mapv ->font-map %)}}))

(rf/reg-event-fx
 ::file-open
 (fn [_ [_ options]]
   {::fx/file-open options}))

(def schema-validator
  (rf/->interceptor
   :id ::schema-validator
   :after (fn [context]
            (let [db (or (rf/get-effect context :db)
                         (rf/get-coeffect context :db))
                  fx (rf/get-effect context :fx)
                  event (rf/get-coeffect context :event)]
              (cond-> context
                db
                (rf/assoc-effect :fx (conj (or fx []) [::fx/validate-db [db event]])))))))

(rf/reg-event-fx
 ::scroll-into-view
 (fn [_ [_ el]]
   {::fx/scroll-into-view el}))

(rf/reg-event-fx
 ::scroll-to-bottom
 (fn [_ [_ el]]
   {::fx/scroll-to-bottom el}))
