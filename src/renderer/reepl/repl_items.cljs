(ns renderer.reepl.repl-items
  (:require
   ["react" :as react]
   [cljs.reader]
   [cljs.tools.reader]
   [reagent.core :as r]
   [renderer.reepl.codemirror :as codemirror]
   [renderer.reepl.show-value :refer [show-value]]
   [renderer.utils.dom :as dom]))

(defmulti repl-item (fn [item _opts] (:type item)))

(defmethod repl-item :input
  [{:keys [_num text]} opts]
  [:div.repl-item
   [:div.text-disabled.font-bold "=>"]
   [:div.flex-1.cursor-pointer.break-words
    {:on-click (partial (:set-text opts) text)}
    text
    #_[codemirror/colored-text text]]])

(defmethod repl-item :log
  [{:keys [value]} opts]
  [:div.repl-item
   [show-value value nil opts]])

(defmethod repl-item :error
  [{:keys [value]} _opts]
  (let [message (.-message value)
        underlying (.-cause value)]
    [:div.repl-item.text-error
     message
     (when underlying
       ;; TODO: also show stack?
       [:span.ml-2.5 (.-message underlying)])]))

(defmethod repl-item :output
  [{:keys [value]} opts]
  [:div..repl-item
   [:div.flex-1.break-words [show-value value nil opts]]])

(defn repl-items [_]
  (let [ref (react/createRef)]
    (r/create-class
     {:component-did-mount
      (fn [_this]
        (let [el (.-current ref)]
          (dom/scroll-to-bottom! el)))
      :component-did-update
      (fn [_this]
        (let [el (.-current ref)]
          (dom/scroll-to-bottom! el)))
      :reagent-render
      (fn [items opts]
        (into
         [:div.flex-1.border-b.border-default.h-full.overflow-y-auto.overflow-x-hidden.p-1
          {:ref ref}]
         (map #(repl-item % opts) items)))})))
