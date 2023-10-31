(ns renderer.home
  (:require
   [re-frame.core :as rf]))

(defn panel []
  [:div.flex.overflow-auto.flex-1.min-h-full.justify-center
   [:div.level-1.w-full.self-center.justify-center.p-12
    {:style {:max-width "1200px"}}

    [:h1.text-xl.mb-1 "repath.studio"]

    [:h4.mb-2 "Scalable Vector Graphics Manipulation"]

    [:h2.mb-1.mt-2 "Start"]

    [:div
     [:a
      {:on-click #(rf/dispatch [:document/new])}
      "New"]
     [:span.text-muted " (Ctrl+N)"]]

    [:div
     [:a
      {:on-click #(rf/dispatch [:document/open])}
      "Open"]
     [:span.text-muted " (Ctrl+O)"]]

    #_[:h2.text-xl "Recent"]

    [:h2.mb-1.mt-2 "Help"]

    [:div
     [:a
      {:on-click #(rf/dispatch [:window/open-remote-url
                                "https://repath.studio/"])}
      "Website"]]

    [:div
     [:a
      {:on-click #(rf/dispatch [:window/open-remote-url
                                "https://repath.studio/docs/getting-started/"])}
      "Getting Started"]]

    [:div
     [:a
      {:on-click #(rf/dispatch [:window/open-remote-url
                                "https://github.com/re-path/studio"])}
      "Source Code"]]]])