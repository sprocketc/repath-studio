(ns renderer.snap.handlers
  (:require
   [clojure.core.matrix :as mat]
   [kdtree :as kdtree]
   [malli.core :as m]
   [renderer.app.db :refer [App]]
   [renderer.element.handlers :as element.h]
   [renderer.frame.handlers :as frame.h]
   [renderer.ruler.handlers :as ruler.h]
   [renderer.snap.db :refer [SnapOption NearestNeighbor]]
   [renderer.snap.subs :as-alias snap.s]
   [renderer.tool.hierarchy :as tool.hierarchy]
   [renderer.utils.math :refer [Vec2D]]))

(m/=> toggle-option [:-> App SnapOption App])
(defn toggle-option
  [db option]
  (if (contains? (-> db :snap :options) option)
    (update-in db [:snap :options] disj option)
    (update-in db [:snap :options] conj option)))

(m/=> nearest-neighbors [:-> App [:sequential NearestNeighbor]])
(defn nearest-neighbors
  [db]
  (->> (tool.hierarchy/snapping-points db)
       (map #(when-let [nneighbor (kdtree/nearest-neighbor (:viewbox-kdtree db) %)]
               (assoc nneighbor :base-point %)))
       (remove nil?)))

(m/=> update-nearest-neighbors [:-> App App])
(defn update-nearest-neighbors
  [db]
  (let [zoom (get-in db [:documents (:active-document db) :zoom])
        threshold (-> db :snap :threshold)
        threshold (Math/pow (/ threshold zoom) 2)
        nneighbors (nearest-neighbors db)
        nneighbors (filter #(< (:dist-squared %) threshold) nneighbors)]
    (assoc db
           :nearest-neighbors nneighbors
           :nearest-neighbor (apply min-key :dist-squared nneighbors))))

(m/=> update-viewport-tree [:-> App App])
(defn update-viewport-tree
  [db]
  (let [[x y width height] (frame.h/viewbox db)
        boundaries [[x (+ x width)] [y (+ y height)]]]
    (assoc db :viewbox-kdtree (-> (:kdtree db)
                                  (kdtree/interval-search boundaries)
                                  (kdtree/build-tree)))))

(m/=> rebuild-tree [:-> App App])
(defn rebuild-tree
  [db]
  (if (-> db :snap :active)
    (let [elements (tool.hierarchy/snapping-elements db)
          points (element.h/snapping-points db elements)
          points (cond-> points
                   (contains? (-> db :snap :options) :grid)
                   (into (ruler.h/steps-intersections db)))]
      (-> (assoc db :kdtree (kdtree/build-tree points))
          (update-viewport-tree)))
    (dissoc db :kdtree :viewbox-kdtree)))

(m/=> update-tree [:-> App fn? [:vector Vec2D] App])
(defn update-tree
  [db f points]
  (if (:kdtree db)
    (if (empty? points)
      db
      (-> (reduce #(update %1 :kdtree f %2) db points)
          (update-viewport-tree)))
    (rebuild-tree db)))

(m/=> insert-to-tree [:-> App [:maybe [:set uuid?]] App])
(defn insert-to-tree
  [db element-ids]
  (let [elements (vals (element.h/entities db element-ids))
        points (element.h/snapping-points db elements)]
    (update-tree db kdtree/insert points)))

(m/=> delete-from-tree [:-> App [:maybe [:set uuid?]] App])
(defn delete-from-tree
  [db element-ids]
  (let [elements (vals (element.h/entities db element-ids))
        points (element.h/snapping-points db elements)]
    (update-tree db kdtree/delete points)))

(m/=> nearest-delta [:-> App Vec2D])
(defn nearest-delta
  [db]
  (if (:nearest-neighbor db)
    (let [{:keys [point base-point]} (:nearest-neighbor db)]
      (mat/sub point base-point))
    [0 0]))

(defn snap-with
  [db f & more]
  (let [db (update-nearest-neighbors db)]
    (if (:nearest-neighbor db)
      (apply f db (nearest-delta db) more)
      db)))
