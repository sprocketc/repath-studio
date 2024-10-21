(ns renderer.element.handlers
  (:require
   [clojure.core.matrix :as mat]
   [clojure.set :as set]
   [clojure.string :as str]
   [hickory.core :as hickory]
   [hickory.zip]
   [malli.core :as m]
   [malli.error :as me]
   [renderer.app.db :refer [App]]
   [renderer.attribute.hierarchy :as attr.hierarchy]
   [renderer.element.db :as db :refer [Element Tag]]
   [renderer.element.hierarchy :as hierarchy]
   [renderer.notification.handlers :as notification.h]
   [renderer.notification.views :as notification.v]
   [renderer.utils.attribute :as attr]
   [renderer.utils.bounds :as bounds :refer [Bounds]]
   [renderer.utils.element :as element]
   [renderer.utils.extra :refer [partial-right]]
   [renderer.utils.hiccup :as hiccup]
   [renderer.utils.map :as map]
   [renderer.utils.math :refer [Vec2D]]
   [renderer.utils.path :as path]
   [renderer.utils.vec :as vec]))

(m/=> path [:function
            [:-> App vector?]
            [:-> App uuid? vector?]])
(defn path
  ([db]
   [:documents (:active-document db) :elements])
  ([db id]
   (conj (path db) id)))

(m/=> entities [:function
                [:-> App [:map-of uuid? Element]]
                [:-> App [:set uuid?] [:map-of uuid? Element]]])
(defn entities
  ([db]
   (get-in db (path db)))
  ([db ids]
   (select-keys (entities db) (vec ids))))

(m/=> entity [:-> App uuid? Element])
(defn entity
  [db id]
  (get (entities db) id))

(m/=> root [:-> App Element])
(defn root
  [db]
  (some #(when (element/root? %) %) (vals (entities db))))

(m/=> locked? [:-> App uuid? boolean?])
(defn locked?
  [db id]
  (-> db (entity id) :locked boolean))

(m/=> selected [:-> App [:sequential Element]])
(defn selected
  [db]
  (filter :selected (vals (entities db))))

(m/=> ratio-locked? [:-> App boolean?])
(defn ratio-locked?
  [db]
  (every? element/ratio-locked? (selected db)))

(m/=> selected-ids [:-> App [:set uuid?]])
(defn selected-ids
  [db]
  (set (map :id (selected db))))

(m/=> children-ids [:-> App uuid? [:vector uuid?]])
(defn children-ids
  [db id]
  (:children (entity db id)))

(m/=> parent [:function
              [:-> App [:maybe Element]]
              [:-> App uuid? [:maybe Element]]])
(defn parent
  ([db]
   (let [selected-ks (selected-ids db)]
     (or (parent db (first selected-ks))
         (root db))))
  ([db id]
   (when-let [parent-id (:parent (entity db id))]
     (entity db parent-id))))

(m/=> parent-container [:-> App uuid? Element])
(defn parent-container
  [db id]
  (loop [parent-el (parent db id)]
    (when parent-el
      (if (element/container? parent-el)
        parent-el
        (recur (parent db (:id parent-el)))))))

(m/=> adjusted-bounds [:-> App uuid? [:maybe Bounds]])
(defn adjusted-bounds
  [db id]
  (when-let [bounds (hierarchy/bounds (entity db id))]
    (if-let [container (parent-container db id)]
      (let [[offset-x offset-y _ _] (hierarchy/bounds container)
            [x1 y1 x2 y2] bounds]
        [(+ x1 offset-x) (+ y1 offset-y) (+ x2 offset-x) (+ y2 offset-y)])
      bounds)))

(m/=> refresh-bounds [:-> App uuid? App])
(defn refresh-bounds
  [db id]
  (let [el (entity db id)
        bounds (if (= (:tag el) :g)
                 (let [b (map #(adjusted-bounds db %) (children-ids db id))]
                   (when (seq b) (apply bounds/union b)))
                 (adjusted-bounds db id))
        assoc-bounds #(assoc % :bounds bounds)]
    (if (or (not bounds) (element/root? el))
      db
      (-> (reduce refresh-bounds db (children-ids db id))
          (update-in (conj (path db) id) assoc-bounds)))))

(defn update-el
  ([db id f]
   (if (locked? db id)
     db
     (-> (update-in db (conj (path db) id) f)
         (refresh-bounds id))))
  ([db id f arg1]
   (if (locked? db id)
     db
     (-> (update-in db (conj (path db) id) f arg1)
         (refresh-bounds id))))
  ([db id f arg1 arg2]
   (if (locked? db id)
     db
     (-> (update-in db (conj (path db) id) f arg1 arg2)
         (refresh-bounds id))))
  ([db id f arg1 arg2 arg3]
   (if (locked? db id)
     db
     (-> (update-in db (conj (path db) id) f arg1 arg2 arg3)
         (refresh-bounds id))))
  ([db id f arg1 arg2 arg3 & more]
   (if (locked? db id)
     db
     (-> (apply update-in db (conj (path db) id) f arg1 arg2 arg3 more)
         (refresh-bounds id)))))

(m/=> siblings-selected? [:-> App boolean?])
(defn siblings-selected?
  [db]
  (let [selected-els (selected db)
        parent-els (set (map :parent selected-els))]
    (and (seq parent-els)
         (empty? (rest parent-els))
         (= (count selected-els)
            (count (children-ids db (first parent-els)))))))

(m/=> siblings [:function
                [:-> App [:vector uuid?]]
                [:-> App uuid? [:vector uuid?]]])
(defn siblings
  ([db]
   (:children (parent db)))
  ([db id]
   (:children (parent db id))))

(m/=> root-children [:-> App [:sequential Element]])
(defn root-children
  [db]
  (->> (children-ids db (:id (root db)))
       (mapv (entities db))))

(m/=> root-svgs [:-> App [:sequential Element]])
(defn root-svgs
  [db]
  (->> db root-children (filter element/svg?)))

(m/=> ancestor-ids [:function
                    [:-> App [:sequential uuid?]]
                    [:-> App uuid? [:sequential uuid?]]])
(defn ancestor-ids
  ([db]
   (reduce #(concat %1 (ancestor-ids db %2)) [] (selected-ids db)))
  ([db id]
   (loop [parent-id (:parent (entity db id))
          parent-ids []]
     (if parent-id
       (recur
        (:parent (entity db parent-id))
        (conj parent-ids parent-id))
       parent-ids))))

(m/=> index [:-> App uuid? [:maybe int?]])
(defn index
  [db id]
  (when-let [sibling-els (siblings db id)]
    (.indexOf sibling-els id)))

(m/=> index-tree-path [:-> App uuid? [:sequential int?]])
(defn index-tree-path
  "Returns a sequence that represents the index tree path of an element.
   For example, the seventh element of the second svg on the canvas will
   return [2 7]. This is useful when we need to figure out the global index
   of nested elements."
  [db id]
  (let [ancestor-els (reverse (ancestor-ids db id))]
    (conj (mapv #(index db %) ancestor-els)
          (index db id))))

(defn descendant-ids
  ([db]
   (reduce #(set/union %1 (descendant-ids db %2)) #{} (selected-ids db)))
  ([db id]
   (loop [children-set (set (children-ids db id))
          child-keys #{}]
     (if (seq children-set)
       (recur
        (reduce #(set/union %1 (set (children-ids db %2))) #{} children-set)
        (set/union child-keys children-set))
       child-keys))))

(m/=> top-ancestor-ids [:-> App [:set uuid?]])
(defn top-ancestor-ids
  [db]
  (set/difference (selected-ids db) (descendant-ids db)))

(m/=> top-selected-ancestors [:-> App [:sequential Element]])
(defn top-selected-ancestors
  [db]
  (->> (top-ancestor-ids db)
       (entities db)
       (vals)))

(m/=> dissoc-temp [:-> App App])
(defn dissoc-temp
  [db]
  (cond-> db
    (:active-document db)
    (update-in [:documents (:active-document db)] dissoc :temp-element)))

(m/=> set-temp [:-> App map? App])
(defn set-temp
  [db el]
  (->> (element/normalize-attrs el)
       (assoc-in db [:documents (:active-document db) :temp-element])))

(m/=> temp [:-> App [:maybe Element]])
(defn temp
  [db]
  (get-in db [:documents (:active-document db) :temp-element]))

(defn update-prop
  ([db id k f]
   (-> (update-in db (conj (path db) id k) f)
       (refresh-bounds id)))
  ([db id k f arg1]
   (-> (update-in db (conj (path db) id k) f arg1)
       (refresh-bounds id)))
  ([db id k f arg1 arg2]
   (-> (update-in db (conj (path db) id k) f arg1 arg2)
       (refresh-bounds id)))
  ([db id k f arg1 arg2 arg3]
   (-> (update-in db (conj (path db) id k) f arg1 arg2 arg3)
       (refresh-bounds id)))
  ([db id k arg1 arg2 arg3 & more]
   (-> (apply update-in db (conj (path db) id k) arg1 arg2 arg3 more)
       (refresh-bounds id))))

(defn assoc-prop
  ([db k v]
   (reduce (partial-right assoc-prop k v) db (selected-ids db)))
  ([db id k v]
   (-> (if (str/blank? v)
         (update-in db (conj (path db) id) dissoc k)
         (assoc-in db (conj (path db) id k) v))
       (refresh-bounds id))))

(defn dissoc-attr
  ([db k]
   (reduce (partial-right dissoc-attr k) db (selected-ids db)))
  ([db id k]
   (cond-> db
     (not (locked? db id))
     (update-prop id :attrs dissoc k))))

(defn assoc-attr
  ([db k v]
   (reduce (partial-right assoc-attr k v) db (selected-ids db)))
  ([db id k v]
   (cond-> db
     (not (locked? db id))
     (-> (assoc-in (conj (path db) id :attrs k) v)
         (refresh-bounds id)))))

(defn set-attr
  ([db k v]
   (reduce (partial-right set-attr k v) db (selected-ids db)))
  ([db id k v]
   (if (and (not (locked? db id))
            (element/supported-attr? (entity db id) k))
     (if (str/blank? v)
       (dissoc-attr db id k)
       (assoc-attr db id k (str/trim (str v))))
     db)))

(defn update-attr
  ([db id k f]
   (if (element/supported-attr? (entity db id) k)
     (update-el db id attr.hierarchy/update-attr k f)
     db))
  ([db id k f arg1]
   (if (element/supported-attr? (entity db id) k)
     (update-el db id attr.hierarchy/update-attr k f arg1)
     db))
  ([db id k f arg1 arg2]
   (if (element/supported-attr? (entity db id) k)
     (update-el db id attr.hierarchy/update-attr k f arg1 arg2)
     db))
  ([db id k f arg1 arg2 arg3]
   (if (element/supported-attr? (entity db id) k)
     (update-el db id attr.hierarchy/update-attr k f arg1 arg2 arg3)
     db))
  ([db id k f arg1 arg2 arg3 & more]
   (if (element/supported-attr? (entity db id) k)
     (apply update-el db id attr.hierarchy/update-attr k f arg1 arg2 arg3 more)
     db)))

(defn deselect
  ([db]
   (reduce deselect db (keys (entities db))))
  ([db id]
   (assoc-prop db id :selected false)))

(defn collapse
  ([db]
   (reduce collapse db (keys (entities db))))
  ([{:keys [active-document] :as db} id]
   (update-in db [:documents active-document :collapsed-ids] conj id)))

(defn expand
  ([db]
   (reduce expand db (keys (entities db))))
  ([db id]
   (update-in db [:documents (:active-document db) :collapsed-ids] disj id)))

(m/=> expand-ancestors [:-> App uuid? App])
(defn expand-ancestors
  [db id]
  (->> (ancestor-ids db id)
       (reduce expand db)))

(defn select
  ([db id]
   (-> db
       (expand-ancestors id)
       (assoc-prop id :selected true)))
  ([db id multiple]
   (if (entity db id)
     (if multiple
       (update-prop db id :selected not)
       (-> db deselect (select id)))
     (deselect db))))

(defn select-ids
  [db ids]
  (reduce (partial-right assoc-prop :selected true) (deselect db) ids))

(m/=> select-all [:-> App App])
(defn select-all
  [db]
  (reduce select db (if (siblings-selected? db)
                      (children-ids db (:id (parent db (:id (parent db)))))
                      (siblings db))))

(m/=> selected-tags [:-> App [:set Tag]])
(defn selected-tags
  [db]
  (->> (selected db)
       (map :tag)
       (set)))

(m/=> filter-by-tag [:-> App Tag [:sequential Element]])
(defn filter-by-tag
  [db tag]
  (filter #(= tag (:tag %)) (selected db)))

(m/=> select-same-tags [:-> App App])
(defn select-same-tags
  [db]
  (let [tags (selected-tags db)]
    (->> (entities db)
         (vals)
         (reduce (fn [db el] (cond-> db
                               (contains? tags (:tag el))
                               (select (:id el)))) db))))

(m/=> selected-sorted [:-> App [:sequential Element]])
(defn selected-sorted
  [db]
  (sort-by #(index-tree-path db (:id %)) (selected db)))

(m/=> top-selected-sorted [:-> App [:sequential Element]])
(defn top-selected-sorted
  [db]
  (sort-by #(index-tree-path db (:id %)) (top-selected-ancestors db)))

(m/=> selected-sorted-ids [:-> App [:vector uuid?]])
(defn selected-sorted-ids
  [db]
  (mapv :id (selected-sorted db)))

(m/=> top-selected-sorted-ids [:-> App [:vector uuid?]])
(defn top-selected-sorted-ids
  [db]
  (mapv :id (top-selected-sorted db)))

(m/=> invert-selection [:-> App App])
(defn invert-selection
  [db]
  (reduce (fn [db {:keys [id tag]}]
            (cond-> db
              (not (contains? #{:svg :canvas} tag))
              (update-prop id :selected not)))
          db
          (vals (entities db))))

(m/=> hover [:-> App [:or uuid? keyword?] App])
(defn hover
  [db id]
  (update-in db [:documents (:active-document db) :hovered-ids] conj id))

(m/=> ignore [:-> App [:or uuid? keyword?] App])
(defn ignore
  [db id]
  (cond-> db
    (and (:active-document db) id)
    (update-in [:documents (:active-document db) :ignored-ids] conj id)))

(m/=> clear-hovered [:-> App App])
(defn clear-hovered
  [db]
  (cond-> db
    (:active-document db)
    (assoc-in [:documents (:active-document db) :hovered-ids] #{})))

(defn clear-ignored
  ([db]
   (cond-> db
     (:active-document db)
     (assoc-in [:documents (:active-document db) :ignored-ids] #{})))
  ([db id]
   (cond-> db
     (:active-document db)
     (update-in [:documents (:active-document db) :ignored-ids] disj id))))

(m/=> bounds [:-> App [:maybe Bounds]])
(defn bounds
  [db]
  (element/united-bounds (selected db)))

(m/=> copy [:-> App App])
(defn copy
  [db]
  (let [els (top-selected-sorted db)]
    (cond-> db
      (seq els)
      (assoc :copied-elements els
             :copied-bounds (bounds db)))))

(defn delete
  ([db]
   (reduce delete db (reverse (selected-sorted-ids db))))
  ([db id]
   (let [el (entity db id)
         db (if (element/root? el) db (reduce delete db (:children el)))]
     (cond-> db
       (not (element/root? el))
       (-> (update-prop (:parent el) :children vec/remove-nth (index db id))
           (update-in (path db) dissoc id)
           (expand id))))))

(defn update-index
  ([db f]
   (reduce (partial-right update-index f) db (selected-sorted-ids db)))
  ([db id f]
   (let [sibling-count (count (siblings db id))
         i (index db id)
         new-index (f i sibling-count)]
     (cond-> db
       (<= 0 new-index (dec sibling-count))
       (update-prop (:id (parent db id)) :children vec/move i new-index)))))

(defn set-parent
  ([db parent-id]
   (reduce #(set-parent %1 parent-id %2) db (selected-sorted-ids db)))
  ([db parent-id id]
   (let [el (entity db id)]
     (cond-> db
       (and el (not= id parent-id) (not (locked? db id)))
       (-> (update-prop (:parent el) :children #(vec (remove #{id} %)))
           (update-prop parent-id :children conj id)
           (assoc-prop id :parent parent-id))))))

(m/=> set-parent-at-index [:-> App uuid? uuid? int? App])
(defn set-parent-at-index
  [db id parent-id i]
  (let [sibling-els (:children (entity db parent-id))
        last-index (count sibling-els)]
    (-> db
        (set-parent parent-id id)
        (update-prop parent-id :children vec/move last-index i))))

(defn hovered-svg
  [db]
  (let [svgs (reverse (root-svgs db))
        pointer-pos (:adjusted-pointer-pos db)]
    (or
     (some #(when (bounds/contained-point? (:bounds %) pointer-pos) %) svgs)
     (root db))))

(defn translate
  ([db offset]
   (reduce (partial-right translate offset) db (top-ancestor-ids db)))
  ([db id offset]
   (update-el db id hierarchy/translate offset)))

(defn place
  ([db pos]
   (reduce (partial-right place pos) db (top-ancestor-ids db)))
  ([db id pos]
   (update-el db id hierarchy/place pos)))

(defn scale
  [db ratio pivot-point {:keys [in-place recursive]}]
  (let [ids-to-scale (cond-> (selected-ids db) recursive (set/union (descendant-ids db)))]
    (reduce
     (fn [db id]
       (let [pivot-point (->> (entity db id) :bounds (take 2) (mat/sub pivot-point))
             db (update-el db id hierarchy/scale ratio pivot-point)]
         (if in-place
           ;; FIXME: Handle locked ratio.
           (let [pointer-delta (mat/sub (:adjusted-pointer-pos db) (:adjusted-pointer-offset db))
                 child-ids (set (children-ids db id))
                 child-ids (set/intersection child-ids ids-to-scale)]
             (reduce (partial-right translate pointer-delta) db child-ids))
           db)))
     db
     ids-to-scale)))

(defn align
  ([db direction]
   (reduce (partial-right align direction) db (selected-ids db)))
  ([db id direction]
   (let [el-bounds (:bounds (entity db id))
         center (bounds/center el-bounds)
         parent-bounds (:bounds (parent db id))
         parent-center (bounds/center parent-bounds)
         [cx cy] (mat/sub parent-center center)
         [x1 y1 x2 y2] (mat/sub parent-bounds el-bounds)]
     (translate db id (case direction
                        :top [0 y1]
                        :center-vertical [0 cy]
                        :bottom [0 y2]
                        :left [x1 0]
                        :center-horizontal [cx 0]
                        :right [x2 0])))))

(defn ->path
  ([db]
   (reduce ->path db (selected-ids db)))
  ([db id]
   (update-el db id element/->path)))

(defn stroke->path
  ([db]
   (reduce stroke->path db (selected-ids db)))
  ([db id]
   (update-el db id element/stroke->path)))

(defn overlapping-svg
  [db el-bounds]
  (let [svgs (reverse (root-svgs db))] ; Reverse to select top svgs first.
    (or
     (some #(when (bounds/contained? el-bounds (:bounds %)) %) svgs)
     (some #(when (bounds/intersect? el-bounds (:bounds %)) %) svgs)
     (root db))))

(defn create-parent-id
  [db el]
  (cond-> el
    (not (element/root? el))
    (assoc :parent (or (:parent el)
                       (:id (if (element/svg? el)
                              (root db)
                              (overlapping-svg db (hierarchy/bounds el))))))))

(defn create
  [db el]
  (let [id (random-uuid) ; REVIEW: Hard to use a coeffect because of recursion.
        new-el (->> (cond-> el (not (string? (:content el))) (dissoc :content))
                    (map/remove-nils)
                    (element/normalize-attrs)
                    (create-parent-id db))
        new-el (merge new-el db/default {:id id})
        child-els (-> (entities db (set (:children el))) vals (concat (:content el)))
        [x1 y1] (hierarchy/bounds (entity db (:parent new-el)))
        add-children (fn [db child-els]
                       (reduce #(cond-> %1
                                  (db/tag? (:tag %2))
                                  (create (assoc %2 :parent id))) db child-els))]
    (if-not (db/valid? new-el)
      (notification.h/add db (notification.v/spec-failed
                              "Invalid element"
                              (-> new-el db/explain me/humanize str)))
      (cond-> db
        :always
        (assoc-in (conj (path db) id) new-el)

        (:parent new-el)
        (update-prop (:parent new-el) :children #(vec (conj % id)))

        (not (or (element/svg? new-el) (element/root? new-el) (:parent el)))
        (translate [(- x1) (- y1)])

        :always
        (refresh-bounds id)

        child-els
        (add-children child-els)))))

(defn create-default-canvas
  [db size]
  (cond-> db
    :always
    (create {:tag :canvas
             :attrs {:fill "#eeeeee"}})

    size
    (-> (create {:tag :svg
                 :attrs {:width (first size)
                         :height (second size)}}))))

(defn add
  ([db]
   (->> (temp db)
        (add db)
        (dissoc-temp)))
  ([db el]
   (create (deselect db) (assoc el :selected true))))

(defn boolean-operation
  [db operation]
  (let [selected-elements (top-selected-sorted db)
        attrs (-> selected-elements first element/->path :attrs)
        new-path (reduce (fn [path-a el]
                           (let [path-b (-> el element/->path :attrs :d)]
                             (path/boolean-operation path-a path-b operation)))
                         (:d attrs)
                         (rest selected-elements))]
    (cond-> (delete db)
      (seq new-path)
      (add {:type :element
            :tag :path
            :parent (-> selected-elements first :parent)
            :attrs (merge attrs {:d new-path})}))))

(defn paste-in-place
  ([db]
   (reduce paste-in-place (deselect db) (:copied-elements db)))
  ([db el]
   (reduce select (add db el) (selected-ids db))))

(defn paste
  ([db]
   (let [parent-el (hovered-svg db)]
     (reduce (partial-right paste parent-el) (deselect db) (:copied-elements db))))
  ([db el parent-el]
   (let [center (bounds/center (:copied-bounds db))
         el-center (bounds/center (:bounds el))
         offset (mat/sub el-center center)
         el (dissoc el :bounds)
         [s-x1 s-y1] (:bounds parent-el)
         pointer-pos (:adjusted-pointer-pos db)]
     (reduce
      select
      (cond-> db
        :always
        (-> (deselect)
            (add (assoc el :parent (:id parent-el)))
            (place (mat/add pointer-pos offset)))

        (not= (:id (root db)) (:id parent-el))
        (translate [(- s-x1) (- s-y1)])) (selected-ids db)))))

(m/=> duplicate-in-place [:-> App App])
(defn duplicate-in-place
  [db]
  (reduce create (deselect db) (top-selected-sorted db)))

(m/=> duplicate [:-> App Vec2D App])
(defn duplicate
  [db offset]
  (-> db
      (duplicate-in-place)
      (translate offset)))

(defn animate
  ([db tag attrs]
   (reduce (partial-right animate tag attrs) (deselect db) (selected db)))
  ([db el tag attrs]
   (reduce select (add db {:tag tag
                           :attrs attrs
                           :parent (:id el)}) (selected-ids db))))

(defn paste-styles
  ([db]
   (reduce paste-styles db (selected db)))
  ([db el]
   ;; TODO: Merge attributes from multiple selected elements.
   (if (= 1 (count (:copied-elements db)))
     (let [attrs (-> db :copied-elements first :attrs)
           style-attrs (disj attr/presentation :transform)]
       (reduce (fn [db attr]
                 (cond-> db
                   (attr attrs)
                   (update-attr (:id el) attr #(if % (-> attrs attr) disj))))
               db style-attrs)) db)))

(defn inherit-attrs
  [db source-el id]
  (reduce
   (fn [db attr]
     (let [source-attr (-> source-el :attrs attr)
           get-value (fn [v] (if (empty? (str v)) source-attr v))]
       (cond-> db
         source-attr
         (update-attr id attr get-value)))) db attr/presentation))

(defn group
  ([db]
   (group db (top-selected-sorted-ids db)))
  ([db ids]
   (reduce (fn [db id] (set-parent db (-> db selected-ids first) id))
           (add db {:tag :g
                    :parent (:id (parent db))}) ids)))

(defn ungroup
  ([db]
   (reduce ungroup db (selected-ids db)))
  ([db id]
   (cond-> db
     (and (not (locked? db id)) (= (:tag (entity db id)) :g))
     (as-> db db
       (let [i (index db id)]
         (reduce
          (fn [db child-id]
            (-> db
                (set-parent-at-index child-id (:parent (entity db id)) i)
                ;; Group attributes are inherited by its children,
                ;; so we need to maintain the presentation attrs.
                (inherit-attrs (entity db id) child-id)
                (select child-id)))
          db (reverse (children-ids db id))))
       (delete db id)))))

(defn manipulate-path
  ([db action]
   (reduce (partial-right manipulate-path action) db (selected-ids db)))
  ([db id action]
   (cond-> db
     (= (:tag (entity db id)) :path)
     (update-attr id path/manipulate action))))

(defn import-svg
  [db {:keys [svg label position]}]
  (let [[x y] position
        hickory (hickory/as-hickory (hickory/parse svg))
        zipper (hickory.zip/hickory-zip hickory)
        svg (hiccup/find-svg zipper)
        svg (-> svg
                (assoc :label label)
                (update :attrs dissoc :desc :version :xmlns)
                (assoc-in [:attrs :x] x)
                (assoc-in [:attrs :y] y))]
    (-> (add db svg)
        (collapse))))

