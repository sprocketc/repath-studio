(ns renderer.tool.hierarchy)

(defmulti on-pointer-down (fn [db _e] (:tool db)))
(defmulti on-pointer-up (fn [db _e] (:tool db)))
(defmulti on-pointer-move (fn [db _e] (:tool db)))
(defmulti on-double-click (fn [db _e] (:tool db)))
(defmulti on-drag-start (fn [db _e] (:tool db)))
(defmulti on-drag (fn [db _e] (:tool db)))
(defmulti on-drag-end (fn [db _e] (:tool db)))
(defmulti on-key-up (fn [db _e] (:tool db)))
(defmulti on-key-down (fn [db _e] (:tool db)))
(defmulti on-activate :tool)
(defmulti on-deactivate :tool)
(defmulti snapping-points (fn [db _e] (:tool db)))
(defmulti snapping-elements (fn [db _e] (:tool db)))
(defmulti render "Renders the tool helpers." identity)
(defmulti properties "Returns the properties of the tool." identity)
(defmulti help "Returns the status bar help text." (fn [tool state] [tool state]))

(defmulti right-panel identity)

(defmethod on-pointer-down :default [db _e] db)
(defmethod on-pointer-up :default [db _e] db)
(defmethod on-pointer-move :default [db _e] db)
(defmethod on-double-click :default [db _e] db)
(defmethod on-drag-start :default [db _e] db)
(defmethod on-drag :default [db _e] db)
(defmethod on-drag-end :default [db _e] db)
(defmethod on-key-up :default [db _e] db)
(defmethod on-key-down :default [db _e] db)
(defmethod on-activate :default [db] db)
(defmethod on-deactivate :default [db] db)
(defmethod render :default [])
(defmethod properties :default [])
(defmethod snapping-points :default [])
(defmethod snapping-elements :default [])
(defmethod help :default [_tool _state] "")
