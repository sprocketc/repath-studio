(ns repath.user
  (:require
   [re-frame.core :as rf]))

(defn move
  "Moves the selected elements."
  ([offset]
   (rf/dispatch [:elements/move offset])
   "")
  ([x y]
   (move [x y])))

(defn m
  [& args]
  (apply move args))

(defn fill
  "Fills the selected elements."
  [color]
  (rf/dispatch [:elements/fill [color]])
  "")

(defn copy
  "Copies the selected elements."
  []
  (rf/dispatch [:elements/copy])
  "")

(defn paste
  "Pastes the selected elements."
  []
  (rf/dispatch [:elements/paste])
  "")

(defn create
  "Creates a new element."
  [element]
  (apply #(rf/dispatch [:elements/create {:type (key %)
                                        :attrs (val %)}]) element)
  "")

(defn select-all
  "Selects all elements."
  []
  (rf/dispatch [:elements/select-all])
  "")

(defn deselect-all
  "Deselects all elements."
  []
  (rf/dispatch [:elements/deselect-all])
  "")

(defn undo
  "Goes back in history."
  ([steps]
   (rf/dispatch [:history/undo steps])
   "")
  ([]
   (undo 1)))

(defn redo
  "Goes forward in history."
  ([steps]
   (rf/dispatch [:history/redo steps])
   "")
  ([]
   (redo 1)))

(defn exit
  "Closes the application."
  [element]
  (apply #(rf/dispatch [:window/close]) element)
  "")