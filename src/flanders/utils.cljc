(ns flanders.utils
  (:require [clojure.zip :as z]
            [flanders.predicates :as fp]
            [flanders.protocols :refer [branch? node-children make-node]]
            [flanders.types :as ft]))

(defn right-loc-seq
  "Lazy seq of loc and its right siblings"
  [loc]
  (if (nil? loc)
    nil
    (lazy-seq (cons loc
                    (right-loc-seq (z/right loc))))))

(defn children-locs [loc]
  (some-> loc z/down right-loc-seq))

(defn ->ddl-zip [root]
  (z/zipper branch? node-children make-node root))

;; Adds zip support for maps.
;; (Source: http://stackoverflow.com/a/15020649/42188)
(defn ->map-zip [m]
  (z/zipper
   (fn [x] (or (map? x) (map? (nth x 1))))
   (fn [x] (seq (if (map? x) x (nth x 1))))
   (fn [x children]
     (if (map? x)
       (into {} children)
       (assoc x 1 (into {} children))))
   m))

(defn- replace-with-any [loc description]
  (z/replace loc
             (ft/map->AnythingType {:description description})))

(defn replace-either-with-any
  "Walks the DDL tree, replacing EitherType nodes with AnythingType nodes"
  [ddl]
  (loop [ddl-loc (->ddl-zip ddl)]
    (cond
      ;; Terminate
      (z/end? ddl-loc)
      (z/root ddl-loc)

      ;; Replace
      (fp/either? (z/node ddl-loc))
      (recur (z/next (replace-with-any ddl-loc
                                       "Simplified conditional branch")))

      ;; Recur
      :else
      (recur (z/next ddl-loc)))))

(defn find-maps [ddl-root]
  (loop [current-map-loc (->ddl-zip (assoc ddl-root
                                           :anchor "top"))
         maps-to-walk []
         result-maps []
         map-counter 1]
    (let [node (some-> current-map-loc z/node)
          root-node (some-> current-map-loc z/root)]
      (cond
        ;; terminate
        (nil? current-map-loc)
        result-maps

        ;; end of current map, go to next map
        (z/end? current-map-loc)
        (recur (some-> maps-to-walk first fu/->ddl-zip)
               (rest maps-to-walk)
               (conj result-maps (z/root current-map-loc))
               map-counter)

        ;; map that is not the root, push it
        (and (instance? MapType node)
             (not= node root-node))
        (let [text (->short-description node)
              map-anchor (str "map" map-counter)
              ref-anchor (str map-anchor "-ref")]
          (recur (z/next (z/replace current-map-loc
                                    (->ReferenceNode text ref-anchor map-anchor)))
                 (conj maps-to-walk (assoc node
                                           :anchor map-anchor
                                           :jump-anchor ref-anchor))
                 result-maps
                 (inc map-counter)))

        ;; else go to next location
        :else
        (recur (z/next current-map-loc)
               maps-to-walk
               result-maps
               map-counter )))))
