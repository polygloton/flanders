(ns flanders.json-schema
  (:require #?(:clj  [flanders.types]
               :cljs [flanders.types
                      :refer [AnythingType BooleanType EitherType InstType
                              IntegerType KeywordType MapEntry MapType
                              NumberType SequenceOfType SetOfType StringType]])
            )
  #?(:clj (:import [flanders.types
                    AnythingType BooleanType EitherType InstType IntegerType
                    KeywordType MapEntry MapType NumberType SequenceOfType
                    SetOfType StringType]
                   [java.util Date])))

(defprotocol JSONSchemaNode
  (->json-schema' [node f]))

(defn ->json-schema [node]
  (->json-schema' node ->json-shema))

(extend-protocol JSONSchemaNode

  ;; Branches

  MapEntry
  (->json-schema' [{:keys [key type required?] :as entry} f]
    [(f (assoc key
               :key? true
               :description (some :description [key entry])))
     (f )])

  MapType
  (->json-schema' [{:keys [entries]} f]
    (describe
     (reduce (fn [m [k v]]
               (assoc-in m ["properties" k] v))
             {}
             (map f entries))))

  ;; Leaves

  AnythingType
  (->json-schema' [_ _]
    {})

  BooleanType
  (->json-schema' [_ _]
    {:type "boolean"})

  InstType
  (->json-schema' [_ _]
    {:type "string" :format "date-time"})

  IntegerType
  (->json-schema' [_ _]
    {:type "integer" :format "int64"})

  KeywordType
  (->json-schema' [_ _]
    {:type "string"})

  NumberType
  (->json-schema' [_ _]
    {:type "number"})

  StringType
  (->json-schema' [_ _]
    {:type "string"})
  )
