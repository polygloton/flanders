(ns flanders.json-schema-test
  (:require [clojure.test :refer [deftest is]]
            [flanders.core :as f]
            [flanders.json-schema :as fjs]))

(deftest test-json-schema
  (is (= (fjs/->json-schema (f/bool))
         {:type "boolean"})))
