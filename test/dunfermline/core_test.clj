(ns dunfermline.core-test
  (:require [clojure.test :refer :all]
            [dunfermline.core :refer :all]))

(deftest basic-parser-test
  (testing "Basic parsers"
    (do
      (is (= ((integer-parser) "-123 bar") [-123 " bar"]))
      (is (= ((integer-parser) "123 bar") [123 " bar"]))
      (is (= ((word-parser "foo") "foo bar") ["foo" " bar"]))
      (is (= ((word-parser "foo") "foolbar") [nil "foolbar"]))
      (is (= ((keyword-parser :foo) "foo bar") [:foo " bar"]))
      (is (= ((whitespace-parser) "
                                  bar") [:whitespace "bar"]))
      (is (= ((identifier-parser) "foo-bar0099***") ["foo-bar0099" "***"]))
      (is (= ((identifier-parser) "-foo-bar0099") [nil "-foo-bar0099"])))))

(deftest compound-parser-test
  (testing "Compound parsers"
    (do
      (let [op (or-parser [(identifier-parser (fn [x] [:id x]))
                          (integer-parser)])]
        (is (= (op "fff 123 asdf") [[:id "fff"] " 123 asdf"]))
        (is (= (op "123 asdf") [123 " asdf"])))
      (let [ap (and-parser [(keyword-parser :id)
                            (whitespace-parser)
                            (integer-parser)])]
        (is (= (ap "id 123") [[:id :whitespace 123] ""]))
        (is (= (ap "pid 123") [nil "pid 123"]))))))
