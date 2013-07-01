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
