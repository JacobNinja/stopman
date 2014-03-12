(ns stopman.checkers-test
  (:require [clojure.test :refer :all]
            [stopman.fixtures :as fixtures]
            [stopman.checkers :as checkers]))

(defn- check-result [fixture expected]
  (is (= expected
         (first (checkers/check fixture)))))

(deftest ssl-verify-check
  (testing "ssl verify none"
    (check-result fixtures/ssl {:src "OpenSSL::VERIFY_NONE"
                                :range [11 31]
                                :type :ssl-verify})))
