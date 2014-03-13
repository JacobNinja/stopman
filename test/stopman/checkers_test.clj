(ns stopman.checkers-test
  (:require [clojure.test :refer :all]
            [stopman.checkers :as checkers]))

(defn- check-result [fixture expected]
  (is (= expected
         (first (checkers/check fixture)))))

(deftest ssl-verify-check
  (testing "ssl verify none"
    (check-result "OpenSSL::VERIFY_NONE"
                  {:src "OpenSSL::VERIFY_NONE"
                   :range [0 20]
                   :type :ssl-verify})))

(deftest object-send
  (testing "Object.send"
    (check-result "Object.send(:foo)"
                  {:src "Object.send(:foo)"
                   :range [0 17]
                   :type :send}))
  (testing "send"
    (check-result "send(:foo)"
                  {:src "send(:foo)"
                   :range [0 10]
                   :type :send}))
  )
