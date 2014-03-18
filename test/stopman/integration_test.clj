(ns stopman.integration-test
  (:require [clojure.test :refer :all]
            [stopman.core :refer :all]))

(defn check-file [file expected]
  (is (= (get (stopcheck "test/stopman/fixtures/rails4") file)
         expected)))

(deftest integration-test
  (testing "rails 4"
    (check-file "app/controllers/application_controller.rb"
                [{:range [554 593]
                  :src "self.verify_mode = OpenSSL::VERIFY_NONE"
                  :type :ssl-verify-none}])
    ))
