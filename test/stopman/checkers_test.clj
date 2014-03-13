(ns stopman.checkers-test
  (:require [clojure.test :refer :all]
            [stopman.checkers :as checkers]))

(defn- check-result [fixture expected]
  (is (= (assoc expected :src (get expected :src fixture))
         (first (checkers/check fixture)))))

(deftest ssl-verify-check
  (testing "ssl verify none"
    (check-result "OpenSSL::VERIFY_NONE"
                  {:range [0 20]
                   :type :ssl-verify})))

(deftest object-send
  (testing "Object.send"
    (check-result "Object.send(:foo)"
                  {:range [0 17]
                   :type :send}))
  (testing "send"
    (check-result "send(:foo)"
                  {:range [0 10]
                   :type :send}))
  )

(deftest skip-filter
  (testing "authenticity token"
    (check-result "skip_before_filter :verify_authenticity_token, except: []"
                  {:range [0 57]
                   :type :skip-filter}))

  (testing "authentication"
    (check-result "skip_before_filter :login_required, except: []"
                  {:range [0 46]
                   :type :skip-filter})
    (check-result "skip_before_filter :require_user, except: []"
                  {:range [0 44]
                   :type :skip-filter})
    (check-result "skip_before_filter :authenticate_user!, except: []"
                  {:range [0 50]
                   :type :skip-filter})
    ))

(deftest unsafe-params
  (testing "backticks"
    (check-result "`#{params[:foo]}`"
                  {:src "`#{params[:foo]}" ; FIXME: methods that wrap arguments
                   :range [0 16]
                   :type :unsafe-command}))
  (testing "system"
    (check-result "system(params[:foo])"
                  {:range [0 20]
                   :type :unsafe-command})
    ))

(deftest unsafe-deserialization
  (testing "yaml"
    (check-result "YAML.load(foo)"
                  {:range [0 14]
                   :type :unsafe-deserialization})
    (check-result "YAML.load_documents(foo)"
                  {:range [0 24]
                   :type :unsafe-deserialization})
    (check-result "YAML.load_stream(foo)"
                  {:range [0 21]
                   :type :unsafe-deserialization})
    (check-result "YAML.parse_documents(foo)"
                  {:range [0 25]
                   :type :unsafe-deserialization})
    (check-result "YAML.parse_stream(foo)"
                  {:range [0 22]
                   :type :unsafe-deserialization}))
  )
