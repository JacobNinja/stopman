(ns stopman.checkers
  (:use [stopman.parser]))

(def ^:dynamic *unsafe-variables* #{"params"})

(defn unsafe-variable? [nodes]
  (some *unsafe-variables* (map get-name nodes)))

(defn- ssl-verify-none? [node]
  (and (attr-assign-node? node)
       (eq-method-name? node "verify_mode=")
       (let [first-arg (first (get-args node))]
         (and (instance? org.jrubyparser.ast.Colon2ConstNode first-arg)
              (= (.. first-arg getLeftNode getName) "OpenSSL")
              (eq-method-name? first-arg "VERIFY_NONE")))))

(defn object-send? [node]
  (and (call-node? node)
       (eq-method-name? node "send")))

(defn skip-filter? [node]
  (and (instance? org.jrubyparser.ast.FCallNode node)
       (eq-method-name? node "skip_before_filter")
       (let [args (get-args node)]
         (and (some #(eq-symbol? (first args) %)
                    ["login_required"
                     "authenticate_user!"
                     "require_user"
                     "verify_authenticity_token"])
              (eq-hash-key? (second args)
                            #(eq-symbol? % "except"))))))

(defn unsafe-command? [node]
  (and (or (instance? org.jrubyparser.ast.DXStrNode node)
           (and (instance? org.jrubyparser.ast.FCallNode node)
                (eq-method-name? node "system")))
       (some #(*unsafe-variables* (get-name %))
             (filter-nodes (rest (make-tree node)) call-node?))))

(defn unsafe-yaml? [node]
  (and (eq-method-call-with-receiver? node
                                 "YAML"
                                 "load" "load_documents" "load_stream" "parse_documents" "parse_stream")
       (unsafe-variable? (get-args node))))

(defn unsafe-csv? [node]
  (and (eq-method-call-with-receiver? node "CSV" "load")
       (unsafe-variable? (get-args node))))

(defn unsafe-marshal? [node]
  (eq-method-call-with-receiver? node "Marshal" "load" "restore"))

(defn unsafe-deserialization? [node]
  (and (call-with-receiver-node? node)
       (or (unsafe-yaml? node)
           (unsafe-csv? node)
           (unsafe-marshal? node))))

(defn unsafe-constantize [node]
  (and (call-with-receiver-node? node)
       (#{"constantize" "safe_constantize"} (get-name node))))

(defn unsafe-get-constant [node]
  (and (call-node? node)
       (#{"const_get" "qualified_const_get"} (get-name node))))

(defn unsafe-reflection? [node]
  (or (unsafe-constantize node)
      (unsafe-get-constant node)))

(defn unsafe-validation-regex? [node]
  (and (call-node? node)
       (#{"validate" "validates_format_of"} (get-name node))
       (when-let [with-option (get-hash-key-value (get-hash-arg node) "with")]
         (and (regex-node? (second with-option))
              (re-find #"^\^(?:.*?)\$$" (get-value (second with-option)))))))

(defn run-checks [rb & check-pairs]
  (let [root (parse-tree rb)]
    (loop [check-pairs check-pairs
           r '()]
      (if (empty? check-pairs)
        r
        (let [[check check-type] (first check-pairs)]
          (recur (rest check-pairs)
                 (concat r (map (serialize rb check-type) (filter-nodes root check)))))))))

(defn check [rb]
  (run-checks rb
              [ssl-verify-none? :ssl-verify-none]
              [object-send? :send]
              [skip-filter? :skip-filter]
              [unsafe-command? :unsafe-command]
              [unsafe-deserialization? :unsafe-deserialization]
              [unsafe-reflection? :unsafe-reflection]
              [unsafe-validation-regex? :validation-regex]
              ))
