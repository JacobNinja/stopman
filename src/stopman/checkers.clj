(ns stopman.checkers
  (:require [stopman.parser :as parser]))

(def ^:dynamic *unsafe-variables* #{"params"})

(defn- filter-nodes [root f]
  (loop [tree (parser/make-tree root)]
    (cond (empty? tree) '()
          (f (first tree)) (cons (first tree) (filter-nodes (rest tree) f))
          (seq? (first tree)) (concat (filter-nodes (first tree) f)
                                      (filter-nodes (rest tree) f))
          :else (recur (rest tree)))))

(defn- line-range [n]
  (let [source-position (.getPosition n)]
    [(.getStartOffset source-position)
     (.getEndOffset source-position)]))

(defn- serialize [rb type]
  (fn [n]
    (let [[start-offset end-offset] (line-range n)]
      {:src (apply str (take (- end-offset start-offset)
                             (drop start-offset rb)))
       :range (line-range n)
       :type type})))

(defn- get-name [n]
  (when (instance? org.jrubyparser.ast.INameNode n)
    (.getName n)))

(defn- get-args [n]
  (rest (parser/make-tree (.getArgs n))))

(defn- hash-node? [n]
  (instance? org.jrubyparser.ast.HashNode n))

(defn- get-hash-arg [n]
  (first (filter-nodes (get-args n) hash-node?)))

(defn- eq-symbol? [n sym]
  (and (instance? org.jrubyparser.ast.SymbolNode n)
       (= (get-name n) sym)))

(defn get-hash-key-values [node]
  (partition 2 (rest (rest (parser/make-tree node)))))

(defn get-hash-key-value [node key]
  (first (filter #(= key (get-name (first %)))
                 (get-hash-key-values node))))

(defn get-value [node]
  (.getValue node))

(defn- eq-hash-key? [n f]
  (let [hash-keys (map first (get-hash-key-values n))]
    (some f hash-keys)))

(defn- call-node? [n]
  (or (instance? org.jrubyparser.ast.VCallNode n)
      (instance? org.jrubyparser.ast.FCallNode n)
      (instance? org.jrubyparser.ast.CallNode n)))

(defn eq-receiver? [n receiver]
  (= receiver (get-name (.getReceiver n))))

(defn call-with-receiver-node? [n]
  (instance? org.jrubyparser.ast.CallNode n))

(defn call-without-receiver-node? [n]
  (instance? org.jrubyparser.ast.FCallNode n))

(defn regex-node? [n]
  (instance? org.jrubyparser.ast.RegexpNode n))

(defn eq-method-name? [n method-name]
  (= (get-name n) method-name))

(defn eq-method-call-with-receiver? [node receiver & method-names]
  (and (call-with-receiver-node? node)
       (eq-receiver? node receiver)
       (some #(eq-method-name? node %) method-names)))

(defn- ssl-verify-none? [node]
  (and (instance? org.jrubyparser.ast.Colon2ConstNode node)
       (= (.. node getLeftNode getName) "OpenSSL")
       (eq-method-name? node "VERIFY_NONE")))

(defn object-send? [node]
  (and (or (instance? org.jrubyparser.ast.CallNode node)
           (instance? org.jrubyparser.ast.FCallNode node))
       (eq-method-name? node "send")))

(defn skip-filter? [node]
  (and (instance? org.jrubyparser.ast.FCallNode node)
       (= (get-name node) "skip_before_filter")
       (let [args (get-args node)]
         (and (some #(eq-symbol? (first args) %)
                    ["login_required"
                     "authenticate_user!"
                     "require_user"
                     "verify_authenticity_token"])
              (eq-hash-key? (first (next args))
                            #(eq-symbol? % "except"))))))

(defn unsafe-command? [node]
  (and (or (instance? org.jrubyparser.ast.DXStrNode node)
           (and (instance? org.jrubyparser.ast.FCallNode node)
                (eq-method-name? node "system")))
       (some #(*unsafe-variables* (get-name %))
             (filter-nodes (rest (parser/make-tree node)) call-node?))))

(defn unsafe-yaml? [node]
  (eq-method-call-with-receiver? node
                                 "YAML"
                                 "load" "load_documents" "load_stream" "parse_documents" "parse_stream"))

(defn unsafe-csv? [node]
  (eq-method-call-with-receiver? node "CSV" "load"))

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
  (let [root (parser/parse-tree rb)]
    (loop [check-pairs check-pairs
           r '()]
      (if (empty? check-pairs)
        r
        (let [[check check-type] (first check-pairs)]
          (recur (rest check-pairs)
                 (concat r (map (serialize rb check-type) (filter-nodes root check)))))))))

(defn check [rb]
  (run-checks rb
              [ssl-verify-none? :ssl-verify]
              [object-send? :send]
              [skip-filter? :skip-filter]
              [unsafe-command? :unsafe-command]
              [unsafe-deserialization? :unsafe-deserialization]
              [unsafe-reflection? :unsafe-reflection]
              [unsafe-validation-regex? :validation-regex]
              ))
