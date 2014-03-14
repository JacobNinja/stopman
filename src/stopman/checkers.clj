(ns stopman.checkers
  (:require [stopman.parser :as parser]))

(defn- filter-nodes [root f]
  (loop [tree root]
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

(defn- eq-symbol? [n sym]
  (and (instance? org.jrubyparser.ast.SymbolNode n)
       (= (.getName n) sym)))

(defn- eq-hash-key? [n f]
  (let [hash-keys (map first (partition 2 (rest (rest (parser/make-tree n)))))]
    (some f hash-keys)))

(defn- get-name [n]
  (.getName n))

(defn- get-args [n]
  (rest (parser/make-tree (.getArgs n))))

(defn- call-node? [n]
  (or (instance? org.jrubyparser.ast.VCallNode n)
      (instance? org.jrubyparser.ast.FCallNode n)
      (instance? org.jrubyparser.ast.CallNode n)))

(defn eq-receiver? [n receiver]
  (= receiver (get-name (.getReceiver n))))

(defn call-with-receiver-node? [n]
  (instance? org.jrubyparser.ast.CallNode n))

(defn eq-method-name? [n method-name]
  (= (get-name n) method-name))

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
       (some #(eq-method-name? % "params")
             (filter-nodes (rest (parser/make-tree node)) call-node?))))

(defn unsafe-deserialization? [node]
  (and (call-with-receiver-node? node)
       (eq-receiver? node "YAML")
       (some #(eq-method-name? node %)
             ["load" "load_documents" "load_stream" "parse_documents" "parse_stream"])))

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
              ))
