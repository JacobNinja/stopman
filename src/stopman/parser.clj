(ns stopman.parser
  (:import (org.jrubyparser Parser CompatVersion)
           (org.jrubyparser.parser ParserConfiguration)
           (java.io StringReader)))

(defn- children? [_] true)

(defn- children [n]
  (.childNodes n))

(defn make-tree [root]
  (if-not (seq? root)
    (tree-seq children? children root)
    root))

(defn filter-nodes [root f]
  (loop [tree (make-tree root)]
    (cond (empty? tree) '()
          (f (first tree)) (cons (first tree) (filter-nodes (rest tree) f))
          (seq? (first tree)) (concat (filter-nodes (first tree) f)
                                      (filter-nodes (rest tree) f))
          :else (recur (rest tree)))))

(defn get-name [n]
  (when (instance? org.jrubyparser.ast.INameNode n)
    (.getName n)))

(defn get-args [n]
  (rest (make-tree (.getArgs n))))

(defn hash-node? [n]
  (instance? org.jrubyparser.ast.HashNode n))

(defn get-hash-arg [n]
  (first (filter-nodes (get-args n) hash-node?)))

(defn eq-symbol? [n sym]
  (and (instance? org.jrubyparser.ast.SymbolNode n)
       (= (get-name n) sym)))

(defn get-hash-key-values [node]
  (partition 2 (rest (rest (make-tree node)))))

(defn get-hash-key-value [node key]
  (first (filter #(= key (get-name (first %)))
                 (get-hash-key-values node))))

(defn get-value [node]
  (.getValue node))

(defn eq-hash-key? [n f]
  (let [hash-keys (map first (get-hash-key-values n))]
    (some f hash-keys)))

(defn call-node? [n]
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

(defn- line-range [n]
  (let [source-position (.getPosition n)]
    [(.getStartOffset source-position)
     (.getEndOffset source-position)]))

(defn serialize [rb type]
  (fn [n]
    (let [[start-offset end-offset] (line-range n)]
      {:src (apply str (take (- end-offset start-offset)
                             (drop start-offset rb)))
       :range (line-range n)
       :type type})))

(defn parse-ruby [rb]
  (let [parser (Parser.)
        config (ParserConfiguration. 0 CompatVersion/RUBY1_9)]
    (.parse parser "" (StringReader. rb) config)))

(defn parse-tree [rb]
  (make-tree (parse-ruby rb)))
