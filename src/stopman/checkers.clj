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

(defn- ssl-verify-none? [node]
  (and (instance? org.jrubyparser.ast.Colon2ConstNode node)
       (= (.. node getLeftNode getName) "OpenSSL")
       (= (.getName node) "VERIFY_NONE")))

(defn object-send? [node]
  (and (instance? org.jrubyparser.ast.CallNode node)
       (= (.getName node) "send")))

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
              ))
