(ns stopman.parser
  (:import (org.jrubyparser Parser CompatVersion)
           (org.jrubyparser.parser ParserConfiguration)
           (java.io StringReader)))

(defn- children? [_] true)

(defn- children [n]
  (.childNodes n))

(defn parse-ruby [rb]
  (let [parser (Parser.)
        config (ParserConfiguration. 0 CompatVersion/RUBY1_9)]
    (.parse parser "" (StringReader. rb) config)))

(defn make-tree [root]
  (tree-seq children? children root))

(defn parse-tree [rb]
  (make-tree (parse-ruby rb)))
