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

(defn parse-tree [rb]
  (tree-seq children? children (parse-ruby rb)))
