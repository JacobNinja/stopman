(ns stopman.core
  (:require [stopman.checkers :as c]))

(defn file-path [base file]
  (.getPath (.relativize (.toURI (clojure.java.io/file base))
                         (.toURI file))))

(defn- ruby-file? [file]
  (re-find #"\.rb$" (.getName file)))

(defn stopcheck [dir]
  (let [files (file-seq (clojure.java.io/file dir))]
    (reduce (fn [r file]
              (let [result (c/check (slurp file))]
                (merge r (when-not (empty? result)
                           {(file-path dir file) result}))))
            {}
            (filter ruby-file? files))))
