(ns muxie.util
  (:require [clojure.java.io :as io]))

(defn mv
  "Move File file to a new path. Returns true if successful."
  [file newpath]
  (and newpath
       (. file renameTo (io/file newpath))))
    