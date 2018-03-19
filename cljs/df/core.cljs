(ns df.core
  (:require [lumo.io :refer [slurp spit]]
            [mach.core :refer [sh file-exists?]]
            [cljs.core.async :refer [<!]])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:refer-clojure :exclude [resolve]))

(defn echo [s]
  (sh ["echo" s]))

(def path (js/require "path"))
(def os (js/require "os"))
(def fs (js/require "fs"))
(defn resolve [s] (.resolve path s))
(def home-dir (.homedir os))

(defn symlinked? [dest]
  (try
    (let [stats (.lstatSync fs dest)]
      (.isSymbolicLink stats))
    (catch js/Error e
      nil)))

(defn attempt-backup [path]
  (let [backup-path (str path "._bak")]
    (println "Attempting to backup" path "to" backup-path)
    (if (file-exists? backup-path)
      (throw (js/Error. (str "Backup file already exists: "
                             backup-path)))
      (sh ["mv" path backup-path]))))

(defn symlink [src dest]
  (let [src (resolve src)
        dest (resolve dest)]
    (go
      (when (symlinked? dest)
        ;; Delete existing symlink (safe since its a symlink)
        (<! (sh ["rm" dest])))
      (when (file-exists? dest)
        (attempt-backup dest))
      (sh ["ln -s" src dest]))))

