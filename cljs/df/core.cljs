(ns df.core
  (:require [lumo.io :refer [slurp spit]]
            [mach.core :refer [sh file-exists?]]
            [cljs.core.async :refer [<!]])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:refer-clojure :exclude [resolve]))

(def path (js/require "path"))
(def os (js/require "os"))
(def fs (js/require "fs"))
(defn resolve [s] (.resolve path s))
(def cwd (.cwd js/process))
(def home-dir (.homedir os))

(defn symlink? [dest]
  (try
    (.isSymbolicLink (.lstatSync fs dest))
    (catch js/Error e
      nil)))

(defn symlinked?
  "Returns true if path is a symlink and points to dest."
  [src dest]
  (and (symlink? dest)
       (= (.realpathSync fs dest)
          (resolve src))))

(defn attempt-backup! [path]
  (let [backup-path (str path "._bak")]
    (println "Attempting to backup" path "to" backup-path)
    (if (file-exists? backup-path)
      (throw (js/Error. (str "Backup file already exists: "
                             backup-path)))
      (sh ["mv" path backup-path]))))

(defn symlink! [src dest]
  (let [src (resolve src)
        dest (resolve dest)]
    (go
      (when (symlink? dest)
        ;; Delete existing symlink (safe since its a symlink)
        (<! (sh ["rm" dest])))
      (when (file-exists? dest)
        (attempt-backup! dest))
      (sh ["ln -s" src dest]))))

(defn clean-link!
  "Remove symlink from src to dest pointing to src iff dest exists and
  is a symlink pointing to src. Otherwise, print a helpful message
  explaining the situation."
  [src dest]
  (cond
    (not (mach.core/file-exists? dest)) :noop
    (df.core/symlinked? src dest) (mach.core/sh ["rm" dest])
    (df.core/symlink? path) (println "unable to clean:"
                                     dest
                                     "is a symlink but does not link to"
                                     (str src ".")
                                     "Remove symlink manually and re-run task")
    :else (println "unable to clean:" dest "exists and is not a symlink")))
