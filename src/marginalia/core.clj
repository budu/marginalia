(ns marginalia.core
  "**Core** provides all of the functionality around parsing clojure source files
   into an easily consumable format."
  (:require [clojure.java.io :as io]
            [clojure.string  :as str])
  (:use [marginalia.html :only (uberdoc-html)]
        [clojure.contrib.find-namespaces :only (read-file-ns-decl)]
        [clojure.contrib.reflect :only (get-field)]))


(def *test* "./src/cljojo/core.clj")
(def *docs* "docs")
(def *comment* #"^\s*;;\s?")
(def *divider-text* "\n;;DIVIDER\n")
(def *divider-html* #"\n*<span class=\"c[1]?\">;;DIVIDER</span>\n*")

;; ## File System Utilities

(defn ls
  [path]
  (let [file (java.io.File. path)]
    (if (.isDirectory file)
      (seq (.list file))
      (when (.exists file)
        [path]))))

(defn mkdir [path]
  (.mkdirs (io/file path)))

(defn ensure-directory! [path]
  (when-not (ls path)
    (mkdir path)))

(defn dir? [path]
  (.isDirectory (java.io.File. path)))

(defn find-clojure-file-paths [dir]
  "Returns a seq of clojure file paths (strings) in alphabetical depth-first order (I think?)."
  (->> (java.io.File. dir)
       (file-seq)
       (filter #(re-find #"\.clj$" (.getAbsolutePath %)))
       (map #(.getAbsolutePath %))))


;; ## Project File Analysis
;; Marginalia will parse info out of your project.clj to display in
;; the generated html file's header.
;;
;; TODO: add pom.xml support.




(defn parse-project-file
  "Parses a project.clj file and returns a map in the following form

       ex. {:name 
            :version
            :dependencies
            :dev-dependencies
            etc...}

   by reading the `defproject` form from your project.clj to obtain name and
   version, then merges in the rest of the defproject forms (`:dependencies`, etc)."
  ([] (parse-project-file "./project.clj"))
  ([path]
      (try
        (let [rdr (clojure.lang.LineNumberingPushbackReader.
                   (java.io.FileReader.
                    (java.io.File. path)))
              project-form (read rdr)]
          (merge {:name (str (second project-form))
                  :version (nth project-form 2)}
                 (apply hash-map (drop 3 project-form))))
        (catch Exception e
          (throw (Exception.
                  (str
                   "There was a problem reading the project definition from "
                   path)))))))


;; ## Source File Analysis

(defn end-of-block? [cur-group groups lines]
  (let [line (first lines)
        next-line (second lines)
        next-line-code (get next-line :code-text "")]
    (when (or (and (:code-text line)
                   (:docs-text next-line))
              (re-find #"^\(def" (str/trim next-line-code)))
      true)))

(defn merge-line [line m]
  (cond
   (:docstring-text line) (assoc m :docs (conj (get m :docs []) line))
   (:code-text line) (assoc m :codes (conj (get m :codes []) line))
   (:docs-text line) (assoc m :docs (conj (get m :docs []) line))))

(defn group-lines [doc-lines]
  (loop [cur-group {}
         groups []
         lines doc-lines]
    (cond
     (empty? lines) (conj groups cur-group)

     (end-of-block? cur-group groups lines)
     (recur (merge-line (first lines) {}) (conj groups cur-group) (rest lines))

     :else (recur (merge-line (first lines) cur-group) groups (rest lines)))))

;; Hacktastic, these ad-hoc checks should be replaced with something
;; more robust.
(defn docstring-line? [line sections]
  (let [l (last sections)
        last-code-text (get l :code-text "")]
    (try
      (or
       ;; Is the last line's code-text a defn, and does the
       ;; current line start with a quote?
       (and (re-find #"\(defn" last-code-text)
            (re-find #"^\"" (str/trim (str line))))
       ;; Is the last line's code-text the start of a ns
       ;; decl, and does the current line start with a quote?
       (and (re-find #"\(ns" last-code-text)
            (re-find #"^\"" (str/trim (str line))))
       ;; Is the prev line a docstring line, the current line _not_
       ;; start with a ( or [, and the current line not an empty string?
       (and (:docstring-text l)
            (not (re-find #"^\(" (str/trim (str line))))
            (not (re-find #"^\[" (str/trim (str line))))
            (not= "" (str/trim (str line))))
       ;; Is the prev line a docstring, the prev line not end with a quote,
       ;; and the current line not an empty string?
       (and (:docstring-text l)
            (not (re-find #"\"$" (str/trim (:docstring-text l))))
            (= "" (str/trim (str line)))))
      (catch Exception e nil))))

(comment
  (call-method clojure.lang.LispReader "getMacro" [Integer/TYPE] nil \;)

  (seq (get-field clojure.lang.LispReader :macros nil))

  (def old-comment-reader (nth (get-field clojure.lang.LispReader :macros nil) (int \;)))

  (nth (get-field clojure.lang.LispReader :macros nil) (int \@))

  (use 'clojure.contrib.reflect)

  (parse ";; Some Comment
        (defn hello-world
          \"here's the docstring\"
          [name] ; inline comment
          (pritnln \"hello\" name \"!\"))
        ;; Some Other Comment
        ([the quick brown fox jumps over the lazy dog])")

  (pprint (parse (slurp "./src/marginalia/html.clj")))

  ;;-> [";; Some Comment" (defn hello-world [name] (pritnln "hello" name "!"))]

  (def lr (clojure.lang.LispReader.))

  (identity clojure.lang.LispReader/macros)

  (clojure.lang.LispReader/getMacro 10)

  (.getMacro lr 10)

  (pprint (group-forms (parse ";; Some Comment
        (defn hello-world
          \"here's the docstring\"
          [name] ; inline comment
          (pritnln \"hello\" name \"!\"))
        ;; Some Other Comment
        ([the quick brown fox jumps over the lazy dog])")))
)


(use 'clojure.pprint)
#_(println (parse ";; Some Comment
        (defn hello-world
          \"here's the docstring\"
          [name] ; inline comment
          (pritnln \"hello\" name\"!\"))
        ;; Some Other Comment
        ([the quick brown fox jumps over the lazy dog])"))


(defn read-comment [reader semicolon]
  (let [sb (StringBuilder.)]
    (.append sb semicolon)
    (loop [ch (char (.read reader))]
      (if (or (= ch \newline)
              (= ch \return)
              (= ch -1))
        (do
          (.unread reader (int ch))
          (.toString sb))
        (do
          (.append sb (Character/toString ch))
          (recur (char (.read reader))))))))

(defn parse [src]
  "The workhorse of marginalia's parsing engine.  Pass in a string containing Clojure code, and you'll get back a map containing the analyzed content.

   This function iterates over `src` line-by-line, tagging each as either code, docs (comments) or docstring."             
  (loop [[line & more] (line-seq src) cnum 1 dnum 0 sections []]
    (if more
      (if (re-find *comment* line)
        (recur more
               cnum
               (inc dnum)
               (conj sections {:docs-text (str (str/replace line *comment* "")) :line (+ cnum dnum)}))
        (recur more
               (inc cnum)
               0
               (if (docstring-line? (str line) sections)
                 (conj sections {:docstring-text (str line) :line cnum})
                 (conj sections {:code-text (str line) :line cnum}))))
      sections)))

(defn drop-nth [n coll]
  "http://groups.google.com/group/clojure/browse_thread/thread/d0ecd17cdeb740f7"
  (lazy-seq 
    (when-let [s (seq coll)] 
      (concat (take n s) (drop-nth n (next (drop n s))))))) 


(defn index-of [el col]
  (loop [col col
         i 0]
    (cond
     (empty? col) nil
     (= (first col) el) i
     :else (recur (rest col) (inc i)))))


(defn pick-docstring [dfn]
  (let [first-symbol-str (str (first dfn))]
    (when (or (= "defn" first-symbol-str)
              (= "ns" first-symbol-str))
      (let [without-cruft (->> dfn
                               (filter #(not (re-find #"^;" (str %))))
                               (filter #(not= 'MARG_NEWLINE %))
                               (filter #(not= 'MARG_SPACE %)))
            third-symbol (when (> (count without-cruft) 2)
                           (nth without-cruft 2))]
        (if (string? third-symbol)
          (let [i (index-of third-symbol dfn)]
            [third-symbol (drop-nth i dfn)]))))))

#_(pick-docstring '(defn hello-world MARG_NEWLINE MARG_NEWLINE"docstring" MARG_NEWLINE [stuff]))

(defn tag-docstrings [col form]
  (if-not (and (:code-form form)
               (seq? (:code-form form)))
    (conj col form)
    (let [code-form (:code-form form)
          inline-comments (filter #(re-find #"^;" (str %))
                                  code-form)
          without-inline-comments (filter #(not (re-find #"^;" (str %))) code-form)
          [docstring without-docstring] (pick-docstring without-inline-comments)]
      (if docstring
        (conj col
              {:docstring-form (apply str
                                      docstring
                                      (interleave
                                       (repeat "\n") inline-comments))
               :line (:line form)}
              {:code-form without-docstring
               :line (:line form)})
        (conj col form)))))

#_(aget (get-field clojure.lang.MargLispReader :macros nil) (int \;))

#_(tag-docstrings [] 
                  {:code-form 'MARG_NEWLINE, :line 8})

(defn replace-in [find replace col]
  (cond
   (seq? col) (map #(if (= find %) replace %) col)
   (= find col) replace
   :else col))

(defn reconstruct-whitespace [form]
  (if (:code-form form)
    (assoc form
      :code-form (->> (:code-form form)
                           (replace-in 'MARG_SPACE " ")
                           (replace-in 'MARG_NEWLINE "\n")))
    form))


(defn parse [src]
  (aset (get-field clojure.lang.MargLispReader :macros nil) (int \;) read-comment)
  (let [rdr (do
              (clojure.lang.LineNumberingPushbackReader.
               (java.io.StringReader. src)))
        forms (take-while
               #(not (nil? (:form %)))
               (repeatedly (fn [] {:form (clojure.lang.MargLispReader/read rdr false nil false) :line (.getLineNumber rdr)})))]
    #_forms
    (->> forms
         (map #(cond
                (re-find #"^;" (str (:form %))) {:comment-form (:form %)
                                                 :line (:line %)}
                :else {:code-form (:form %)
                       :line (:line %)}))
         (reduce tag-docstrings [])
         (map reconstruct-whitespace))))

(pprint (parse ";; Some Comment
 (defn hello-world
   \"here's the
     multi-line docstring\"
    [name] ; inline comment
    (pritnln \"hello\" name \"!\"))
 ;; Some Other Comment
 ([the quick brown fox jumps over the lazy dog])"))

(defn end-of-block? [cur-group groups forms]
  (let [cur-form (first forms)
        next-form (second forms)]
    (and (:code-form cur-form)
         (or (:docstring-form next-form)
             (:comment-form next-form)))))

(defn merge-form [form m]
  (cond
   (:docstring-form form) (assoc m :docs (conj (get m :docs []) form))
   (:code-form form) (assoc m :codes (conj (get m :codes []) form))
   (:comment-form form) (assoc m :docs (conj (get m :docs []) form))))

(defn group-forms [tagged-forms]
  (loop [cur-group {}
         groups []
         forms tagged-forms]
    (cond
     (empty? forms) (conj groups cur-group)
     
     (end-of-block? cur-group groups forms)
     (recur {} (conj groups (merge-form (first forms) cur-group)) (rest forms))

     :else (recur (merge-form (first forms) cur-group) groups (rest forms)))))

;; How is this handled?
;; I wonder?
;; No idea ne
(defn gen-doc! [path]
  (println "Generating documentation for " path)
  (with-open [src (io/reader (io/file path))]
    (doseq [section (parse src)]
      ;; and this?
      (println section))))

(defn gen-doc! [path]
  (with-open [src (io/reader (io/file path))]
    (parse src)))

(re-find *comment* "  ;; this is a comment")

(defn path-to-doc [fn]
  (let [ns (-> (java.io.File. fn)
               (read-file-ns-decl)
               (second)
               (str))
        groups (->> fn
                    (gen-doc!)
                    (group-lines))]
    {:ns ns
     :groups groups}))


;; ## Ouput Generation

(defn uberdoc! [output-file-name files-to-analyze]
  "Generates an uberdoc html file from 3 pieces of information:

   1. Results from processing source files (`path-to-doc`)
   2. Project metadata obtained from `parse-project-file`.
   3. The path to spit the result (`output-file-name`)"
  (let [docs (map path-to-doc files-to-analyze)
        source (uberdoc-html
                output-file-name
                (parse-project-file)
                (map path-to-doc files-to-analyze))]
    (spit output-file-name source)))

;; ## External Interface
;; ### (command-line, lein, cake, etc)

(defn format-sources [sources]
  (if (nil? sources)
    (find-clojure-file-paths "./src")
    (->> sources
         (map #(if (dir? %)
                 (find-clojure-file-paths %)
                 [%]))
         (flatten))))

(defn run-marginalia [sources]
  (let [sources (format-sources sources)]
    (if-not sources
      (do
        (println "Wrong number of arguments passed to marginalia.")
        (println "Please present paths to source files as follows:"))
      (do
        (println "Generating uberdoc for the following source files:")
        (doseq [s sources]
          (println "  " s))
        (println)
        (ensure-directory! "./docs")
        (uberdoc! "./docs/uberdoc.html" sources)
        (println "Done generating your docs, please see ./docs/uberdoc.html")
        (println)))))

(defn -main
  "main docstring
   Multi line"
  [sources]
  (run-marginalia sources))

;; # Example Usage
(comment

  ;; Command line example
  (-main ["./src/marginalia/core.clj" "./src/marginalia/html.clj"])
  
  ;; This will find all marginalia source files, and then generate an uberdoc.
  (-main (find-clojure-file-paths "./src"))

;; Move these to tests
  (merge-line {:docstring-text "hello world" :line 3} {:docs ["stuff"]})
  (merge-line {:code-text "(defn asdf" :line 4} {:docs ["stuff"]})
  (merge-line {:docs-text "There's only one method in this module", :line 4} {}))

