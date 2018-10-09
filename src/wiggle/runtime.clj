(ns wiggle.runtime
  (:gen-class)
  (:require [clojure.edn :as edn])
  (:refer-clojure :exclude [eval fn? resolve]))

(set! *warn-on-reflection* true)

(declare eval)

(def initial-env
  {'+ +
   'println println})

(def self-evaluating? (some-fn number? string? keyword? vector? map? set?))

(defn resolve [env sym]
  (if (contains? env sym)
    (get env sym)
    (throw (ex-info (format "%s has not been defined!" sym) {:sym sym}))))

(def empty-coll? (complement not-empty))

(defn contains-keys? [m ks]
  (every? #(contains? m %) ks))

(defn fn? [f]
  (and (map? f) (contains-keys? f #{:env :params :body})))

(defn call [f & args]
  (cond
    (clojure.core/fn? f) (apply f args)
    (fn? f) (let [{:keys [env params body]} f]
              (eval (merge env (zipmap params args))
                    ;; FIXME: evaluate entire body
                    (last body)))))

(defn evlist [env [op & args :as l]]
  (if (empty-coll? l)
    ()
    (let [f (eval env op)
          arg-vals (map (partial eval env) args)]
      (apply call f arg-vals))))

(defn eval
  ([env form]
   (cond
     (self-evaluating? form) form
     (symbol? form) (resolve env form)
     (list? form) (evlist env form))))

;; An example Wiggle function
(def println2
  {:params '[text]
   :env {'println println}
   :body '[(println text)]})

(defn -main
  "Treats first arg as a program to evaluate."
  [& args]
  (let [program-string (first args)
        program (edn/read-string program-string)]
    (print "Program: ")
    (pr program)
    (newline)
    (println (eval initial-env program))))

;; (eval initial-env '(+ 3 3))
