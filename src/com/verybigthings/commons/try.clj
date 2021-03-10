(ns com.verybigthings.commons.try
  (:require [clojure.spec.alpha :as s]))

(defn check-asserts
  [f]
  (let [old-value (s/check-asserts?)]
    (s/check-asserts true)
    (f)
    (s/check-asserts old-value)))

(defn- catch-matching-like? [val]
  (and (list? val)
    (= 'catch-matching (first val))))

(defn- finally-like? [val]
  (and (list? val)
    (= 'finally (first val))))

(s/def ::try-as->form
  (s/or
    :catch-matching ::catch-matching
    :form #(not (catch-matching-like? %))))

(s/def ::catch-match
  (s/or
    :keyword keyword?
    :contains? (s/and symbol? #(= '_ %))
    :symbol symbol?
    :catch-match (s/map-of any? ::catch-match)
    :fn-or-val any?))

(s/def ::root-catch-match
  (s/or
    :catch-match (s/map-of any? ::catch-match)
    :fn-or-val #(not= % :else)))

(s/def ::catch-matching-body
  (s/cat
    :ex-binding simple-symbol?
    :body (s/cat
            :catch-blocks
            (s/*
              (s/cat
                :catch-case (s/or
                              :class symbol?
                              :match ::root-catch-match)
                :catch-body any?))
            :else (s/? (s/cat
                         :else-kw #(= % :else)
                         :else-body any?)))))

(s/def ::catch-matching
  (s/and
    list?
    (s/cat
      :catch-matching-sym (s/and simple-symbol? #(= "catch-matching" (name %)))
      :catch-matching-body ::catch-matching-body)))

(s/def ::finally
  (s/and
    list?
    (s/cat
      :finally-sym (s/and simple-symbol? #(= "finally" (name %)))
      :finally-body any?)))

(s/def ::try-as->
  (s/cat
    :expr any?
    :binding simple-symbol?
    :forms (s/* ::try-as->form)))

(s/def ::try+
  (s/cat
    :body (s/* #(not (or (finally-like? %) (catch-matching-like? %))))
    :catch-matching (s/? ::catch-matching)
    :finally (s/? ::finally)))

(s/def ::try-let
  (s/cat
    :let-bindings (s/and vector? #(even? (count %)))
    :body (s/alt
            :stanzas (s/cat
                       :body (s/* #(not (or (finally-like? %) (catch-matching-like? %))))
                       :catch-matching (s/? ::catch-matching)
                       :finally (s/? ::finally))
            :stanzas (s/cat
                       :catch-matching (s/? ::catch-matching)
                       :finally (s/? ::finally)
                       :body (s/* #(not (or (finally-like? %) (catch-matching-like? %))))))))

(defn exception? [val]
  (instance? Exception val))

(defn- make-catch-matching-cond [form]
  (let [{:keys [ex-binding body]} form
        else (or (get-in body [:else :else-body]) ex-binding)
        catch-blocks (:catch-blocks body)
        catch-block-types (set (map #(-> % :catch-case first) catch-blocks))
        ex-data-binding (gensym "ex-data-binding")]
    `(let [~ex-data-binding ~(if (contains? catch-block-types :match) `(ex-data ~ex-binding) `nil)]
       (cond
         ~@(mapcat
             (fn [{:keys [catch-case catch-body]}]
               [(let [[catch-type catch] catch-case]
                  (if (= :class catch-type)
                    `(instance? ~catch ~ex-binding)
                    (let [cur (gensym "cur")]
                      (letfn [(builder-keyword [cur match]
                                `(isa? ~cur ~match))
                              (builder-symbol [cur match]
                                `(~match ~cur))
                              (builder-catch-match [cur match]
                                (let [matchers
                                      (map
                                        (fn [[k m]]
                                          (if (= :contains? (first m))
                                            `(contains? ~cur ~k)
                                            `(let [~cur (get ~cur ~k)]
                                               ~(builder cur m))))
                                        match)]
                                  `(and ~@matchers)))
                              (builder-fn-or-val [cur match]
                                `(if (fn? ~match)
                                   (~match ~cur)
                                   (= ~cur ~match)))
                              (builder [cur [match-type match]]
                                (let [b ({:keyword builder-keyword
                                          :symbol builder-symbol
                                          :catch-match builder-catch-match
                                          :fn-or-val builder-fn-or-val}
                                         match-type)]
                                  (b cur match)))]
                        `(let [~cur ~ex-data-binding]
                           ~(builder cur catch))))))
                `~catch-body])
             catch-blocks)
         :else ~else))))

(defn- make-catch-matching-fn [binding form]
  (let [ex-binding (:ex-binding form)]
    `(fn [~binding ~ex-binding]
       ~(make-catch-matching-cond form))))

(defn- make-try-as->fns [binding forms]
  (mapv
    (fn [[form-type form]]
      (let [f (if (= :form form-type)
                `(fn [~binding] ~form)
                (make-catch-matching-fn binding (:catch-matching-body form)))]
        {:form-type form-type
         :form-fn f}))
    forms))

(defmacro try-as-> [& args]
  (check-asserts #(s/assert ::try-as-> args))
  (let [conformed (s/conform ::try-as-> args)
        {:keys [expr binding forms]} conformed]
    `(let [{result# :result
            exception# :exception
            current-type# :current-type}
           (reduce
             (fn [{result# :result current-type# :current-type exception# :exception :as acc#}
                  {form-type# :form-type form-fn# :form-fn}]

               (cond
                 (and (= :result current-type#) (= :form form-type#))
                 (try
                   (assoc acc# :result (form-fn# result#))
                   (catch Exception e#
                     (assoc acc# :current-type :exception
                                 :exception e#)))

                 (and (= :exception current-type#) (= :catch-matching form-type#))
                 (try
                   (let [res# (form-fn# result# exception#)]
                     (if (exception? res#)
                       (assoc acc# :exception res#)
                       {:result res#
                        :current-type :result})))

                 :else acc#))
             {:result ~expr
              :current-type :result}
             ~(make-try-as->fns binding forms))]
       (if (= :exception current-type#)
         (throw exception#)
         result#))))

(defn- make-try+-catch-matching [form]
  (let [ex-binding (:ex-binding form)]
    `(catch Exception ~ex-binding
       (let [res# ~(make-catch-matching-cond form)]
         (if (exception? res#)
           (throw res#)
           res#)))))

(defn- make-try+-finally [form]
  `(finally
     ~form))

(defn- make-try+-catch-matching-and-finally [forms]
  (let [catch-matching-body (get-in forms [:catch-matching :catch-matching-body])
        finally-body (get-in forms [:finally :finally-body])]
    (cond-> []
      catch-matching-body (conj (make-try+-catch-matching catch-matching-body))
      finally-body (conj (make-try+-finally finally-body)))))

(defmacro try+ [& args]
  (check-asserts #(s/assert ::try+ args))
  (let [conformed (s/conform ::try+ args)
        body (:body conformed)]
    `(try
       ~@body
       ~@(make-try+-catch-matching-and-finally conformed))))

(defmacro try-let [& args]
  (check-asserts #(s/assert ::try-let args))
  (let [conformed (s/conform ::try-let args)
        {:keys [let-bindings]} conformed
        [_ {:keys [body] :as stanzas}] (:body conformed)
        bindings-destructured (destructure let-bindings)
        bindings-ls (take-nth 2 bindings-destructured)
        gensyms (take (count bindings-ls) (repeatedly gensym))]
    `(let [[ok# ~@gensyms]
           (try
             (let [~@bindings-destructured] [true ~@bindings-ls])
             ~@(make-try+-catch-matching-and-finally stanzas))]
       (if ok#
         (let [~@(interleave bindings-ls gensyms)]
           ~@body)
         ~(first gensyms)))))
