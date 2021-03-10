(ns com.verybigthings.commons.try-test
  (:require [clojure.test :refer :all]
            [com.verybigthings.commons.try :refer [try+ try-as-> try-let]]))

(derive ::derived-qux ::qux)

(defn ex-info! [msg data]
  (throw (ex-info msg data)))


(deftest test-try+
  (testing "Matching by class"
    (let [finally-called* (atom false)
          afn #(try+
                 (/ 1 0)
                 (catch-matching e
                   ArithmeticException 0)
                 (finally
                   (swap! finally-called* not)))]
      (is (= 0 (afn)))
      (is true? @finally-called*)))

  (testing "Matching by pattern, class, else"
    (let [finally-called* (atom false)
          afn             (fn [arg]
                            (try+
                              (reset! finally-called* false)
                              (case arg
                                1 (ex-info! "Error" {:foo :bar})
                                2 (ex-info! "Error" {:foo 2})
                                3 (ex-info! "Error" {:foo "3"})
                                4 (ex-info! "Error" {:foo ::derived-qux})
                                5 (ex-info! "Error" {:foo [:some :data]})
                                6 (ex-info! "Error" {:foo {:bar :baz}})
                                7 (ex-info! "Error" {:bar :baz})
                                8 (/ 1 0)
                                9 (ex-info! "Error" {}))
                              (catch-matching e
                                {:foo :bar} 1
                                {:foo 2} 2
                                {:foo "3"} 3
                                {:foo ::qux} 4
                                {:foo #(= % [:some :data])} 5
                                {:foo {:bar :baz}} 6
                                #(= {:bar :baz} %) 7
                                ArithmeticException 8
                                :else 9)
                              (finally
                                (swap! finally-called* not))))]
      (doseq [i (range 1 10)]
        (is (= i (afn i)))
        (is true? @finally-called*))))

  (testing "Matching by pattern, class, else without finally clause"
    (let [afn (fn [arg]
                (try+
                  (case arg
                    1 (ex-info! "Error" {:foo :bar})
                    2 (ex-info! "Error" {:foo 2})
                    3 (ex-info! "Error" {:foo "3"})
                    4 (ex-info! "Error" {:foo ::derived-qux})
                    5 (ex-info! "Error" {:foo [:some :data]})
                    6 (ex-info! "Error" {:foo {:bar :baz}})
                    7 (ex-info! "Error" {:bar :baz})
                    8 (/ 1 0)
                    9 (ex-info! "Error" {}))
                  (catch-matching e
                    {:foo :bar} 1
                    {:foo 2} 2
                    {:foo "3"} 3
                    {:foo ::qux} 4
                    {:foo #(= % [:some :data])} 5
                    {:foo {:bar :baz}} 6
                    #(= {:bar :baz} %) 7
                    ArithmeticException 8
                    :else 9)))]
      (doseq [i (range 1 10)]
        (is (= i (afn i))))))

  (testing "If not matched, exception will be rethrown"
    (let [finally-called* (atom false)
          afn #(try+
                 (/ 1 0)
                 (catch-matching e
                   {:foo :bar} :will-not-match)
                 (finally
                   (swap! finally-called* not)))]
      (is (thrown? ArithmeticException (afn)))
      (is true? @finally-called*)))

  (testing "Finally without catch"
    (let [finally-called* (atom false)
          afn #(try+
                 (/ 1 0)
                 (finally
                   (swap! finally-called* not)))]
      (is (thrown? ArithmeticException (afn)))
      (is true? @finally-called*))))

(deftest test-try-let
  (testing "Try-let ok"
    (let [afn (fn []
                (try-let [a 999]
                  a
                  (catch-matching e
                    {:foo :bar} 1)))]
      (is (= 999 (afn)))))

  (testing "Try-let catch"
    (let [afn (fn []
                (try-let [a (ex-info! "Error" {:foo :bar})]
                  a
                  (catch-matching e
                    {:foo :bar} 1)))]
      (is (= 1 (afn)))))

  (testing "Try-let catch, finally"
    (let [finally-called* (atom false)
          afn (fn []
                (try-let [a (ex-info! "Error" {:foo :bar})]
                  a
                  (catch-matching e
                    {:foo :bar} 1)
                  (finally
                    (swap! finally-called* not))))]
      (is (= 1 (afn)))))

  (testing "Try-let catch, finally before body (1)"
    (let [finally-called* (atom false)
          afn (fn []
                (try-let [a (ex-info! "Error" {:foo :bar})]
                  (catch-matching e
                    {:foo :bar} 1)
                  (finally
                    (swap! finally-called* not))
                  a))]
      (is (= 1 (afn)))))

  (testing "Try-let catch, finally before body (2)"
    (let [finally-called* (atom false)
          afn (fn []
                (try-let [a 999]
                  (catch-matching e
                    {:foo :bar} 1)
                  (finally
                    (swap! finally-called* not))
                  a))]
      (is (= 999 (afn))))))


(deftest test-try-as->
  (testing "Returning exception from error track"
    (let [afn (fn []
                (try-as-> 1 $
                  (/ $ 0)
                  (catch-matching e
                    ArithmeticException
                    (ex-info "Other ex" {}))))]
      (is (thrown-with-msg? Exception #"Other ex" (afn)))))

  (testing "Throwing exception from error track"
    (let [afn (fn []
                (try-as-> 1 $
                  (/ $ 0)
                  (catch-matching e
                    ArithmeticException
                    (ex-info! "Other ex" {}))))]
      (is (thrown-with-msg? Exception #"Other ex" (afn)))))

  (testing "Catching exception from error track in a different catch-matching block"
    (let [afn (fn []
                (try-as-> 1 $
                  (/ $ 0)
                  (catch-matching e
                    ArithmeticException
                    (ex-info! "Other ex" {:foo :bar}))
                  (catch-matching e
                    {:foo :bar}
                    (ex-info "Other other ex" {:bar :baz}))
                  (catch-matching e
                    {:bar :baz}
                    (ex-info "Other other other ex" {}))))]
      (is (thrown-with-msg? Exception #"Other other other ex" (afn)))))

  (testing "Recovering form exception"
    (let [afn (fn []
                (try-as-> 1 $
                  (/ $ 0)
                  (catch-matching e
                    ArithmeticException
                    999)
                  (inc $)))]
      (is (= 1000 (afn))))))