(ns strategic-backup.timing-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [strategic-backup.timing :as timing]))

;; ---------------------------------------------------------------------------
;; elapsed-ms
;; ---------------------------------------------------------------------------

(deftest elapsed-ms-converts-nanos-to-millis
  (testing "converts a start/end nanoTime pair into whole elapsed milliseconds"
    (is (= 1500 (timing/elapsed-ms 0 1500000000)))
    (is (= 1 (timing/elapsed-ms 1000000000 1001000000)))))

(deftest elapsed-ms-zero-when-no-time-elapsed
  (testing "returns 0 when start and end are identical"
    (is (= 0 (timing/elapsed-ms 5000 5000)))))

(deftest elapsed-ms-truncates-not-rounds
  (testing "truncates the sub-millisecond remainder rather than rounding"
    (is (= 1 (timing/elapsed-ms 0 1999999)))))

;; ---------------------------------------------------------------------------
;; Property 3 (pipeline-timing spec design.md): Elapsed Time Is Never Negative
;;
;; For any two nanoTime values where end >= start, elapsed-ms SHALL return a
;; non-negative value, and SHALL equal (end - start) converted to whole
;; milliseconds with no rounding beyond truncation.
;;
;; **Validates: Requirements 2.6, 2.7**
;; ---------------------------------------------------------------------------

(defspec elapsed-ms-never-negative-and-truncates-correctly 100
  (prop/for-all [start       (gen/choose 0 1000000000000)
                 delta-nanos (gen/choose 0 10000000000000)]
    (let [end    (+ start delta-nanos)
          result (timing/elapsed-ms start end)]
      (and (>= result 0)
           (<= (* result 1000000) delta-nanos)
           (< delta-nanos (* (inc result) 1000000))))))

;; ---------------------------------------------------------------------------
;; time-stage!
;; ---------------------------------------------------------------------------

(deftest time-stage-returns-thunks-value
  (testing "returns the thunk's return value directly, unwrapped"
    (is (= 42 (timing/time-stage! :foo (fn [] 42))))))

(deftest time-stage-runs-thunk-exactly-once
  (testing "invokes the thunk exactly once"
    (let [calls (atom 0)]
      (timing/time-stage! :foo (fn [] (swap! calls inc)))
      (is (= 1 @calls)))))

(deftest time-stage-propagates-exceptions
  (testing "an exception thrown by the thunk propagates unchanged"
    (is (thrown-with-msg? Exception #"boom"
                          (timing/time-stage! :foo (fn [] (throw (Exception. "boom"))))))))

;; ---------------------------------------------------------------------------
;; time-stage-with-result!
;; ---------------------------------------------------------------------------

(deftest time-stage-with-result-returns-result-and-elapsed-ms
  (testing "returns {:result :elapsed-ms} rather than the bare value"
    (let [{:keys [result elapsed-ms]} (timing/time-stage-with-result! :foo (fn [] :done))]
      (is (= :done result))
      (is (>= elapsed-ms 0)))))

(deftest time-stage-with-result-runs-thunk-exactly-once
  (testing "invokes the thunk exactly once"
    (let [calls (atom 0)]
      (timing/time-stage-with-result! :foo (fn [] (swap! calls inc)))
      (is (= 1 @calls)))))

(deftest time-stage-with-result-propagates-exceptions
  (testing "an exception thrown by the thunk propagates unchanged"
    (is (thrown-with-msg? Exception #"kaboom"
                          (timing/time-stage-with-result! :foo (fn [] (throw (Exception. "kaboom"))))))))
