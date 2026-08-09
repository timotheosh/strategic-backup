(ns strategic-backup.timing
  "Per-stage wall-clock timing instrumentation (pipeline-timing spec).

   The single shared mechanism for measuring and logging how long a
   pipeline stage took — used throughout core.clj's run-backup!/
   run-checksums! and restore.clj's run-restore-test! so the
   measurement+logging logic itself is never duplicated at each call site.

   Uses System/nanoTime, not a wall-clock reading (e.g. java.time.Instant),
   because nanoTime is monotonic — immune to system clock adjustments
   (NTP sync, manual changes) that could otherwise move a wall-clock
   reading backward mid-measurement on a long-running stage."
  (:require [taoensso.timbre :as log]))

(defn elapsed-ms
  "Calculation. Converts a System/nanoTime start/end pair into whole
   elapsed milliseconds (truncated, not rounded)."
  [start-nanos end-nanos]
  (quot (- end-nanos start-nanos) 1000000))

(defn- time-stage-impl!
  "Action (private). Shared measurement+logging core for time-stage! and
   time-stage-with-result! — runs `thunk`, logging {:stage stage
   :elapsed-ms N} via log/info on success. On failure, logs the elapsed
   time up to that point via log/warn, then rethrows the exception
   unchanged — this is pure instrumentation around whatever error handling
   already exists at the call site, never a new error path of its own.
   Returns {:result .. :elapsed-ms ..} on success."
  [stage thunk]
  (let [start (System/nanoTime)]
    (try
      (let [result (thunk)
            ms     (elapsed-ms start (System/nanoTime))]
        (log/info "Stage complete" {:stage stage :elapsed-ms ms})
        {:result result :elapsed-ms ms})
      (catch Throwable t
        (log/warn "Stage failed" {:stage stage :elapsed-ms (elapsed-ms start (System/nanoTime))})
        (throw t)))))

(defn time-stage!
  "Action. Runs `thunk`, logging {:stage stage :elapsed-ms N}, and returns
   thunk's return value directly (unwrapped) — the ergonomic form for
   inline call-site wrapping in run-backup!/run-restore-test!."
  [stage thunk]
  (:result (time-stage-impl! stage thunk)))

(defn time-stage-with-result!
  "Action. Same measurement/logging as time-stage!, but returns
   {:result .. :elapsed-ms ..} — for callers (e.g. run-checksums!) that
   need the elapsed time itself, not just a log line."
  [stage thunk]
  (time-stage-impl! stage thunk))
