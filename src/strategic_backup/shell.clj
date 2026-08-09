(ns strategic-backup.shell
  "Shell command execution abstraction.
   This is the single seam for mocking in tests — every namespace that
   invokes external commands accepts a ShellExecutor as a parameter."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Protocol
;; ---------------------------------------------------------------------------

(defprotocol ShellExecutor
  "Abstraction over shell command execution.

   All methods return a result map of the shape:
     {:exit N      ; integer exit code
      :out  \"...\" ; stdout as string
      :err  \"...\" ; stderr as string
      :cmd  \"...\" ; the command that was run (for logging)}"

  (run-cmd [this cmd args opts]
    "Execute a simple command.

     Parameters:
       cmd  — command name string (e.g. \"zfs\")
       args — vector of argument strings (e.g. [\"snapshot\" \"tank/data@backup\"])
       opts — options map; currently supports:
                :env (a map of env-var-name → value strings to merge into
                      the process environment)
                :in  (a string piped to the subprocess's stdin — the safe
                      way to hand a command a secret it reads interactively,
                      e.g. `zfs load-key`, without ever putting it in argv
                      or a file)

     Returns {:exit N :out \"...\" :err \"...\" :cmd \"<cmd> <args...>\"}")

  (run-pipeline [this shell-str]
    "Execute a shell pipeline string via /bin/sh -c.

     Parameters:
       shell-str — a complete shell command string, potentially containing
                   pipes, redirections, etc.
                   (e.g. \"zfs send -w snap | gzip -c > /staging/out.gz\")

     Returns {:exit N :out \"...\" :err \"...\" :cmd <shell-str>}"))

;; ---------------------------------------------------------------------------
;; Default implementation — uses clojure.java.shell/sh
;; ---------------------------------------------------------------------------

(defrecord DefaultShellExecutor []
  ShellExecutor

  (run-cmd [_this cmd args opts]
    (let [env-map  (get opts :env {})
          in-str   (get opts :in)
          ;; clojure.java.shell/sh accepts :env as a map of String→String
          ;; and :in as a string piped to the subprocess's stdin
          sh-args  (concat [cmd] args
                           (when (seq env-map) [:env env-map])
                           (when (some? in-str) [:in in-str]))
          result   (apply sh/sh sh-args)
          cmd-str  (str/join " " (cons cmd args))]
      (assoc result :cmd cmd-str)))

  (run-pipeline [_this shell-str]
    (let [result (sh/sh "/bin/sh" "-c" shell-str)]
      (assoc result :cmd shell-str))))

;; ---------------------------------------------------------------------------
;; Mock implementation — returns pre-configured results for use in tests
;; ---------------------------------------------------------------------------

(def ^:private default-mock-result
  {:exit 0 :out "" :err "" :cmd ""})

(defrecord MockShellExecutor
  [responses       ; map of cmd-string → result map
   default-result] ; result map returned when cmd-string is not in responses

  ShellExecutor

  (run-cmd [_this cmd args _opts]
    (let [cmd-str (str/join " " (cons cmd args))
          base    (get responses cmd-str
                       (or default-result default-mock-result))]
      (assoc base :cmd cmd-str)))

  (run-pipeline [_this shell-str]
    (let [base (get responses shell-str
                    (or default-result default-mock-result))]
      (assoc base :cmd shell-str))))

;; ---------------------------------------------------------------------------
;; Constructor helpers
;; ---------------------------------------------------------------------------

(defn make-default-executor
  "Return a DefaultShellExecutor instance."
  []
  (->DefaultShellExecutor))

(defn make-mock-executor
  "Return a MockShellExecutor.

   Parameters:
     responses      — (optional) map of cmd-string → result map
     default-result — (optional) result map returned for unrecognised commands;
                      defaults to {:exit 0 :out \"\" :err \"\" :cmd \"\"}"
  ([]
   (->MockShellExecutor {} nil))
  ([responses]
   (->MockShellExecutor responses nil))
  ([responses default-result]
   (->MockShellExecutor responses default-result)))
