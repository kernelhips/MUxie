(ns muxie.core
  ""
  (:use [server.socket :only (create-server)]
	[clojure.java.io :only (reader, writer)]
	[clojure.string :only (lower-case, join)])
  (:import [java.net Socket SocketException]))

;; TODO: Lots; change user representation

; of these Vars, only perc-commands does anything right now
(def cmd-extensions (atom []))
(def perc-commands (atom {}))
(def trig-extensions (atom []))

(def users (atom {}))
(def userlogs (atom {}))
(def ^:dynamic *user*)
(def ^:dynamic *server*)

(def *prefs
     "These will be the settable user preferences."
     { :user ["MU Username", ""] })

(defrecord User [name password cxn outs log])

(defn make-user
  ([name pw] (User. name pw nil [] (atom []))))

(defprotocol UserLike
  (get-user [u])
  (user-name [u]))

(extend-protocol UserLike
  String
  (get-user [u] (deref (get @users u)))
  (user-name [u] u)
  clojure.lang.IDeref
  (get-user [u] @u)
  (user-name [u] (:name @u)))
  
(defn get-user [uname]
     (get @users uname))

					; user connections
(defn connected? [uref]
  (not (nil? (:cxnin @uref))))

(defn remove-user-out! [uref out]
    (swap! uref #(assoc % :outs (vec (remove #{out} (:outs %))))))

(defn clear-connection! [uref]
    (swap! uref assoc :cxnin nil :cxnout nil))

					; user commands
(defmacro def-perc-cmd
  [defname argvec & body]
  (swap! perc-commands
	 assoc
	 (lower-case (name defname))
	 (eval
	  `(fn ~defname ~argvec
	    ~@body))))

(defn prompt [s]
  (print s)
  (flush)
  (read-line))

					; user history
(defn userlog
  [uref msg]
  (let [log (:log @uref)]
    (swap! log conj msg)))

(defn reset-userlog
  [uref]
  (swap! uref assoc :log (atom [])))

(defn log-empty?
  [uref]
  (empty? (deref (:log @uref))))

(declare userprint)
(defn print-user-log
  ([uref]
     (userprint uref (join (deref (:log @uref)))))
  ([] (print-user-log *user*)))

(defn print-and-discard-user-log
  ([uref]
     (print-user-log uref)
     (reset-userlog uref))
  ([] (print-and-discard-user-log *user*)))

					; Recording runtime errors
(def errors (atom []))
(defn record-error
  [err]
  (swap! errors conj err))

(defn clear-errors []
  (reset! errors []))
  
(defn userprint
  "Print a message to all sockets associated with a username."
  [uref msg]
  (let [outs (:outs @uref)]
    (if (seq outs)
      (doseq [out outs]
	(try
	  (.write out msg)
	  (.flush out)
	  (catch SocketException sexc
	    (remove-user-out! uref out))))
      
      (userlog (:name @uref) msg))))

(defn userprintln
  [uref msg]
  (userprint uref (str msg "\r\n")))

(defn userwrite
  [uref msg]
  (let [out (:cxnout @uref)]
    (.write out msg)
    (.write out "\r\n")
    (.flush out)))

					; open a connection to the specified server and port
(defn open-connection
  [server port]
  (let [sock (Socket. server port)]
    {:socket sock
     :in (reader (.getInputStream sock))
     :out (writer (.getOutputStream sock))}))

(defn conn-handler
  "Process input from the world."
  [uref]
  (try
    (loop []
      (if-let [cxn (:cxnin @uref)]
	(if-let [line (.readLine cxn)]
	  (do
	    (userprintln uref line)
	    (recur))
	  (do
	    (userprintln uref "% Connection terminated. %")
	    (clear-connection! uref)))))
    (catch SocketException secx
      (userprintln uref "% Connection terminated. %")
      (clear-connection! uref))))

(defn connect-user!
  "Connect a user to a server."
  [uref server port]
  (cond
   (connected? uref) uref
   :default (when (and server port)
	      (let [port (if (integer? port) port (Integer/parseInt port))
		    {:keys [in out]} (open-connection server port)]
		(swap! uref assoc :cxnin in :cxnout out)
		(println "done")
		(doto (Thread. #(conn-handler uref)) .start)
		uref))))

(defn add-out-stream!
  "Add an out stream to a user."
  [uref out]
  (swap! uref (fn [usr] (assoc usr :outs (conj (:outs usr) out)))))

(defn attempt-login
  ([server port username password]
     (loop [uref (get-user username)]
       (if uref
	 (if (= (:password @uref) password)
	   (do
	     (add-out-stream! uref *out*)
	     (if (and server port)
	       (connect-user! uref server port))
	     uref)
	   (println "Invalid password"))
	 ; create a new user:
       	 (let [uref (atom (make-user username password))]
	   (swap! users assoc username uref)
	   (println "New user created:", username)
	   (recur uref)))))
  ([username password]
     (attempt-login nil nil username password)))

(defn login []
  (loop [line (prompt "Bouncer login: ")]
    (if-let [[_ _ _ server port username password] (re-matches #"con\w*(\s+\"?(([a-zA-Z0-9_.]+):(\d+)))?\s+([a-zA-Z_]+)\"? (\S+)" line)]
      (or (attempt-login server port username password)
	  (println "Invalid login."))
      (do
	(println "Unrecognized command: ", line)
	(recur (prompt "Bouncer login: "))))))

					; Commands
(declare normal-state connect-state start-state)

(def-perc-cmd commands
  [_]
  (doseq [cmd (keys @perc-commands)]
    (println "-" cmd)))

(def-perc-cmd hooks
  [_]
  (println "Entering hook editing mode."))

(def-perc-cmd help
  [_]
  (println "Type %dc to quit."))

(def-perc-cmd dc
  [_]
  (println "Goodbye.")
  :exit)

(def-perc-cmd prefs
  [_]
  (println "Displaying preferences:")
  (doseq [[prefkey [prefname prefdesc]] *prefs]
    (println prefname)))

(defn do-paste [pref lines]
  (doseq [line lines]
    (userwrite *user* (str pref " " line))))

(defn make-paste-state [pref]
  (let [lines (atom [])]
    (fn []
      (let [line (read-line)]
	(case line
	 (".") (do (do-paste pref @lines)
		   normal-state)
	 (".q" ".quit") (do (println "% Paste mode aborted. %")
			    normal-state)
	 (do
	   (swap! lines conj line)
	   :current))))))

(def-perc-cmd paste
  [pref]
  (println "% Entering paste mode. Type . to finish or .q to abort. %")
  (make-paste-state pref))

(def-perc-cmd rept
  [times]
  (fn []
    (let [line (read-line)]
      (do-paste "" (take (Integer/parseInt times) (repeat line)))
      normal-state)))

(def-perc-cmd reload
  [_]
  (println "% Reloading normal state. %")
  normal-state)
					; Normal States
(defn normal-state
  ([input]
     (let [[_ cmd _ arg] (re-matches #"^%(\w+)( (.*))?" input)]
       (if (and cmd (contains? @perc-commands cmd))
	 (try
	   (let [returned ((get @perc-commands cmd) arg)]
	     (cond
	      (= returned :exit) nil
	      (fn? returned) returned
	      :default :current))
	   (catch Exception exc
	     (println "% An error occurred while executing the command. %")
	     (record-error (format "Error while executing %%%s: %s" cmd (str exc)))
	     :current))
	 (do
	   (userwrite *user* input)
	   :current))))
  ([] (normal-state (read-line))))

(defn start-state []
  (if (log-empty? *user*)
    (println "% No history to display. %")
    (do
      (println "% Displaying history. %")
      (print-and-discard-user-log *user*)
      (println "% End history. %")))
  normal-state)

(defn connect-state []
  (try
    (let [server (prompt "Server: ")
	  port (Integer/parseInt (prompt "Port: "))]
      (print *user*)
      (when (connect-user! *user* server port)
	(print-user-log *user*)
	normal-state))
    (catch Exception e
      (println "Invalid server name or port."))))

					; Core server
(defn got-input [in out]
    (binding [*in* (reader in), *out* (writer out)]
      (binding [*user* (login)]
	(println "% Welcome to MUxie.  %help displays help. %")
	(try
	  (loop [state (if (connected? *user*) start-state, connect-state)]
	    (if-let [newstate (state)]
	      (recur (if (= newstate :current) state newstate))))
	  (catch Exception exc
	    (record-error (format "Error in got-input: %s" (str exc))))
	  (finally
	   (remove-user-out! *user* *out*))))))

(defn run-server []
  (def *server*
       (create-server 3586 #'got-input)))

