(defproject muxie "1.0.0-SNAPSHOT"
  :description "A MU bouncer"
  :main muxie.core
  :dependencies [[org.clojure/clojure "1.3.0"]
		 [server-socket "1.0.0"]]

  :dev-dependencies [;[swank-clojure "1.4.0"]
		     [clojure-source "1.3.0"]]
  :jvm-opts ["-Xmx128m" "-server"]
;  :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"]
;  :extra-classpath-dirs ["/usr/lib/jvm/java-6-sun/lib/tools.jar"]
  )