(defproject cloj_mlia "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"] ; asynchrony
                 [midje "1.6.3"]; alternative to clojure.test with smooth progression path.
                 
                 [cheshire "5.3.1"]; JSON encoding library for Clojure. 
                 [http-kit "2.1.16"] ; High-performance, event-driven, asynchronous, Ring-compatible HTTP & Websocket client/server for clojure.
                 
                 [incanter "1.5.4"]; Incanter statistical computing and graphics environment.
                 [nz.ac.waikato.cms.weka/weka-stable "3.6.6"]; Weka machine-learning workbench.
                 ])
