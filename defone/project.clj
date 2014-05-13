(defproject defone "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repl-options {; :init-ns defone.core
                 :timeout 120000}
  :aot [defone.bruce]
  :plugins [[cider/cider-nrepl "0.6.0"]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.nrepl "0.2.3"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [net.n01se/clojure-jna "1.0.0"]
                 ])
