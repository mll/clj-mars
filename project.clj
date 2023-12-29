(defproject clj-mars "1.0.2"
  :description "Memory Array Redcode Simulator written in pure clojure. Supporting ICWS-88 standard."
  :url "https://www.spinney.io"
  :license {:name "GPL v2"
            :url "https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html#SEC1"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [instaparse "1.4.12"]
                 [clojure2d "1.4.5"]]
  :main corewars.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
