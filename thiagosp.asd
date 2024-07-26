(defsystem "thiagosp"
  :version "0.0.1"
  :author "Thiago da Silva Pinto"
  :license ""
  :depends-on ("com.inuoe.jzon"
               "dexador"
               "easy-routes"
               "hunchentoot"
               "local-time"
               "serapeum"
               "spinneret")
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "in-theaters" :depends-on ("packages"))
                 (:file "pages" :depends-on ("packages" "in-theaters"))
                 (:file "main" :depends-on ("packages" "in-theaters" "pages")))))
  :description ""
  :in-order-to ((test-op (test-op "thiagosp/tests")))
  :build-operation "program-op"
  :build-pathname "runserver"
  :entry-point "com.thiagosp:run-server-cli")

(defsystem "thiagosp/tests"
  :author ""
  :license ""
  :depends-on ("thiagosp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for thiagosp"
  :perform (test-op (op c) (symbol-call :rove :run c)))
