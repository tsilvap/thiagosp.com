(defsystem "thiagosp"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "thiagosp/tests"))))

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
