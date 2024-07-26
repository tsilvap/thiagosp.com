(defpackage :com.thiagosp.pages
  (:use :cl)
  (:export #:home-page))

(defpackage :com.thiagosp
  (:use :cl :com.thiagosp.pages)
  (:export #:generate-site
           #:run-server-cli))
