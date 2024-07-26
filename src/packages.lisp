(defpackage :com.thiagosp.in-theaters
  (:use :cl)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:import-from :serapeum #:@)
  (:export #:get-movies
           #:get-sorted-movies
           #:get-rating
           #:movie-not-found-error))

(defpackage :com.thiagosp.pages
  (:use :cl :com.thiagosp.in-theaters)
  (:import-from :serapeum #:@)
  (:export #:home-page
           #:in-theaters-page))

(defpackage :com.thiagosp
  (:use :cl :com.thiagosp.pages :com.thiagosp.in-theaters)
  (:export #:generate-site
           #:run-server-cli))
