(defpackage :com.thiagosp.notes
  (:use :cl)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:import-from :serapeum #:@)
  (:import-from :cl-ppcre
                #:regex-replace
                #:regex-replace-all)
  (:export #:make-note
           #:get-all-notes
           #:title
           #:slug
           #:from-slug
           #:note-to-html))

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
  (:local-nicknames (:notes :com.thiagosp.notes))
  (:import-from :serapeum #:@)
  (:export #:home-page
           #:notes-page
           #:note-page
           #:in-theaters-page))

(defpackage :com.thiagosp
  (:use :cl :com.thiagosp.pages :com.thiagosp.in-theaters)
  (:local-nicknames (:notes :com.thiagosp.notes))
  (:import-from :alexandria #:with-gensyms)
  (:export #:generate-site
           #:run-server-cli))
