(in-package :com.thiagosp)

(defmacro generate-page ((&key destination) &body body)
  (with-gensyms (dest-pathname f)
    `(let ((,dest-pathname (asdf:system-relative-pathname
                            "thiagosp" ,destination)))
       (ensure-directories-exist ,dest-pathname)
       (with-open-file (,f ,dest-pathname
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
         (write-sequence (progn ,@body) ,f))
       ,@body)))

(defun generate-site (sorted-movies)
  "Generates all the files for the static website."
  (generate-page (:destination "dist/index.html")
    (home-page))

  (generate-page (:destination "dist/notes.html")
    (notes-page))
  (loop for note in (notes:get-all-notes)
        do (generate-page (:destination
                           (format nil "dist/notes/~A.html" (notes:slug note)))
             (note-page (notes:slug note))))

  (generate-page (:destination "dist/in-theaters.html")
    (in-theaters-page sorted-movies)))

;;; Set up simple Hunchentoot server for development only. In
;;; production, we serve the static website from an external web
;;; server.

(defvar *server* nil)

(defun run-server (&key (port 4242))
  (when (null *server*)
    (setf *server* (make-instance 'easy-routes:easy-routes-acceptor :port port)))
  (hunchentoot:start *server*))

(defun stop-server ()
  (when (not (null *server*))
    (hunchentoot:stop *server*)))

(defun run-server-cli (&key (port 4242))
  (let ((server (make-instance 'easy-routes:easy-routes-acceptor :port port)))
    (hunchentoot:start server)
    (loop do (sleep 1000))))

(push (hunchentoot:create-folder-dispatcher-and-handler
       "/static/" (asdf:system-relative-pathname "thiagosp" "dist/static/"))
      hunchentoot:*dispatch-table*)

(defparameter *sorted-movies* (get-sorted-movies))

(easy-routes:defroute root ("/" :method :get) ()
  (generate-page (:destination "dist/index.html")
    (home-page)))

(easy-routes:defroute notes ("/notes" :method :get) ()
  (generate-page (:destination "dist/notes.html")
    (notes-page)))

(easy-routes:defroute note ("/notes/:note-slug" :method :get) ()
  (generate-page (:destination (format nil "dist/notes/~A.html" note-slug))
    (note-page note-slug)))

(easy-routes:defroute in-theaters ("/in-theaters" :method :get) ()
  (generate-page (:destination "dist/in-theaters.html")
    (in-theaters-page *sorted-movies*)))

(generate-site *sorted-movies*)
