(in-package :com.thiagosp)

(defmacro generate-page (system-relative-dest-path &body body)
  (with-gensyms (dest-path f)
    `(let ((,dest-path (asdf:system-relative-pathname
                        "thiagosp" ,system-relative-dest-path)))
       (ensure-directories-exist ,dest-path)
       (with-open-file (,f ,dest-path
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
         (write-sequence (progn ,@body) ,f)))))

(defun generate-site (sorted-movies)
  "Generates all the files for the static website."
  (generate-page "dist/index.html" (home-page))

  (generate-page "dist/notes.html" (notes-page))
  (loop for note in (notes:get-all-notes)
        do (generate-page (format nil "dist/notes/~A.html" (notes:slug note))
             (note-page (notes:slug note))))

  (generate-page "dist/in-theaters.html" (in-theaters-page sorted-movies)))

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
  (home-page))

(easy-routes:defroute notes ("/notes" :method :get) ()
  (notes-page))

(easy-routes:defroute note ("/notes/:note-slug" :method :get) ()
  (note-page note-slug))

(easy-routes:defroute in-theaters ("/in-theaters" :method :get) ()
  (in-theaters-page *sorted-movies*))

(generate-site *sorted-movies*)
