(in-package :com.thiagosp)

(defun generate-site (sorted-movies)
  "Generates all the files for the static website."
  (with-open-file (f (asdf:system-relative-pathname "thiagosp"
                                                    "dist/index.html")
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-sequence (home-page) f))

  (with-open-file (f (asdf:system-relative-pathname "thiagosp"
                                                    "dist/in-theaters.html")
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-sequence (in-theaters-page sorted-movies) f)))

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

(easy-routes:defroute in-theaters ("/in-theaters" :method :get) ()
  (in-theaters-page *sorted-movies*))

(generate-site *sorted-movies*)
