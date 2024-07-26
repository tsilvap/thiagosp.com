(in-package :com.thiagosp)

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

(defun generate-site ()
  "Generates all the files for the static website."
  (with-open-file (f (asdf:system-relative-pathname "thiagosp"
                                                    "dist/index.html")
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-sequence (home-page) f)))

;;; Routes for development server. In production, we'll serve a
;;; static website from a proper web server.

(easy-routes:defroute root ("/" :method :get) ()
  (home-page))
