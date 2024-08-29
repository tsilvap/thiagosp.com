(in-package :com.thiagosp.notes)

(defparameter *config*
  (with-open-file (s (asdf:system-relative-pathname "thiagosp" "config.json")
                     :direction :input
                     :if-does-not-exist :error)
    (jzon:parse s)))

(defparameter *notes-directory*
  (@ *config* "notes" "dir"))

(defclass note ()
  ((title
    :reader title
    :documentation "Note title.")
   (slug
    :initarg :slug
    :reader slug
    :documentation "Note slug.")
   (created-at
    :initarg :created-at
    :reader created-at
    :documentation "The date this note was created.")
   (path
    :initarg :path
    :documentation "Path of the file that contains the note.")))

(defun make-note (note-path)
  (make-instance 'note
                 :path (merge-pathnames note-path *notes-directory*)))

(defun from-slug (note-slug)
  (make-note (note-path-from-slug note-slug)))

(defmethod initialize-instance :after ((n note) &key)
  (with-slots (title slug created-at path) n
    (setf title (get-org-keyword "title" path))
    (setf created-at (parse-org-timestamp
                      (get-org-keyword "date" path)))
    (setf slug (note-path-to-slug path))))

(defmethod note-to-html ((n note))
  "Returns a string with the HTML representation of the note."
  (with-slots (path) n
    (uiop:run-program (list "pandoc" "--to=html" "--shift-heading-level-by=1"
                            (namestring path))
                      :output :string)))

(defun get-all-notes ()
  "Return a list of all notes, most recent first."
  (loop for path in (reverse (uiop:directory-files *notes-directory*))
        collect (make-note path)))

(defun note-path-from-slug (slug)
  (find slug
        (uiop:directory-files *notes-directory*)
        :key #'note-path-to-slug
        :test #'string=))

(defun note-path-to-slug (path)
  (let* ((basename (pathname-name path))
         (slug (regex-replace "[0-9]+-" basename ""))
         (slug (regex-replace-all "_" slug "-")))
    slug))

(defun get-org-keyword (keyword org-file-path)
  "Gets the value of the keyword in an Org file."
  (let ((keyword-prefix (format nil "#+~a: " keyword)))
    (str:substring (length keyword-prefix)
                   t
                   (find keyword-prefix (uiop:read-file-lines (namestring org-file-path))
                         :test #'str:starts-with-p
                         :key #'string-downcase))))

(defun parse-org-timestamp (org-timestamp)
  "Returns a local-time object from a Org timestamp like <2024-08-29 Thu>."
  (local-time:parse-timestring
   (car (ppcre:all-matches-as-strings "\\d+-\\d+-\\d+" org-timestamp))))
