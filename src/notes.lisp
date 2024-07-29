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
   (path
    :initarg :path
    :documentation "Path of the file that contains the note.")))

(defun make-note (note-path)
  (make-instance 'note
                 :path (merge-pathnames note-path *notes-directory*)))

(defun from-slug (note-slug)
  (make-note (note-path-from-slug note-slug)))

(defmethod initialize-instance :after ((n note) &key)
  (with-slots (title slug path) n
    (let ((title-prefix "#+title: "))
      (setf title
            (str:substring (length title-prefix)
                           t
                           (find title-prefix (uiop:read-file-lines (namestring path))
                                 :test #'str:starts-with-p)))
      (setf slug (note-path-to-slug path)))))

(defmethod note-to-html ((n note))
  "Returns a string with the HTML representation of the note."
  (with-slots (path) n
    (uiop:run-program (list "pandoc" "--to=html" "--shift-heading-level-by=1"
                            (namestring path))
                      :output :string)))

(defun get-all-notes ()
  (loop for path in (uiop:directory-files *notes-directory*)
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
