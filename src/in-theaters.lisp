(in-package :com.thiagosp.in-theaters)

(defparameter *config*
  (with-open-file (s (asdf:system-relative-pathname "thiagosp" "config.json")
                     :direction :input
                     :if-does-not-exist :error)
    (jzon:parse s)))

(defparameter *cinesystem-cine-code*
  (@ *config* "in-theaters" "cinesystem" "cine-code"))
(defparameter *mdblist-api-key*
  (@ *config* "in-theaters" "mdblist" "api-key"))
(defparameter *tmdb-access-token*
  (@ *config* "in-theaters" "tmdb" "access-token"))

;;; Notes:
;;;
;;; - We use TMDB instead of mdblist to get IMDb IDs from the movie
;;;   titles. Reason: "tituloOriginal" is sometimes in language other
;;;   than English (e.g. "C'è ancora domani"), but mdblist's title
;;;   search only works well with English titles (in this case,
;;;   "There's Still Tomorrow".) Even with English titles its results
;;;   are terrible, the ordering is seemingly random so that the
;;;   closest match is often in the middle.
;;;
;;;   TMDB, on the other hand, works well with the original title and
;;;   has good ordering (we can just pick the first result since it's
;;;   very likely the correct movie).
;;;
;;;   Note: keep using mdblist for movie details since it has RT
;;;   ratings, posters, and so on. TMDB doesn't have the 3rd party
;;;   ratings, and OMDB gatekeeps the posters, etc. for Patrons only.

(define-condition movie-not-found-error (error)
  ((text :initarg :text :reader text)))

(defun get-movies ()
  "Return list of movies."
  (let* ((cinesystem-movies (cinesystem/get-movies :cine *cinesystem-cine-code*))
         (movie-titles
           (remove-duplicates
            (loop for entry across cinesystem-movies
                  collect (string-trim " " (@ entry "filme" "tituloOriginal")))
            :test #'string=))
         (tmdb-ids (loop for title in movie-titles
                         for tmdb-id = (handler-case (tmdb/get-tmdb-id title)
                                         (movie-not-found-error (c)
                                           (format *error-output*
                                                   "Getting movie ID failed with error: `~A'. Skipping it.~%"
                                                   (text c))
                                           nil))
                         when tmdb-id collect it))
         (movies (loop for id in tmdb-ids
                       collect (mdblist/get-movie-by-tmdb-id id))))
    movies))

(defun get-rating (movie rating-source)
  (let ((rating (find rating-source (@ movie "ratings")
                      :key (lambda (el) (@ el "source"))
                      :test #'string=)))
    (@ rating "score")))

(defun get-sorted-movies (&key (rating-source "tomatoes"))
  "Return list of movies, sorted by RATING-SOURCE score, highest first.

If RATING-SOURCE is not specified, sort it by Tomatometer score."
  (sort (get-movies) #'>
        :key (lambda (movie)
               (let ((rating (get-rating movie rating-source)))
                 (if (eq rating 'null) 0 rating)))))

(defun cinesystem/get-movies (&key cine)
  "Get list of movies showing today from Cinesystem's API."
  (let* ((url (format nil "https://www.cinesystem.com.br/site-api/programacao/?cine=~A&data=~A"
                      cine
                      (local-time:format-timestring
                       nil (local-time:now) :format '(:year (:month 2) (:day 2)))))
         (response (dex:get url)))
    (jzon:parse response)))

(defun mdblist/get-imdb-id (movie-title)
  "Returns the IMDb ID of the movie with title MOVIE-TITLE."
  (let* ((mdblist-search-response-parsed
           (jzon:parse (dex:get (format nil "https://mdblist.com/api/?apikey=~A&s=~A"
                                        *mdblist-api-key*
                                        (quri:url-encode movie-title)))))
         (result (find movie-title (@ mdblist-search-response-parsed "search")
                       :key (lambda (entry) (@ entry "title"))
                       :test #'string=)))
    (if result
        (@ result "imdbid")
        (error 'movie-not-found-error
               :text (format nil "Movie with title `~A' not found." movie-title)))))

(defun tmdb/get-tmdb-id (movie-title)
  "Returns the TMDB ID of the movie with title MOVIE-TITLE."
  (let* ((response-parsed
           (jzon:parse
            (dex:get (format nil "https://api.themoviedb.org/3/search/movie?query=~A"
                             (quri:url-encode movie-title))
                     :headers `(("Authorization"
                                 . ,(format nil "Bearer ~A" *tmdb-access-token*))))))
         (results (@ response-parsed "results"))
         (result (if (> (length results) 0) (elt results 0) nil)))
    (if result
        (@ result "id")
        (error 'movie-not-found-error
               :text (format nil "Movie with title `~A' not found." movie-title)))))

(defun mdblist/get-movie-by-imdb-id (imdb-id)
  (jzon:parse (dex:get (format nil "https://mdblist.com/api/?apikey=~A&i=~A"
                               *mdblist-api-key*
                               imdb-id))))

(defun mdblist/get-movie-by-tmdb-id (tmdb-id)
  (jzon:parse (dex:get (format nil "https://mdblist.com/api/?apikey=~A&tm=~A"
                               *mdblist-api-key*
                               tmdb-id))))

(defun pprint-element (element)
  "Pretty prints ELEMENT as JSON to the standard output."
  (princ (jzon:stringify element :pretty t)))
