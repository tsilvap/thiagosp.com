(in-package :com.thiagosp.pages)

(defparameter *email* "thiagodasilva@protonmail.com")

(defmacro with-page ((&key title footer) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:link :href "static/css/main.css" :rel "stylesheet")
       (:title ,title))
      (:body
       ,@body
       (:footer.footer.footer-center.bg-base-300.text-base-content.p-4.mt-16
        (:aside
         (let ((spinneret:*html-style* :tree)
               (spinneret:*suppress-inserted-spaces* t)
               (*print-pretty* nil))
           ,(when footer
             footer)
           (:p "The source code for this website is on "
               (:a.link :href "https://github.com/tsilvap/thiagosp.com"
                        "GitHub")
               ".")
           (:p "Made with " (:span.inline-flex.items-baseline
                             (:a.link :href "https://lisp-lang.org/" "secret alien technology")
                             "."
                             (:img.h-5.ml-1 :src "/static/img/lisplogo_alien_128.png"
                                            :alt "Lisp alien logo by Conrad Barski"))))))))))

(defun home-page ()
  (with-page (:title "Thiago S. Pinto")
    (:nav.grid.grid-flow-col.max-w-sm.mx-auto.text-center.my-8
     (:a.link.link-hover :href "https://github.com/tsilvap" "github")
     (:a.link.link-hover :href "https://www.linkedin.com/in/thidasilva/" "linkedin")
     (:a.link.link-hover :href "/cv" "cv"))

    (:div.hero.py-8
     (:div.hero-content
      (:div
       (:h1.text-5xl.font-bold.mb-6 "Hi, I'm Thiago S. Pinto")
       (:div.prose.mb-6
        (:p "I'm a software engineer with extensive professional experience, specializing in backend development.")
        (:p "My specialties are: Go, Python, SQL, Kafka, Bash scripting, Unix and Linux, CI/CD, AWS, test-driven development, and more."))
       (:div.alert.max-w-xl
        :role "alert"
        (:div
         (:h3.font-bold.mb-2 "Open to work!")
         (:p
          "I'm currently looking for a new job. If you're interested in what you see, send me an "
          (:a.link
           :href (format nil "mailto:~a" *email*) "email")
          " and we can work out the details."))))))

    (:div.max-w-2xl.mx-auto
     (:h2.text-2xl.font-bold.mb-6 "Micro projects")
     (:ul.grid.gap-6
      (:li
       (:a :href "/in-theaters"
           (:div.card.bg-base-200.shadow-xl
            :class "hover:bg-base-300"
            (:div.card-body
             (:h3.card-title "In Theaters")
             (:p "Shows the list of movies currently showing in various Brazilian theaters, along with Rotten Tomatoes, Metacritic, IMDb scores, and more.")))))
      (:li
       (:a :href "/hermes"
           (:div.card.bg-base-200.shadow-xl
            :class "hover:bg-base-300"
            (:div.card-body
             (:h3.card-title "Hermes")
             (:p "Hermes is a text and file hosting website that just works.")))))))))

(defun formatted-duration (total-minutes)
  "Returns a formatted string of hours and minutes corresponding to TOTAL-MINUTES."
  (let* ((hours (floor total-minutes 60))
         (remaining-minutes (floor (mod total-minutes 60))))
    (format nil "~Ah ~Amin" hours remaining-minutes)))

(defun formatted-rating (movie rating-source)
  (let ((rating (get-rating movie rating-source)))
    (if (eq rating 'null) "N/A" rating)))

(defun in-theaters-page (sorted-movies)
  (with-page
      (:title "In Theaters"
       :footer
       (:p "Powered by " (:a.link :href "https://www.themoviedb.org/" "TMDB")
           " and " (:a.link :href "https://mdblist.com/" "mdblist") "."))
    (:div.container.p-4
     (:header.prose.mb-5
      (:h1 "In Theaters"))
     (:main
      (:div.grid.grid-cols-2.gap-4
       (loop for movie in sorted-movies do
         (:div.card.card-compact.card-side.bg-base-200.shadow-xl
          (:figure.w-2/6
           (:img :src (@ movie "poster")
                 :alt (format nil "Poster for '~A'" (@ movie "title"))))
          (:div.card-body.prose
           (:h2.card-title (format nil "~A (~A)" (@ movie "title") (@ movie "year"))
                           (:div.badge.badge-neutral
                            (formatted-duration (@ movie "runtime"))))
           (:p (@ movie "description"))
           (:ul (:li (:a.link.link-primary
                      :href (@ movie "trailer") :target "_blank"
                      "Trailer (YouTube)"))
                (:li (format nil "Tomatoes: ~A"
                             (formatted-rating movie "tomatoes")))
                (:li (format nil "Tomatoes (audience): ~A"
                             (formatted-rating movie "tomatoesaudience")))
                (:li (format nil "Metacritic: ~A"
                             (formatted-rating movie "metacritic")))
                (:li (format nil "IMDb: ~A"
                             (formatted-rating movie "imdb"))))))))))))
