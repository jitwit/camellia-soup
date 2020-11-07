#lang racket

(require html-parsing
         sxml/sxpath
         sxml
         net/url
         srfi/1)

(define thes-url
  "https://camellia-sinensis.com/fr/thes/")

(define (fetch url target)
  (unless (file-exists? target)
    (display "downloading: ") (display url) (newline)
    (with-output-to-file target
      (lambda ()
        (write
         (html->xexp
          (get-pure-port (string->url url) '()))))))
  (with-input-from-file target read))

(define tea-products
  (lambda (cs-sxml)
    ((sxpath '(// (a (@ (equal? (class "m-product-tile__link"))))
                  @ href *text*))
     cs-sxml)))

(define tea-pages
  (lambda (cs-sxml)
    ((sxpath '(// (ul (@ (equal? (class "paging__list"))))
                  li a @ href *text*))
     cs-sxml)))

(define (download-tea-type type)
  (define url (string-append "https://camellia-sinensis.com/fr/thes/the-" type))
  (define out (string-append "data/" type "-1.sexp"))
  (define sxml (fetch url out))
  (define pages (tea-pages sxml))
  (for/list ((p pages) (j (iota (length pages) 2 1)))
    (fetch p (string-append "data/" type "-" (number->string j) ".sexp")))
  (void))

(define (tea-type-products type)
  (download-tea-type type)
  (define pages
    (find-files (lambda (file)
                  (and (string-contains? (path->string file) type)
                       (string-suffix? (path->string file) ".sexp")))
                "data"))
  (apply append
         (for/list ((p pages))
           (tea-products
            (with-input-from-file p read)))))

(define (simple-tea? page)
  (not (or (string-contains? page "boite") ; maybe?
           (string-contains? page "sachets")
           (string-contains? page "portions")
           (string-contains? page "40g")
           (string-contains? page "65g")
           (string-contains? page "100g"))))

(define (page->tea-title page)
  (cadr (reverse (string-split page "/"))))

(define (download-given-tea page)
  (define title (page->tea-title page))
  (fetch page (string-append "data/tea/" title ".sexp"))
  (void))

(define (scrape tea)
  (display (string-append "downloading: " tea)) (newline)
  (time
   (for-each download-given-tea
             (filter simple-tea? (tea-type-products tea)))))

(define (allez)
  (for-each scrape '("blanc" "vert" "noir" "wulong" "pu-er-et-vieilli")))

(allez)
