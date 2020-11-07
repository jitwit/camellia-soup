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
        (pretty-write
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
  (fetch page (string-append "data/tea/" title ".sexp")))

(define (scrape tea)
  (display (string-append "scraping: " tea)) (newline)
  (time
   (for-each download-given-tea
             (filter simple-tea? (tea-type-products tea)))))

(define (allez-scraper)
  (for-each scrape
            '("blanc" "vert" "noir" "wulong" "pu-er-et-vieilli")))

(define (get-caff/anti sxml)
  (delete-duplicates
   ((sxpath '(// (span (@ (equal?
                           (class "product-attributes__meter-value"))))
                 *text*))
    sxml)))

(define (get-altitude sxml)
  (for/list ((dat ((sxpath '(// (li (@ (equal?
                                        (class "product-details__info altitude"))))
                                span
                                *text*))
                   sxml)))
    (let ((meters (cadr (string-split dat " "))))
      (string->number
       (substring meters 0 (- (string-length meters) 1))))))

(define (get-description sxml)
  (define class
    '(class "product-details__description-wrapper m-wysiwyg m-wysiwyg__regular"))
  (for/list ((dat ((sxpath `(// (div (@ (equal? ,class)))
                                p ;; nb. also (p span) for some
                                *text*))
                   sxml)))
    dat))

(define (get-flavor-wheel sxml)
  (for/list ((dat ((sxpath `(// (div (@ (equal?
                                         (class "product-attributes__aroma-wrapper"))))
                                div @ class *text*))
                   sxml)))
    (apply cons
           (map apply
                (list string->symbol string->number)
                (for/list ((x (cdr (string-split dat " "))))
                  (cdr (string-split x "-")))))))

(define (get-price sxml)
  ;; some teas have stringe prices
  (define prices
    (filter-map (lambda (node)
                  (and (string-suffix? node "$")
                       (not (string-prefix? node "échantillon"))
                       (string-contains? node "50g")
                       (let* ((xs (string-split node))
                              (wt (car xs))
                              (wt (string->number
                                   (substring wt 0 (- (string-length wt) 1))))
                              (dl (string->number
                                   (car (string-split (car (last-pair xs)) ",")))))
                         ;; dollars per gram i guess
                         (and wt dl (/ dl wt 1.)))))
                ((sxpath `(// option *text*))
                 sxml)))
  (car (append prices (list +inf.0))))

(define (parse-tea tea)
  (define the (string-append "data/tea/" tea ".sexp"))
  (define sxml (with-input-from-file the read))
  `((tea ,tea)
    (chemsitry ,(get-caff/anti sxml))
    (altitude ,(get-altitude sxml))
    (flavor-wheel ,(get-flavor-wheel sxml))
    (price ,(get-price sxml))))

(define (allez-parser)
  (for/list ((t (directory-list "data/tea")))
    (let ((t (car (string-split (path->string t) "."))))
      (parse-tea t))))

(define (tea-table)
  (define teas.sexp "data/teas.sexp")
  (unless (file-exists? teas.sexp)
    (with-output-to-file teas.sexp
      (compose pretty-write allez-parser)))
  (with-input-from-file teas.sexp read))

