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

(define (allez)
  (for-each scrape
            '("blanc" "vert" "noir" "wulong" "pu-er-et-vieilli")))

(define (get-caff/anti sxml)
  (for/list ((dat (delete-duplicates
                   ((sxpath '(// (span (@ (equal?
                                           (class "product-attributes__meter-value"))))
                                 *text*))
                    sxml))))
    ;; caff first, unit is mg, ati second, unit is umol
    ;; (string->number (car (string-split dat " ")))
    dat))

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
                                p ;; span for some
                                *text*))
                   sxml)))
    dat))

;;  (span (@ (class "product-attributes__title")) "Roue des saveurs")
(define (get-flavor-wheel sxml)
  (for/list ((dat ((sxpath `(// (div (@ (equal?
                                         (class "product-attributes__aroma-wrapper"))))
                                div
                                @
                                class
                                *text*))
                   sxml)))
    (for/list ((x (cdr (string-split dat " "))))
      (cadr (string-split x "-")))))

(define (parse-tea tea)
  (define the (string-append "data/tea/" tea ".sexp"))
  (define sxml (with-input-from-file the read))
  `(,(get-caff/anti sxml)
    ,(get-altitude sxml)
    ,(get-flavor-wheel sxml)
    ;; ,(get-description sxml)
    ))

(parse-tea "jingning-yin-zhen")
(parse-tea "bai-hao")
(parse-tea "anxi-tie-guan-yin")

