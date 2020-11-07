#lang racket

(require html-parsing
         sxml/sxpath
         sxml
         net/url)

(define thes-url
  "https://camellia-sinensis.com/fr/thes/")

(define the-blanc
  (string-append thes-url "the-blanc"))

(define eg.sexp "eg.sexp")

; /html/body/div[1]/main/section[3]/div[2]/div[2]/div/div/div[1]/article/div[2]/div[1]/h3/a
; looking for: <a class="m-product-title__link"> </a>
(define (fetch-from-http/file url target)
  (unless (file-exists? target)
    (with-output-to-file target
      (lambda ()
        (write
         (html->xexp
          (get-pure-port (string->url url)
                         '()))))))
  (with-input-from-file target read))

(define thes-blancs-1
  (fetch-from-http/file "https://camellia-sinensis.com/fr/thes/the-blanc" "blanc.sexp"))

(define thes-verts-1
  (fetch-from-http/file "https://camellia-sinensis.com/fr/thes/the-vert" "vert.sexp"))

(define tea-products
  (lambda (cs-sxml)
    ;; teas seem to be listed by following class, and further '(@ href
    ;; *text*) gets the urls
    ((sxpath '(// (a (@ (equal? (class "m-product-tile__link"))))
                  @ href *text*))
     cs-sxml)))

(define tea-pages
  (lambda (cs-sxml)
    ((sxpath '(// (ul (@ (equal? (class "paging__list"))))
                  li a @ href *text*))
     cs-sxml)))

(tea-products thes-blancs-1)
(tea-pages thes-verts-1)
