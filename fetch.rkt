#lang racket

(require html-parsing
         sxml/sxpath
         sxml
         net/url
         )

(define thes-url
  "https://camellia-sinensis.com/fr/thes/")

(define the-blanc
  (string-append thes-url "the-blanc"))

(define eg.sexp "eg.sexp")



; /html/body/div[1]/main/section[3]/div[2]/div[2]/div/div/div[1]/article/div[2]/div[1]/h3/a
; looking for: <a class="m-product-title__link"> </a>
(define (blaha)
  (with-output-to-file eg.sexp
    (lambda ()
      (write
       (html->xexp
        (get-pure-port (string->url the-blanc)
                       '()))))))

(define thes-blancs
  (begin
    (unless (file-exists? eg.sexp)
      (blaha))
    (with-input-from-file "eg.sexp"
      read)))

(define tea-products
  (lambda (sxml)
    ((sxpath '(// (a (@ (equal? (class "m-product-tile__link"))))
                  @ href))
     sxml)))

(tea-products thes-blancs)
 
