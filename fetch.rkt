#lang racket

(require html-parsing
         sxml/sxpath
         sxml
         net/url
         srfi/1)

(define thes-url
  "https://camellia-sinensis.com/fr/thes/")

(define the-blanc
  (string-append thes-url "the-blanc"))

(define eg.sexp "eg.sexp")

; /html/body/div[1]/main/section[3]/div[2]/div[2]/div/div/div[1]/article/div[2]/div[1]/h3/a
; looking for: <a class="m-product-title__link"> </a>
(define (fetch-from-http/file url target)
  (unless (file-exists? target)
    (display "downloading: ") (display url) (newline)
    (with-output-to-file target
      (lambda ()
        (write
         (html->xexp
          (get-pure-port (string->url url)
                         '()))))))
  (with-input-from-file target read))

(define thes-blancs-1
  (fetch-from-http/file "https://camellia-sinensis.com/fr/thes/the-blanc" "blanc-1.sexp"))

(define thes-verts-1
  (fetch-from-http/file "https://camellia-sinensis.com/fr/thes/the-vert" "vert-1.sexp"))

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

(define (download-tea type)
  (define url (string-append "https://camellia-sinensis.com/fr/thes/the-" type))
  (define out (string-append type "-1.sexp"))
  (define sxml (fetch-from-http/file url out))
  (define pages (tea-pages sxml))
  (for/list ((p pages) (j (iota (length pages) 2 1)))
    (fetch-from-http/file p (string-append type "-" (number->string j) ".sexp")))
  (void))

(download-tea "blanc")
