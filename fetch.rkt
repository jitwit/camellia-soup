#lang racket

(require html-parsing
         sxml/sxpath
         sxml
         racket/fasl
         net/url)

(provide fetch)

(define (fetch url target)
  ;; should ensure directory exists, too
  (unless (file-exists? target)
    (display "downloading: ") (display url) (newline)
    (with-handlers ((exn:fail:network?
                     (lambda (fail)
                       (display (string-append "download failed: " url))
                       (newline))))
      (define response
        (get-pure-port (string->url url) '()))
      (with-output-to-file target
        (lambda ()
          (pretty-write (html->xexp response))))))
  (with-input-from-file target read))
