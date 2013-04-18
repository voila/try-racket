#lang racket
(require  web-server/servlet
          web-server/servlet-env
          web-server/templates
          web-server/dispatch
          web-server/http
          racket/sandbox
          (planet dherman/json:4:=0)
          racket/runtime-path)

(define APPLICATION/JSON-MIME-TYPE #"application/json;charset=utf-8")

(module+ test (require rackunit))

;;------------------------------------------------------------------
;; sandbox
;;------------------------------------------------------------------
;; make-ev : -> evaluator
(define (make-ev)
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-propagate-exceptions #f]
                   [sandbox-eval-limits (list 10 50)]
                   [sandbox-path-permissions '([exists "/"])])
      (call-with-limits #f #f
                        (lambda () (make-evaluator 'racket/init)))))

;; run-code : evaluator string -> eval-result
(define (run-code ev str)
  (define res (ev str))
  (define out (get-output ev))
  (define err (get-error-output ev))
  ;(kill-evaluator ev) TODO are they garbage collected ?
  (list (if (void? res) "" (format "~v" res))
        (and (not (equal? out "")) out)
        (and (not (equal? err "")) err)))



;;------------------------------------------------------------------
;; Routes
;;------------------------------------------------------------------
(define-values (dispatch urls)
    (dispatch-rules
     [("") home]
     [("home") home]
     [("links") links]
     [("about") about]
     [("tutorial") #:method "post" tutorial]))

;;------------------------------------------------------------------
;; Responses
;;------------------------------------------------------------------
;; make-response : ... string -> response
(define (make-response #:code [code 200]
                       #:message [message #"OK"]
                       #:seconds [seconds (current-seconds)]
                       #:mime-type [mime-type TEXT/HTML-MIME-TYPE] 
                       #:headers [headers (list (make-header #"Cache-Control" #"no-cache"))]
                       content)
  (response/full code message seconds mime-type headers 
                 (list (string->bytes/utf-8 content))))
          
;;------------------------------------------------------------------
;; Request Handlers
;;------------------------------------------------------------------
;; Tutorial pages
(define (tutorial request)
  (define page (dict-ref (request-bindings request) 'page #f))
  (make-response
   (match page
     ("page1" (include-template "templates/tutorial/page1.html"))
     ("page2" (include-template "templates/tutorial/page2.html"))
     ("page3" (include-template "templates/tutorial/page3.html"))
     ("page4" (include-template "templates/tutorial/page4.html"))
     ("page5" (include-template "templates/tutorial/page5.html"))
     ("page6" (include-template "templates/tutorial/page6.html"))
     ("page7" (include-template "templates/tutorial/page7.html"))
     ("page8" (include-template "templates/tutorial/page8.html"))
     ("page9" (include-template "templates/tutorial/page9.html"))
     ("page10" (include-template "templates/tutorial/page10.html"))
     ("page11" (include-template "templates/tutorial/page11.html"))
     ("end" (include-template "templates/tutorial/end.html")))))

;; Links page
(define (links request)
    (make-response
     (include-template "templates/links.html"))) 

;; About page
(define (about request)
    (make-response
     (include-template "templates/about.html"))) 

;; Home page
(define (home request)
    (home-with (make-ev) request))
  
(define (home-with ev request) 
  (local [(define (response-generator embed/url)
            (let ([url (embed/url next-eval)])
              (make-response
               (include-template "templates/home.html"))))
            (define (next-eval request)
              (eval-with ev request))]
    (parameterize
    ([current-servlet-continuation-expiration-handler
      (lambda (req)
        (make-response 
         #:mime-type APPLICATION/JSON-MIME-TYPE
         (jsexpr->json 
          (json-error "" "Your session has expired. Please reload the page."))))])
      (send/suspend/dispatch response-generator))))

;; Eval 
(define (json-error expr msg)
  (hasheq "expr" expr "error" true "message" msg))

(define (json-result expr res)
  (hasheq "expr" expr "result" res))

;; string eval-result -> jsexpr
(define (result-json expr lst)
   (match lst
     ((list res #f #f) 
      (json-result expr res))
     ((list res out #f) 
      (json-result expr (string-append out res)))
     ((list res out err)
      (define err1 (car (regexp-split "\n+" err))) ;; TODO how to preserve the complete error message ?
      (json-error expr err1))))


(module+ test
  (define ev (make-ev))
  (define (eval-to-json expr)
    (jsexpr->json 
    (hash-ref (result-json "" (run-code ev expr)) "result")))
  
  (check-equal? 
   (eval-to-json "(+ 3 3)") "\"6\"")
  (check-equal? 
   (eval-to-json "(display \"6\")") "\"6\"")
  (check-equal? 
   (eval-to-json "(write \"6\")") "\"\\\"6\\\"\"")
  (check-equal? 
   (eval-to-json "(begin (display \"6 + \") \"6\")") "\"6 + \\\"6\\\"\"")
)  

(define (eval-with ev request) 
  (define bindings (request-bindings request))
  (if (exists-binding? 'expr bindings)
      (let ([expr (extract-binding/single 'expr bindings)])
        (make-response 
         #:mime-type APPLICATION/JSON-MIME-TYPE
         (jsexpr->json (result-json expr (run-code ev expr)))))
      (make-response #:code 400 #:message #"Bad Request" "")))
      

;;------------------------------------------------------------------
;; Server
;;------------------------------------------------------------------

(define-runtime-path static "./static")

(serve/servlet
 dispatch
 #:stateless? #f       
 #:launch-browser? #f
 #:connection-close? #t
 #:quit? #f 
 #:listen-ip #f 
 #:port 8000
 #:servlet-regexp #rx""
 #:extra-files-paths (list static)
 #:servlet-path "/")
