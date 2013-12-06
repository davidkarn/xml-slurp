(define (self x) x)
(define (o fn1 fn2)
  (lambda args
    (fn2 (apply fn2 args))))


(define (delay-function f . a)
  (lambda args
    (apply f (fill-arglist args a))))
(define (->number x)
  (if (number? x) 
      x
      (string->number x)))
(define (->integer i)
  (inexact->exact 
   (round (if (string? i) 
	      (string->number i)
	      i))))
(define (fill-arglist args a)
  (cond ((empty? args)
         a)
        ((empty? a)
         args)
        ((equal? #f (car a))
         (cons (car args) (fill-arglist (cdr args) (cdr a))))
        (#t
         (cons (car a) (fill-arglist args (cdr a))))))
(define (symbol-prefix prefix sym)
  (string->symbol (s+ (->string prefix) (symbol->string sym))))
(define (symbol-suffix sym suffix)
  (string->symbol (s+ (symbol->string sym) (->string suffix))))
(define !  make-property-condition)
(define !!  make-composite-condition)
(define (!? condition #!rest kinds)
  (empty?
   (filter not
	   (map (lambda (kind) 
		  ((condition-predicate kind) condition))
		kinds))))
(define (s+ . strings)
  (string-concatenate strings))
(define (!-> condition kind key #!optional default)
  (get-condition-property condition kind key (or default #f)))
(define (a-> alis key )
  (let ((cns (assoc key alis)))
    (if (and (list? cns) (empty? (cddr cns)))
	(second cns)
	(cdr cns))))
(define (memoize fn)
  (let ((table (make-hash-table equal?)))
    (lambda args
      (if (hash-table-exists? table args)
          (hash-table-ref table args)
          (begin
            (hash-table-set! table args (apply fn args))
            (hash-table-ref table args))))))

(define (tester x)
  (lambda (y)
    (equal? x y)))
(define (car-tester x)
  (lambda (y)
    (equal? x (car y))))
(define empty? (tester '()))
(define (cadaddr l)
  (cadadr (cdr l)))

(define (string-word str str2)
  (if (or (char-set-contains? char-set:letter+digit (string-ref str 0))
          (equal? (string-ref str 0) #\-)
          (equal? (string-ref str 0) #\.)
          (equal? (string-ref str 0) #\!)
          (equal? (string-ref str 0) #\:)
          (equal? (string-ref str 0) #\#))
      (string-word (substring/shared str 1) (string-concatenate (list str2 (substring/shared str 0 1))))
      (if (and (not (equal? str2 "")) (equal? (string-take-right str2 1) #\/))
          (cons (string-drop-right str2 1)
                (string-concatenate (list "/" str)))
          (cons str2 str))))

(define (char->string c)
  (make-string 1 c))

(define (string-word2 str str2)
  (if (and (not (char-set-contains? char-set:whitespace (string-ref str 0)))
	   (not (equal? (string-ref str 0) #\>)))
      (string-word2 (substring/shared str 1) (string-concatenate (list str2 (substring/shared str 0 1))))
      (if (and (not (equal? str2 "")) (equal? (string-take-right str2 1) "/"))
          (cons (string-drop-right str2 1)
                (string-concatenate (list "/" str)))
          (cons str2 str))))

(define *word2-charset* (char-set #\space #\tab #\newline #\>))

(define (stream-word2 str char)
  (let* ((index (stream-charset-index str *word2-charset*)))
    (if (not index)
        (cons "" str)
        (cons (stream->substring str 0 index) (substream-from str index)))))

(define (string-quote string quotec quote)
  (if (not (member quotec (list "'" "\"" " ")))
      (string-word2 (string-concatenate (list quotec string)) quotec)
      (if (string= (substring/shared string 0 1) "\\")
          (string-quote (substring/shared string 2) quotec (string-concatenate (list quote (substring/shared string 1 2))))
          (if (equal? (substring/shared string 0 1) quotec)
              (cons  quote (substring/shared string 1))
              (string-quote (substring/shared string 1) quotec (string-concatenate (list quote (substring/shared string 0 1))))))))

(define *word-charset* (char-set #\space #\tab #\newline #\/ #\> #\& #\=))

(define (string-after str char)
  (let ((index (string-index str char)))
    (if (not index)
        ""
        (substring/shared str (+ index 1)))))

(define (string-after-prefix str pre)
  (let ((index (string-contains str pre)))
    (if (not index)
        ""
        (substring/shared str (+ index (string-length pre))))))

(define (unnest list)
  (if (and (not (empty? list)) (empty? (cdr list)) (list? (car list))
           (all-lists (car list)))
      (unnest (car list))
      list))

(define (all-lists list)
  (if (empty? list)
      #t
      (if (list? (car list))
          (all-lists (cdr list))
          #f)))

(define (->string= a b)
  (string= (if (string? a) a (->string a))
	   (if (string? b) b (->string b))))


(define (nth i lis)
  (if (empty? lis)
      #f
      (if (= i 0)
	  (car lis)
	  (nth (- i 1) (cdr lis)))))

(define (not-empty? x)
  (not (empty? x)))

(define (after-string string in)
  (string-drop in (string-length string)))

(define (first-match list schema)
  (if (empty? list)
      '()
      (if (not (empty? (cdr (assoc (car list) schema))))
	  (cdr (assoc (car list) schema))
	  (first-match (cdr list) schema))))


(define (http-get2 url)
  (call-with-values (lambda () (process "curl" (list url "-L")))
    (lambda (in out id)
      (process-wait id)
      (read-all in))))

(define (escape-arg arg)
  (if (string= (string-take arg 1) "-")
      arg
      (s+ "'" (escape-quotes (string->list arg)) "'")))

(define (escape-quotes strlis)
  (if (empty? strlis) 
      ""
      (if (equal? (car strlis) #\')
	  (s+ "\'" (escape-quotes (cdr strlis)))
	  (s+ (->string (car strlis)) (escape-quotes (cdr strlis))))))

(define (arglist->cmdline argslist)
  (string-join (map escape-arg argslist) " "))

(define (alist-update alist . key-values)
  (if (empty? key-values) 
      alist
      (let ((key (car key-values))
	    (value (cadr key-values))
	    (rest (cddr key-values)))
	(apply alist-update 
	       (cons (alist-cons key value 
				 (alist-delete key alist))
		     rest)))))
(define (urlencode  s)
  (let ((s (string->list s)))
    (urlencode-lis s)))

(define (urlencode-char c)
  (if (char-set-contains? char-set:letter+digit c)
      (->string c)
      (s+ "%" (->string (char->integer c)))))

(define (urlencode-lis s)
  (if (empty? s)
      ""
      (s+ (urlencode-char (car s)) (urlencode-lis (cdr s)))))

(define (assoc->postdata a)
  (string-join 
   (map (lambda (r) (s+ (urlencode (car r)) "=" (urlencode (if (list? r) (cadr r) (cdr r)))))
	a)
   "&"))

(define (curl-post url data)
  (list url (assoc->postdata data)))

(define (timestamp)
  (->integer (current-seconds)))

(define (file-put-buffer fn string)
  (let ((f (file-open fn (+ open/wronly open/creat open/trunc))))
    (file-write f string)
    (file-close f)
    fn))