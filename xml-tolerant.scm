; A library for parsing xml/html while being tolerant of common errors and malformed
; syntax. Parse-xml should return the most representative tree possible of any input, and 
; never throw an error.

(define *single-tags*
  '("br" "input" "img" "option" "script" "link" "meta" "!DOCTYPE"))

(define *verbatim-tags*
  '("script" "style" "pre"))

(define *xml-entities*
  '(("&quote;" . "\"")
    ("&apos;"  . "'")
    ("&amp;"   . "&")))

(define (fix-xml-entities str)
  (string-substitute* str *xml-entities*))

(define (new-xml-tag tag attrs children)
  (list tag attrs children))

(define (xml-tag:tag? x)
  (and (list?      x)
       (not-empty?  x)
       (equal?     (length x) 3)
       (string?    (first x))
       (list?      (second x))
       (list?      (third x))))

(define (list-nth lis nth . def)
  (if (and (list? lis) (>= (length lis) nth))
      (car (drop lis (- nth 1)))
      (if (empty? def) #f (car def))))

(define xml-tag:tag       (delay-function list-nth #f 1 ""))
(define xml-tag:attrs     (delay-function list-nth #f 2 '()))
(define xml-tag:children  (delay-function list-nth #f 3 '()))
(define xml-tag:parent    (delay-function list-nth #f 4 '()))

(define (xml-tag:attr-matching tag a)
  (if (string? tag) 
      tag
      (let ((r (xml-tag:attrs tag)))
        (let ((r2 (assoc (->string a) r)))
          (if (not r2) ""
              (cdr r2))))))

(define (xml-tag:child-matching tag a)
  (let ((r (xml-tag:children-matching tag a)))
    (if (empty? r) '() (car r))))

(define (xml-tag:children-matching tag a)
  (filter (lambda (x) 
            (and (list? x)
                 (not-empty? x)
                 (equal? (car x) a)))
          (xml-tag:children tag)))

(define (xml-text x)
  (if (string? x)
      x
      (xml-tag:to-text x)))

(define (xml-tag:to-text tag)
  (if (and (member (xml-tag:tag tag) *single-tags* string-ci=)
	   (empty? (xml-tag:children tag)))
      (string-append "<" (xml-tag:tag tag) 
		     (string-join (map (lambda (x)
					 (string-append (car x) "=\"" (cdr x) "\""))
                                       (xml-tag:attrs tag))
				  " " 'prefix)
		     " />")
      (string-append "<" (xml-tag:tag tag) 
		     (string-join (map (lambda (x)
					 (string-append (car x) "=\"" (cdr x) "\"")) 
                                       (xml-tag:attrs tag))
				  " " 'prefix)
		     ">"
		     (xml-tag:innertext tag)
		     "</" (xml-tag:tag tag) ">")))

(define (xml-tag:innertext tag)
  (if (and (list? tag) (not (empty? tag)))
      (string-join (map xml-text (xml-tag:children tag)) "")
      (if (string? tag) tag "")))

(define (multiline . x)
  (regexp
   (apply string-append x) #t #t))

(define multiline (memoize multiline))

(define (xml-cdata? str)
  (equal? (string-upcase (stream->substring str 0 9)) 
	  "[CDATA["))

(define (xml-comment? str)
  (equal?  (stream->substring str 0 4)
	   "<!--"))

(define (do-unquote str)
  (let ((str (if (member (string-take str 1) '("'" "\""))
                 (string-drop str 1)
                 str)))
    (if (member (string-take-right str 1) '("'" "\""))
        (string-drop-right str 1)
        str)))

(define (parse-xml-text str)
  (let ((index (stream-charset-index str (char-set #\<))))
    (if (not index)
        (cons (stream->string str) (stream))
        (cons (stream->substring str 0 index) (substream-from str index)))))

(define (stream->string str)
  (let ((i (stream-eq-index str #!eof)))
    (car str)))

(define (parse-xml-block tag attributes rest tags)
  (let* ((children (parse-xml-body rest (cons tag tags))))
    (cons (list tag attributes (car children))
	  (cdr children))))

(define (parse-all-attributes str attrs)
  (let ((word (stream-word! str "")))
    (if (string= (car word) "")
        (cons attrs str)
        (let ((word (car word))
              (rest (cdr word)))
          (if (equal? (stream-car rest) #\=)
              (let ((value (stream-quote (substream-from rest 1))))
                (parse-all-attributes (stream-trim-all! (cdr value)) 
                                      (cons (cons word (car value))
                                            attrs)))
              (parse-all-attributes (stream-trim-all! rest)
                                    (cons (cons word "true") 
                                          attrs)))))))

(define (pxa-helper a b)
  (cons (cons a (car b))
        (cdr b)))

(define (parse-xml-body stream tags)
  (if (not (stream-occupied? stream))
      '(())
      (begin
	(stream-clean! stream)

	(let ((stream (stream-trim-all! stream)))
	  (if (not (stream-occupied? stream))
	      '(())
	      (if (equal? (stream-car stream) #\<)
		  (let* ((stream (stream-trim-all! (substream-from stream 1)))
			 (first  (stream-car stream)))
		    (if (equal? first #\/)
			(let* ((tag  (stream-word! (stream-trim-all! 
                                                   (substream-from stream 1)) 
                                                  ""))
			       (rest (stream-after! (cdr tag) #\>))
			       (tag  (car tag)))
			  (cond ((and (not-empty? tags) (string-ci= tag (car tags)))
				 (cons '() rest))
				((member tag tags)
				 (cons '() (stream-prefix (string-concatenate 
                                                           (list "</" tag ">")) 
                                                          rest)))
				(#t (parse-xml-body rest tags))))
			(if (equal? first #\!)
			    (let* ((stream (stream-clean! (substream-from stream 1)))
				   (first (stream-car stream)))
			      (cond ((and (equal? first #\[)
					  (xml-cdata? stream))
				     (let* ((r (parse-xml-cdata stream))
					    (rest (parse-xml-body (cdr r) tags)))
				       (cons (cons (car r) (car rest)) (cdr rest))))
				    ((and (equal? first #\-)
					  (equal? (stream-ref stream 1) #\-))
				     (let ((rest (parse-xml-body 
                                                  (stream-after-prefix stream "-->")
                                                  tags)))
				       (cons (cons '("!--" () ())
						   (car rest))
					     (cdr rest))))
				    (#t
				     (let* ((tag (stream-word! stream ""))
					    (attributes (parse-xml-attributes
                                                         (stream-trim-all! (cdr tag))))
					    (tag (string-concatenate 
                                                  (list "!" (car tag))))
					    (rest (parse-xml-body 
                                                   (stream-after! (cdr attributes)
                                                                  #\>) 
                                                   tags))
					    (attributes (car attributes)))
				       (cons (cons (new-xml-tag tag attributes '()) 
                                                   (car rest)) 
                                             (cdr rest))))))
			    (let* ((tag          (stream-word! 
                                                  (substream-from-port! stream) ""))
				   (attributes   (parse-xml-attributes
                                                  (stream-trim-all! (cdr tag))))
				   (tag          (car tag))
				   (rest         (stream-trim-all! (cdr attributes)))
				   (attributes   (car attributes)))
			      (cond ((member tag *verbatim-tags* string-ci=)
				     (let* ((str2  (stream-after! rest #\>))
					    (r     (stream-before-pattern 
                                                    (list #\< 'whitespace #\/ 
                                                          'whitespace tag 
                                                          'whitespace #\>) 
                                                    str2))
					    (rest  (parse-xml-body (cdr r) tags)))
				       (cons (cons 
                                              (new-xml-tag tag attributes 
                                                           (list (car r))) 
                                              (car rest))
                                             (cdr rest))))
				    ((or (member tag *single-tags* string-ci=)
					 (equal? (stream-car rest) #\/))
				     (let ((rest (parse-xml-body 
                                                  (stream-after! rest #\>) tags)))
				       (cons (cons (new-xml-tag tag attributes '())
						   (car rest))
					     (cdr rest))))
				    (#t
				     (let* ((r (parse-xml-block tag attributes
                                                                (stream-after! rest #\>)
                                                                tags))
					    (rest (parse-xml-body (cdr r) tags)))
				       (cons (cons (car r) 
                                                   (car rest))
                                             (cdr rest)))))))))
		  (let* ((r    (parse-xml-text stream))
			 (rest (parse-xml-body (cdr r) tags)))
		    (cons (cons (car r)
                                (car rest)) 
                          (cdr rest)))))))))

(define (parse-xml stream)
  (new-xml-tag "xml-document"
               '() 
               (car (parse-xml-body stream '()))))

(define (file-parse path)
  (call-with-input-pipe (string-concatenate (list "cat \"" path "\""))
			(lambda (port)
			  (parse-xml (list "" port)))))

(define (http-parse url)
  (call-with-values (lambda () (process "curl" (list url "-L" "-k")))
    (lambda (in out id)
      (process-wait id)
      (parse-xml (list "" in)))))
