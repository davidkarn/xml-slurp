
(define (slurp:tag-matches? tag def)
  (if (xml-tag:tag? tag)
      (cond ((and (not (list? def)) (->string= def (xml-tag:tag tag)))
	     #t)
	    ((and (list? def) 
		  (->string= (car def) (xml-tag:tag tag))
		  (slurp:attr-matches tag (cadr def)))
	     #t)
	    (#t #f))
      #f))

(define (slurp:?take-all-or tag def)
  (cond ((not (and (list? (car def)) (equal? '?or (caar def))))
         (slurp:?do-take-all tag def))
        ((empty? (cdar def))
         "")
        (#t
         (let ((r (slurp:?do-take-all tag (cons (cadar def) (cdr def)))))
           (if (and r (not (empty? r)) (not (equal? "" r)))
               r
               (slurp:?take-all-or tag (list (cons '?or (cddar def))
                                             (cdr def))))))))

(define (slurp:?do-take-all tag def)
  (let ((r (list-of-tags (unnest (slurp:?take-all tag (car def)))))
        (rest (if (empty? (cdr def)) '() (cadr def))))
    (map (delay-function xml:slurp #f rest) r)))

					; take a list of tags, with more tag-lists mixed in, for exable: (tag tag (tag tag) tag ((tag (tag tag))) tag)
					; and converts into a flat list of tags
(define (list-of-tags tags-list)
  (if (not (list? tags-list))
      '()
      (reduce append '()
	      (filter list?
		      (map (lambda (tag) 
			     (if (list? tag)
				 (if (xml-tag:tag? tag)
				     (list tag)
				     (list-of-tags tag))
				 '()))
			   tags-list)))))

(define (slurp:?take-all tag def)
  (if (not (list? tag))
      #f
      (if (slurp:tag-matches? tag def)
	  tag
	  (begin
	    (filter (lambda (x) (and x (not (empty? x))))
		    (reduce (lambda (x y)
			      (cond ((and (list? x) (not (list? y)))
				     (cons y x))
				    ((and (list? y) (not (list? x)))
				     (cons x y))
				    ((and (list? y) (list? x))
				     (append x y))
				    ((and (not  (list? x)) (not (list? y)))
				     (list x y))))
			    (map (delay-function slurp:?take-all #f def) (xml-tag:children tag))
			    '()))))))

(define (slurp:attr-matches tag def)
  (equal? (length def)
          (length (filter (lambda (x)
                            (equal?
			     (cdr x)
			     (xml-tag:attr-matching tag (->string (car x)))))
                          def))))

(define (slurp:?text tag subtag)
  (let* ((subtag (if (list? subtag)
		     (if (empty? subtag) #f
			 (car subtag)))))
    (if subtag
	(if (not (list? tag)) #f
	    (xml-tag:innertext (slurp:get-first-tag tag subtag)))
	(xml-tag:innertext tag))))

(define (slurp:child-matching tag subtag-def)
  (let ((children (filter list? (xml-tag:children tag))))
    (if (or (not (list? tag)) (empty? children))
	#f
	(let ((matches (filter (delay-function slurp:tag-matches? #f subtag-def) 
			       children)))
	  (if (empty? matches)
	      (reduce (lambda (x y)
			(if x x (if y y #f)))
		      #f
		      (map (delay-function slurp:child-matching #f subtag-def)
			   children))
	      (car matches))))))

(define (slurp:get-first-tag tag subtag)
  (if (xml-tag:tag? tag)
      (let ((test (slurp:child-matching tag subtag)))
	(if (not-empty? test)
	    test
	    (if (> (length (xml-tag:children tag)) 0)
		(let ((test (filter self
				    (map (delay-function slurp:get-first-tag #f subtag) (xml-tag:children tag)))))
		  
		  (if (not-empty? test)
		      (car test)
		      #f))
		#f)))
      #f))

(define (slurp:?nth tag def)
  (if (xml-tag:tag? tag)
      (let ((test (xml-tag:children-matching tag (cadr def))))
	(if (not-empty? test)
	    (xml:slurp (nth (car def) test) (caddr def))
	    #f))))

(define (slurp:?date tag subtag)
  (let ((date (slurp:?text tag subtag)))
    date))

(define slurp:?attr xml-tag:attr-matching)

(define (slurp:?or tag def)
  (if (empty? def)
      ""
      (let ((r (xml:slurp tag (car def))))
        (if (and r (not (empty? r)) (not (equal? "" r)))
            r
            (slurp:?or tag (cdr def))))))

(define (slurp:?tag tag def)
  (if (xml-tag:tag? tag)
      (let ((subtag 
             (filter self 
                     (map (lambda (x)
                            (if (slurp:tag-matches? x (car def))
                                (if (and (> (length def) 1) 
                                         (or (equal? (caadr def) '?regex)
                                             (equal? (caadr def) '?take-all-regex)))
                                    (if (string-search (multiline (cadadr def))
                                                       (xml-tag:innertext x))
                                        x
                                        #f)
                                    x)
                                #f))
                          (xml-tag:children tag)))))
	(if (and subtag (not (empty? subtag)))
	    (if (> (length def) 1)
	        (xml:slurp (car subtag) (cadr def))
		#t)
	    (let ((subtag (filter self (map (delay-function slurp:?tag #f def)
					    (xml-tag:children tag)))))
	      (if (and subtag (not (empty? subtag)))
		  (car subtag)
		  #f))))
      #f))

(define (slurp:?regex tag regex taking)
  (let ((r (string-search regex (xml-tag:innertext tag))))
    (if r
	(last (take r (+ taking 1)))
	#f)))

(define (slurp:?take-all-regex tag regex)
  (string-split-fields (multiline regex)
                       (xml-tag:innertext tag)
                       #t))

(define (slurp:following-nested tag def)
  (let* ((tags (map (lambda (x)
		      (if (xml:slurp (list "fake" '() (list x)) def)
			  (cons #t x)
			  x))
		    (xml-tag:children tag)))
	 (thetag (find (lambda (x) 
                         (and (list? x) 
                              (equal? (car x) #t)))
		       tags)))
    (if thetag
	(let ((test (slurp:following-nested (cdr thetag) def)))
	  (if test test
	      (list-item-following (cdr thetag)
                                   (xml-tag:children tag))))
	#f)))

(define (list-item-following item list)
  (second (find-tail (lambda (x) (equal? x item)) list)))

(define (slurp:?following tag def)
  (let ((tag (slurp:following-nested tag (car def))))
    (if tag
	(xml:slurp tag (cadr def))
	#f)))

(define (xml:slurp tag def)
  (if (or (empty? tag) (empty? def)) '()
      (case (car def)
	((?tag) (slurp:?tag tag (cdr def)))
	((?or) (slurp:?or tag (cdr def)))
	((?take-all) (slurp:?take-all-or tag (cdr def)))
	((?text)
	 (xml:slurp-command slurp:?text tag (cdr def)))
	((?nth)
	 (slurp:?nth tag (cdr def)))
	((?date)
	 (build-date (parse-date (xml:slurp-command slurp:?date tag (cadr def)))))
	((?following)
	 (slurp:?following tag (cdr def)))
	((?regex)
	 (slurp:?regex tag (cadr def) (caddr def)))
	((?take-all-regex)
	 (slurp:?take-all-regex tag (cadr def)))
	((?attr)
	 (xml:slurp-command slurp:?attr tag (cadr def)))
	(else
	 (map (lambda (x)
		(if (list? x)
		    (xml:slurp tag x)
		    x))
	      def)))))

(define (xml:slurp-command f tag def)
  (cond ((not (list? def)) (f tag def))
        ((equal? def '(?or)) "")
        ((and (not (empty? def))(equal? (car def) '?or))
         (let ((test (f tag (cadr def))))
           (if (and test (not (empty? test)) (not (equal? "" test)))
               test
               (xml:slurp-command f tag (cons (car def) (cddr def))))))
        (#t (f tag def))))
