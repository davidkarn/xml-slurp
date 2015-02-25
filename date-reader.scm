; A library for reading dates. parse-date should be able to take a string representation of a date/
; time in almost any format and return a correctly parsed srfi-19 representation of that date,
; the format of the date need not be supplied, in most cases the code will automatically guess
; the correct format.

(define *date-symbols*
  '(("d" day-leading-zero         "([0-2][0-9]|3[0-1])")
    ("D" day-three-letters        "(Sun|Mon|Tue|Wed|Thu|Fri|Sat)")
    ("j" day                      "([1-9]|[1-2][0-9]|3[0-1])")
    ("l" day-full                 "(Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday)")
    ("N" iso-8601-day-of-week     "([1-7])")
    ("S" day-suffix               "(st|nd|rd|th)")
    ("w" day-of-week              "([0-6])")
    ("z" day-of-year              "([0-9]|[1-9][0-9]|[1-2][0-9][0-9]|3[0-5][0-9]|36[0-5])")
    ("F" month-full               "(January|February|March|April|June|July|August|September|October|November|December)")
    ("m" month-leading-zero       "(0[1-9]|1[0-2])")
    ("M" month-three-letters      "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)")
    ("n" month                    "([1-9]|1[0-2])")
    ("t" days-in-month            "(28|29|30|31)")
    ("L" leap-year?               "(1|0)")
    ("o" iso-8601-year            "([0-9][0-9][0-9][0-9])")
    ("Y" year                     "([0-9][0-9][0-9][0-9])")
    ("y" year-two-digit           "([0-9][0-9])")
    ("a" am-pm                    "(am|pm)")
    ("B" swatch-internet-time     "([0-9][0-9][0-9])")
    ("g" hour                     "([1-9]|1[0-2])")
    ("G" 24-hour                  "([0-9]|1[0-9]|2[0-3])")
    ("h" hour-leading-zero        "(0[1-9]|1[0-1])")
    ("H" 24-hour-leading-zero     "([0-1][0-9]|2[0-3])")
    ("i" minute-leading-zero      "([0-5][0-9])")
    ("s" second-leading-zero      "([0-5][0-9])")
    ("x" tenth-second             "([0-9])")
    ("T" timezone-abr             "(UTC|GMT|WET|CET|EET|MSK|IRT|SAMT|YEKT|TMT|TJT|OMST|NOVT|LKT|MMT|KRAT|ICT|WIT|WAST|IRKT|ULAT|CST|CIT|BNT|YAKT|JST|KST|EIT|ACST|VLAT|SAKT|GST|MAGT|IDLE|PETT|NZST|WAT|AT|EBT|NT|WBT|AST|EST|CST|MST|PST|YST|AHST|CAT|HST|NT|IDLW)")
    ("I" daylight-savings?        "(1|0)")
    ("O" gmt-difference           "(Z|[-+](?:[0-1][0-9]|2[0-3])[0-5][0-9])")
    ("P" gmt-difference-colon     "(Z|[-+](?:[0-1][0-9]|2[0-3]):[0-5][0-9])")
    ("Z" timezone-offset-seconds  "(-4[0-2][0-9][0-9][0-9]|-43[0-2][0-9][0-9]|-[0-3][0-9][0-9][0-9][0-9]|50[0-3][0-9][0-9]|50400|[0-4][0-9][0-9][0-9][0-9])")
    ("u" unix-time                "([0-9]{1,10})")
    ("U" unix-time-full           "^([0-9]{1,10})$")))

(define *date-formats*
  '((iso-8601-date-time-seconds-tenth-second-timezone (iso-8601-year "-" month-leading-zero "-" day-leading-zero "T" 24-hour-leading-zero ":" minute-leading-zero ":" second-leading-zero "." tenth-second gmt-difference-colon))
    (iso-8601-date-time-seconds-timezone (iso-8601-year "-" month-leading-zero "-" day-leading-zero "T" 24-hour-leading-zero ":" minute-leading-zero ":" second-leading-zero gmt-difference-colon))
    (iso-8601-date-time-timezone        (iso-8601-year "-" month-leading-zero "-" day-leading-zero "T" 24-hour-leading-zero ":" minute-leading-zero gmt-difference-colon))
    (iso-8601-date-time                 (iso-8601-year "-" month-leading-zero "-" day-leading-zero "T" 24-hour-leading-zero ":"  minute-leading-zero))
    (iso-8601-date                      (iso-8601-year "-" month-leading-zero "-" day-leading-zero))
    (gmt-difference                     (gmt-difference))
    (gmt-difference-colon               (gmt-difference-colon))
    (12-hour-minute-time                (hour ":" minute-leading-zero am-pm))
    (12-hour-minute-time-leading-zero   (hour-leading-zero ":" minute-leading-zero am-pm))
    (12-hour-time                       (hour am-pm))
    (12-hour-time-leading-zero          (hour-leading-zero am-pm))
    (iso-8601-time-seconds-tenth-second (24-hour-leading-zero ":" minute-leading-zero ":" second-leading-zero "." tenth-second))
    (iso-8601-time-seconds              (24-hour-leading-zero ":" minute-leading-zero ":" second-leading-zero))
    (iso-8601-time                      (24-hour-leading-zero ":" minute-leading-zero))
    (day                                (day-three-letters ", " day-leading-zero))
    (day2                               (day-three-letters ", " day))
    (month-year                         (month-three-letters " " year))
    (month-year                         (month-full " " year))
    (timezone-abr                       (" " timezone-abr))
    (unix-time                          (unix-time-full))))
(define *srfi-19-fields*
  `((nanosecond
     (tenth-second ,(lambda (x) (* x 100000))))
    (second
     (second-leading-zero ,string->number))
    (minute
     (minute-leading-zero ,string->number))
    (hour
     ((24-hour 24-hour-leading-zero)
      ,string->number)
     ((hour hour-leading-zero)
      ,string->number))
    (day
     ((day-leading-zero day)
      ,string->number))
    (month
     ((month-leading-zero month) ,string->number)
     ((month-full month-three-letters)
      ,(lambda (x)
	 (cdr (assoc x '(("January" . 1) ("Jan" . 1)
			 ("Feburary" . 2) ("Feb" . 2)
			 ("March" . 3) ("Mar" . 3)
			 ("April" . 4) ("Apr" . 4)
			 ("May" . 5) ("May" . 5)
                         ("June" . 6) ("Jun" . 6)
			 ("July" . 7) ("Jul" . 7)
			 ("August" . 8) ("Aug" . 8)
			 ("September" . 9) ("Sep" . 9)
			 ("October" . 10) ("Oct" . 10)
			 ("November" . 11) ("Nov" . 11)
			 ("December" . 12) ("Dec" . 12))
		     string-ci=)))))
    (year
     ((iso-8601-year year) ,string->number)
     (year-two-digit ,(lambda (x) (if (> x 20)
				      (+ x 1900)
				      (+ x 2000)))))
    (zone-offset 
     (timezone-offset-seconds ,string->number)
     ((gmt-difference gmt-difference-colon)
      ,(lambda (s)
	 (let ((n (+ (* 60 (string->number (string-take-right s 2)))
		     (* (* 60 60) (string->number (string-drop (string-take s 3) 1))))))
	   (if (equal? (string-take s 1) "+")
	       n
	       (- (* 24 60 60) n))))))
    (tz-name
     (timezone-abr ,self))
    (dst-flag
     (daylight-savings? ,(tester 1)))))

(define (date-part part schema)
  (if (empty? part) #f
      (let ((match 
	     (if (list? (caar part))
		 (first-match (caar part) schema)
		 (cdr (assoc (caar part) schema)))))
	(if (not (empty? match))
	    ((second (car part)) match)
	    (date-part (cdr part) schema)))))

(define (check-date-schema date-schema)
  "Update hour values to match am or pm"
  (if (and (not (empty? (cdr (assoc 'am-pm date-schema))))
           (string-ci= "pm" (cdr (assoc 'am-pm date-schema))))
      (if (not (empty? (cdr (assoc 'hour date-schema))))
          (cons (cons 'hour (number->string
                             (+ 12
                                (string->number (cdr (assoc 'hour date-schema))))))
                date-schema)
          (if (not (empty? (cdr (assoc 'hour-leading-zero date-schema))))
              (cons (cons 'hour-leading-zero (number->string
                                              (+ 12
                                                 (string->number (cdr (assoc 'hour-leading-zero date-schema))))))
                    date-schema)))
      date-schema))

(define (build-date date-schema)
  (set! date-schema (check-date-schema date-schema))
  (apply make-date
         (map 
	  (lambda (x)
	    (let ((name (car x))
		  (value (date-part (cdr x) date-schema)))
	      (if value value
		  (case name
		    ((nanosecond second minute hour) 0)
		    ((day)              (date-day (current-date)))
		    ((month)            (date-month (current-date)))
		    ((year)             (date-year (current-date)))
		    ((zone-offset)      (date-zone-offset (current-date)))
		    ((tz-name dst-flag)  #f)))))
	  *srfi-19-fields*)))


(define (parse-date date . rest)
  (let ((slots (if (>= (length rest) 2) 
                   (second rest) 
                   (map (lambda (x)
                          (list (second x))) 
                        *date-symbols*)))
        (formats (if (not (empty? rest)) 
                     (car rest)
                     (map second *date-formats*))))
    (if (empty? formats)
        slots
        (let ((r (parse-date-format date 
                                    (car formats) 
                                    slots 
                                    #t)))
          (if r (set! slots r))
          (parse-date date
                      (cdr formats)
                      slots 
                      #t)))))

(define (parse-date-format date format slots first)
  (if first
      (let ((r (string-search (regexp (build-format-regex format) '(caseless)) date)))
        (if r
            (set! date (car r))
            (set! date ""))))
  (cond ((empty? format) slots)
        ((string= "" date) #f)
        ((string? (car format))
         (if (string-prefix-ci? (car format) date)
             (parse-date-format (after-string (car format) 
                                              date) 
                                (cdr format) 
                                slots 
                                #f)
             #f))
        (#t
         (if (not (empty? (cdr (assoc (car format) slots))))
             #f
             (let* ((r (string-search (multiline "^" (lookup-date-regex (car format))) 
                                      date)))
               (if (not r)
                   #f
                   (parse-date-format (after-regex
                                       (multiline "^"
                                                  (lookup-date-regex (car format))) 
                                       date)
                                      (cdr format)
                                      (cons (cons (car format)
                                                  (second r)) 
                                            slots)
                                      #f)))))))
(define (build-format-regex lis)
  (apply string-append (map (lambda (x)
                              (if (string? x)
                                  (regexp-escape x)
                                  (lookup-date-regex x)))
			    lis)))

(define (lookup-date-regex x) 
  (second (assoc x (map cdr *date-symbols*))))
