
(define (stream-occupied? stream)
  (not (or
        (empty? stream)
        (and (equal? (car stream) "")
             (equal? (stream-peek stream) #!eof)))))

(define (after-regex regex str)
  (string-drop str (cadar (string-search-positions regex str))))

(define (second-and-after-regex regex str)
  (let ((r (string-search-positions regex str)))
    (list (substring/shared str (caadr r) (cadadr r))
          (string-drop str (cadar r)))))

(define (stream)
  (list "" (call-with-input-string "" self)))

(define (string->stream string)
  (list "" (call-with-input-string string self)))

(define (substream-from-port! stream)
  (list "" (second stream) ))

(define (stream-cons s stream)
  (set-car! stream (string-concatenate (list (char->string s) (car stream))))
  stream)

(define (stream-car stream)
  (if (equal? (car stream) "")
      (peek-char (second stream))
      (string-ref (car stream) 0)))

(define (stream-prefix str stream)
  (set-car! stream (string-concatenate (list str (car stream))))
  stream)

(define (stream-trim-port! stream)
  (do ((c (peek-char (second stream)) (peek-char (second stream))))
      ((or
        (equal? c #!eof)
        (not (char-set-contains? (char-set #\space #\newline #\return #\tab) c)))
       stream)
    (read-byte (second stream))))

(define (substream-from stream index)
  (if (< index (string-length (first stream)))
      (list (substring/shared (first stream) index)
            (second stream)
            )
      (begin
        (stream-read-string stream (- index (string-length (first stream))))
        (list "" (second stream)))))

(define (stream-read-string stream chars)
  (let ((string (read-string chars (second stream))))
    (set-car! stream (string-concatenate (list (car stream) string)))
    string))

(define (stream-peek stream)
  (peek-char (second stream)))

(define (stream-clean! stream)
  (set-cdr! (cdr stream) '())
  stream)

(define (stream-trim-all! stream)
  (set-car! stream (string-trim (car stream)))
  (if (equal? (car stream) "")
      (stream-trim-port! stream)
      stream))

(define (stream-ref stream i)
  (let ((l (string-length (car stream))))
    (if (>= i l)
        (let ((str (stream-read-string stream (+ 1 (- i l)))))
          (if (equal? str "")
              #f
              (string-ref str (+ 0 (- i l)))))
        (string-ref (car stream) i))))

(define (stream-charset-index stream test #!optional (index 0))
  (let ((c (stream-ref stream index)))
    (if (or (not c) (equal? c #!eof))
        #f
        (if (char-set-contains? test c)
            index
            (stream-charset-index stream test (+ index 1))))))

(define (stream-fn-index stream test #!optional (index 0))
  (let ((c (stream-ref stream index)))
    (if (or (not c) (equal? c #!eof))
        #f
        (if (test c)
            index
            (stream-fn-index stream test (+ index 1))))))

(define (stream-eq-index stream test #!optional (index 0))
  (let ((c (stream-ref stream index)))
    (if (or (not c) (equal? c #!eof))
        #f
        (if (equal? test c)
            index
            (stream-eq-index stream test (+ index 1))))))

(define (stream->substring stream a b)
  (if (> (string-length (car stream)) b)
      (substring/shared (car stream) a b)
      (string-concatenate (list (substring/shared (car stream) a)
                                (stream-read-string stream (- b (string-length (car stream))))))))

(define (stream-word! str str2)
  (let ((index (stream-charset-index str *word-charset*)))
    (if (not index)
        (cons "" str)
        (cons (substring/shared (car str) 0 index) (list (substring/shared (car str) index) (second str))))))


(define (stream-quote stream1)
  (let ((quotec (stream-car stream1))
        (stream (substream-from stream1 1)))
    (if (not (char-set-contains? (char-set #\' #\") quotec))
	(stream-word2 stream1 quotec)
	(let* ((index (stream-eq-index stream quotec)))
	  (do ((indices (list)))
	      ((or (equal? index 0)
		   (not (equal? (string-ref (car stream) (- index 1)) #\\)))
	       (let ((str (string-drop-right (car stream) 1))) ;(stream->substring stream 0 index)))
		 (do ((indices indices (cdr indices)))
		     ((empty? indices) (cons str (substream-from stream (+ 1 index))))
		   (set! str (string-concatenate (list (substring/shared str 0 (- (car indices) 1))
						       (substring/shared str (car indices))))))))
	    (set! indices (cons index indices))
	    (set! index (stream-eq-index stream quotec (+ index 1))))))))

(define (stream-after! stream char)
  (stream-fn-index stream (lambda (x)
			    (or  (equal? x char)
				 (equal? x #!eof))))
  (substream-from-port! stream))

(define (stream-after-prefix stream word)
  (cdr (do-stream-before-pattern stream (list word) (list word) 0 0)))

(define (do-stream-after-prefix stream word curword)
  (if (empty? curword)
      stream
      (if (not (stream-occupied? stream))
          stream
          (let ((c (stream-read! stream)))
            (if (equal? c (car curword))
                (do-stream-after-prefix (stream-drop 1 stream) word (cdr curword))
                (do-stream-after-prefix (stream-drop 1 stream) word word))))))

(define (stream-before-pattern pattern stream)
  (do-stream-before-pattern stream pattern pattern 0 0))

(define (do-stream-before-pattern stream pattern cpat index istart)
  (if (empty? cpat)
      (cons (stream->substring stream 0 istart)
            (substream-from stream index))
      (let ((c (stream-ref stream index))
            (m (car cpat)))
        (if (equal? c #!eof)
            (cons (stream->substring stream 0 index)
                  (stream))
            (cond ((equal? 'whitespace m)
                   (if (char-set-contains? (char-set #\space #\newline #\return #\tab) c)
                       (do-stream-before-pattern stream pattern cpat (+ index 1)
                                                 istart)
                       (do-stream-before-pattern stream pattern (cdr cpat) index
                                                 istart)))
                  ((string? m)
                   (if (equal? m "")
                       (do-stream-before-pattern stream pattern (cdr cpat) index istart)
                       (if (equal? (string-ref m 0) c)
                           (do-stream-before-pattern stream pattern (cons (substring/shared m 1) (cdr cpat)) (+ index 1) istart)
                           (do-stream-before-pattern stream pattern pattern (+ istart 1) (+ istart 1)))))
                  (#t
                   (if (equal? m c)
                       (do-stream-before-pattern stream pattern (cdr cpat) (+ index 1) istart)
                       (do-stream-before-pattern stream pattern pattern (+ istart 1) (+ istart 1)))))))))
