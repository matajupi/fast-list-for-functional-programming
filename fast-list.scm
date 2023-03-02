; 副作用を許容する通常の可変長リスト
; ランダムアクセスO(1)
(define (make-vlist n vec)
    (define (vref i)
        (vector-ref vec i))

    (define (vset! i x)
        (vector-set! vec i x))

    (define (vinsert! i x)
        (expand)
        (let iter((j (- n 1)))
            (if (>= (- j 1) i)
                (begin
                    (vset! j (vref (- j 1)))
                    (iter (- j 1)))))
        (vset! i x))

    (define (vremove! i)
        (let iter((j i))
            (if (< (+ j 1) n)
                (begin
                    (vset! j (vref (+ j 1)))
                    (iter (+ j 1)))))
        (shrink))

    (define (vcopy)
        (make-vlist n (vector-copy vec)))

    (define (expand)
        (if (> (+ n 1) (vector-length vec))
            (let ((new-vec (make-vector (* n 2))))
                (vector-copy! new-vec 0 vec)
                (set! vec new-vec)))
        (set! n (+ n 1)))

    (define (shrink)
        (set! n (- n 1)))

    (define (dispatch m)
        (cond ((eq? m 'length) n)
              ((eq? m 'ref) vref)
              ((eq? m 'set!) vset!)
              ((eq? m 'insert!) vinsert!)
              ((eq? m 'remove!) vremove!)
              ((eq? m 'copy) (vcopy))
              (else (error "Unknown request: VLIST" m))))

    dispatch)

(define (vlist) (make-vlist 0 (make-vector 1)))
(define (vlist-length vs) (vs 'length))
(define (vlist-ref vs i) ((vs 'ref) i))
(define (vlist-set! vs i x) ((vs 'set!) i x))
(define (vlist-insert! vs i x) ((vs 'insert!) i x))
(define (vlist-remove! vs i) ((vs 'remove!) i))
(define (vlist-copy vs) (vs 'copy))


; Update履歴
(define (make-hU idx old new)
    (define (call! vs)
        (vlist-set! vs idx new)
        vs)

    (define (dispatch m)
        (cond ((eq? m 'type) 'U)
              ((eq? m 'rev) (make-hU idx new old))
              ((eq? m 'call!) call!)
              ((eq? m 'idx) idx)
              ((eq? m 'old) old)
              ((eq? m 'new) new)
              (else (error "Unknown request: HU" m))))
    dispatch)


; Remove履歴
(define (make-hR idx old)
    (define (call! vs)
        (vlist-remove! vs idx)
        vs)

    (define (dispatch m)
        (cond ((eq? m 'type) 'R)
              ((eq? m 'rev) (make-hI idx old))
              ((eq? m 'call!) call!)
              ((eq? m 'idx) idx)
              ((eq? m 'old) old)
              (else (error "Unknown request: HR" m))))
    dispatch)


; Insert履歴
(define (make-hI idx new)
    (define (call! vs)
        (vlist-insert! vs idx new)
        vs)

    (define (dispatch m)
        (cond ((eq? m 'type) 'I)
              ((eq? m 'rev) (make-hR idx new))
              ((eq? m 'call!) call!)
              ((eq? m 'idx) idx)
              ((eq? m 'old) old)
              (else (error "Unknown request: HI" m))))
    dispatch)


(define (hist-type h) (h 'type))
(define (hist-rev h) (h 'rev))
(define (hist-call! h vs) ((h 'call!) vs))
(define (hist-uncall! h vs) (((hist-rev h) 'call!) vs))
(define (hist-idx h) (h 'idx))
(define (hist-old h) (h 'old))
(define (hist-new h) (h 'new))


; 履歴リスト
(define (make-hlist n hists)

    (define (hcons! h)
        (set! hists (cons h hists))
        (set! n (+ n 1))
        dispatch)

    (define (dispatch m)
        (cond ((eq? m 'length) n)
              ((eq? m 'cons!) hcons!)
              ((eq? m 'car) (car hists))
              ((eq? m 'cdr) (make-hlist (- n 1) (cdr hists)))
              (else (error "Unknown request: HLIST" m))))

    dispatch)

(define (hlist) (make-hlist 0 '()))
(define (hlist-length hs) (hs 'length))
(define (hlist-cons! h hs) ((hs 'cons!) h))
(define (hlist-car hs) (hs 'car))
(define (hlist-cdr hs) (hs 'cdr))


; 参照透過性を保つ高速リスト
(define (make-rlist ver vs hs)

    (define (rlength)
        (vlist-length (car (restore))))

    (define (rref i)
        (vlist-ref (car (restore)) i))

    (define (rupdate! i x)
        (let* ((p (restore))
               (vs (car p))
               (hs (cdr p))
               (h (make-hU i (rref i) x)))
            (make-rlist (+ ver 1) (hist-call! h vs) (hlist-cons! h hs))))

    (define (rinsert! i x)
        (let* ((p (restore))
               (vs (car p))
               (hs (cdr p))
               (h (make-hI i x)))
            (make-rlist (+ ver 1) (hist-call! h vs) (hlist-cons! h hs))))

    (define (rremove! i)
        (let* ((p (restore))
               (vs (car p))
               (hs (cdr p))
               (h (make-hR i (rref i))))
            (make-rlist (+ ver 1) (hist-call! h vs) (hlist-cons! h hs))))

    (define (restore)
        (if (= ver (hlist-length hs))
            (cons vs hs)
            (restore-helper (vlist-copy vs) hs)))

    (define (restore-helper vs hs)
        (if (= ver (hlist-length hs))
            (cons vs hs)
            (restore-helper (hist-uncall! (hlist-car hs) vs) (hlist-cdr hs))))

    (define (dispatch m)
        (cond ((eq? m 'length) (rlength))
              ((eq? m 'ref) rref)
              ((eq? m 'update) rupdate!)
              ((eq? m 'insert) rinsert!)
              ((eq? m 'remove) rremove!)
              (else (error "Unknown request: RLIST" m))))

    dispatch)

(define (rlist) (make-rlist 0 (vlist) (hlist)))
(define (rlist-length rs) (rs 'length))
(define (rlist-ref rs i) ((rs 'ref) i))
(define (rlist-update rs i x) ((rs 'update) i x))
(define (rlist-insert rs i x) ((rs 'insert) i x))
(define (rlist-remove rs i) ((rs 'remove) i))

(define (rlist-append rs x)
    (rlist-insert rs (rlist-length rs) x))

(define (rlist-range beg end)
    (let iter((rs (rlist)))
        (if (< (rlist-length rs) (- end beg))
            (iter (rlist-append rs (+ beg (rlist-length rs))))
            rs)))

