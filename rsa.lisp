(defun random-range (min max)
  (+ min (random (- max min))))

(defun is-prime (num)
  (defun iter (i)
    (cond ((= i num) T)
          ((zerop (mod num i)) NIL)
          (T (iter (+ i 1)))))
  (iter 2))

(defun gen-prime (min max)
  (labels ((iter ()
             (let ((num (random-range min max)))
               (if (is-prime num)
                   num
                   (iter)))))
    (iter)))

(defun gen-n ()
  (let* ((min 500)
         (max 1000)
         (p (gen-prime min max))
         (q (loop for q = (gen-prime min max)
                  when (/= q p) do (return q))))
    (values (* p q) (lcm (- p 1)  (- q 1)))))

(defun gen-n-test (amount)
  (loop repeat amount do
    (let ((n (gen-n)))
      (format t "n is: ~A~%" n))))

(defun choose-e (lambda-n)
  (loop for e = (+ 1 (random lambda-n))
        when (= (gcd lambda-n e) 1)
          do (return e)))

(defun mod-inverse (n m)
  (loop for d from 1
       until (= (mod (* n d) m) 1)
       finally (return d)))

(defun keygen ()
  (multiple-value-bind (n lambda-n) (gen-n)
      (let* ((e (choose-e lambda-n))
             (d (mod-inverse e lambda-n)))
        (values (list e n) (list d n)))))


(defun rsa-encrypt (message pubkey)
  (mod (expt message (car pubkey)) (cadr pubkey)))

(defun rsa-decrypt (message privkey)
  (mod (expt message (car privkey)) (cadr privkey)))

(defun rsa-test ()
  (multiple-value-bind (pubkey privkey) (keygen)
    (let* ((e (car pubkey))
          (n (cadr pubkey))
          (d (car privkey))
          (n2 (cadr privkey))
          (message (+ 1 (random 100)))
          (encrypted (rsa-encrypt message pubkey))
          (decrypted (rsa-decrypt encrypted privkey)))
      (format t "e is: ~A~%" e)
      (format t "n is: ~A~%" n)
      (format t "d is: ~A~%" d)
      (format t "n2 is: ~A~%" n2)
      (format t "message is: ~A~%" message)
      (format t "encrypted message is: ~A~%" encrypted)
      (format t "decrypted message is: ~A~%" decrypted))))


;; (gen-n-test 10)

;; (mod-inverse 5 13)
