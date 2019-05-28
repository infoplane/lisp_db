(defvar *db* nil)

(defun make-eco (name company email telephone)
  (list :name name :company company :email email :telephone telephone))
  
(defun add-record (eco) (push eco *db*))

(defun dump-db ()
  (dolist( eco *db*)
    (format t "~{~a:~10t~a~%~}~%" eco)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-eco ()
  (make-eco
   (prompt-read "Name")
   (prompt-read "Company")
   (prompt-read "Email")
   (prompt-read "Telephone No")))

(defun add-ecologist ()
  (loop (add-record (prompt-for-eco))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

  
(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
	(print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select-by-name (name)
  (remove-if-not
   #'(lambda (eco) (equal (getf eco :name) name))
   *db*))


(defun select (selector-fn)
  (remove-if-not
   selector-fn
   *db*))

(defun where (&key name company email telephone)
  #'(lambda (eco)
      (and
       (if name (equal (getf eco :name) name) t)
       (if company (equal (getf eco :company) company) t)
       (if email (equal (getf eco :email) email) t)
       (if telephone (equal (getf eco :telephone) telephone) t))))
