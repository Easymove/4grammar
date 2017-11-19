(in-package #:4grammar)

(defparameter *server* nil)
(defparameter *port* 7777)

(defclass 4grammar-server (easy-acceptor)
  ())

(defun run-server ()
  (setf *server* (make-instance '4grammar-server
                                :name "4grammar-server"
                                :port *port*))
  (setf *storage* (default-storage))
  (open-storage)
  (start *server*))

(defun stop-server ()
  (stop *server*))

(defclass query () ())

(defclass response () ())

(define-easy-handler (easy-demo :uri "/run" :default-request-type :post)
    ((json :parameter-type 'string))
  (encode (execute (decode json))))

(defun encode (object)
  (with-output-to-string (stream)
    (encode-m object stream)))

(defun decode (string)
  (decode-m (yason:parse string)))

(defgeneric execute (query)
  (:documentation "The main function that executes the given query."))

(defgeneric decode-m (object))

(defgeneric encode-m (object stream))

(defparameter +default-class-keys+ (list "class" "Class"))

(defmethod decode-m ((obj hash-table))
  (let ((class :hash-table)
        (args))
    (maphash (lambda (k val)
               (if (member k +default-class-keys+ :test #'string=)
                   (setf class (intern (string-upcase val) :4grammar))
                   (push (cons (make-keyword (string-upcase k)) (decode-m val)) args)))
             obj)
    (case class
      (:hash-table (alist-hash-table args :test #'equal))
      (otherwise (apply #'make-instance class (alist-plist args))))))

(defmethod decode-m ((obj t))
  obj)

(defmethod encode-m ((obj number) stream)
  (yason:encode obj stream))

(defmethod encode-m ((obj string) stream)
  (yason:encode obj stream))

(defmethod encode-m ((obj symbol) stream)
  (yason:encode (string-downcase (symbol-name obj)) stream))

(defmethod encode-m ((obj hash-table) stream)
  (let ((first? t))
    (format stream "{")
    (maphash (lambda (key val)
               (unless first?
                 (format stream ","))
               (setf first? nil)
               (encode-m key stream)
               (format stream ":")
               (encode-m val stream))
             obj)
    (format stream "}")))

(defmethod encode-m ((obj list) stream)
  (let ((first? t))
    (format stream "[")
    (dolist (val obj)
      (unless first?
        (format stream ","))
      (setf first? nil)
      (encode-m val stream))
    (format stream "]")))

(defmethod encode-m ((obj t) stream)
  (format stream "{")
  (encode-m (first +default-class-keys+) stream)
  (format stream ":")
  (encode-m (string-downcase (string (class-name (class-of obj)))) stream)
  (dolist (slot (sb-mop:class-slots (class-of obj)))
    (let* ((slot-name (sb-mop:slot-definition-name slot))
           (key (string-downcase (symbol-name (first (sb-mop:slot-definition-initargs slot)))))
           (val (slot-value obj slot-name)))
      (format stream ",")
      (encode-m key stream)
      (format stream ":")
      (encode-m val stream)))
  (format stream "}"))


(defun run-query (query)
  (let ((query-string (if (probe-file query)
                          (with-open-file (stream query)
                            (let ((contents (make-string (file-length stream))))
                              (read-sequence contents stream)
                              contents))
                          query)))
    (drakma:http-request (format nil "http://localhost:~A/run" *port*)
                         :method :post
                         :parameters (list (cons "json" query-string)))))
