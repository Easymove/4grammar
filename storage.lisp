(in-package :4grammar)

;;; ----------------------------------------------------------------------------
;;; Storage interface
;;; ----------------------------------------------------------------------------
(defgeneric get-from-storage-m (storage key))

(defgeneric set-to-storage-m (storage key val))

(defgeneric close-storage-m (storage))

(defgeneric open-storage-m (storage))

(defun get-from-storage (key)
  (get-from-storage-m *storage* key))

(defun set-to-storage (key val)
  (set-to-storage-m *storage* key val))

(defun open-storage ()
  (open-storage-m *storage*)
  *storage*)

(defun close-storage ()
  (close-storage-m *storage*)
  (setf *storage* nil))

;;; ----------------------------------------------------------------------------
;;; Define your storages here
;;; ----------------------------------------------------------------------------
(defclass memory-storage ()
  ((hash-table :accessor storage-table
               :initarg :table
               :type hash-table
               :initform (make-hash-table :test #'equal))))

(defmethod get-from-storage-m ((st memory-storage) key)
  (gethash key (storage-table st)))

(defmethod set-to-storage-m ((st memory-storage) key val)
  (setf (gethash key (storage-table st)) val))

(defmethod open-storage-m ((st memory-storage))
  nil)

(defmethod close-storage-m ((st memory-storage))
  nil)

;;; ----------------------------------------------------------------------------
;;; default storage
;;; ----------------------------------------------------------------------------
(defparameter *storage* (make-instance 'memory-storage))

(defun default-storage ()
  (make-instance 'memory-storage))
