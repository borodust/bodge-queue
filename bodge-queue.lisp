(cl:defpackage :bodge-queue
  (:use :cl)
  (:export #:make-queue
           #:queue-push
           #:queue-pop
           #:queue-empty-p
           #:queue-length
           #:queue-peek
           #:queue-clear
           #:qref))
(cl:in-package :bodge-queue)


(defconstant +default-expansion-factor+ 2)


;;;
;;; QUEUE
;;;
(defstruct (queue
            (:constructor %make-queue (array expansion-factor)))
  (array (make-array 0) :type array)
  (expansion-factor +default-expansion-factor+ :type number)
  (head-idx -1 :type fixnum)
  (tail-idx -1 :type fixnum))


(defun make-queue (&key (initial-buffer-size 1)
                     element-type
                     (expansion-factor +default-expansion-factor+))
  (unless (> initial-buffer-size 0)
    (error "INITIAL-BUFFER-SIZE must be greater than 0"))
  (%make-queue (make-array initial-buffer-size
                           :element-type (or element-type t))
               (or expansion-factor +default-expansion-factor+)))


(declaim (inline prev-idx))
(defun prev-idx (queue idx)
  (if (> idx 0)
      (1- idx)
      (length (queue-array queue))))


(declaim (inline next-idx))
(defun next-idx (queue idx)
  (if (< idx (1- (length (queue-array queue))))
      (1+ idx)
      0))


(declaim (inline wrap-idx))
(defun wrap-idx (queue idx)
  (let ((array (queue-array queue))
        (ref-idx (+ (queue-head-idx queue) idx)))
    (if (>= ref-idx (length array))
        (- ref-idx (length array))
        ref-idx)))


(defun qref (queue idx)
  (aref (queue-array queue) (wrap-idx queue idx)))


(defun (setf qref) (value queue idx)
  (setf (aref (queue-array queue) (wrap-idx queue idx)) value))


(defun %queue-extend (queue)
  (let* ((old-queue (queue-array queue))
         (new-queue (make-array (* (ceiling (length old-queue))
                                   (queue-expansion-factor queue))
                                :element-type (array-element-type old-queue)))
         (head-idx (queue-head-idx queue))
         (tail-idx (queue-tail-idx queue)))
    (if (= head-idx 0)
        (setf (subseq new-queue 0) old-queue)
        (let* ((subhead-len (- (length old-queue) head-idx))
               (head-view (make-array subhead-len :displaced-to old-queue
                                                  :displaced-index-offset head-idx))
               (tail-view (make-array (1+ tail-idx) :displaced-to old-queue)))
          (setf (subseq new-queue 0) head-view
                (subseq new-queue subhead-len) tail-view)))
    (setf (queue-array queue) new-queue
          (queue-head-idx queue) 0
          (queue-tail-idx queue) (1- (length old-queue)))))


(defun queue-push (queue item)
  (let* ((head-idx (queue-head-idx queue))
         (tail-idx (queue-tail-idx queue)))
    (cond
      ((= tail-idx -1) (setf (queue-head-idx queue) 0))
      ((= (next-idx queue tail-idx) head-idx) (%queue-extend queue)))
    (setf (queue-tail-idx queue) (next-idx queue (queue-tail-idx queue))
          (aref (queue-array queue) (queue-tail-idx queue)) item)))


(defun queue-pop (queue)
  (let ((head-idx (queue-head-idx queue))
        (tail-idx (queue-tail-idx queue)))
    (if (= -1 tail-idx)
        (values nil nil)
        (values
         (prog1 (aref (queue-array queue) head-idx)
           (let ((new-head-idx (next-idx queue head-idx)))
             (if (= new-head-idx (next-idx queue tail-idx))
                 (setf (queue-head-idx queue) -1
                       (queue-tail-idx queue) -1)
                 (setf (queue-head-idx queue) new-head-idx))))
         t))))


(defun queue-empty-p (queue)
  (= (queue-head-idx queue) -1))


(defun queue-length (queue)
  (if (= (queue-head-idx queue) -1)
      0
      (- (queue-tail-idx queue) (queue-head-idx queue) -1)))


(defun queue-peek (queue)
  (let ((head-idx (queue-head-idx queue)))
    (if (= -1 head-idx)
        (values nil nil)
        (values (aref (queue-array queue) head-idx) t))))


(defun queue-clear (queue)
  (setf (queue-head-idx queue) -1
        (queue-tail-idx queue) -1)
  (values))
