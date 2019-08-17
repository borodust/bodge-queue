(cl:defpackage :trivial-fifo
  (:use :cl)
  (:export #:make-fifo
           #:fifo-push
           #:fifo-pop
           #:fifo-empty-p
           #:fifo-length
           #:fifo-peek
           #:fifo-clear))
(cl:in-package :trivial-fifo)


(defconstant +default-expansion-factor+ 2)


;;;
;;; FIFO
;;;
(defstruct (fifo
            (:constructor %make-fifo (queue expansion-factor)))
  (queue (make-array 0) :type array)
  (expansion-factor +default-expansion-factor+ :type number)
  (head-idx -1 :type fixnum)
  (tail-idx -1 :type fixnum))


(defun make-fifo (&key (initial-buffer-size 1)
                    element-type
                    (expansion-factor +default-expansion-factor+))
  (unless (> initial-buffer-size 0)
    (error "INITIAL-BUFFER-SIZE must be greater than 0"))
  (%make-fifo (make-array initial-buffer-size
                          :element-type (or element-type t))
              (or expansion-factor +default-expansion-factor+)))


(defun prev-idx (fifo idx)
  (if (> idx 0)
      (1- idx)
      (length (fifo-queue fifo))))


(defun next-idx (fifo idx)
  (if (< idx (1- (length (fifo-queue fifo))))
      (1+ idx)
      0))


(defun %fifo-extend (fifo)
  (let* ((old-queue (fifo-queue fifo))
         (new-queue (make-array (* (ceiling (length old-queue))
                                   (fifo-expansion-factor fifo))
                                :element-type (array-element-type old-queue)))
         (head-idx (fifo-head-idx fifo))
         (tail-idx (fifo-tail-idx fifo)))
    (if (= head-idx 0)
        (setf (subseq new-queue 0) old-queue)
        (let* ((subhead-len (- (length old-queue) head-idx))
               (head-view (make-array subhead-len :displaced-to old-queue
                                                  :displaced-index-offset head-idx))
               (tail-view (make-array (1+ tail-idx) :displaced-to old-queue)))
          (setf (subseq new-queue 0) head-view
                (subseq new-queue subhead-len) tail-view)))
    (setf (fifo-queue fifo) new-queue
          (fifo-head-idx fifo) 0
          (fifo-tail-idx fifo) (1- (length old-queue)))))


(defun fifo-push (fifo item)
  (let* ((head-idx (fifo-head-idx fifo))
         (tail-idx (fifo-tail-idx fifo)))
    (cond
      ((= tail-idx -1) (setf (fifo-head-idx fifo) 0))
      ((= (next-idx fifo tail-idx) head-idx) (%fifo-extend fifo)))
    (setf (fifo-tail-idx fifo) (next-idx fifo (fifo-tail-idx fifo))
          (aref (fifo-queue fifo) (fifo-tail-idx fifo)) item)))


(defun fifo-pop (fifo)
  (let ((head-idx (fifo-head-idx fifo))
        (tail-idx (fifo-tail-idx fifo)))
    (if (= -1 tail-idx)
        (values nil nil)
        (values
         (prog1 (aref (fifo-queue fifo) head-idx)
           (let ((new-head-idx (next-idx fifo head-idx)))
             (if (= new-head-idx (next-idx fifo tail-idx))
                 (setf (fifo-head-idx fifo) -1
                       (fifo-tail-idx fifo) -1)
                 (setf (fifo-head-idx fifo) new-head-idx))))
         t))))


(defun fifo-empty-p (fifo)
  (= (fifo-head-idx fifo) -1))


(defun fifo-length (fifo)
  (if (= (fifo-head-idx fifo) -1)
      0
      (- (fifo-tail-idx fifo) (fifo-head-idx fifo) -1)))


(defun fifo-peek (fifo)
  (let ((head-idx (fifo-head-idx fifo)))
    (if (= -1 head-idx)
        (values nil nil)
        (values (aref (fifo-queue fifo) head-idx) t))))


(defun fifo-clear (fifo)
  (setf (fifo-head-idx fifo) -1
        (fifo-tail-idx fifo) -1)
  (values))
