(cl:defpackage :trivial-fifo.tests
  (:use :cl :trivial-fifo))
(5am:def-suite :trivial-fifo.tests)

(cl:in-package :trivial-fifo.tests)
(5am:in-suite :trivial-fifo.tests)


(5am:test fifo
  (let ((fifo (make-fifo)))
    (loop for i below 10
          do (fifo-push fifo i))
    (multiple-value-bind (result control)
        (loop for i below 11
              collect (fifo-pop fifo) into result
              collect (unless (= i 10) i) into control
              finally (return (values result control)))
      (5am:is (equal result control)))))


(5am:test push-pop
  (let ((fifo (make-fifo))
        (val 111))
    (5am:is (equal val (fifo-push fifo val)))
    (5am:is (equal nil (fifo-empty-p fifo)))
    (5am:is (equal 1 (fifo-length fifo)))
    (5am:is (equal val (fifo-pop fifo)))
    (5am:is (equal nil (fifo-pop fifo)))
    (5am:is (equal 0 (fifo-length fifo)))
    (5am:is (equal t (fifo-empty-p fifo)))))


(5am:test twin-pop
  (let ((fifo (make-fifo))
        (val 111))
    (fifo-push fifo val)
    (fifo-push fifo val)
    (fifo-pop fifo)
    (fifo-pop fifo)
    (5am:is (equal nil (fifo-pop fifo)))))
