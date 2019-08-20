(cl:defpackage :bodge-queue.tests
  (:use :cl :bodge-queue))
(5am:def-suite :bodge-queue.tests)

(cl:in-package :bodge-queue.tests)
(5am:in-suite :bodge-queue.tests)


(5am:test queue
  (let ((queue (make-queue)))
    (loop for i below 10
          do (queue-push queue i))
    (multiple-value-bind (result control)
        (loop for i below 11
              collect (queue-pop queue) into result
              collect (unless (= i 10) i) into control
              finally (return (values result control)))
      (5am:is (equal result control)))))


(5am:test push-pop
  (let ((queue (make-queue))
        (val 111))
    (5am:is (equal val (queue-push queue val)))
    (5am:is (equal nil (queue-empty-p queue)))
    (5am:is (equal 1 (queue-length queue)))
    (5am:is (equal val (queue-pop queue)))
    (5am:is (equal nil (queue-pop queue)))
    (5am:is (equal 0 (queue-length queue)))
    (5am:is (equal t (queue-empty-p queue)))))


(5am:test twin-pop
  (let ((queue (make-queue))
        (val 111))
    (queue-push queue val)
    (queue-push queue val)
    (queue-pop queue)
    (queue-pop queue)
    (5am:is (equal nil (queue-pop queue)))))


(5am:test wrapped
  (let ((queue (make-queue)))
    (queue-push queue 111)
    (queue-push queue 222)
    (queue-pop queue)
    (queue-push queue 333)
    (5am:is (equal '(222 333) (loop for i below (queue-length queue)
                                    collect (qref queue i))))))
