(asdf:defsystem :bodge-queue
  :description "Simple FIFO implementation with no external dependencies"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :serial t
  :components ((:file "bodge-queue")))


(asdf:defsystem :bodge-queue/tests
  :description "FIFO tests"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (:fiveam :bodge-queue)
  :serial t
  :components ((:file "bodge-queue-tests")))
