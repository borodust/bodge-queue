(asdf:defsystem :trivial-fifo
  :description "Simple FIFO implementation with no external dependencies"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :serial t
  :components ((:file "trivial-fifo")))


(asdf:defsystem :trivial-fifo/tests
  :description "FIFO tests"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :serial t
  :components ((:file "trivial-fifo-tests")))
