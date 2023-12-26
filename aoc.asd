(in-package :common-lisp-user)

(defpackage :aoc-asd
  (:use #:cl #:asdf))

(in-package :aoc-asd)

(defsystem #:aoc
  :name "AoC"
  :version "1.0"
  :description "Advent of Code 2023"

  :serial t
  :depends-on (#:cl-ppcre)

  :components ((:file "package")
               (:file "day1")
               (:file "day2")
               (:file "day3")
               (:file "day4")
               (:file "day5")
               (:file "day6")
               (:file "day7")
               (:file "day7-part2")
               (:file "day8")
               (:file "day9")
               ))
