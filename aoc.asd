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
               (:file "day10")
               (:file "day11")
               (:file "day12")
               (:file "day12-part2")
               (:file "day13")
               (:file "day13-part2")
               (:file "day14")
               (:file "day15")
               (:file "day16")
               ))

