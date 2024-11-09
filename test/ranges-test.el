;;; ranges-test.el --- Tests for the range helpers for ()visual-replace-read) -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Stephane Zermatten

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; `http://www.gnu.org/licenses/'.

(require 'visual-replace)

(ert-deftest test-visual-replace-merge-range ()
  (should (equal
           '((1 . 10) (20 . 30))
           (visual-replace--ranges-nmerge '((20 . 30) (1 . 10)))))
  (should (equal
           '((1 . 15))
           (visual-replace--ranges-nmerge '((1 . 10) (5 . 15)))))
  (should (equal
           '((1 . 15))
           (visual-replace--ranges-nmerge '((1 . 10) (1 . 15)))))
  (should (equal
           '((0 . 100))
           (visual-replace--ranges-nmerge '((1 . 10) (0 . 100)))))
  (should (equal
           '((1 . 10) (20 . 30))
           (visual-replace--ranges-nmerge '((20 . 25) (25 . 30) (1 . 5) (5 . 10))))))

(ert-deftest test-visual-replace-intersect-sorted ()
  (should (equal
           '((10 . 20) (30 . 40))
           (visual-replace--range-intersect-sorted '((0 . 100))
                                               '((10 . 20) (30 . 40) (200 . 210)))))
  (should (equal
           '((10 . 15) (30 . 35))
           (visual-replace--range-intersect-sorted '((0 . 15) (30 . 40))
                                               '((10 . 20) (25 . 35)))))
  (should (equal
           '((10 . 15) (30 . 35))
           (visual-replace--range-intersect-sorted '((10 . 15) (30 . 35))
                                               '((10 . 15) (30 . 35)))))
  (should (equal
           nil
           (visual-replace--range-intersect-sorted nil '((10 . 15) (30 . 35)))))
  (should (equal
           nil
           (visual-replace--range-intersect-sorted nil nil))))


(ert-deftest test-visual-replace-range-fix ()
  (should (equal nil (visual-replace--ranges-fix nil)))
  (should (equal '((10 . 100)) (visual-replace--ranges-fix '((100 . 10)))))
  (should (equal '((10 . 20) (30 . 40)) (visual-replace--ranges-fix '((10 . 20) (30 . 35) (32 . 40))))))

(ert-deftest test-visual-replace-substract-sorted-pairs ()
  ;;   A +-------+
  ;;   B    +-------+
  ;; A-B +--+
  (should (equal
           '((10 . 20))
           (visual-replace--range-substract-sorted
            '((10 . 100))
            '((20 . 120)))))
  ;;   A   +-----+
  ;;   B +----+
  ;; A-B      +--+
  (should (equal
           '((100 . 120))
           (visual-replace--range-substract-sorted
            '((20 . 120))
            '((10 . 100)))))
  ;;   A   +--------+
  ;;   B +---------------+
  ;; A-B
  (should (equal
           nil
           (visual-replace--range-substract-sorted
            '((20 . 100))
            '((10 . 120)))))
  ;;   A +---------------+
  ;;   B   +--------+
  ;; A-B +-+        +----+
  (should (equal
           '((10 . 20) (100 . 120))
           (visual-replace--range-substract-sorted
            '((10 . 120))
            '((20 . 100)))))
  ;;   A  +----+
  ;;   B          +----+
  ;; A-B  +----+
  (should (equal
           '((10 . 20))
           (visual-replace--range-substract-sorted
            '((10 . 20))
            '((100 . 120)))))
  ;;   A          +----+
  ;;   B  +----+
  ;; A-B          +----+
  (should (equal
           '((100 . 120))
           (visual-replace--range-substract-sorted
            '((100 . 120))
            '((10 . 20))))))

(ert-deftest test-visual-replace-substract-sorted-ranges ()
  ;;   A    +-----+  +----+ +----+     +--+  +--+
  ;;   B +----+ +-------------+      +----+
  ;; A-B      +-+             +--+           +--+
  (should (equal
           '((30 . 40) (90 . 100) (140 . 150))
           (visual-replace--range-substract-sorted
            '((20 . 50) (60 . 70) (80 . 100) (120 . 130) (140 . 150))
            '((10 . 30) (40 . 90) (110 . 130))))))

(ert-deftest test-visual-replace-contains-sorted ()
  (should-not (visual-replace--range-contains-sorted nil 10))
  (should-not (visual-replace--range-contains-sorted '((20 . 100)) 10))
  (should (visual-replace--range-contains-sorted '((0 . 100)) 10))
  (should (visual-replace--range-contains-sorted '((10 . 100)) 10))
  (should-not (visual-replace--range-contains-sorted '((20 . 100)) 10))
  (should-not (visual-replace--range-contains-sorted '((0 . 9) (11 . 100)) 10))
  (should (visual-replace--range-contains-sorted '((0 . 9) (10 . 100)) 10)))

(ert-deftest test-visual-replace-range-bytesize ()
  (should (equal 0 (visual-replace--ranges-bytesize nil)))
  (should (equal 0 (visual-replace--ranges-bytesize '((10 . 10)))))
  (should (equal 1000 (visual-replace--ranges-bytesize
                       '((100 . 1100)))))
  (should (equal 2000 (visual-replace--ranges-bytesize
                       '((100 . 200) (500 . 600) (900 . 2100))))))

;;; ranges-test.el ends here
