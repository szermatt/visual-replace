;;; ranges-test.el --- Tests for the range helpers for ()visual-replace-read) -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Stephane Zermatten

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; `http://www.gnu.org/licenses/'.

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

;;; ranges-test.el ends here
