;;; irregular-plist-test.el --- Test irregular-plist -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'irregular-plist)

(when noninteractive
  (transient-mark-mode))

(ert-deftest irregular-plist-test-member ()
  (should (equal '(:bar 3)          (irregular-plist-member '(:foo 1 2 :bar 3) :bar)))
  (should (equal '(:foo 1 2 :bar 3) (irregular-plist-member '(:foo 1 2 :bar 3)     )))
  (should (equal nil                (irregular-plist-member '(:foo 1 2 :bar 3) :qux))))

(ert-deftest irregular-plist-test-get ()
  (should (equal '(1 2) (irregular-plist-get '(:foo 1 2 :bar 3) :foo)))
  (should (equal nil    (irregular-plist-get '(:foo     :bar 3) :foo)))
  (should (equal 3      (irregular-plist-get '(:foo 1 2 :bar 3) :bar)))
  (should (equal nil    (irregular-plist-get '(:foo 1 2 :bar 3) :qux))))

(ert-deftest irregular-plist-test-put ()
  (should (equal '(:foo 4 5 :bar 3)        (irregular-plist-put '(:foo 1 2 :bar 3) :foo 4 5)))
  (should (equal '(:foo 4 5 :bar 3)        (irregular-plist-put '(:foo     :bar 3) :foo 4 5)))
  (should (equal '(:foo 1 2 :bar 4)        (irregular-plist-put '(:foo 1 2 :bar 3) :bar 4)))
  (should (equal '(:foo 1 2 :bar 3 :qux 6) (irregular-plist-put '(:foo 1 2 :bar 3) :qux 6)))
  (let* ((pl1 '())
         (pl1-ret (irregular-plist-put pl1 :foo 4 5)))
    (should (equal '(:foo 4 5) pl1-ret))
    (should (equal '() pl1))))

(ert-deftest irregular-plist-test-mapc ()
  (let ((pl1 '(:foo 1 2 :bar 3))
        (pl2 '(:foo 4 5 :qux 6)))
    (irregular-plist-mapc (lambda (&rest key-vals)
                            (apply #'irregular-plist-put pl1 key-vals))
                          pl2)
    (should (equal '(:foo 4 5 :bar 3 :qux 6) pl1))))

(ert-deftest irregular-plist-test-update ()
  (let* ((pl1 '(:foo 1 2 :bar 3))
         (pl2 '(:foo 4 5 :qux 6))
         (pl-ret (irregular-plist-update pl1 pl2)))
    (should (equal '(:foo 4 5 :bar 3 :qux 6) pl-ret))
    (should (equal '(:foo 4 5 :bar 3 :qux 6) pl1))))

(ert-deftest irregular-plist-test-merge ()
  (let* ((pl1 '(:foo 1 2 :bar 3))
         (pl2 '(:foo 4 5 :qux 6))
         (pl3 '(:foo 7 8))
         (pl-ret (irregular-plist-merge pl1 pl2 pl3)))
    (should (equal '(:foo 7 8 :bar 3 :qux 6) pl-ret))
    (should (equal '(:foo 1 2 :bar 3) pl1))))

(provide 'irregular-plist-test)

;;; irregular-plist-test.el ends here
