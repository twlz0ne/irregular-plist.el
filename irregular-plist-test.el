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

(defmacro irregular-plist-test--put (expected _ind1 init _ind2 &rest input)
  `(let* ((pl1 (if (symbolp ',init) nil ',init))
          (pl-ret (irregular-plist-put pl1 ,@input)))
     (should (equal pl-ret ',expected))
     (should (equal pl1    ',expected))))

(defmacro irregular-plist-test--update (expected _ind1 init _ind2 input)
  `(let* ((pl1 (if (symbolp ',init) nil ',init))
          (pl-ret (irregular-plist-update pl1 ',input)))
     (should (equal pl-ret ',expected))
     (should (equal pl1    ',expected))))

;;;

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
  (irregular-plist-test--put (:foo 4 5 :bar 3)        <= (:foo 1 2 :bar 3) ++ :foo 4 5)
  (irregular-plist-test--put (:foo 4 5 :bar 3)        <= (:foo     :bar 3) ++ :foo 4 5)
  (irregular-plist-test--put (:foo 1 2 :bar 4)        <= (:foo 1 2 :bar 3) ++ :bar 4)
  (irregular-plist-test--put (:foo 1 2 :bar 3 :qux 6) <= (:foo 1 2 :bar 3) ++ :qux 6)
  (irregular-plist-test--put (:qux 6)                 <= (               ) ++ :qux 6))

(ert-deftest irregular-plist-test-mapc ()
  (let ((pl1 '(:foo 1 2 :bar 3))
        (pl2 '(:foo 4 5 :qux 6)))
    (irregular-plist-mapc (lambda (&rest key-vals)
                            (apply #'irregular-plist--put pl1 key-vals))
                          pl2)
    (should (equal '(:foo 4 5 :bar 3 :qux 6) pl1))))

(ert-deftest irregular-plist-test-update ()
  (irregular-plist-test--update (:foo 4 5 :bar 3 :qux 6) <= (:foo 1 2 :bar 3) ++ (:foo 4 5 :qux 6))
  (irregular-plist-test--update (:foo 4 5 :qux 6)        <= (               ) ++ (:foo 4 5 :qux 6))
  (let* ((pl1 '())
         (pl2 '(:foo 3 4 :bar 5))
         (pl2-copy (cl-copy-list pl2))
         (_ret1 (irregular-plist-update pl1 pl2))
         (_ret2 (irregular-plist-update pl1 '(:foo 7 8 :qux 9))))
    (should (equal pl2 pl2-copy))))

(ert-deftest irregular-plist-test-merge ()
  (let* ((pl1 '(:foo 1 2 :bar 3))
         (pl2 '(:foo 4 5 :qux 6))
         (pl3 '(:foo 7 8))
         (pl-ret (irregular-plist-merge pl1 pl2 pl3)))
    (should (equal '(:foo 7 8 :bar 3 :qux 6) pl-ret))
    (should (equal '(:foo 1 2 :bar 3) pl1))))

(ert-deftest irregular-plist-test-normalize ()
  (let ((pl1 '(:foo 1 2   :bar 3))
        (pl2 '(:foo nil   :bar 4))
        (pl3 '(:foo ()    :bar 5))
        (pl4 '(:foo (6 7) :bar 8)))
    (should (equal (irregular-plist-normalize pl1) '(:foo (1 2) :bar 3)))
    (should (equal (irregular-plist-normalize pl2) '(:foo nil   :bar 4)))
    (should (equal (irregular-plist-normalize pl3) '(:foo nil   :bar 5)))
    (should (equal (irregular-plist-normalize pl4) '(:foo (6 7) :bar 8)))))

(provide 'irregular-plist-test)

;;; irregular-plist-test.el ends here
