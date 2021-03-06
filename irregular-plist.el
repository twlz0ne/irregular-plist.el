;;; irregular-plist.el --- Utilities for irregular plist -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2020/01/09
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/twlz0ne/irregular-plist.el
;; Keywords: lists

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities for irregular plist.

;; Example:
;; 
;; ```
;; (let ((pl '(:foo (1) 2 :bar 3)))
;;   (irregular-plist-put pl :foo 4 5 6)
;;   pl)
;; ;; => (:foo 4 5 6 :bar 3)
;; ```

;; See README.md for more information.

;;; Change Log:

;;  0.1.0  2020/01/09  Initial commit.

;;; Code:

(require 'cl-lib)

(defun irregular-plist-member (iplist &optional prop)
  "Return non-nil if IPLIST has the property KEY.

If PROP is nil, return on first property.

\(irregular-plist-member '(:foo 1 2 :bar 3) :bar)
=> (:bar 3)

\(irregular-plist-member '(:foo 1 2 :bar 3))
=> (:foo 1 2 :bar 3)"
  (let* ((i 0 ))
    (catch 'break
      (mapc (lambda (it)
              (if (and (keywordp it) (or (not prop) (eq it prop)))
                  (throw 'break (nthcdr i iplist))
                (setq i (1+ i))))
            iplist)
      nil)))

(defun irregular-plist-get (iplist prop)
  "Extra value in IPLIST of PROP.

\(irregular-plist-get '(:foo 1 2 :bar 3) :foo)
=> (1 2)

\(irregular-plist-get '(:foo 1 2 :bar 3) :bar)
=> 3"
  (let (match vals)
    (catch 'break
      (mapc (lambda (it)
              (if (and match (keywordp it))
                  (throw 'break nil)
                (if (not match)
                    (when (and (keywordp it) (eq it prop))
                      (setq match it))
                  (push it vals))))
            iplist))
    (if (cdr vals)
        (reverse vals)
      (car vals))))

(defun irregular-plist--put (iplist prop &rest vals)
  "Change value in IPLIST of PROP to VALS.

The IPLIST must not be nil (i.e. '()).
The IPLIST is modified by side effect.

\(irregular-plist--put \\='(:foo 1 2 :bar 3) :foo 4 5)
=> (:foo 4 5 :bar 3)

\(irregular-plist--put \\='(:foo 1 2 :bar 3) :bar 4)
=> (:foo 1 2 :bar 4)

\(irregular-plist--put \\='(:foo 1 2 :bar 3) :qux 6)
=> (:foo 1 2 :bar 3 :qux 6)"
  (let ((seq1 (irregular-plist-member iplist prop)))
    (cond
     (seq1
      (let ((seq2 (irregular-plist-member (cdr seq1))))
        (setf (cdr seq1) `(,@vals ,@seq2))))
     (t
      (let ((n (1- (length iplist))))
        (setf (cdr (nthcdr n iplist)) `(,prop ,@vals)))))
    iplist))

(defmacro irregular-plist-put! (iplist prop &rest vals)
  "Change value in IPLIST of PROP to VALS.

This macro is based on `irregular-plist--put' but accept empty IPLIST.

\(let ((pl \\='(:foo 1 2))) (irregular-plist-put! pl :bar 3 4) pl)
=> (:foo 1 2 :bar 3 4)

\(let ((pl)) (irregular-plist-put! pl :bar 3 4) pl)
=> (:bar 3 4)"
  `(if ,iplist
       (funcall #'irregular-plist--put ,iplist ,prop ,@vals)
     (setf ,iplist (list ,prop ,@vals))))

(defalias 'irregular-plist-put 'irregular-plist-put!)

(defun irregular-plist-mapc (func iplist)
  "Apply FUNC to each prop-values paire of IPLIST.

FUNC takes a &rest parameter."
  (let (prop vals)
    (mapc (lambda (it)
            (if (not prop)
                (if (keywordp it)
                    (setq prop it))
              (if (keywordp it)
                  (progn
                    (apply func prop (reverse vals))
                    (setq prop it)
                    (setq vals nil))
                (push it vals))))
          iplist)
    (apply func prop (reverse vals))
    iplist))

(defun irregular-plist--update (iplist iplist-from)
  "Update IPLIST according to every key/value pair in IPLIST-FROM.

The IPLIST must not be nil (i.e. '()).
The IPLIST is modified by side effect.

\(let ((pl \\='(:foo 1 2 :bar 3))) (irregular-plist--update pl \\='(:foo 4 5)) pl)
=> (:foo 4 5 :bar 3)

\(let ((pl \\='(:foo 1 2 :bar 3))) (irregular-plist--update pl \\='(:bar 4)) pl)
=> (:foo 1 2 :bar 4)

\(let ((pl \\='(:foo 1 2 :bar 3))) (irregular-plist--update pl \\='(:qux 6)) pl)
=> (:foo 1 2 :bar 3 :qux 6)"
  (irregular-plist-mapc (lambda (key &rest vals)
                          (apply #'irregular-plist--put iplist key vals))
                        iplist-from)
  iplist)

(defmacro irregular-plist-update! (iplist iplist-from)
  "Update IPLIST according to every key/value pair in IPLIST-FROM.

This macro is based on `irregular-plist--update' but accept empty IPLIST.

\(let ((pl \\='(:foo 1 2))) (irregular-plist-update! pl \\='(:bar 3 4)) pl)
=> (:foo 1 2 :bar 3 4)

\(let ((pl)) (irregular-plist-update! pl \\='(:bar 3 4)) pl)
=> (:bar 3 4)"
  `(if ,iplist
       (funcall #'irregular-plist--update ,iplist ,iplist-from)
     (setf ,iplist (cl-copy-list ,iplist-from))))

(defalias 'irregular-plist-update 'irregular-plist-update!)

(defun irregular-plist-merge (&rest iplists)
  "Crete a new list that includes all the key/value pairs from IPLISTS.

If multiple lists have the same key, the value in the last list is used."
  (let ((merged '()))
    (mapc (lambda (iplist)
            (when iplist
              (irregular-plist-update! merged iplist)))
          iplists)
    merged))

(defun irregular-plist-normalize (iplist)
  "Convert IPLIST to a normal plist.

\(irregular-plist-merge \\='(:foo 1 2 :bar 3))
=> (:foo (1 2) :bar 3)"
  (let ((normal-pl))
    (irregular-plist-mapc
     (lambda (key &rest vals)
       (let ((new-pl
              (pcase vals
                (`((. ,rest)) `(,@normal-pl ,key ,rest))
                (_            `(,@normal-pl ,key ,vals)))))
         (when new-pl (setq normal-pl new-pl))))
     iplist)
    normal-pl))

(provide 'irregular-plist)

;;; irregular-plist.el ends here
