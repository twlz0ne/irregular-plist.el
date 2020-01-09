[![Build Status](https://travis-ci.com/twlz0ne/irregular-plist.el.svg?branch=master)](https://travis-ci.com/twlz0ne/irregular-plist.el)

# irregular-plist.el

Utilities for irregular plist.

## Functions

- [irregular-plist-member](#irregular-plist-member-iplist-optional-prop) `(iplist &optional prop)`
- [irregular-plist-gut](#irregular-plist-get-iplist-prop)                `(iplist prop)`
- [irregular-plist-put](#irregular-plist-put-iplist-prop-rest-vals)      `(iplist prop &rest vals)`
- [irregular-plist-mapc](#irregular-plist-mapc-func-iplist)              `(func iplist)`
- [irregular-plist-merge](#irregular-plist-merge-iplist1-iplist2)        `(iplist1 iplist2)`

## Examples

#### irregular-plist-member `(iplist &optional prop)`

```elisp
(irregular-plist-member '(:foo 1 2 :bar 3) :bar)
;; => (:bar 3)

(irregular-plist-member '(:foo 1 2 :bar 3))
;; => (:foo 1 2 :bar 3)
```

#### irregular-plist-get `(iplist prop)`

```elisp
(irregular-plist-get '(:foo 1 2 :bar 3) :foo)
;; => (1 2)

(irregular-plist-get '(:foo 1 2 :bar 3) :bar)
;; => 3
```

#### irregular-plist-put `(iplist prop &rest vals)`

```elisp
(irregular-plist-put '(:foo 1 2 :bar 3) :foo 4 5)
;; => (:foo 4 5 :bar 3)

(irregular-plist-put '(:foo 1 2 :bar 3) :bar 4)
;; => (:foo 1 2 :bar 4)

(irregular-plist-put '(:foo 1 2 :bar 3) :qux 6)
;; => (:foo 1 2 :bar 3 :qux 6)
```

#### irregular-plist-mapc `(func iplist)`

```elisp
(let (values)
  (irregular-plist-mapc (lambda (_key &rest vals)
                          (setq values `(,@values ,@vals)))
                        '(:foo 1 2 :bar 3))
  values)
;; => (1 2 3)
```

#### irregular-plist-merge `(iplist1 iplist2)`

```elisp
(irregular-plist-merge '(:foo (1) 2 :bar 3)
                       '(:foo 4 5 6))
;; => (:foo 4 5 6 :bar 3)
```

