[![Build Status](https://travis-ci.com/twlz0ne/irregular-plist.el.svg?branch=master)](https://travis-ci.com/twlz0ne/irregular-plist.el)

# irregular-plist.el

Utilities for irregular plist.

## Functions

- [irregular-plist-member](#irregular-plist-member-iplist-optional-prop) `(iplist &optional prop)`
- [irregular-plist-get](#irregular-plist-get-iplist-prop)                `(iplist prop)`
- [irregular-plist-put](#irregular-plist-put-iplist-prop-rest-vals)      `(iplist prop &rest vals)`
- [irregular-plist-mapc](#irregular-plist-mapc-func-iplist)              `(func iplist)`
- [irregular-plist-update](#irregular-plist-update-iplist1-iplist2)      `(iplist1 iplist2)`
- [irregular-plist-merge](#irregular-plist-merge-rest-iplists)           `(&rest iplists)`

## Examples

### irregular-plist-member `(iplist &optional prop)`

```elisp
(irregular-plist-member '(:foo 1 2 :bar 3) :bar)
;; => (:bar 3)

(irregular-plist-member '(:foo 1 2 :bar 3))
;; => (:foo 1 2 :bar 3)
```

### irregular-plist-get `(iplist prop)`

```elisp
(irregular-plist-get '(:foo 1 2 :bar 3) :foo)
;; => (1 2)

(irregular-plist-get '(:foo 1 2 :bar 3) :bar)
;; => 3
```

### irregular-plist-put `(iplist prop &rest vals)`

```elisp
(let ((pl '(:foo 1 2 :bar 3))) (irregular-plist-put pl :foo 4 5) pl)
;; => (:foo 4 5 :bar 3)

(let ((pl '(:foo 1 2 :bar 3))) (irregular-plist-put pl :bar 4) pl)
;; => (:foo 1 2 :bar 4)

(let ((pl '(:foo 1 2 :bar 3))) (irregular-plist-put pl :qux 6) pl)
;; => (:foo 1 2 :bar 3 :qux 6)
```

### irregular-plist-mapc `(func iplist)`

```elisp
(let (values)
  (irregular-plist-mapc (lambda (_key &rest vals)
                          (setq values `(,@values ,@vals)))
                        '(:foo 1 2 :bar 3))
  values)
;; => (1 2 3)
```

### irregular-plist-update `(iplist1 iplist2)`

``` elisp
(let ((pl '(:foo (1) 2 :bar 3)))
  (irregular-plist-update pl '(:foo 4 5 6))
  pl)
;; => (:foo 4 5 6 :bar 3)
```

### irregular-plist-merge (&rest iplists)

```elisp
(let* ((pl1 '(:foo 1 2 :bar 3 4))
       (pl2 '(:foo 7 8 :qux 6))
       (pl-new (irregular-plist-merge pl1 pl2)))
  pl-new)
;; => (:foo 7 8 :bar 3 4 :qux 6)
```

