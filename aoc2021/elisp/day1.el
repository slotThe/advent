;;; day1 -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)
(require 'packed)
(require 's)

(defun parse ()
  (packed-with-file "day1.txt"
    (->> (buffer-substring (point-min) (point-max))
         s-split-words
         (-map 'string-to-number))))

(defun solve (n list)
  (cl-count 't (-zip-with '< list (-drop n list))))

;; day1-1: 1521
(solve 1 (parse))

;; day1-2: 1543
(solve 3 (parse))                       ; a + b + c < b + c + d  ⇔  a < d
