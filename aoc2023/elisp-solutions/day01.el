(defconst replacements
  '("_" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defun solve (re s)
  (cl-flet ((to-digit (s)
              (or (-elem-index s replacements)
                  (cl-parse-integer s))))
    (let (first second)
      (with-temp-buffer
        (insert s)
        (goto-char (point-min))
        (re-search-forward re (pos-eol))
        (setq first (to-digit (match-string 0)))
        (goto-char (pos-eol))
        (re-search-backward re (pos-bol))
        (setq second (to-digit (match-string 0))))
      (+ (* 10 first) second))))

(defun day01 ()
  (let ((inp (-drop-last 1 (s-lines (f-read "../inputs/day01.txt"))))
        (res (lambda (re) (-sum (--map (solve re it) inp)))))
    (cons (funcall res (rx (any "1-9")))
          (funcall res (eval `(rx (or (any "1-9") ,@replacements)))))))
