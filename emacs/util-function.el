;;; util-function.el --- Utility functions for elisp

(defun shuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defun time-diff-in-minutes (time-str1 time-str2)
  "Return the difference between time-str1 and timestr2 in minutes."
  (let* ((time1 (apply #'encode-time (parse-time-string time-str1)))
         (time2 (apply #'encode-time (parse-time-string time-str2)))
         (diff (abs (float-time (time-subtract time1 time2)))))
    (floor (/ diff 60))))

(defun time-diff-in-dhms (time-str1 time-str2)
  "Return the difference between time-str1 and timestr2 in the format 'days hours minutes seconds."
  (let* ((time1 (apply #'encode-time (parse-time-string time-str1)))
         (time2 (apply #'encode-time (parse-time-string time-str2)))
         (diff (abs (float-time (time-subtract time1 time2))))
         (days (floor (/ diff 86400)))
         (hours (floor (mod (/ diff 3600) 24)))
         (minutes (floor (mod (/ diff 60) 60)))
         (seconds (floor (mod diff 60))))
    (format "%d일 %d시간 %d분 %d초" days hours minutes seconds)))

(defun minutes-to-dhm (minutes)
  "Convert total MINUTES into a string formatted as 'X days Y hours Z minutes'."
  (let* ((total-minutes (abs minutes)) ;; 음수 대비 절댓값 처리
         (days (/ total-minutes 1440)) ;; 1일 = 1440분
         (hours (/ (% total-minutes 1440) 60))
         (mins (% total-minutes 60)))
    (format "%d일 %d시간 %d분" days hours mins)))
