;;; org-clone-subtree-cleanly.el --- Clone orgmode subtrees without duplicating IDs
;;; -*- lexical-binding: t; -*-
;;; Author: Hauke Rehfeld <emacs@haukerehfeld.de>
;;; Version: 0.1
;;; Package-Requires: ((emacs "24.3"))
;;; Keywords: convenience
;;; URL: https://github.com/hrehfeld/org-clone-subtree-cleanly
;;; Commentary:

;;; Code:
(require 'org)
(require 'cl-lib)

;;;###autoload
(defun org-clone-subtree-cleanly ()
  "Clone a subtree, but delete all ID properties to avoid duplicate IDs."
  (interactive)
  (org-copy-subtree)
  (org-back-to-heading)
  (org-yank)
  (save-excursion
   (org-map-entries (lambda () (org-delete-property "ID")) nil 'tree)
  ))

(defvar org-clone-subtree-cleanly-last-start nil)
(defvar org-clone-subtree-cleanly-last-end nil)

(defconst org-clone-subtree-cleanly--clock-properties
  '(:year :month :day :hour :minute))


(defun org-clone-subtree-cleanly--timestamp-time (type timestamp)
  "Get the time of a timestamp entry as a list in the order of `org-clone-subtree-cleanly--clock-properties'. `TYPE' can be 'start or 'end."
  (cl-assert (memq type '(start end)) t "TYPE must be 'start or 'end")
  (cl-assert (org-element-type timestamp) t "TIMESTAMP must be an org-element of type timestamp")
  (let ((type (or type 'start)))
    (cl-loop
     for prop in org-clone-subtree-cleanly--clock-properties
     collect
     (let ((prop-sym (intern (format "%s-%s" prop type))))
       (org-element-property prop-sym timestamp)))))

(defun org-clone-subtree-cleanly--clock-time (type clock)
  (cl-assert (org-element-type clock) t "CLOCK must be an org-element of type clock")
  (let ((timestamp (org-element-property :value clock)))
    (org-clone-subtree-cleanly--timestamp-time type timestamp)))


(defun org-clone-subtree-cleanly--timelist-delta (time-a time-b)
  (cl-loop
   for i from 0 to (1- (length org-clone-subtree-cleanly--clock-properties))
   collect
   (let ((a (nth i time-a))
         (b (nth i time-b)))
     (- a b))))

;; (org-clone-subtree-cleanly--timelist-delta '(2024 4 16 13 5) '(2024 4 16 13 6))


(defun org-clone-subtree-cleanly--timelist< (time-a time-b)
  (cl-loop
   for i from 0 to (length org-clone-subtree-cleanly--clock-properties)
   do
   (let ((a (nth i time-a))
         (b (nth i time-b)))
     (when (not (eq a b))
       (cl-return (< a b))))))
;; (org-clone-subtree-cleanly--timelist< '(2024 4 16 13 5) '(2024 4 16 13 6))
;; (org-clone-subtree-cleanly--timelist< '(2024 4 16 13 5) '(2024 4 16 13 5))
;; (org-clone-subtree-cleanly--timelist< '(2024 4 16 13 6) '(2024 4 16 13 5))

;; (org-clone-subtree-cleanly--clock< 'start (org-clone-subtree-cleanly--parse-timestamp "[2024-04-16 Tue 13:05]") (org-clone-subtree-cleanly--parse-timestamp "[2024-04-16 Tue 13:05]"))
;; (org-clone-subtree-cleanly--clock< 'end (org-clone-subtree-cleanly--parse-timestamp "[2024-04-16 Tue 13:05]") (org-clone-subtree-cleanly--parse-timestamp "[2024-04-16 Tue 13:06]"))

(defun org-clone-subtree-cleanly-clock-intersection (clock start-inclusive end-inclusive)
  ;; return nil if clock completely outside of range
  ;; return t if clock completely inside of range
  ;; return a list of two timestamps if clock intersects with range
  (let ((clock-start (org-clone-subtree-cleanly--clock-time 'start clock))
        (clock-end (org-clone-subtree-cleanly--clock-time 'end clock))
        (start-inclusive (org-clone-subtree-cleanly--timestamp-time 'start start-inclusive))
        (end-inclusive (org-clone-subtree-cleanly--timestamp-time 'end end-inclusive)))
    (cond
     ;; outside of range
     ((or (org-clone-subtree-cleanly--timelist< clock-end start-inclusive)
          (org-clone-subtree-cleanly--timelist< end-inclusive clock-start))
      nil)
      ;; completely inside of range
      ((and (org-clone-subtree-cleanly--timelist< start-inclusive clock-start)
            (org-clone-subtree-cleanly--timelist< clock-end end-inclusive))
       t)
      ;; intersecting with range
      (t

       (list
        (if (org-clone-subtree-cleanly--timelist< clock-start start-inclusive)
            start-inclusive
          clock-start)
        (if (org-clone-subtree-cleanly--timelist< end-inclusive clock-end)
            end-inclusive
          clock-end))))))
;; (org-clone-subtree-cleanly-clock-intersection (org-clone-subtree-cleanly--parse-clock "CLOCK: [2024-04-16 Tue 13:05]--[2024-04-16 Tu 14:09] => 0:09")
;;                                             (org-clone-subtree-cleanly--parse-timestamp "[2024-04-16 Tue 13:05]")
;;                                             (org-clone-subtree-cleanly--parse-timestamp "[2024-04-16 Tue 13:30]"))

(defun org-clone-subtree-cleanly--parse-timestamp (timestamp)
  (let (parsed)
    (with-temp-buffer
      (insert timestamp)
      (goto-char (point-min))
      (setq parsed (org-element-timestamp-parser))
      (cl-assert parsed t "Failed to parse timestamp %S" timestamp))
    parsed))
;; (org-clone-subtree-cleanly--parse-timestamp "[2024-04-16]")

(defun org-clone-subtree-cleanly--parse-clock (clock)
  (let (parsed)
    (with-temp-buffer
      (insert clock)
      (goto-char (point-min))
      (setq parsed (org-element-clock-parser (point-max)))
      (cl-assert parsed t "Failed to parse clock %S" clock))
    parsed))
;; (org-clone-subtree-cleanly--parse-clock "CLOCK: [2024-04-16 Tue 13:05]-- [2024-04-16 Tue 14:05] =>  1:00")



;; (date-to-time "2024-04-16 13:05")

(defun org-clone-subtree-cleanly--read-timestamp (&optional prompt default-input)
  (format "[%s]" (org-read-date nil nil nil prompt nil default-input))
);; (org-clone-subtree-cleanly--read-timestamp "test" nil)



(defun org-clone-subtree-cleanly--format-timestamp (timestamp)
  (let ((timestamp (org-element-create 'timestamp (append (list :type 'inactive) (-interleave (mapcar (lambda (prop) (intern (format "%s-start" prop)))
                                                                      org-clone-subtree-cleanly--clock-properties)
                                                              timestamp)))))
    (format "%s" (org-element-timestamp-interpreter timestamp nil))))
;; (org-clone-subtree-cleanly--format-timestamp '(2024 4 16 13 5))


(defun org-clone-subtree-cleanly-timerange (start-inclusive end-inclusive)
  ;; get start-inclusive and end-inclusive from org-read-date
  (interactive (list (org-clone-subtree-cleanly--read-timestamp "Cull earlier than" org-clone-subtree-cleanly-last-start)
                     (org-clone-subtree-cleanly--read-timestamp "Cull later than" org-clone-subtree-cleanly-last-end)))
  (setq org-clone-subtree-cleanly-last-start start-inclusive
        org-clone-subtree-cleanly-last-end end-inclusive)
  (let ((start-inclusive (org-clone-subtree-cleanly--parse-timestamp start-inclusive))
        (end-inclusive (org-clone-subtree-cleanly--parse-timestamp end-inclusive)))
    (org-clone-subtree-cleanly)
    (let ((end (save-excursion (org-end-of-subtree t))))
      (while (search-forward-regexp org-clock-line-re end t)
        ;; parse clock entries
        (beginning-of-line)
        (let* ((clock (org-element-clock-parser end))
               (inside (org-clone-subtree-cleanly-clock-intersection clock start-inclusive end-inclusive)))
            (message "Line: %s" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
            (delete-line)
            (when inside
              (let* ((intersected? (not (eq inside t)))
                       (clock-start (org-clone-subtree-cleanly--clock-time 'start clock))
                       (clock-end (org-clone-subtree-cleanly--clock-time 'end clock))
                       (start (if intersected? (car inside) clock-start))
                       (end (if intersected? (cadr inside) clock-end)))
                  (message "Intersection: %S %S" inside (point))
                  (message " %S
 %S
 %S
 %S" clock-start clock-end start-inclusive end-inclusive)

                  (insert (format "CLOCK: %s--%s => %s\n"
                                  (org-clone-subtree-cleanly--format-timestamp start)
                                  (org-clone-subtree-cleanly--format-timestamp end)
                                  "0:00")))))
          )
        (end-of-line)
        ))
  (org-update-clock-times))

(provide 'org-clone-subtree-cleanly)
;;; org-clone-subtree-cleanly.el ends here
