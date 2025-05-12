;;; org-clone-subtree-cleanly.el --- Clone orgmode subtrees without duplicating IDs -*- lexical-binding: t; -*-
;;; Author: Hauke Rehfeld <emacs@haukerehfeld.de>
;;; Version: 0.1
;;; Package-Requires: ((emacs "26.1"))
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


(defun org-clone-subtree-cleanly--timestamp-get-props (type timestamp)
  (cl-assert (memq type '(start end)) t "TYPE must be 'start or 'end")
  (cl-assert (org-element-type timestamp) t "TIMESTAMP must be an org-element of type timestamp")
  (cl-loop
   for prop in org-clone-subtree-cleanly--clock-properties
   collect
   (let ((prop-sym (intern (format "%s-%s" prop type))))
     (org-element-property prop-sym timestamp))))

(defun org-clone-subtree-cleanly--timestamp-time (type timestamp)
  "Get the time of a timestamp entry as a list in the order of `org-clone-subtree-cleanly--clock-properties'. `TYPE' can be 'start or 'end."
  (cl-assert (memq type '(start end)) t "TYPE must be 'start or 'end")
  (cl-assert (org-element-type timestamp) t "TIMESTAMP must be an org-element of type timestamp")
  (cl-loop
   for prop in org-clone-subtree-cleanly--clock-properties
   collect
   (let ((prop-sym (intern (format "%s-%s" prop type))))
     (org-element-property prop-sym timestamp))))

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

(defun org-clone-subtree-cleanly--clock-or-timestamp< (type time-a time-b)
  (cl-assert (memq type '(start end)) t "TYPE must be 'start or 'end")
  (cl-assert (memq (org-element-type time-a) '(timestamp clock)) t "TIME-A must be an org-element of type timestamp or clock")
  (cl-assert (memq (org-element-type time-b) '(timestamp clock)) t "TIME-B must be an org-element of type timestamp or clock")
  (when (eq (org-element-type time-a) 'clock)
    (setq time-a (org-element-property :value time-a))
    (cl-assert (eq (org-element-type time-a) 'timestamp) t "TIME-A must be an org-element of type timestamp"))

  (when (eq (org-element-type time-b) 'clock)
    (setq time-b (org-element-property :value time-b))
    (cl-assert (eq (org-element-type time-b) 'timestamp) t "TIME-B must be an org-element of type timestamp"))

  (cl-labels ((safe-clock-property (key timestamp)
                (let ((prop-value (org-element-property key timestamp))
                      (default
                       (cond ((member key '(:hour-start :hour-end
                                            :minute-start :minute-end
                                            :second-start :second-end))
                              0)
                             ((member key '(:year-start :year-end
                                            :month-start :month-end
                                            :day-start :day-end))
                              1))))
                  (or prop-value default))))

    (cl-loop
     for p in org-clone-subtree-cleanly--clock-properties
     do
     (let ((p (intern (format "%s-%s" p type))))
       (let ((a (safe-clock-property p time-a))
             (b (safe-clock-property p time-b)))
         (message "org-clone-subtree-cleanly--clock-or-timestamp<: %S %S %S %S" (point) p time-a time-b)
         (cl-check-type a number-or-marker t)
         (cl-check-type b number-or-marker t)
         (when (not (eq a b))
           (cl-return (< a b))))))))
;; (org-clone-subtree-cleanly--clock-or-timestamp< 'start (org-clone-subtree-cleanly--parse-timestamp "[2024-04-16 Tue 13:05]") (org-clone-subtree-cleanly--parse-timestamp "[2024-04-16 Tue 13:06]"))

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


(defun org-clone-subtree-cleanly-clock-split (clock split-time)
  "Split the time range in CLOCK at SPLIT-TIME, returning `(list before-clock after-clock)'.

Either of the returned clocks can be nil if all of CLOCK is
before or after SPLIT-TIME."
  (cl-assert (eq (org-element-type clock) 'clock) t "CLOCK must be an org-element of type clock")
  (cl-assert (eq (org-element-type split-time) 'timestamp) t "SPLIT-TIME must be an org-element of type timestamp")
  (cond
   ;; split before clock
   ((org-clone-subtree-cleanly--clock-or-timestamp< 'start split-time clock)
    (list nil clock))
   ;; split after clock
   ((org-clone-subtree-cleanly--clock-or-timestamp< 'end clock split-time)
    (list clock nil))
   ;; split inside clock
   (t
    (list
     (org-clone-subtree-cleanly--combine-clock
      (org-element-property :value clock)
      split-time
      clock)
     (org-clone-subtree-cleanly--combine-clock
      split-time
      (org-element-property :value clock)
      clock)))))

;; (org-clone-subtree-cleanly-clock-split
;;  (org-clone-subtree-cleanly--parse-clock "CLOCK: [2024-04-16 Tue 13:05]--[2024-04-16 Tu 14:09] => 0:09")
;;  (org-clone-subtree-cleanly--parse-timestamp "[2024-04-16 Tue 13:10]"))

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
  (format "[%s]" (org-read-date nil nil nil prompt nil default-input)))
;; (org-clone-subtree-cleanly--read-timestamp "test" nil)


(defun org-clone-subtree-cleanly--combine-clock (start end &optional clock)
  (cl-assert (org-element-type start) t "START must be an org-element of type timestamp")
  (cl-assert (org-element-type end) t "END must be an org-element of type timestamp")
  (let* ((clock (or clock (org-element-create 'clock)))
         (start-props (cadr start))
         (end-props (cadr end))
         (selected-props
          (cl-loop
           for (type value) in `((start ,start) (end ,end))
           append
           (cl-loop
            for prop in org-clone-subtree-cleanly--clock-properties
            append
            (let* ((prop-sym (intern (format "%s-%s" prop type))))
              (list prop-sym (org-element-property prop-sym value))))))
         (all-props (append selected-props start-props end-props)))
    ;;(message "%S %S %S" selected-props start-props end-props)
    (org-element-put-property clock :value (org-element-create 'timestamp all-props))))
;; (org-clone-subtree-cleanly--combine-clock (org-clone-subtree-cleanly--parse-timestamp "[2024-04-16 Tue 13:05]") (org-clone-subtree-cleanly--parse-timestamp "[2024-04-16 Tue 14:05]"))


(defun org-clone-subtree-cleanly-timerange (start-inclusive end-inclusive &optional arg)
  ;; get start-inclusive and end-inclusive from org-read-date
  (interactive (list (org-clone-subtree-cleanly--read-timestamp "Cull earlier than" org-clone-subtree-cleanly-last-start)
                     (org-clone-subtree-cleanly--read-timestamp "Cull later than" org-clone-subtree-cleanly-last-end)
                     current-prefix-arg))
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
               (intersection (org-clone-subtree-cleanly-clock-intersection clock start-inclusive end-inclusive))
               (intersected? (not (eq intersection t))))
          ;;(message "Line: %s" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          (when (or (not intersection) intersected?)
            (delete-line))
          (if intersection
              (let* ((clock-start (org-clone-subtree-cleanly--clock-time 'start clock))
                     (clock-end (org-clone-subtree-cleanly--clock-time 'end clock))
                     (start (if intersected? (car intersection) clock-start))
                     (end (if intersected? (cadr intersection) clock-end)))
                ;;(message "Intersection: %S %S" intersection (point))
                ;; (message " %S
                ;; %S
                ;; %S
                ;; %S" clock-start clock-end start-inclusive end-inclusive)

                (insert (format "CLOCK: %s--%s => %s\n"
                                (org-clone-subtree-cleanly--format-timestamp start)
                                (org-clone-subtree-cleanly--format-timestamp end)
                                "0:00")))))
        )
      (end-of-line)
      ))
  )

(defvar-local org-clone-subtree-cleanly-last-split-time nil)


(defun org-clone-subtree-cleanly-split-clocks (split-time keep-before?)
  (let* ((clock-fn (if keep-before? #'car #'cadr))
         (subtree-end (make-marker)))
    (set-marker subtree-end(save-excursion (org-end-of-subtree t)))
    ;; all clock drawers
    (while (search-forward-regexp org-drawer-regexp subtree-end t)
      (let* ((drawer-name (match-string 1))
             (is-clock-drawer? (string-equal drawer-name (org-clock-drawer-name))))
        (if (not is-clock-drawer?)
            (goto-char (match-end 0))
          ;;(message "clock drawer at %s" (point))
          ;; all clock lines
          (let* ((drawer-end-position (save-excursion (search-forward-regexp org-clock-drawer-end-re subtree-end t))))
            (when drawer-end-position
              (let* ((drawer-end (make-marker))
                     (drawer-end (set-marker drawer-end drawer-end-position)))

                ;; update subtree end and drawer end after changes
                (while (search-forward-regexp org-clock-line-re drawer-end t)
                  ;;(message "Line: %s" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                  (beginning-of-line)
                  ;; parse clock entries
                  (let* ((clock (org-element-clock-parser (line-end-position)))
                         (split-info (org-clone-subtree-cleanly-clock-split clock split-time))
                         (split? (and (car split-info) (cadr split-info)))
                         (valid-clock (funcall clock-fn split-info)))
                    (end-of-line)
                    (when (or split? (not valid-clock))
                      (delete-line)
                      )

                    (when split?
                      (insert (format "CLOCK: %s\n" (org-element-clock-interpreter valid-clock))))))))))))))

(defun org-clone-subtree-cleanly-subtree-has-clock-entries-p ()
  "Check if the current subtree has clock entries."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (let ((has-clock nil))
        (while (and (not has-clock) (re-search-forward org-clock-string nil t))
          (setq has-clock t))
        has-clock))))

(defun org-clone-subtree-cleanly-subtree-map-headings (fn)
  "Map over current all headings in the current subtree until FN returns 'return.

Return 'return if FN returns 'return for any heading."
  (interactive)
  (save-excursion
    (save-restriction
      (org-back-to-heading t)
      (org-narrow-to-subtree)
      (let ((return? nil))
        (while (and (not return?) (re-search-forward org-heading-regexp nil t))
          (let ((heading-end (make-marker)))
            (set-marker heading-end (match-end 0))
            ;;(message "Line before changes: %s" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          (goto-char (match-beginning 0))
          (when (eq (save-excursion (funcall fn)) 'return)
            ;;(message "org-clone-subtree-cleanly-subtree-map-headings: found return")
            (setq return? 'return))
          (goto-char heading-end)
          ;;(message "Line after changes: %s" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          ))
        return?))))

(defun org-clone-subtree-cleanly-subtree-done? ()
  (interactive)
  (not (eq (org-clone-subtree-cleanly-subtree-map-headings
            (lambda ()
              (when (and (org-get-todo-state) (not (org-entry-is-done-p)))
                ;; stop iteration when a todo is found
                ;;(message "org-clone-subtree-cleanly-subtree-done?: found todo. %S %S" (org-get-todo-state) (not (org-entry-is-done-p)))
                'return)))
           'return)))

(defun org-clone-subtree-cleanly-subtree-kill-headings-without-clock-entries (&optional keep-todos?)
  (interactive current-prefix-arg)
  (org-clone-subtree-cleanly-subtree-map-headings
   (lambda ()
     ;;(message "Line: %s (%S %S %S)" (buffer-substring-no-properties (line-beginning-position) (line-end-position)) (org-clone-subtree-cleanly-subtree-has-clock-entries-p) keep-todos? (not (org-clone-subtree-cleanly-subtree-done?)))
     (unless (or (org-clone-subtree-cleanly-subtree-has-clock-entries-p)
                 ;; optionally kill subtrees without todos
                 (and keep-todos? (not (org-clone-subtree-cleanly-subtree-done?))))
       (org-cut-subtree)))
   ))

(defun org-clone-subtree-cleanly-split-timerange (split-time)
  "Clone current subtree, and keep clock times later than SPLIT-TIME in original subtree.

Cloned subtree will only contain non-DONE headings and headings with clock times earlier than SPLIT-TIME."
  ;; get start-inclusive and end-inclusive from org-read-date
  (interactive (list (org-clone-subtree-cleanly--read-timestamp "Split at: " org-clone-subtree-cleanly-last-split-time)))
  (setq org-clone-subtree-cleanly-last-split-time split-time)
  (let ((split-time (org-clone-subtree-cleanly--parse-timestamp split-time)))
    (org-clone-subtree-cleanly)
    (save-excursion
      (org-backward-heading-same-level 1)
      (save-excursion
        (org-clone-subtree-cleanly-split-clocks split-time nil))
      (org-clone-subtree-cleanly-subtree-kill-headings-without-clock-entries t))
    (save-excursion
      (org-clone-subtree-cleanly-split-clocks split-time t))
    (save-excursion
      (org-clone-subtree-cleanly-subtree-kill-headings-without-clock-entries))))
(provide 'org-clone-subtree-cleanly)
;;; org-clone-subtree-cleanly.el ends here
