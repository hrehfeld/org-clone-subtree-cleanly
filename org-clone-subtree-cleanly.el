;;; org-clone-subtree-cleanly.el --- Clone orgmode subtrees without duplicating IDs
;;; -*- lexical-binding: t; -*-
;;; Author: Hauke Rehfeld <emacs@haukerehfeld.de>
;;; Version: 0.1
;;; Package-Requires: ((emacs "24.1") org)
;;; Keywords: convenience
;;; URL: https://github.com/hrehfeld/org-clone-subtree-cleanly
;;; Commentary:

;;; Code:
(require 'org)

;;;###autoload
(defun org-clone-subtree-cleanly ()
  "Clone a subtree, but delete all ID properties to avoid duplicate IDs."
  (interactive)
  (org-copy-subtree)
  (org-back-to-heading)
  (org-yank)
  (org-map-entries (lambda () (org-delete-property "ID")) nil 'tree)
  )

(provide 'org-clone-subtree-cleanly)
;;; org-clone-subtree-cleanly.el ends here
