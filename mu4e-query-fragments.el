;;; mu4e-query-fragments.el --- mu4e query fragments extension  -*- lexical-binding: t -*-

;; Author: Yuri D'Elia <wavexx@thregr.org>
;; Version: 1.0
;; URL: https://github.com/wavexx/mu4e-query-fragments.el
;; Package-Requires: ((emacs "24.4"))
;; Keywords: mu4e, mail, convenience

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; `mu4e-query-fragments' allows to define query snippets ("fragments")
;; that can be used in regular `mu4e' searches or bookmars. Fragments
;; can be used to define complex filters to apply in existing searches,
;; or supplant bookmarks entirely. Fragments compose properly with
;; regular mu4e/xapian operators, and can be arbitrarily nested.
;;
;; `mu4e-query-fragments' can also append a default filter to new
;; queries, using `mu4e-qf-append-to-new-queries'. Default filters are
;; very often useful to exclude junk messages from regular queries.
;;
;; To use `mu4e-query-fragments', use the following:
;;
;; (require 'mu4e-query-fragments)
;; (setq mu4e-qf-fragments
;;   '(("%junk" . "maildir:/Junk OR subject:SPAM")
;;     ("%hidden" . "flag:trashed OR %junk")))
;; (setq mu4e-qf-append-to-new-queries "AND NOT %hidden")
;;
;; The terms %junk and %hidden can subsequently be used anywhere in
;; mu4e. See the documentation of `mu4e-qf-fragments' for more details.
;;
;; Fragments are *not* shown expanded in order to keep the modeline
;; short. To test an expansion, use `mu4e-qf-query-expand'.

;;; Code:

(require 'mu4e)

;;;###autoload
(defvar mu4e-qf-fragments nil
  "Define query fragments available in `mu4e' searches and bookmarks.
List of (FRAGMENT . EXPANSION), where FRAGMENT is the string to be
substituted and EXPANSION is the query string to be expanded.

FRAGMENT should be an unique text symbol that doesn't conflict with the
regular mu4e/xapian search syntax or previous fragments. EXPANSION is
expanded as (EXPANSION), composing properly with boolean operators and
can contain fragments in itself.

Example:

\(setq mu4e-qf-fragments
   '((\"%junk\" . \"maildir:/Junk OR subject:SPAM\")
     (\"%hidden\" . \"flag:trashed OR %junk\")))")

;;;###autoload
(defvar mu4e-qf-append-to-new-queries nil
  "Query fragment appended to new searches by `mu4e-qf-search-headers'.")

(defun mu4e-qf--expand-1 (frags str)
  (if (null frags) str
    (with-syntax-table (standard-syntax-table)
      (let ((case-fold-search nil))
	(replace-regexp-in-string
	 (regexp-opt (mapcar 'car frags) 'symbol)
	 (lambda (it) (cdr (assoc it frags)))
	 str t t)))))

;;;###autoload
(defun mu4e-qf-query-expand (query)
  "Expand fragments defined in `mu4e-qf-fragments' in QUERY."
  (let (tmp (frags (mapcar (lambda (entry)
			     (cons (car entry) (concat "(" (cdr entry) ")")))
			   mu4e-qf-fragments)))
    ;; expand recursively until nothing is substituted
    (while (not (string-equal
		 (setq tmp (mu4e-qf--expand-1 frags query))
		 query))
      (setq query tmp)))
  query)

(defun mu4e-qf--proc-find-query-expand (args)
  (let ((query (car args))
	(rest (cdr args)))
    (cons (mu4e-qf-query-expand query) rest)))

(advice-add 'mu4e~proc-find :filter-args 'mu4e-qf--proc-find-query-expand)

(defun mu4e-qf-headers-search (&optional arg)
  "Search for EXPR and switch to the output buffer for the results.
Like `mu4e-headers-search', but appends `mu4e-qf-append-to-new-queries'
at the end of the query if called without a prefix argument."
  (interactive "P")
  (if (or (not (null arg)) (null mu4e-qf-append-to-new-queries))
      (mu4e-headers-search)
    (let ((expr (read-string "Search for: " nil 'mu4e~headers-search-hist)))
      (mu4e-headers-search (concat expr " " mu4e-qf-append-to-new-queries)))))

(define-key mu4e-headers-mode-map (kbd "s") 'mu4e-qf-headers-search)

(provide 'mu4e-query-fragments)

;;; mu4e-query-fragments.el ends here
