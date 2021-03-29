;;; init-sql.el --- Init for spinner

;; Filename: init-sql.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(defvar-local sql-last-prompt-pos 1
  "position of last prompt when added recording started")
;; (make-variable-buffer-local 'sql-last-prompt-pos)
;; (put 'sql-last-prompt-pos 'permanent-local t)

(defun sql-add-newline-first (output)
  "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'
    This fixes up the display of queries sent to the inferior buffer
    programatically."
  (concat "\n" output))

(defun sqli-add-hooks ()
  "Add hooks to `sql-interactive-mode-hook'."
  (add-hook 'comint-preoutput-filter-functions
            'sql-add-newline-first))

(defun kj/sql-interactive-mode-hook ()
  (sqli-add-hooks)
  (when (is-windows?)
    (set-terminal-coding-system 'utf-8)
    (set-buffer-process-coding-system 'utf-8 'utf-8)
    (set-buffer-file-coding-system 'utf-8)))

(use-package sql
  :config
  (when (is-windows?)
    (setq sql-mysql-options '("-C" "-f" "-t" "-n" "--auto-vertical-output"
                              "--default-character-set=utf8")))
  (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
  (add-hook 'sql-interactive-mode-hook #'kj/sql-interactive-mode-hook))



(provide 'init-sql)

;;; init-sql.el ends here
