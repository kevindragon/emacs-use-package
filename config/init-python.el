;;; init-python.el --- Init for spinner

;; Filename: init-python.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(defun kj/python-mode-hook ()
  (set (make-local-variable 'forward-sexp-function) nil)
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (symbol-overlay-mode))

(use-package python
  :hook (python-mode . kj/python-mode-hook)
  :config
  (setenv "PYTHONIOENCODING" "utf-8"))

(use-package python-pytest)

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp)))
  :config
  (with-eval-after-load "dap-mode"
    (require 'dap-python)))


;;;; ein
(require 'cl-lib)
(require 'company)

(defun kj/ein:complete-handle-reply (callback matches &rest args)
  (funcall callback (append (plist-get matches :matches) nil)))

(defun kj/ein:complete-at-point (callback)
  (let* ((kernel (ein:get-kernel))
         (content (list
                   :code (buffer-substring (line-beginning-position) (point))
                   :cursor_pos (current-column)))
         (msg (ein:kernel--get-msg kernel "complete_request" content))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ein:websocket-send-shell-channel kernel msg)
    (ein:kernel-set-callbacks-for-msg
     kernel msg-id
     `(:complete_reply (kj/ein:complete-handle-reply . ,callback)))))

(defun kj/ein:company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'kj/ein:company-backend))
    (prefix (when (bound-and-true-p ein:notebook-mode)
              (company-grab-symbol-cons "\\." 2)))
    (candidates (cons :async 'kj/ein:complete-at-point))))

(use-package ein)
(with-eval-after-load "ein-notebook"
  (ein:notebook--define-key ein:notebook-mode-map
                            "<tab>" kj/ein:company-backend))



(provide 'init-python)

;;; init-python.el ends here
