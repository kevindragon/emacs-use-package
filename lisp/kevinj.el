;; -*- lexical-binding: t; -*-
;;; kevinj.el --- Config load when kevinj

;; Filename: kevinj.el
;; Description: Config load when kevinj
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2014, Kevin Jiang, all rights reserved.

;;; Require

(require 's)
(require 'projectile)

;;; Code:

(defvar *w32-notification-id* 0)

(defun kj/clean-notifies ()
  (interactive)
  (w32-notification-close *w32-notification-id*))

(defun kj/newline-and-indent ()
  "在当前行下面添加一个空白行，并且缩进."
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-for-tab-command))

(global-set-key (kbd "C-S-o") 'kj/newline-and-indent)

;; window split horizontal and vertical toggle
(defun kj/toggle-window-split ()
  "切换两个窗口的横竖模式，当只有两个窗口的时候才生效."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'kj/toggle-window-split)

;; maximum frame
(global-set-key (kbd "C-x w m") 'toggle-frame-maximized)

;; 设置窗口透明
(setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       (message "alpha is %s" a))
     (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))))
(global-set-key [(f11)] 'loop-alpha)

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun kj/insert-current-datetime ()
  "插入当前时间"
  (interactive)
  (let ((current-date-time-format "%Y-%m-%d %H:%M:%S"))
    (insert (format-time-string current-date-time-format))))

(defun kj/insert-current-date ()
  "插入当前时间"
  (interactive)
  (let ((current-date-time-format "%Y-%m-%d"))
    (insert (format-time-string current-date-time-format))))

(defun kj/copy-buffer-file-name ()
  "复制buffer对应的文件的绝对路径."
  (interactive)
  (when buffer-file-name
    (kill-new buffer-file-name)))

(defun kj/copy-buffer-name ()
  "复制buffer对应的文件的文件名，不包含路径."
  (interactive)
  (kill-new (buffer-name)))

(defun kj/copy-project-file-name ()
  "复制项目文件的相对根目录的路径"
  (interactive)
  (when buffer-file-name
    (kill-new
     (s-replace
      (or (ignore-errors (projectile-project-root)) "")
      "" buffer-file-name))))

(defun kj/uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))

(defun kj/move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun kj/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (kj/move-text-internal arg))

(defun kj/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (kj/move-text-internal (- arg)))

(global-set-key (kbd "<M-S-up>") 'kj/move-text-up)
(global-set-key (kbd "<M-S-down>") 'kj/move-text-down)

(defun kj/url-decode (s)
  (decode-coding-string (url-unhex-string s) 'utf-8))

(defun kj/url-encode (s)
  (url-hexify-string s))


(defun kj/sync-to-remote ()
  (interactive)
  (when (and (boundp 'project-enable-remote) project-enable-remote)
    (let* ((relative-path (s-replace
                           (or (ignore-errors (projectile-project-root)) "")
                           "" buffer-file-name))
           (default-directory (file-name-as-directory (projectile-project-root)))
           (buf-name (format "*[scp %s]*" (projectile-project-root)))
           (buffer (get-buffer-create buf-name)))
      (when (not (buffer-live-p buffer))
        (with-current-buffer buffer
	  (comint-mode)
	  (goto-char (point-max))
	  (insert (format "working dir at %s\n" default-directory))
	  (display-buffer buffer)
	  (set-terminal-coding-system 'utf-8)
	  (set-buffer-file-coding-system 'utf-8)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (kj/insert-current-datetime))
      (set-process-sentinel
       (start-process buf-name buffer "C:/Program Files/Git/usr/bin/scp.exe"
                      buffer-file-name
                      (format "%s@%s:%s/%s"
                              remote-user
                              remote-host
                              remote-path
                              relative-path))
       (lambda (process event)
         (message "%s %s uploaded, %s"
                  (format-time-string "%Y-%m-%d %H:%M:%S")
                  relative-path
                  (s-trim event)))))))

(add-hook 'after-save-hook 'kj/sync-to-remote)


(defun kj/flask-buffer-name (dir)
  (format "*flask[%s]*" dir))

(defun kj/flask-buffer (dir)
  (get-buffer-create (kj/flask-buffer-name dir)))

(defun kj/flask-buffer-process (dir)
  (let ((buffer (kj/flask-buffer dir)))
    (get-buffer-process buffer)))

(defun kj/flask-run-server ()
  (interactive)
  (let* ((default-directory (projectile-project-root))
	 (buf-name (kj/flask-buffer-name default-directory))
	 (buffer (kj/flask-buffer default-directory)))
    (with-current-buffer buffer
      (comint-mode)
      (goto-char (point-max))
      (insert (format "working dir at %s\n" default-directory))
      (display-buffer buffer)
      (set-terminal-coding-system 'utf-8)
      (set-buffer-file-coding-system 'utf-8))
    (start-process buf-name buffer "python" "-u" "app/server.py")))

(defun kj/flask-stop-server ()
  (interactive)
  (let ((default-directory (projectile-project-root))
        (proc (kj/flask-buffer-process default-directory)))
    (when (process-live-p proc)
      (kill-process proc))))

(defun kj/flask-restart-server ()
  (interactive)
  (let ((default-directory (projectile-project-root))
        (proc (kj/flask-buffer-process default-directory)))
    (kj/flask-stop-server)
    (kj/flask-run-server)))

(defun kj/vue-buffer-name (dir)
  (format "*vue[%s]*" dir))

(defun kj/vue-buffer (dir)
  (get-buffer-create (kj/vue-buffer-name dir)))

(defun kj/vue-buffer-process (dir)
  (get-buffer-process (kj/vue-buffer dir)))

(defun kj/vue-dir ()
  (if (and (boundp 'project-vue-dir) project-vue-dir)
      (file-name-as-directory
       (expand-file-name project-vue-dir (projectile-project-root)))
    (projectile-project-root)))

(defun kj/vue-run-server ()
  (interactive)
  (let* ((default-directory (kj/vue-dir))
	 (buf-name (kj/vue-buffer-name default-directory))
	 (buffer (kj/vue-buffer default-directory)))
    (with-current-buffer buffer
      (comint-mode)
      (goto-char (point-max))
      (insert (format "working dir at %s\n" default-directory))
      (display-buffer buffer)
      (set-terminal-coding-system 'utf-8)
      (when (process-live-p *vue-server-proc*)
	(set-buffer-process-coding-system 'utf-8 'utf-8))
      (set-buffer-file-coding-system 'utf-8))
    (start-process buf-name buffer "npm" "run" "serve")))

(defun kj/vue-stop-server ()
  (interactive)
  (let* ((proc (kj/vue-buffer-process (kj/vue-dir))))
    (when (process-live-p proc)
      (kill-process proc))))

(defun kj/vue-restart-server ()
  (interactive)
  (kj/vue-stop-server)
  (kj/vue-run-server))

(defun kj/vue-build ()
  (interactive)
  (when (> *w32-notification-id* 0)
    (w32-notification-close *w32-notification-id*))
  (let* ((default-directory (kj/vue-dir)))
    (set-process-sentinel
     (start-process "npm" "*npm run build*" "npm" "run" "build")
     (lambda (process event)
       (setq *w32-notification-id*
             (w32-notification-notify
              :title "npm run build"
              :body (format "%s" event)))))))


;;; npm
(defcustom kj/npm-buffer-name "*npm kj*"
  "docker命令运行的buffer name."
  :group 'kj/npm
  :type 'string)

(defun kj/npm-buf ()
  (get-buffer-create kj/npm-buffer-name))

(defun kj/npm--show-side-window ()
  (let ((buf (kj/npm-buf)))
    (display-buffer-in-side-window buf '((side . bottom) (slot . 1)))
    (with-current-buffer buf
      (goto-char (point-max))
      (set-window-point (get-buffer-window buf) (point-max)))))

(defun kj/npm-update (pkg-name)
  (interactive "sPackage name: ")
  (let* ((global (y-or-n-p "Update globally installed packages?"))
         (opts (if global "-g" nil)))
    (start-process "npm" (kj/npm-buf) "npm" "update" opts pkg-name)
    (kj/npm--show-side-window)))


(defun kj/pip-install ()
  (interactive)
  (let* ((name (read-string "Type package name: "))
         (buf (get-buffer-create "*kj/pip*")))
    (start-process "pip" buf "pip" "install" name)
    (display-buffer-in-side-window buf '((side . bottom) (slot . 1)))))


(defun kj/display-shell (buf-name)
  "在side window ((side . bottom) (slot . 1))显示shell."
  (interactive (list (completing-read
                      "Select a buffer:"
                      (-filter (lambda (x) (string-prefix-p "*shell" x))
                               (mapcar 'buffer-name (buffer-list))))))
  (let ((buf (get-buffer buf-name)))
    (display-buffer-in-side-window buf '((side . bottom) (slot . 1)))
    (switch-to-buffer-other-window buf)))

(defun kj/display-side-window (buf-name)
  (interactive (list (completing-read
                      "Select a buffer:"
                      (mapcar 'buffer-name (buffer-list)))))
  (let ((buf (get-buffer buf-name)))
    (display-buffer-in-side-window buf '((side . bottom) (slot . 1)))
    (switch-to-buffer-other-window buf)))



(provide 'kevinj)

;;; kevinj.el ends here
