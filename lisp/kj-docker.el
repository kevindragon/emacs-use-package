;;; kj-docker.el --- Docker misc

;; Filename: kj-docker.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(defcustom kj/docker-buffer-name "*docker kj*"
  "docker命令运行的buffer name."
  :group 'kj/docker
  :type 'string)

(defcustom kj/docker-container-name "my-container"
  "container的名字."
  :group 'kj/docker
  :type 'string)

(defcustom kj/docker-image-name "my-image"
  "docker image的名字."
  :group 'kj/docker
  :type 'string)

(defun kj/docker-buf ()
  (get-buffer-create kj/docker-buffer-name))

(defun kj/docker--show-side-window (&optional buf-name)
  (let ((buf (or buf-name (kj/docker-buf))))
    (display-buffer-in-side-window buf '((side . bottom) (slot . 1)))
    (with-current-buffer buf
      (goto-char (point-max))
      (set-window-point (get-buffer-window buf) (point-max)))))

(defun kj/docker-pull (name)
  (interactive "sNAME: ")
  (let ((buf (kj/docker-buf)))
    (start-process "kj/docker-pull" buf "docker" "pull" name)
    (kj/docker--show-side-window)))

(defun kj/docker-commit ()
  (interactive)
  (start-process "kj/docker-commit" (kj/docker-buf)
                 "docker" "commit" kj/docker-container-name kj/docker-image-name)
  (kj/docker--show-side-window))

(defun kj/docker-run ()
  (interactive)
  (start-process
   "kj/docker-run" (kj/docker-buf)
   "docker" "run" "-d" "-it"
   "--name" kj/docker-container-name
   "-v" "c:/workspace:/workspace:rw"
   "-w" "/workspace"
   kj/docker-image-name "bash")
  (kj/docker--show-side-window))

(defun kj/docker-stop ()
  (interactive)
  (start-process "kj/docker-stop" (kj/docker-buf)
                 "docker" "stop" kj/docker-container-name)
  (kj/docker--show-side-window))

(defun kj/docker-start ()
  (interactive)
  (start-process "kj/docker-stop" (kj/docker-buf)
                 "docker" "start" kj/docker-container-name)
  (kj/docker--show-side-window))

(defvar *kj/docker-mysql-buf-name* "*docker mysql*")
(defvar *kj/docker-mysql-container-name* "mysql")
(defvar *kj/docker-mysql-image-name* "mysql")

(defun kj/docker-mysql-buf ()
  (get-buffer-create *kj/docker-mysql-buf-name*))

(defun kj/docker-mysql-run ()
  (interactive)
  (start-process
   "kj/docker-mysql" (kj/docker-mysql-buf)
   "docker" "run" "-d"
   "-v" "c:/workspace/Docker/storage/MySQL:/var/lib/mysql:rw"
   "-p" "3306:3306"
   "-e" "MYSQL_ROOT_PASSWORD=qwe123"
   "--name" *kj/docker-mysql-container-name*
   *kj/docker-mysql-image-name*)
  (kj/docker--show-side-window (kj/docker-mysql-buf)))

(defun kj/docker-matomo-run ()
  (interactive)
  (start-process
   "kj/docker-matomo" (kj/docker-mysql-buf)
   "docker" "run" "-d"
   "--link" "mysql:db"
   "--name" *kj/docker-mysql-container-name*
   *kj/docker-mysql-image-name*)
  (kj/docker--show-side-window (kj/docker-mysql-buf)))

(defvar *kj/docker-solr-buf-name* "*docker solr*")
(defvar *kj/docker-solr-container-name* "solr")
(defvar *kj/docker-solr-image-name* "solr")

(defun kj/docker-solr-buf ()
  (get-buffer-create *kj/docker-solr-buf-name*))

(defun kj/docker-solr-run ()
  (interactive)
  (start-process
   "kj/docker-solr" (kj/docker-solr-buf)
   "docker" "run" "-d" "--name" *kj/docker-solr-container-name*
   "-v" "c:/workspace/Docker/storage/Solr-8.8:/var/solr:rw"
   "-p" "8983:8983"
   *kj/docker-solr-image-name*)
  (kj/docker--show-side-window (kj/docker-solr-buf)))



(provide 'kj-docker)

;;; kj-docker.el ends here
