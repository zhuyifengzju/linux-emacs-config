;;; init.el -- Bootstrap org-init.org file
;;; Commentary:

;;; Code:

;; load the preload-custom.el
(setq custom-file (format "%spreload-custom.el" user-emacs-directory))
(if (not (file-exists-p custom-file))
    (with-temp-buffer
      (insert "(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
")
(write-file custom-file)))
(load custom-file)

;; Setup custom group
(defgroup rhol-emacs nil
  "Customizable options for rhol Emacs config."
  :group 'emacs
  :prefix "rhol-"
  :tag "Rhol Emacs Configuration")

;; Setup customize file location
(defcustom init--custom-file nil
  "Location of `custom-file'."
  :type 'file
  :group 'rhol-emacs
  :tag "Custom File Path")

;; If preload-custom.el does not have the init--custom-file set prompt
;; user for where they want to store customizations
(if init--custom-file
    nil
  (customize-save-variable 'init--custom-file
			   (format "%s%s" user-emacs-directory
				   (let ((system-custom-file-name (format "system-%s-custom.el" (system-name))))
					   (let ((choice (completing-read
							  (format "Where do you want to store customizations? ((c)ustom.el (s)%s): "
								  (substring system-custom-file-name 1))
							  '(("c" 1)
							    ("s" 2))
							  nil t "s")))
					     (cond
					      ((string= choice "c")
					       "custom-file.el")
					      ((string= choice "s")
					       (format "%s" system-custom-file-name))))))))

;; now change the custom-file to specified custom file
(setq custom-file init--custom-file)
;; create default file if doesn't exist
(if (not (file-exists-p custom-file))
    (with-temp-buffer
      (insert "(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
")
(write-file custom-file)))
;; reload the custom file
(load custom-file)

;; Setup Package archives
(require 'package)

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; setup melpa
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; Pre-install required packages for rest of config
(setq package-list
      '(use-package))

;; activate all packages
(package-initialize)

;; fetch list of packages available
(unless package-archive-contents
  (progn
    (message "Refreshing package contents...")
    (package-refresh-contents)))

;; Install specified packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (progn (message "Installing package %s" package)
	   (package-install package))))

;; Load use-package
(require 'use-package)
(use-package use-package-ensure-system-package
  :ensure t)

;; setup cask
(defvar init--cask-folder
  (expand-file-name "~/.cask")
  "Folder that Cask package manager is installed to.")

(defun init--install-cask ()
  "Install cask on system."
  (if (yes-or-no-p "Run command 'curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python' ")
      (shell-command "curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python")
    (message "Skipping cask install.")))

(defun init--change-cask-var ()
  "Prompt user for new cask-folder location."
  (setq init--cask-folder
	(read-file-name "Enter the path to the cask folder " nil init--cask-folder t nil 'file-directory-p)))

(defun init--cask-folder-correct-p ()
  "Check that cask folder exists."
  (file-directory-p init--cask-folder))

(defun init--check-cask-folder ()
  "Check cask folder, and handle cases where invalid."
  (if (not (init--cask-folder-correct-p))
      (progn
	(let ((choice (completing-read
		       (format
			"cask-folder %s does not exist.\nChoose an option ((I)nstall cask, (C)hange cask location, (Q)uit): "
			init--cask-folder)
		       '(("I" 1)
			 ("C" 2)
			 ("Q" 3))
		       nil t "I")))
	  (cond
	   ((string= choice "I")
	    (init--install-cask))
	   ((string= choice "C")
	    (init--change-cask-var))
	   ((string= choice "Q")
	    (message "Ignoring invalid cask folder. You will likely have errors."))
	   (t
            (user-error "Unknown choice %s" choice)))))))

(init--check-cask-folder)

(use-package cask
  :load-path init--cask-folder
  :config
  (let ((cask-bundle (cask-setup user-emacs-directory)))
    (cask-initialize user-emacs-directory)))

;; Setup Pallet
;; This package automatically adds manually installed pacakges to the Cask file
(use-package pallet
  :ensure t
  :config
  (pallet-mode t))

;; ;; setup cus-edit+
;; ;; Better customize behavior
;; (use-package cus-edit+
;;   :ensure t)

;; load org mode
(use-package org
  :ensure t
  :pin manual)

;; Ensure dash is available
(use-package dash
  :ensure t)

;; force reload of org to get the new version, not the built in
;; version
(org-reload)

;; tangle the org init file
(org-babel-load-file (concat user-emacs-directory "org-init.org"))

;;; init.el ends here
