;;; plain-org-wiki.el --- Simple jump-to-org-files in a directory package

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Version: 0.1.0
;; Keywords: completion

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(use-package helm)

(defgroup plain-org-wiki nil
  "Simple jump-to-org-file package."
  :group 'org
  :prefix "plain-org-wiki-")

(defcustom pow-directory "~/Dropbox/org-mode/wiki/"
  "Directory where files for `plain-org-wiki' are stored."
  :type 'directory)

(cond
 ((string= system-name "rhol-desktop")
  (setq pow-directory "~/workspace/personal_wiki"))
 ((string= system-name "devins-macbookpro")
  (setq pow-directory "~/workspace/personal_wiki"))
 ((string= system-name "gs15624.sp.cs.cmu.edu")
  (setq pow-directory "~/workspace/personal_wiki"))
 ((string= system-name "devin-lenovo")
  (setq pow-directory "~/workspace/personal_wiki"))
 ((string= system-name "dschwab-macbookpro.roam.corp.google.com")
  (setq pow-directory "~/Google Drive/org-mode/wiki/"))
 ((string= system-name "dschwab.lon.corp.google.com")
  (setq pow-directory "~/gdrive/org-mode/wiki/"))
 (t
  (setq pow-directory "~/Dropbox/org-mode/wiki")))

(defun pow-files ()
  "Return .org files in `pow-directory'."
  (let ((default-directory pow-directory))
    (mapcar #'file-name-sans-extension
            (file-expand-wildcards "*.org"))))

(defun pow-files-recursive ()
  "Return .org files in `pow-directory' and subdirectories."
  (let ((ffip-project-root pow-directory))
    (delq nil
          (mapcar (lambda (x)
                    (when (equal (file-name-extension (car x)) "org")
                      (file-name-sans-extension (car x))))
                  (ffip-project-files)))))

(defun pow-find-file (x)
  "Open X as a file with org extension in `pow-directory'."
  (message "Opening file %s.org" x)
  (find-file (expand-file-name
              (format "%s.org" x)
              pow-directory)))

(defvar wiki-pages-source '((name . "Wiki Pages")
		     (candidates . pow-files)
		     (action . (("open" . pow-find-file)))))

(defvar new-wiki-page-source (helm-build-dummy-source
				 "Create Page"
			       :action (helm-make-actions
					"Create Page"
					;; (lambda (x) (pow-find-file x))
					'pow-find-file
					)))

;;;###autoload
(defun plain-org-wiki-helm ()
  "Select an org-file to jump to."
  (interactive)
  (require 'helm)
;;  (require 'helm-match-plugin)
  (helm :sources '(wiki-pages-source new-wiki-page-source)))

(provide 'plain-org-wiki)

(defun pow-search-files ()
  "Search all files in the wiki directory for the given regex."
  (interactive)
  (helm-ag pow-directory))

;;; plain-org-wiki.el ends here
