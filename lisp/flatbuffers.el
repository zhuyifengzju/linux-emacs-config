;;; flatbuffers-mode --- Major mode for flatbuffer messages

;;; Commentary:

;; Used for editing flatbuffers mode

;;; Code:

;; define keyword categories
(setq flatbuffers-types '("bool" "byte" "ubyte" "short" "ushort" "int" "uint" "float" "long" "ulong" "double" "string"))
(setq flatbuffers-keywords '("enum" "namespace" "table" "struct" "union" "root_type"))

;; generate regexp for keywords
(setq flatbuffers-types-regexp (regexp-opt flatbuffers-types 'words))
(setq flatbuffers-keywords-regexp (regexp-opt flatbuffers-keywords 'words))

(setq flatbuffers-font-lock-keywords
      `(
	(,flatbuffers-types-regexp . font-lock-type-face)
	(,flatbuffers-keywords-regexp . font-lock-keyword-face)))

(defvar flatbuffers-indent-offset 2
  "*Indentation offset for `flatbuffers-mode'.")

(defun flatbuffers-indent-line ()
  "Indent current line for `flatbuffers-mode'."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "[[{]")
              (setq indent-col (+ indent-col flatbuffers-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "[]}]") (>= indent-col flatbuffers-indent-offset))
        (setq indent-col (- indent-col flatbuffers-indent-offset))))
    (indent-line-to indent-col)))

(defvar flatbuffers-mode-hook '())

(define-derived-mode flatbuffers-mode prog-mode "flatbuffers-mode"
  "Major mode for editing Flatbuffer Messages"
  (setq font-lock-defaults '((flatbuffers-font-lock-keywords)))
  (setq comment-start "// ")
  (setq comment-end "")
  (setq indent-line-function 'flatbuffers-indent-line)
  (setq tab-width flatbuffers-indent-offset)
  (setq indent-tabs-mode nil))

(provide 'flatbuffers-mode)


;; Local Variables:
;; coding: utf-8
;; End:

;;; flatbuffers.el ends here
