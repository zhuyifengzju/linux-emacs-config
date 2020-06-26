;; Search words
(require 'browse-url)

(defun lookup-word-on-internet (&optional @word @url)
  (interactive)
  (let ($word $refURL $myURL)
    (setq $word
	  (if @word
	      @word
	    (if (region-active-p)
		(buffer-substring-no-properties (region-beginning) (region-end))
	      (current-word))))
    (setq $word (replace-regexp-in-string " " "%20" $word))
    (setq $refURL
	  (if @url
	      @url
	    "http://www.google.com/search?q=word02051" ))
    (setq $myURL (replace-regexp-in-string "word02051" $word $refURL t t))
    (funcall browse-url-browser-function $myURL)
    )
  )

(defun lookup-google (&optional @word)
  (interactive)
  (lookup-word-on-internet
   @word
   "https://www.google.com/search?q=word02051")
  )

(defun lookup-scholar-google (&optional @word)
  (interactive)
  (lookup-word-on-internet
   @word
   "https://scholar.google.com/scholar?q=word02051")
  )

(defun lookup-wikipedia (&optional @word)
  "Look up the word under cursor in wikipedia. This command would switch to browser"
  (interactive)
  (lookup-word-on-internet
   @word
   "https://en.wikipedia.org/wiki/word02051")
  )

(defun lookup-definition (&optional @word)
  (interactive)
  (lookup-word-on-internet
   @word
   "https://www.thefreedictionary.com/word02051")
  )

(defun lookup-etymology (&optional @word)
  (interactive)
  (lookup-word-on-internet
   @word
   "https://www.etymonline.com/search?q=word02051")
  )

;; Independent example of searching in wikipedia
;; (defun lookup-wikipedia ()
;;   "Look up the word under cursor in wikipedia. This command would switch to browser"
;;   (interactive)
;;   (let (word)
;;     (setq word
;; 	  (if (use-region-p)
;; 	      (buffer-substring-no-properties (region-beginning) (region-end))
;; 	    (current-word)))
;;     (setq word (replace-regexp-in-string " " "_" word))
;;     (browse-url (concat "http://en.wikipedia.org/wiki/" word))
;;     ))
