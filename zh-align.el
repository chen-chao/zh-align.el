;;; zh-align.el, let Chinese font's alignment be compatible with English font in Emacs
;; Keywords: Chinese, font, alignment
;; Author: Chen Chao <cchao.im@gmail.com>
;; License: MIT
;; URL: https://github.com/chen-chao/emacs-zh-align

;; Usage:
;;   (setq zh-align-charsets '(han kana cjk-misc))
;;   (zh-align-set-faces '(faces))

;;; Code:

(defgroup zh-align nil
  "Let Chinses font size be compatible with English font in Emacs"
  :prefix "zh-align-"
  :group 'align)

(defcustom zh-align-charsets '(han kana cjk-misc)
  "Specify charsets or scripts to create zh-align--fontset"
  :group 'zh-align
  :type 'list)

(defcustom zh-align-fontwidth-ratio 2
  "Specify the ratio of Chinese font width to English font"
  :group 'zh-align
  :type 'number)

(defun zh-align--screen-char-width (s)
  "Return the width in pixels of character s in the current
window's default font. If the font is mono-spaced, this will also
be the width of all other printable characters."
  (let ((window (selected-window))
	(remapping face-remapping-alist))
    (with-temp-buffer
      (make-local-variable 'face-remapping-alist)
      (setq face-remapping-alist remapping)
      (set-window-buffer window (current-buffer))
      (insert s)
      (aref (aref (font-get-glyphs (font-at 1) 1 2) 0) 4))))


(defun zh-align--screen-char-height (s)
  (aref (font-info (face-font 'default nil s)) 2))

(defun zh-align--set-fontset-size (fset charset font size)
  (set-fontset-font fset charset
		    (font-spec :family font :size size)))

(defun zh-align--get-char-from-charset (charset)
  "Get an example character from a given charset"
  (let* ((range (plist-get (charset-plist charset) :code-space))
	 (i (aref range 0))
	 (max (aref range 1))
	 (min2 (aref range 2))
	 (max2 (aref range 3))
	 (row (/ (+ min2 max2) 2))
	 ch)
    (catch 'loop
      (while (< i max)
	(if (setq ch (decode-char charset (+ (* row 256) i)))
	    (throw 'loop ch)
	  (setq i (1+ i)))))
    ch))

(defun zh-align--get-char-from-script (script)
  "Get an example character from a given script(e.g. han, kana)"
  (let ((entry (assoc script script-representative-chars))
	value)
    (if entry
	(progn
	  (setq value (cdr entry))
	  (if (vectorp value)
	      (aref value 0)
	    (car value)))
      nil)))

(defun zh-align--get-char (script-or-charset)
  "Get an example character from a given script or charset"
  (or (zh-align--get-char-from-script script-or-charset)
      (zh-align--get-char-from-charset script-or-charset)))

(defun zh-align--fontset-size-at-width (char charset width &optional action)
  "Return a font size for charset so that the given char's width
on screen will be the given width. If optional action is
specified, the fontset's font size will be changed."
  (let* ((font (split-string (face-font 'default nil char) "-"))
	 (fontname (nth 2 font))
	 (fontsize (string-to-number (nth 7 font)))
	 (tempsize fontsize)
	 (fset (frame-parameter nil 'font)))

    (while (< (zh-align--screen-char-width char) width)
      (setq tempsize (1+ tempsize))
      (zh-align--set-fontset-size fset charset fontname tempsize))

    (while (> (zh-align--screen-char-width char) width)
      (setq tempsize (1- tempsize))
      (zh-align--set-fontset-size fset charset fontname tempsize))

    (unless action
      (zh-align--set-fontset-size fset charset fontname fontsize))
    tempsize))

(defun zh-align--fontset (charsets)
  "Export a fontset whose Chinese font's width is twice of
English font"
  (let* ((expected-width (* zh-align-fontwidth-ratio (zh-align--screen-char-width ?m)))
	 (fset (frame-parameter nil 'font))
	 (fset-string (replace-regexp-in-string "-iso.*$" "-fontset-zhalign" fset))
	 (fset-twice (create-fontset-from-fontset-spec fset-string)))
    (dolist (charset charsets)
      (let* ((char (zh-align--get-char charset))
	     (charset-fontsize (zh-align--fontset-size-at-width char charset expected-width))
	     (charset-font (split-string (face-font 'default nil char) "-"))
	     (charset-fontname (nth 2 charset-font)))
	(zh-align--set-fontset-size fset-twice charset charset-fontname charset-fontsize)))
    fset-twice))

;;;###autoload
(defun zh-align-set-faces (faces)
  "Apply zh-align--fontset to FACE or FACES list."
  (when (display-graphic-p)
    (let ((fontset (zh-align--fontset zh-align-charsets)))
      (if (listp faces)
	  (dolist (face faces)
	    (set-face-attribute face nil :fontset fontset))
	(set-face-attribute faces nil :fontset fontset)))))

(provide 'zh-align)

;;; zh-align.el ends here
