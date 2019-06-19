;;; zh-align.el, let Chinese font's alignment be compatible with English font in Emacs"
;; Keywords: Chinese, font, alignment
;; Author: Chen Chao <cchao.im@gmail.com>
;; License: MIT
;; URL: https://github.com/chen-chao/emacs-zh-align

;; Usage:
;;   (setq zh-align-faces '(org-table))
;;   (zh-align-frame-faces)
;; For Emacs daemon:
;;   (add-hook 'after-make-frame-functions #'zh-align-frame-faces)
;;   (add-hook 'window-setup-hook #'zh-align-frame-faces)

;;; Code:

(defgroup zh-align nil
  "Let Chinses font size be compatible with English font in Emacs"
  :prefix "zh-align-"
  :group 'align)

(defcustom zh-align-faces nil
  "Specify faces to apply zh-align--fontset"
  :group 'zh-align
  :type 'list)

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
    tempsize)
  )

;;;###autoload
(defun zh-align-set-as-twice-en-width ()
  "Set global Chinese font's width as twice of English font"
  (interactive)
  (let ((expected-width (* 2 (zh-align--screen-char-width ?m))))
    (zh-align--fontset-size-at-width ?中 'han expected-width t))
  )

;;;###autoload
(defun zh-align-set-as-en-height ()
  "set global chinese font's height as english font"
  (interactive)
  (let* ((fset (frame-parameter nil 'font))
	 (font (split-string (face-font 'default nil ?中) "-"))
	 (fontname (nth 2 font))
	 (fontsize (zh-align--screen-char-height ?m)))
    (zh-align--set-fontset-size fset 'han fontname fontsize))
  )

(defun zh-align--fontset ()
  "Export a fontset whose Chinese font's width is twice of
English font"
  (let* ((expected-width (* 2 (zh-align--screen-char-width ?m)))
	 (cn-fontsize (zh-align--fontset-size-at-width ?中 'han expected-width))
	 (cn-font (split-string (face-font 'default nil ?中) "-"))
	 (cn-fontname (nth 2 cn-font))
	 (fset (frame-parameter nil 'font))
	 (fset-string (replace-regexp-in-string "-iso.*$" "-fontset-zhalign" fset))
	 (fset-twice (create-fontset-from-fontset-spec fset-string)))
    (zh-align--set-fontset-size fset-twice 'han cn-fontname cn-fontsize)
    fset-twice)
  )

(defun zh-align--faces-fontset (faces)
  "Set faces' fontset to zh-align--fontset"
  (let ((fontset (zh-align--fontset)))
    (dolist (face faces)
      (set-face-attribute face nil :fontset fontset)))
  )

;;;###autoload
(defun zh-align-frame-faces (&optional frame)
  "Apply zh-align--faces-fontset to faces in frame"
  (if frame
      (with-selected-frame frame
	(when (display-graphic-p)
	  (zh-align--faces-fontset zh-align-faces)))
    (when (display-graphic-p)
      (zh-align--faces-fontset zh-align-faces)))
  (redisplay t)
  )

(provide 'zh-align)
