# zh-align

zh-align 是一个用来配置Emacs中文字体大小, 在表格环境(如`org-table`,
`gnus-summary`或者`elfeed-search`等)中实现中英文对齐的工具. 注意, 只对
等宽字体有效.

zh-align 会读取Emacs的默认字体, 然后根据英文字符的宽度, 生成一个独立的
fontset, 其中汉字字符的宽度会等于两倍英文字符. 然后可以根据需要设置各
种`face`的属性. 如:

``` emacs-lisp
(setq zh-align-faces '(org-table)) ;; or other faces
(zh-align-frame-faces)
```

如果是Emacs daemon:

``` emacs-lisp
(setq zh-align-faces '(org-table)) ;; or other faces
(add-hook 'after-make-frame-functions #'zh-align-frame-faces)
(add-hook 'window-setup-hook #'zh-align-frame-faces)
```
