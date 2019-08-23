# zh-align

zh-align 是一个用来配置Emacs中文字体大小, 在表格环境(如`org-table`,
`gnus-summary`或者`elfeed-search`等)中实现中英文对齐的工具. 注意, 只对
等宽字体有效.

zh-align 会读取Emacs的默认字体, 然后根据英文字符的宽度, 生成一个独立的
fontset, 其中汉字字符的宽度会等于两倍英文字符. 然后可以根据需要设置各
种`face`的属性. 如:

``` emacs-lisp
(use-package zh-align
  :load-path "path/to/emacs-zh-align/"
  :demand t
  :init
  (setq zh-align-charsets '(han kana cjk-misc))
  )

(use-package org
  :config
  (zh-align-set-faces '(org-table))
  )

(use-package elfeed
  :config
  (zh-align-set-faces '(elfeed-search-title-face
			elfeed-search-feed-face))
  )
```

## 已知问题

如果使用

``` emacs-lisp
(add-to-list 'default-frame-alist '(font . "fontset"))
```

设置 frame 参数, 那么 zh-align 在启动时将无法读取到预期的字体设置. 建
议通过 face-attribute 来设置Emacs的字体大小:

``` emacs-lisp
(set-face-attribute 'default nil :height 200)
```
