## Emacs Dotfile
### Installation, Usage and Setup

**Backup first**

```sh
mkdir -p ~/Desktop/p/emacs-backup/
cp ~/.emacs.d/init.el ~/Desktop/p/emacs-backup/init`date +%Y%m%d%H`.el
cp ~/.emacs.d/readme.md ~/Desktop/p/emacs-backup/readme`date +%Y%m%d%H`.md
cp ~/.emacs.d/development.md ~/Desktop/p/emacs-backup/development`date +%Y%m%d%H`.md
ls ~/Desktop/p/emacs-backup/
```

**Sync**

```sh
cp ./init.el  ~/.emacs.d/init.el
cp ./readme.md  ~/.emacs.d/readme.md
cp ./development.md  ~/.emacs.d/development.md
cp ./example.md  ~/.emacs.d/example.md
cp ./test.md  ~/.emacs.d/test.md
```

### Emacs Configuration

**Basic Setup**
```emacs-lisp
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq use-dialog-box nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area t)
(setq initial-scratch-message nil)

(setq ring-bel-function 'ignore)
(setq visible-bell t)
(setq custom-safe-themes t)
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)
(setq find-file-visit-truename t)
(setq large-file-warning-threshold (* 25 1024 1024))
(setq comment-style 'extra-line)
(fset 'yes-or-no-p 'y-or-n-p)
(setq redisplay-dont-pause t)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq backup-inhibited t)
(setq auto-save-list-file-prefix nil)

;; (setq line-number-mode t)
(setq indicate-empty-lines t)
;; (setq global-hl-line-mode t)
(setq tab-width 2)
(setq toggle-truncate-lines t)
(setq indent-tabs-mode nil)

(set-default (quote cursor-type) t)
(blink-cursor-mode -1)
(defvar cursor-initial-color (face-attribute 'cursor :background))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))  ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 2)  ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)
 
```

**Declare Key bindings**

```emacs-lisp

(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")
```

**El-Get Bundler**

```emacs-lisp

(defun initialize-el-get ()
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")  
  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes"))


(initialize-el-get)

(el-get-bundle github-theme
 :url "https://raw.githubusercontent.com/dakrone/emacs-github-theme/master/github-theme.el")

(el-get-bundle polymode/polymode
 :type github :pkgname "polymode/polymode")

(el-get-bundle polymode/poly-markdown
 :type github :pkgname "polymode/poly-markdown")

(el-get-bundle defunkt/markdown-mode
 :type github :pkgname "defunkt/markdown-mode")


(el-get 'sync) ;; el should sync dependencies before package-initialize 
(package-initialize)
```

** Package Configuration**

```emacs-lisp
;; github theme
(load "~/.emacs.d/el-get/github-theme/github-theme.el")
(load-theme 'github t)

;; markdown-mode
(require 'markdown-mode)
(with-eval-after-load 'markdown-mode
  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

;;polymode
(require 'polymode)
(require 'poly-markdown)
(with-eval-after-load 'poly-markdown
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)))
  

```