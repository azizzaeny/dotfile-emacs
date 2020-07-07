## Dotfile Emacs
Load dotfiles from markdown

```lisp
;; file=./init.el

;; create functions to load Markdown

(defun load-markdown (paths) 
  (let (file-path) 
    (setq file-path (expand-file-name paths))
    (if (file-exists-p file-path) ;;if exists
        (with-temp-buffer ;;create tmp buffer
          (insert-file-contents file-path) ;;insert contents
          (goto-char (point-min)) ;; go to the first point
          (while (not (eobp) ) ;; while not end of buffer
            (forward-line 1)  ;; forward one line
            (re-search-forward "^```emacs-lisp$" (point-max) t) ;; search for begining block
            (let ((point-region (match-end 0))) ;; store the point region
              (re-search-forward "^```$" (point-max) t) ;; search for ending block
              (eval-region point-region (match-beginning 0))))) ;;eval each region selected
      (message "No file to be founds"))))

;; load two markdowns readme.md and development.md
;; in the folder we three another one is example.md but loaded

(load-markdown "~/.emacs.d/readme.md")
(load-markdown "~/.emacs.d/development.md")

;; (load-markdown "./test.md")

(custom-set-variables
 '(custom-safe-themes t))

```

### Installation, Usage and Setup

**Develop**
to start `git clone https://github.com/azizzaeny/dotfile-emacs.git` cd into the folder
`sudo chmod u+x ./setup` then `./setup` then start synch files `./sync`  

**Prepare Setup**

```sh
#!/usr/bin/sh
mkdir -p ~/Desktop/p/emacs-backup/
mkdir -p ~/.emacs.d/snippet/yas
sudo chmod u+x ./sync
sudo chmod u+x ./backup
# file=./setup
```

**Sync**
```sh
#!/usr/bin/sh
cp ./init.el  ~/.emacs.d/init.el
cp ./readme.md  ~/.emacs.d/readme.md
cp ./development.md  ~/.emacs.d/development.md
cp ./example.md  ~/.emacs.d/example.md
cp ./test.md  ~/.emacs.d/test.md
cp -r ./snippet/ ~/.emacs.d/snippet/ 

# file=./sync
```

**Backup**
```sh
cp ~/.emacs.d/init.el ~/Desktop/p/emacs-backup/init`date +%Y%m%d%H`.el
cp ~/.emacs.d/readme.md ~/Desktop/p/emacs-backup/readme`date +%Y%m%d%H`.md
cp ~/.emacs.d/development.md ~/Desktop/p/emacs-backup/development`date +%Y%m%d%H`.md
cp ~/.emacs.d/test.md ~/Desktop/p/emacs-backup/test`date +%Y%m%d%H`.md
cp -r ~/.emacs.d/snippet ~/Desktop/p/emacs-backup/snippet`date +%Y%m%d%H`/

# file=./backup
```
### Emacs Configuration

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

(setq line-number-mode nil)
(setq indicate-empty-lines t)
(setq global-hl-line-mode nil)
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


(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

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

(el-get-bundle mooz/js2-mode
  :type github :pkgname "mooz/js2-mode")

(el-get-bundle clojure-mode)
(el-get-bundle parinfer)
(el-get-bundle paredit)
(el-get-bundle rainbow-delimiters)
(el-get-bundle aggressive-indent)
(el-get-bundle smartparens)

(el-get-bundle fgallina/multi-web-mode
  :type github :pkgname "fgallina/multi-web-mode")

(el-get-bundle emmet-mode)
(el-get-bundle python-mode)

(el-get-bundle yasnippet)
(el-get-bundle gist)
(el-get-bundle counsel)
(el-get-bundle ivy)
(el-get-bundle which-key)
(el-get-bundle auto-complete)
(el-get-bundle multiple-cursors)
(el-get-bundle autopair)

(el-get-bundle iqbalansari/restart-emacs
  :type github :pkgname "iqbalansari/restart-emacs")



(el-get 'sync)

(package-initialize)

;; add to load-path

```


```emacs-lisp
(load "~/.emacs.d/el-get/github-theme/github-theme.el")
(load-theme 'github t)

;; uniqify buffer
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; show-paren
(require 'paren)
(setq show-paren-delay 0.4)
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(show-paren-mode 1)

;; markdown-mode
(require 'markdown-mode)
(with-eval-after-load 'markdown-mode
  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

;; polymode poly-markdown
(require 'polymode)
(require 'poly-markdown)
(with-eval-after-load 'poly-markdown
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)))

;; js2-mode

;; clojure-mode
(require 'clojure-mode)
(setq clojure-indent-style 'always-indent)
(setq comment-column 0)

;;parinfer
(require 'parinfer)

(setq parinfer-extensions
	  '(defaults pretty-parens paredit smart-tab smart-yank))
(add-hook 'clojure-mode-hook #'parinfer-mode)
(add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
(add-hook 'common-lisp-mode-hook #'parinfer-mode)
(add-hook 'scheme-mode-hook #'parinfer-mode)
(add-hook 'lisp-mode-hook #'parinfer-mode)
(setq parinfer-auto-switch-indent-mode nil)  ;; default
(setq parinfer-auto-switch-indent-mode-when-closing nil)  ;; default
(setq parinfer-delay-invoke-threshold 6000)  ;; default
(setq parinfer-delay-invoke-idle 0.3)  ;; default

;; paredit

;; rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;; smartparen
(require 'smartparens-config)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'js-mode-hook #'smartparens-mode)

;;aggressive indent
(require 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;;multi-web
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;;emmet
(require 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode) 
(add-hook 'html-mode-hook 'emmet-mode)
(setq emmet-indentation 2)
(setq emmet-self-closing-tag-style " /")

;; python
;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs '("~/.emacs.d/snippet/yas"))

;; gist
;; git config --global github.user <your-github-user-name>
;; git config --global github.oauth-token <your-personal-access-token-with-gist-scope>

;; counsel, ivy, swiper
(require 'counsel)
(require 'ivy)
(require 'swiper)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers nil)
;; (setq search-default-mode #'char-fold-to-regexp)
(setq ivy-initial-inputs-alist nil)
(setq ivy-count-format "")
(setq ivy-display-style nil)
(setq ivy-minibuffer-faces nil)

(add-to-list 'ivy-highlight-functions-alist
             '(swiper--re-builder . ivy--highlight-ignore-order))

(setq ivy-re-builders-alist
	  '((ivy-switch-buffer . ivy--regex-plus)
		(swiper . ivy--regex-plus)))

;; which-key
;; auto-complete
;; multi-cursors

;; auto-pair
(require 'autopair)
(autopair-global-mode)

(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

(defun define-custom-key (key fn &optional binding)
  (let (b)
	(if (equal binding nil)
		(setq b custom-bindings-map)
	  (setq b binding))
	(global-set-key (kbd key) nil)  
	(define-key b (kbd key)  fn)))

(define-custom-key "C-c e" 'mc/edit-lines)
(define-custom-key "C-c n" 'mc/mark-next-like-this)
(define-custom-key "C-c a"  'mc/mark-all-like-this)
(define-custom-key "C-c j" 'emmet-expand-line)

(define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
(define-key ivy-minibuffer-map (kbd "C-g") 'minibuffer-keyboard-quit)
(define-key swiper-map (kbd "C-g") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; (define-key ivy-minibuffer-map (kbd "<up>") 'ivy-next-line)
;; (define-key ivy-minibuffer-map (kbd "<down>") 'ivy-previous-line)

(define-custom-key "C-s" 'swiper)
(define-custom-key "C-x f" 'counsel-describe-function)
(define-custom-key "C-x l" 'counsel-find-library)
(define-custom-key "C-x C-f" 'counsel-find-file)
(define-custom-key "C-x ag" 'counsel-ag)

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  t nil custom-bindings-map)

(custom-bindings-mode 1)

```
## Bugs and Strange behaviour 
- The functions doesnt take the last emacs-lisp value when there is some code on it so make it at least two emacs-lisp and blank last-line 


**Blank**
Intentionally blank eval last line

```emacs-lisp
```

