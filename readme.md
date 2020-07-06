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

```emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;; Setup Default Basic ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/setup-gui-mode ()
  (scroll-bar-mode -1)
  (tool-bar-mode   -1)
  (tooltip-mode    -1)
  (menu-bar-mode   -1)
  (add-to-list 'default-frame-alist (cons 'height 24))
  (add-to-list 'default-frame-alist (cons 'width  148)))

(defun my/setup-encoding ()
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8))

(defun my/setup-welcomescreen ()
  ;; welcomescreen, dont show any dialog box
  (custom-set-variables
   '(use-dialog-box nil)
   '(inhibit-startup-message t)
   '(inhibit-startup-echo-area-message t)
   '(initial-scratch-message nil)))

(defun my/setup-default-basic ()
  ;; ring bell annoy
  (custom-set-variables
   '(ring-bell-function 'ignore)
   '(visible-bell t)
   '(custom-safe-themes t)
   '(ns-use-proxy-icon  nil)
   '(frame-title-format nil)
   '(find-file-visit-truename t)
   '(large-file-warning-threshold (* 25 1024 1024))
   '(comment-style 'extra-line)
   '(fset 'yes-or-no-p 'y-or-n-p)
   '(redisplay-dont-pause t)

   
   )
  )

(defun my/setup-paren ()
  (custom-set-variables
   '(show-paren-delay 0.5))
  (require 'paren)
  ;; (set-face-background 'show-paren-match "#def")
  (set-face-foreground 'show-paren-match "#def")  
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (show-paren-mode 1))

(defun my/setup-backup ()
  (custom-set-variables
   '(make-backup-files nil) ; stop creating backup~ files
   '(auto-save-default nil) ; stop creating #autosave# files
   '(create-lockfiles nil)
   '(backup-inhibited t)   
   '(auto-save-list-file-prefix nil)))


(defun my/setup-spacing-tabs ()
  ;;(setq line-number-mode t)
  (custom-set-variables
   '(indicate-empty-lines t)
  ;; Highlights the current cursor line
  ;; (global-hl-line-mode t)
   ;; tabs
   '(tab-width 2)
   '(toggle-truncate-lines t)
   '(indent-tabs-mode nil)))

  
(defun my/setup-cursor ()
  ;;cursor type
  (set-default (quote cursor-type) t)
  ;; no cursor blink
  (blink-cursor-mode -1)
  ;; dimming cursor
  (defvar cursor-initial-color (face-attribute 'cursor :background))
  ;; set scrolling behaviour
  (custom-set-variables
   '(mouse-wheel-scroll-amount '(1 ((shift) . 1)))  ;; one line at a time
   '(mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
   '(mouse-wheel-follow-mouse 't) ;; scroll window under mouse
   '(scroll-step 2)  ;; keyboard scroll one line at a time
   '(scroll-conservatively 10000)
   '(scroll-preserve-screen-position t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Custom Key bindings  ;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;  El get Bundle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun my/github-theme ()
  (el-get-bundle github-theme
    :url "https://raw.githubusercontent.com/dakrone/emacs-github-theme/master/github-theme.el"
    (load "~/.emacs.d/el-get/github-theme/github-theme.el")
    (load-theme 'github t)))


(defun my/search-minor-setup ()
  (el-get-bundle counsel)
  (el-get-bundle ivy)
  (custom-set-variables
   '(ivy-initial-inputs-alist nil)
   '(ivy-use-virtual-buffers t)
   '(ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-plus)
                             (swiper . ivy--regex-plus)))
   
   '(enable-recursive-minibuffers nil)
   '(ivy-count-format "")
   '(ivy-display-style nil)
   '(ivy-minibuffer-faces nil)
  
   '(ivy-count-format "%d/%d "))
  
   (with-eval-after-load 'ivy
     (define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit))
   
   (with-eval-after-load 'swiper
     (define-key swiper-map (kbd "C-g") 'minibuffer-keyboard-quit))
   
   (with-eval-after-load 'counsel
     (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))
   
   (global-set-key (kbd "C-s") 'swiper)
   (global-set-key (kbd "<f1> f") 'counsel-describe-function)
   (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
   (global-set-key (kbd "<f1> l") 'counsel-find-library)
   (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
   (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
   (global-set-key (kbd "C-c g") 'counsel-git)
   (global-set-key (kbd "C-c j") 'counsel-git-grep)
   (global-set-key (kbd "C-c k") 'counsel-ag)
   (global-set-key (kbd "C-x l") 'counsel-locate)
   (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
   ;;(global-set-key (kbd "M-x") 'counsel-M-x)
   (global-set-key (kbd "C-x C-f") 'counsel-find-file)

   (ivy-mode 1)
   
   
  )

(defun my/global-mode-setup ()
  ;; which-key
  (el-get-bundle which-key
    :type github
    :pkgname "justbur/emacs-which-key"
    (which-key-mode 1)    
    (custom-set-variables
     '(which-key-separator " ")
     '(which-key-key-prefix-prefix "+")))
  
  ;; simple autocomplete 
  (el-get-bundle auto-complete
    (ac-config-default))
  
  (el-get-bundle autopair
    (autopair-global-mode))
  
  (el-get-bundle hungree-delete
   :type github
   :pkgname "nflath/hungry-delete"
   (global-hungry-delete-mode))

  (el-get-bundle aggressive-indent)

  ;;(el-get-bundle smart-tab-mode)
  ;; sync should to the bottoms
  
  (el-get-bundle persistent-scratch)
  (el-get-bundle neotree)
  (el-get-bundle all-the-icons)
  (el-get-bundle  multiple-cursors)
  )

(defun my/snippet-setup ()
  (el-get-bundle yasnippet
    (yas-global-mode 1))
  (el-get-bundle gist)
  
)

(defun my/web-mode-setup ()
  (el-get-bundle markdown-mode
    (auto-fill-mode 0)
    (visual-line-mode 1)
    ;;markdown-toggle-fontify-code-blocks-natively
    (custom-set-variables
     '(markdown-fontify-code-blocks-natively t))
    )
  (with-eval-after-load 'markdown-mode
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
    )
  
  (el-get-bundle org)
  (el-get-bundle emmet-mode)
  (with-eval-after-load 'emmet-mode
    (add-hook 'css-mode-hook 'emmet-mode)
    (define-key emmet-mode-keymap
      (kbd "C-n") 'emmet-expand-line))

  ;; (el-get-bundle mmm-mode) ;;error
  ;; manually
  ;;gitclone
    
  )

(defun my/clojure-mode-setup () 
  (el-get-bundle cider)
  ;;(el-get-bundle inf-clojure)
  (el-get-bundle clojure-mode)
  (el-get-bundle parinfer)
  )


(defun my/js-mode-setup ()

  (el-get-bundle js2-mode)
  
  ;; (el-get-bundle prettier-js)
  ;; (el-get-bundle nodejs-repl)
  (el-get-bundle simple-httpd)
  (el-get-bundle skewer-mode)
)


(defun my/python-mode-setup ()
  ;;(el-get-bundle python-mode)
  )

(defun my/go-mode-setup ()
  ;;(el-get-bundle go-mode)  
  )

(defun my/ess-eir-mode-setup ()
  (el-get-bundle eval-in-repl)
  (with-eval-after-load 'eval-in-repl
    (require 'eval-in-repl-shell))  
  )

(defun my/setup-same-file-uniq ()
  (require 'uniquify)
  (custom-set-variables
   '(uniquify-buffer-name-style 'post-forward-angle-brackets))
  )


;;;;;;;;;;;;;;;;;; Initialization Functions ;;;;;;;;;;;;;;;

(defun my/initialize-el-get ()
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")  
  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes"))


(defun my/initialize-basic-setup ()  
  (my/setup-gui-mode)
  (my/setup-encoding)
  (my/setup-welcomescreen)
  ;; faster keystroke ;;  (setq echo-keystrokes 0.1)
  (my/setup-default-basic)
  (my/setup-paren)
  (my/setup-backup)
  (my/setup-spacing-tabs)
  (my/setup-cursor)
  )

(defun my/initialize-major-mode ()
  (my/github-theme)
  (my/search-minor-setup)
  (my/global-mode-setup)
  (my/snippet-setup)
  (my/web-mode-setup)
  (my/clojure-mode-setup)
  (my/js-mode-setup)
  (my/python-mode-setup)
  (my/go-mode-setup)
  (my/ess-eir-mode-setup)
  (my/setup-same-file-uniq)
  )

(my/initialize-basic-setup)
(my/initialize-el-get)
(my/initialize-major-mode)

(el-get 'sync) ;; el should sync dependencies before package-initialize 

(package-initialize)

(add-to-list 'load-path "/home/io/.emacs.d/lisp/poly-markdown")
(add-to-list 'load-path "/home/io/.emacs.d/lisp/polymode")
(require 'polymode)
(require 'poly-markdown)

(with-eval-after-load 'poly-markdown
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)))
(with-eval-after-load 'polymode
  (custom-set-variables
   '(polymode-prefix-key "C-c n"))
   
  ;; (define-hostmode poly-markdown-hostmode
  ;;   :mode 'markdown-mode)
  ;; (define-innermode poly-markdown-yaml-metadata-innermode
  ;;   :mode 'yaml-mode
  ;;   :head-matcher "\`[ \t\n]*---\n"
  ;;   :tail-matcher "^---\n"
  ;;   :head-mode 'host
  ;;   :tail-mode 'host)
  ;; )
  ;; (define-auto-innermode poly-markdown-fenced-code-innermode
  ;;   :head-matcher (cons "^[ \t]*\\(```{?[[:alpha:]].*\n\\)" 1)
  ;;   :tail-matcher (cons "^[ \t]*\\(```\\)[ \t]*$" 1)
  ;;   :mode-matcher (cons "```[ \t]*{?\\(?:lang *= *\\)?\\([^ \t\n;=,}]+\\)" 1)
  ;;   :head-mode 'host
  ;;   :tail-mode 'host)
  ;; (define-polymode poly-markdown-mode
  ;;   :hostmode 'pm-host/markdown
  ;;   :innermodes '(poly-markdown-yaml-metadata-innermode
  ;;                 poly-markdown-fenced-code-innermode))

  
  ;; (define-hostmode poly-markdown-host :mode 'markdown-mode)
  ;; (define-innermode poly-markdown-clj
  ;;   :mode 'clojure-mode
  ;;   :head-matcher "^```clj[\n\r]+"
  ;;   :tail-matcher "^```$"
  ;;   :head-mode 'host
  ;;   :tail-mode 'host)
  ;; (define-polymode poly-markdown-clj
  ;;   :hostmode 'poly-markdown-host
  ;;   :innermodes '(poly-markdown-clj))
    
  )
;; (add-to-list 'load-path "/home/io/.emacs.d/lisp/mmm-mode/")
;; (require 'mmm-mode)
;; (require 'mmm-auto)


;; (with-eval-after-load 'mmm-mode

;;   (custom-set-variables
;;    '(mmm-global-mode 'maybe))
  
;;   (defun auto-mode-markdown(lang &optional submode)
;;     (let ((class (intern (concat "markdown-" lang)))
;;           (submode (or submode (intern (concat lang "-mode"))))
;;           (front (concat "^```" lang "[\n\r]+"))
;;           (back "^```$"))
;;       (mmm-add-classes (list (list class :submode submode :front front :back back )))
;;       (mmm-add-mode-ext-class 'markdown-mode nil class)))

;;   (mapc 'auto-mode-markdown
;;         '("lisp" "js" "clojure" "python" "html" "css" "markdown"))
;;   (mmm-add-classes
;;    '((markdown-clj
;;       :submode clojure-mode
;;       :face mmm-declaration-submode-face
;;       :front "^```clj[\n\r]+"
;;       :back "^```$")))
;;   (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-clj))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Minor Mode that override custom bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key custom-bindings-map (kbd "C-c e")  'mc/edit-lines)
(define-key custom-bindings-map (kbd "C-c a")  'mc/mark-all-like-this)
(define-key custom-bindings-map (kbd "C-c n")  'mc/mark-next-like-this)
(define-key custom-bindings-map (kbd "C-c C-e") 'eir-eval-in-shell)
(define-key custom-bindings-map (kbd "C-x ]") 'enlarge-window-horizontally)
(define-key custom-bindings-map (kbd "C-x [") 'shrink-window-horizontally)

;(define-key custom-bindings-map (kbd "C-e") 'eir-eval-in-shell)
(define-key custom-bindings-map (kbd "<M-RET>")   'eir-eval-in-shell)

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  t nil custom-bindings-map)

(custom-bindings-mode 1)

(defun ansi-start ()
  (interactive)
  (ansi-term "/usr/bin/zsh"))

(defvar my/point nil)

(defun save-point ()
  (interactive)
  (setq my/point (point)))

(defun jump-back ()
  (interactive)
  (goto-char my/point))

(defun my/insert-date ()
  (interactive)
  (insert (format-time-string" %Y%m%d")))

```
