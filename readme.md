## Configuring Emacs

I choose to load my emacs configuration and snippet from markdown. i personally think it's better to manage all the snippet and configuration from one files.   

Here it is. (Long live literate mode! ;)  

**init.el**

```lisp
;; file=./init.el
(defun load-markdown (paths)  
  (let (file-path) 
    (setq file-path (expand-file-name paths))
    (if (file-exists-p file-path) ;;if exists
        (with-temp-buffer ;;create tmp buffer
          (insert-file-contents file-path) ;;insert contents
          (goto-char (point-min)) ;; go to the first point
          (while (not (eobp) ) ;; while not end of buffer
            (forward-line 1)  ;; forward one line
            (re-search-forward "^```emacs-lisp" (point-max) t) ;; search for begining block
            (let ((point-region (match-end 0))) ;; store the point region
              (re-search-forward "^```$" (point-max) t) ;; search for ending block
              (eval-region point-region (match-beginning 0))))) ;;eval each region selected
      (message "No file to be founds"))))

(load-markdown "~/.emacs.d/readme.md")

(custom-set-variables
 '(custom-safe-themes t))
```

### Emacs Configuration

**Basic UI Setup and General Setup**

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
```

**Backup-files**   

```emacs-lisp
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq backup-inhibited t)
(setq auto-save-list-file-prefix nil)
```

**Line-number, spacing**  

```emacs-lisp
(setq line-number-mode nil)
(setq indicate-empty-lines t)
(setq global-hl-line-mode nil)
(setq tab-width 2)
(setq toggle-truncate-lines t)
(setq indent-tabs-mode nil)
```

**Cursor, mouse-wheel, scroll**

```emacs-lisp

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

**The el-get bundler capitano**  

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

(el-get-bundle general)

(el-get 'sync)

(package-initialize)

;; add to load-path
```

**Configuring each packages**   

```emacs-lisp
;; ah.. yes. white themes
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
;; (add-hook 'clojure-mode-hook #'parinfer-mode)
;; (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;; (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;; (add-hook 'scheme-mode-hook #'parinfer-mode)
;; (add-hook 'lisp-mode-hook #'parinfer-mode)
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
(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))


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
(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-bottom)
(which-key-setup-minibuffer)
(setq which-key-idle-delay 1)
(setq which-key-idle-secondary-delay 0.01)
(setq which-key-popup-type 'minibuffer)


;; auto-complete
;; (require 'auto-complete)
;; (ac-config-default)


;; multi-cursors
(require 'multiple-cursors)

;; auto-pair
(require 'autopair)
(autopair-global-mode)
```

**Custom Key Bindings Basic Editing/Searching**

```emacs-lisp
(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

(general-define-key
 "C-g"     'minibuffer-keyboard-quit
 "C-s"     'counsel-grep-or-swiper
 "C-x C-f" 'counsel-find-file
 "C-x ag"  'counsel-ag
 "C-x f"   'counsel-describe-function
 "C-x l"   'counsel-find-library
 "C-x f" nil )


(defconst custom-key "C-c")


(general-create-definer
  custom-key :prefix "C-c")


(custom-key
  "me" 'mc/edit-lines
  "mn" 'mc/mark-next-lines
  "me" 'emmet-expand-line)


(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  t nil custom-bindings-map)


(custom-bindings-mode 1)
```


**Poor Man's Git ;)**  

```emacs-lisp

(defun git/status ()
  (interactive)
  (shell-command "git status" "*git*"))


(defun remove-output-async ()
  (add-to-list 'display-buffer-alist
			   (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil))))


(defun git/add ()
  (interactive)
  (let (which-file) (setq which-file (read-file-name "git add ~"))
	   (async-shell-command (concat "git add " which-file))))


(defun git/commit ()
  (interactive)
  (let (msg)
	(setq msg (read-string "git commit -m "))
	(shell-command (concat "git commit -m '" msg "'") "*git*")))


(defun git/push (origin branch)
  (interactive)
  (shell-command (concat "git push -u " origin " " branch)))


(defun git/push-origin ()
  (interactive)
  (let (branch b)
	(setq branch (read-string "master?"))
	(if (or (not branch) (equal branch "") (equal branch " "))
		(setq b "master")
	  (setq b branch))
	(git/push "origin" b)))


(defun git/remote-add (origin url)
  (interactive)
  (shell-command (concat "git remote add " origin " " url)))


(defun git/remote-add-origin ()
  (interactive)
  (let (url)
	(setq url (read-string "url"))
	(git/remote-add "origin" url)))

(remove-output-async) ;; make it silence

(custom-key
  "gits" 'git/status
  "gita" 'git/add
  "gitc" 'git/commit
  "gitm" 'git/commit  
  "gitp" 'git/push-origin
  "gitu" 'git/push-origin
  "gitr" 'git/remote-add-origin)

```

**Load and Tangle Snippet**


```emacs-lisp

(defun filter-list (@predicate @sequence)
  (delete "e3824ad41f2ec1ed" (mapcar (lambda ($x) (if (funcall @predicate $x)  $x "e3824ad41f2ec1ed" )) @sequence)))


(defun snippet/write-file (list-block)
  (interactive)
  (let ((filename (cdr (assoc 'file list-block)))
		(content  (cdr (assoc 'content list-block))))
	(unless (file-exists-p filename)
	  (let ((dir (file-name-directory filename)))
		(unless (file-exists-p dir)	  
		  (make-directory dir t))))
	(with-temp-buffer
	  (insert content)
	  (write-region (point-min) (point-max) filename))))


(defun snippet/get-file-string (path)
  (with-temp-buffer
	(insert-file-contents path)
	(buffer-string)))


(defun snippet/read-meta (str)
  (interactive)
  (let ((res (list)))
	(with-temp-buffer
	  (insert str)
	  (goto-char (point-min))	  
	  (while (not (eobp))
		(forward-line 1)
	 	(re-search-forward "^```" (point-max) t)
		(let (s e c f)
		  (re-search-forward "file=\\([^\s]+\\)" (point-max) t)
		  (setq s (progn (beginning-of-line) (forward-line 1) (point)))		  
		  (setq f (match-string 1))
		  (re-search-forward "^```" (point-max) t)
		  (setq e (match-beginning 0))
		  (setq c  (buffer-substring-no-properties s e))
		  (add-to-list 'res (list (cons 'start s)
								  (cons 'end e)
								  (cons 'file f)
								  (cons 'content c))))))
	res))

;; todo probably just use seq-filter instead of filter-method above
;; (require 'seq)
;; (seq-filter 'numberp '(1 "2" 3))

(defun snippet/filter-nil-file (list-block)
  (filter-list (lambda (l)
				 (not (equal (cdr (assoc 'file l)) nil))
				 ) list-block))

(defun snippet/tangle-all ()
  (interactive)
  (let ((p (expand-file-name "~/.emacs.d/snippet.md"))
		list-block)
	(setq list-block (snippet/read-meta (snippet/get-file-string p)))
	(mapcar 'snippet/write-file (snippet/filter-nil-file list-block))))


(defun snippet/clean ()
  (interactive)
  (shell-command "rm -rf ~/.emacs.d/snippets/*"))

(defun snippet/reload-yas ()
  (interactive)
  (snippet/clean)
  (snippet/tangle-all)
  (shell-command "cp -rv ./snippets/. ~/.emacs.d/snippets/.")
  (yas-reload-all))

(custom-key
  "snr" 'snippet/reload-yas
  "snc" 'snippet/clean
  "snt" 'snippet/tangle-all)


```

**Sorting keybindings**

```emacs-lisp

(custom-key
  "sortl" 'sort-lines
  "sortp" 'sort-paragraphs
  "sortc" 'sort-columns
  "sortr" 'reverse-region
  )


```

**Reloading, Syncing dotfile-emacs**


```emacs-lisp

(defun emacs/sync-dotfile ()
  (interactive)
  (shell-command "./bin/sync"))

(defun emacs/reload-markdown-init ()
  (interactive)
  (load-markdown "~/.emacs.d/readme.md"))

(custom-key
  "esy" 'emacs/sync-dotfile
  "erl" 'emacs/reload-markdown-init
  "ers" 'restart-emacs)

```

**Send Code Block to Repl**

```emacs-lisp 

(defun repl/start-repl (&optional init-script repl-name)
  "start a new shell repl"
  (interactive)
  (let ((current-buffer-window (selected-window)))	
	(if (not repl-name)
		(shell "*srepl*")
	  (shell repl-name))	
	(when init-script	  
	  (insert init-script)
	  (sit-for 0.1)
	  (comint-send-input))
	;; go back to origin window
	(select-window current-buffer-window)))

(defun repl/start ()
  (interactive)
  (repl/start-repl))

(defun repl/stop-repl ()
  "stop buffer repl"
  (interactive)
  ;; todo check if existsted buffer
  (kill-buffer "*srepl*"))

(defun repl/send-to-repl (input-str)
  (interactive)
  (let ((current-window (selected-window)))
	;; todo: check if exists buffer
	(switch-to-buffer-other-window "*srepl*")
	(goto-char (point-max))
	(insert input-str)
	(comint-send-input)
	(select-window current-window)))

(defun repl/send-buffer ()
  (interactive)
  (repl/send-to-repl
   (buffer-substring-no-properties (point-min) (point-max))))

(defun repl/send-line ()
  (interactive)
  ;; todo save-excursion
  (let ((init-point (point))) ;; save initial potitions
	(beginning-of-line)
	(set-mark (point)) ;; creating marker from begining of the line
	(end-of-line) ;; to the edge line
	(repl/send-to-repl
	 (buffer-substring-no-properties (point) (mark)))
	(setq mark-active nil)
	(goto-char init-point)))

(defun repl/send-region ()
  "send region when mark is active"
  (interactive)
  (save-excursion
	(if (and transient-mark-mode mark-active)
		(repl/send-to-repl
		 (buffer-substring-no-properties (point) (mark))))))


(defun repl/send-markdown-block ())
(defun repl/send-markdown-buffer())

;; simple but works
;; todo fix: behaviour when no defun 
(defun repl/send-lisp ()
  (interactive)  
  (set-mark (line-beginning-position))
  (forward-sexp)
  (repl/send-to-repl
   (buffer-substring-no-properties (point) (mark)))
  (setq mark-active nil)
  (forward-sexp)) ;; go to the next expression

(defun repl/send-clojure ())
;; todo clojure forward send-expression


;; this is also simple but works
;; from atlantis.net
;; todo look at js2 implementation
(require 'js2-mode)
(defun repl/send-javascript ()
  (interactive)
  (let ((fn (js2-mode-function-at-point (point))))
	(when fn
	  (let ((beg (js2-node-abs-pos fn))
			(end (js2-node-abs-end fn)))
		(repl/send-to-repl
		 (buffer-substring-no-properties beg end))
		))))

;; (srepl/stop-repl)
;; (srepl/start-repl "clojure")
;; (srepl/start-repl "ls" "**foo**")
;; (srepl/send-to-repl "clojure")

(custom-key
  "re" 'repl/send-line
  "rb" 'repl/send-buffer
  "rj" 'repl/send-javascript
  "rr" 'repl/send-region
  "rp" 'repl/stop-rep
  "rs" 'repl/start)

```


### Installation, Usage and Setup

**Develop**  
to start `git clone https://github.com/azizzaeny/dotfile-emacs.git`    
cd into that folder  `sudo chmod u+x ./bin/setup` then `./bin/setup` then start synch files `./bin/sync`  

**Prepare Setup**  

```sh	

#!/usr/bin/sh
mkdir -p ~/Desktop/p/emacs-backup/
mkdir -p ~/.emacs.d/snippets/
sudo chmod u+x ./bin/sync
sudo chmod u+x ./bin/backup
# file=./bin/setup
```

**Sync configuration**   

```sh		
#!/usr/bin/sh
cp -v ./init.el  ~/.emacs.d/init.el
cp -v ./readme.md  ~/.emacs.d/readme.md
cp -v ./snippet.md  ~/.emacs.d/snippet.md
# cp -rv ./snippet/ ~/.emacs.d/snippet/ 
# file=./bin/sync

```

**Backup configuration**

```sh
#!/usr/bin/sh 
cp -v ~/.emacs.d/init.el ~/Desktop/p/emacs-backup/init`date +%Y%m%d%H`.el
cp -v ~/.emacs.d/readme.md ~/Desktop/p/emacs-backup/readme`date +%Y%m%d%H`.md
cp -v ~/.emacs.d/snippet.md ~/Desktop/p/emacs-backup/snippet`date +%Y%m%d%H`.md
# file=./bin/backup
```


## Bugs and Strange behaviour 
- The functions doesnt take the last emacs-lisp value when there is some code on it so make it at least two emacs-lisp and blank last-line 


**Blank**
Intentionally blank eval last line

```emacs-lisp
```

