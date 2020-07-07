## Emacs Elisp Development

```emacs-lisp

;;;; Poor Man Git comit add, status

(defun dev/git-status ()
  (interactive)
  (shell-command "git status" "*git*"))

(defun dev/remove-output-async ()
  (add-to-list 'display-buffer-alist
			   (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil))))

(defun dev/git-add ()
  (interactive)
  (let (which-file) (setq which-file (read-file-name "git add ~"))
	   (async-shell-command (concat "git add " which-file))))

(defun dev/git-commit ()
  (interactive)
  (let (msg)
	(setq msg (read-string "git commit -m "))
	(shell-command (concat "git commit -m '" msg "'") "*git*")))

(defun dev/git-push (origin branch)
  (interactive)
  (shell-command (concat "git push -u " origin " " branch)))

(defun dev/git-push-origin ()
  (interactive)
  (let (branch b)
	(setq branch (read-string "master?"))
	(if (or (not branch) (equal branch "") (equal branch " "))
		(setq b "master")
	  (setq b branch))
	(dev/git-push "origin" b)))

(defun dev/git-remote-add (origin url)
  (interactive)
  (shell-command (concat "git remote add " origin " " url)))

(defun dev/git-remote-add-origin ()
  (interactive)
  (let (url)
	(setq url (read-string "url"))
	(dev/git-remote-add "origin" url)))

(dev/remove-output-async) ;; make it silence

(custom-key
  "gs" 'dev/git-status
  "ga" 'dev/git-add
  "gc" 'dev/git-commit
  "gm" 'dev/git-commit  
  "gp" 'dev/git-push-origin
  "gr" 'dev/git-remote-add-origin)

(defun dev/sync-dotfile ()
  (interactive)
  (shell-command "./sync"))

(defun dev/reload-markdown-init ()
  (interactive)
  (load-markdown "~/.emacs.d/readme.md")
  (load-markdown "~/.emacs.d/development.md"))

(custom-key
  "sy" 'dev/sync-dotfile
  "rl" 'dev/reload-markdown-init
  "rs" 'restart-emacs)

```

Notes:

```text
- reload-markdown on key bind
- git push gp optional args, when no args push to master, origin/master origin/develop
- more control for git, change branch,gi git init, git remote add,
- gpu origin, letter on add can change branch and origin 
```

End of line 

```emacs-lisp
```


