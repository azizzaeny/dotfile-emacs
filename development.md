## Emacs Elisp Development

```emacs-lisp

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

(dev/remove-output-async) ;; make it silence

(custom-key
  "gs" 'dev/git-status
  "ga" 'dev/git-add
  "gc" 'dev/git-commit)

(defun dev/sync-dotfile ()
  (interactive)
  (shell-command "./sync"))

(defun dev/reload-markdown-init ()
  (interactive)
  (load-markdown "~/.emacs.d/readme.md")
  (load-markdown "~/.emacs.d/development.md"))

(custom-key
  "sy" 'dev/sync-dotfile
  "rl" 'dev/reload-markdown-init)

```

Notes:

```lisp
reload-markdown on key bind
```

End of line 

```emacs-lisp
```


