## Emacs Elisp Development

```emacs-lisp

(defun dev/sync-dotfile ()
  (interactive)
  (shell-command "./sync"))

(custom-key
  "sy" 'dev/sync-dotfile)

(defun dev/prompt-which-file ()
  (interactive)
  (message "string is %s" (read-file-name "which to add")))

(defun dev/git-status ()
  (interactive)
  (shell-command "git status" "*git*"))


(defun dev/git-add ()
  (interactive)
  (let (which-file) (setq which-file (read-file-name "git add ~"))
	   (shell-command (concat "git add " which-file))))


```

End of line 

```emacs-lisp
```


