## Emacs Elisp Development

```emacs-lisp

(defun dev/sync-dotfile ()
  (interactive)
  (shell-command "./sync"))

(custom-key
  "sy" 'dev/sync-dotfile)


```
```emacs-lisp
```


