## Emacs Elisp Development

```emacs-lisp

(defun dev/ansi-start ()
  (interactive)
  (ansi-term "/usr/bin/zsh"))

(add-hook 'emacs-startup-hook #'dev/ansi-start)

```
