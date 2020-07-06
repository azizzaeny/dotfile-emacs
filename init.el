
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


(load-markdown "./readme.md")
;; (load-markdown "./development.md")
;;(load-markdown "./example.md")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-list-file-prefix nil)
 '(backup-inhibited t t)
 '(comment-style (quote extra-line))
 '(create-lockfiles nil)
 '(custom-safe-themes t)
 '(enable-recursive-minibuffers nil)
 '(exec-path-from-shell-check-startup-files nil)
 '(find-file-visit-truename t)
 '(frame-title-format nil t)
 '(fset (quote yes-or-no-p) t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(init-loader-show-log-after-init (quote error-only))
 '(initial-scratch-message nil)
 '(ivy-count-format "%d/%d ")
 '(ivy-display-style nil)
 '(ivy-initial-inputs-alist nil)
 '(ivy-minibuffer-faces nil)
 '(ivy-re-builders-alist
   (quote
    ((ivy-switch-buffer . ivy--regex-plus)
     (swiper . ivy--regex-plus))) t)
 '(ivy-use-virtual-buffers t)
 '(large-file-warning-threshold (* 25 1024 1024))
 '(make-backup-files nil)
 '(mouse-wheel-follow-mouse (quote t))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
 '(ns-use-proxy-icon nil t)
 '(package-selected-packages
   (quote
    (mmm-mode persistent-scratch org eval-in-repl parinfer queue counsel)))
 '(redisplay-dont-pause t t)
 '(ring-bell-function (quote ignore))
 '(scroll-conservatively 10000)
 '(scroll-preserve-screen-position t)
 '(scroll-step 2)
 '(show-paren-delay 0.5)
 '(tab-width 2)
 '(toggle-truncate-lines t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(use-dialog-box nil)
 '(visible-bell t)
 '(which-key-idle-delay 0.5)
 '(which-key-key-prefix-prefix "+")
 '(which-key-mode t)
 '(which-key-separator " "))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
