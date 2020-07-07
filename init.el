;; file=./init.el

;; create functions to load Markdown

(defun load-markdown (paths) 
  (let (file-path) 
    (setq file-path (expand-file-name paths))
    (if (file-exists-p file-path) ;;if exists
        (with-temp-buffer ;;create tmp buffer
          (insert-file-contents file-path) ;;insert contents
          (goto-char (point-min)) ;; go to the first point
          (while (not (eobp)) ;; while not end of buffer
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
