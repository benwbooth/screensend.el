
(defun screen-list ()
  "Get list of active screen sessions."
  (interactive)
  (let ((output (with-output-to-string
                  (with-current-buffer standard-output
                    (call-process "screen" nil '(t nil) nil "-list"))))
        (re "^\\s-+\\(\\S-+\\)")
        (sessions '()))
    (when (string-match re output)
      (push (match-string 1 output) sessions))
    (while (string-match re output (match-end 1))
      (push (match-string 1 output) sessions))
    sessions))
  
(defun screen-select (session)
  "Select a screen session"
  (interactive
   (list (completing-read "Select a screen session:" (screen-list))))
  (setq screen-session session))

(defun screen-send ()
  "Send selected region or currently-surrounding blank line-separated \
block of text to the dedicated multi-term buffer"
  (interactive)
  (when (not screen-session)
    (call-interactively 'screen-select))
  (let ((selected (progn 
                    (when (equal mark-active nil) 
                      (mark-paragraph))
                    (concat (buffer-substring (mark) (point)) "\n")))
        (tmpfile (make-temp-file "screen-send.")))
    (with-temp-file tmpfile
      (insert selected))
    (call-process "screen" nil nil nil
                  "-S" screen-session
                  "-X" "eval"
                  "msgminwait 0"
                  "msgwait 0"
                  (concat "readbuf " (shell-quote-argument tmpfile))
                  "paste ."
                  "msgwait 5"
                  "msgminwait 1")
    (delete-file tmpfile)
    (deactivate-mark)))

(defun tmux-list ()
  "Get list of active tmux sessions."
  (interactive)
  (let ((output (with-output-to-string
                  (with-current-buffer standard-output
                    (call-process "tmux" nil '(t nil) nil "list-sessions"))))
        (re "^\\([^:]*\\):")
        (sessions '()))
    (when (string-match re output)
      (push (match-string 1 output) sessions))
    (while (string-match re output (match-end 1))
      (push (match-string 1 output) sessions))
    sessions))
  
(defun tmux-select (session)
  "Select a tmux session."
  (interactive
   (list (completing-read "Select a tmux session:" (tmux-list))))
  (setq tmux-session session))

(defun tmux-send ()
  "Send selected region or currently-surrounding blank line-separated \
block of text to the selected tmux session."
  (interactive)
  (when (not tmux-session)
    (call-interactively 'tmux-select))
  (let ((selected (progn 
                    (when (equal mark-active nil) 
                      (mark-paragraph))
                    (concat (buffer-substring (mark) (point)) "\n")))
        (tmpfile (make-temp-file "tmux-send.")))
    (with-temp-file tmpfile
      (insert selected))
    (call-process "tmux" nil nil nil
                  "load-buffer" tmpfile ";"
                  "paste-buffer" "-t" tmux-session ";")
    (delete-file tmpfile)
    (deactivate-mark)))
