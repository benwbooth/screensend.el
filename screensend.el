;;; screensend.el --- 

;; Copyright (C) 2010-2011 Ben Booth
;; Author: Ben Booth <benwbooth@gmail.com>
;; Created: 2011-11-16
;; Version: 1.0.0
;; Keywords: lisp, screen, tmux, send, terminal
;; EmacsWiki: http://www.emacswiki.org/emacs/RainbowDelimiters
;; Github: http://github.com/jlr/rainbow-delimiters
;; URL: http://www.emacswiki.org/emacs/download/rainbow-delimiters.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:
;;
;; Rainbow-delimiters is a “rainbow parentheses”-like mode which highlights
;; parentheses, brackets, and braces according to their depth. Each
;; successive level is highlighted in a different color. This makes it easy
;; to spot matching delimiters, orient yourself in the code, and tell which
;; statements are at a given level.
;;
;; Great care has been taken to make this mode FAST. You should see no
;; discernible change in scrolling or editing speed while using it,
;; even in delimiter-rich languages like Clojure, Lisp, and Scheme.
;;
;; Default colors are subtle, with the philosophy that syntax highlighting
;; shouldn't being visually intrusive. Color schemes are always a matter
;; of taste.  If you take the time to design a new color scheme,
;; please share it (even a simple list of colors works) on the EmacsWiki
;; page or via github.
;; EmacsWiki: http://www.emacswiki.org/emacs/RainbowDelimiters
;; Github: http://github.com/jlr/rainbow-delimiters


;;; Installation:

;; 1. Place rainbow-delimiters.el on your emacs load-path.
;;
;; 2. Compile the file (necessary for speed):
;; M-x byte-compile-file <location of rainbow-delimiters.el>
;;
;; 3. Add the following to your dot-emacs/init file:
;; (require 'rainbow-delimiters)
;;
;; 4. Activate the mode in your init file.
;;    You can choose to enable it only in certain modes, or Emacs-wide:
;;
;; - To enable it only in specific modes, add lines like the following:
;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;;
;; - To activate the mode globally, add to your init file:
;; (global-rainbow-delimiters-mode)
;;
;; - To temporarily activate rainbow-delimiters mode in an open buffer:
;; M-x rainbow-delimiters-mode
;;
;; - To toggle global-rainbow-delimiters-mode:
;; M-x global-rainbow-delimiters-mode

;;; Customization:

;; To customize various options, including the color scheme:
;; M-x customize-group rainbow-delimiters
;;
;; color-theme.el users:
;; If you use the color-theme package, you can specify custom colors
;; by adding the appropriate faces to your theme.
;; - Faces take the form of:
;;   'rainbow-delimiters-depth-#-face' with # being the depth.
;;   Depth begins at 1, the outermost color.
;;   Faces exist for depths 1-9.
;; - The unmatched delimiter face (normally colored red) is:
;;   'rainbow-delimiters-unmatched-face'


;;; Change Log:

;; 1.0 - Initial release.
;; 1.1 - Stop tracking each delimiter's depth independently.
;;       This had lead to confusing results when viewing clojure
;;       code. Instead, just color based on current nesting inside
;;       all delimiters combined.
;;     - Added 'all-delimiters' faces to apply a color scheme to
;;       all delimiters at once. Other faces inherit from this group.
;; 1.1.1 - Change color scheme to a lighter, more subtle style.
;; 1.1.2: (2011-03-25)
;;  - Add an unmatched-delimiter face and correct problem with
;;    coloring of text following unmatched closing delims.
;; 1.2: (2011-03-28)
;;  - Unify delimiter faces: all delimiter types now use the same depth
;;    faces, of form 'rainbow-delimiters-depth-#-face'.
;; 1.2.1: (2011-03-29)
;;  - Conform to ELPA conventions.
;; 1.3: (2011-05-24)
;;  - Add separate color schemes for light and dark background modes.
;;  - Checkboxes to enable/disable highlighting for each delimiter type.
;;  - Improvements to Customize interface.
;;  - Infinite depth support by cycling through defined faces repeatedly.
;;  - Documentation changes.
;; 1.3.1 (2011-05-25)
;;  - Light color theme appears entirely grey on SRGB monitors. Revert to
;;    old color theme until a nicer light background theme can be added.
;;  - Correct typo in the installation step for users of dark backgrounds.
;; 1.3.2 (2011-10-14)
;;  - Add 'global-rainbow-delimiters-mode'.
;;  - Respect syntax of current buffer major-mode so delimiters
;;    highlight correctly in non-lisp languages.

;;; TODO:

;; - Add support for independent depth tracking of each delimiter type
;;   for users of C-like languages.
;; - Python style - increase depth with each new indentation.
;; - Add support for nested tags (XML, HTML)
;; - Set up proper example color-theme.el themes for rainbow-delimiters mode.
;; - Intelligent support for other languages: Ruby, LaTeX tags, et al.

;;; Issues:

;; - Rainbow-delimiters mode does not appear to change the color of
;;   delimiters when Org-mode is also enabled.


;;; Code:


;;;###autoload
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
  
;;;###autoload
(defun screen-select (session)
  "Select a screen session"
  (interactive
   (list (completing-read "Select a screen session:" (screen-list))))
  (setq screen-session session))

;;;###autoload
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

;;;###autoload
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
  
;;;###autoload
(defun tmux-select (session)
  "Select a tmux session."
  (interactive
   (list (completing-read "Select a tmux session:" (tmux-list))))
  (setq tmux-session session))

;;;###autoload
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

(provide 'screensend)