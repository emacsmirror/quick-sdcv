;;; easysdcv.el --- Interface for sdcv (StartDict console version) -*- lexical-binding: t -*-

;; Filename: easysdcv.el
;; Description: Interface for sdcv (StartDict console version).
;; Package-Requires: ((emacs "25.1"))
;; Optional-Requirements: ((posframe "1.1.2"))
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-05 22:04:02
;; Version: 3.4
;; Last-Updated: 2020-06-12 19:32:08
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/easysdcv.el
;; Keywords: docs, startdict, sdcv
;; Compatibility: GNU Emacs 25.1
;;
;; Features that might be required by this library:
;;
;; `posframe' `outline'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Interface for sdcv (StartDict console version).
;;
;; Translate word by sdcv (console version of Stardict), and display
;; translation using posframe or in buffer.
;;
;; Below are commands you can use:
;;
;; `easysdcv-search-pointer'
;; Search around word and display in buffer.
;; `easysdcv-search-pointer+'
;; Search around word and display with `posframe'.
;; `easysdcv-search-input'
;; Search input word and display in buffer.
;; `easysdcv-search-input+'
;; Search input word and display with `posframe'.
;;
;; Tips:
;;
;; If current mark is active, sdcv commands will translate
;; contents in region, otherwise translate word at point.
;;

;;; Installation:
;;
;; To use this extension, you have to install Stardict and sdcv
;; If you use Debian, it's simple, just:
;;
;;      sudo aptitude install stardict sdcv -y
;;
;; And make sure you have installed `posframe.el'.
;; You can get it from:
;; https://raw.githubusercontent.com/tumashu/posframe/master/posframe.el
;;
;; Put easysdcv.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'easysdcv)
;;
;; And then you need to set two options.
;;
;;  easysdcv-dictionary-simple-list         (a simple dictionary list for posframe display)
;;  easysdcv-dictionary-complete-list       (a complete dictionary list for buffer display)
;;
;; Example, setup like this:
;;
;; (setq easysdcv-dictionary-simple-list
;;       (list "Simple English-Chinese Dictionary"
;;             "Simple Chinese-English Dictionary"
;;             "KDic 110,000 English-Chinese Dictionary")
;;       easysdcv-dictionary-complete-list
;;       (list "KDic 110,000 English-Chinese Dictionary"
;;             "Simple English-Chinese Dictionary"
;;             "English-Chinese Dictionary 5.0"
;;             "XDICT English-Chinese Dictionary"
;;             "Chinese-English Dictionary 5.0"
;;             "XDICT Chinese-English Dictionary"
;;             "Simple Chinese-English Dictionary"
;;             "Oxford English-Chinese Double Explanation Beautified Version"
;;             "stardict 1.3 English-Chinese Dictionary"
;;             "English-Chinese and Chinese-English Professional Dictionary"
;;             "CDICT5 English-Chinese Dictionary"
;;             "Jargon"
;;             "FOLDOC"
;;             "WordNet")
;;       easysdcv-dictionary-data-dir "your_sdcv_dict_dir") ; local sdcv dict dir
;;
;;; Customize:
;;
;; `easysdcv-buffer-name'
;; The name of sdcv buffer.
;;
;; `easysdcv-dictionary-simple-list'
;; The dictionary list for simple description.
;;
;; `easysdcv-dictionary-complete-list'
;; The dictionary list for complete description.
;;
;; `easysdcv-dictionary-data-dir'
;; The directory where stardict dictionaries are stored.
;;
;; `easysdcv-tooltip-face'
;; The foreground/background colors of sdcv tooltip.
;;
;; All of the above can customize by:
;;      M-x customize-group RET sdcv RET
;;

;;; Require

(require 'json)
(require 'subr-x)
(require 'outline)
(require 'subword)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup easysdcv nil
  "Interface for sdcv (StartDict console version)."
  :group 'edit)

(defcustom easysdcv-buffer-name "*SDCV*"
  "The name of the sdcv buffer."
  :type 'string
  :group 'easysdcv)

(defcustom easysdcv-tooltip-name "*sdcv*"
  "The name of sdcv tooltip name."
  :type 'string
  :group 'easysdcv)

(defcustom easysdcv-program "sdcv"
  "Path to sdcv."
  :type 'file
  :group 'easysdcv)

(defcustom easysdcv-tooltip-timeout 5
  "The timeout for sdcv tooltip, in seconds."
  :type 'integer
  :group 'easysdcv)

(defcustom easysdcv-dictionary-complete-list nil
  "The complete dictionary list for translation."
  :type 'list
  :group 'easysdcv)

(defcustom easysdcv-dictionary-simple-list nil
  "The simple dictionary list for translation."
  :type 'list
  :group 'easysdcv)

(defcustom easysdcv-dictionary-data-dir nil
  "Default, sdcv search word in /usr/share/startdict/dict/.
If you customize this value with local dir, then you don't need
to copy dict data to /usr/share directory everytime when you
finish system installation."
  :type '(choice (const :tag "Default" nil) directory)
  :group 'easysdcv)

(defcustom easysdcv-only-data-dir t
  "Search is performed using only `easysdcv-dictionary-data-dir'."
  :type 'boolean
  :group 'easysdcv)

(defcustom easysdcv-tooltip-border-width 10
  "The border width of sdcv tooltip, in pixels."
  :type 'integer
  :group 'easysdcv)

(defcustom easysdcv-say-word-p nil
  "Say word after searching if this option is non-nil.

It will use system feature if you use OSX, otherwise youdao.com."
  :type 'boolean
  :group 'easysdcv)

(defcustom easysdcv-env-lang "zh_CN.UTF-8"
  "Default LANG environment for sdcv program.

Default is zh_CN.UTF-8, maybe you need to change it to other
coding if your system is not zh_CN.UTF-8."
  :type 'string
  :group 'easysdcv)

(defface easysdcv-tooltip-face
  '((t (:foreground "green" :background "gray12")))
  "Face for sdcv tooltip."
  :group 'easysdcv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar easysdcv-previous-window-configuration nil
  "Window configuration before switching to sdcv buffer.")

(defvar easysdcv-current-translate-object nil
  "The search object.")

(defvar easysdcv-fail-notify-string
  "No explanation available. Consider searching with additional dictionaries."
  "User notification message on failed search.")

(defvar easysdcv-mode-font-lock-keywords
  '(;; Dictionary name
    ("^-->\\(.*\\)\n-" . (1 font-lock-type-face))
    ;; Search word
    ("^-->\\(.*\\)[ \t\n]*" . (1 font-lock-function-name-face))
    ;; Serial number
    ("\\(^[0-9] \\|[0-9]+:\\|[0-9]+\\.\\)" . (1 font-lock-constant-face))
    ;; Type name
    ("^<<\\([^>]*\\)>>$" . (1 font-lock-comment-face))
    ;; Phonetic symbol
    ("^/\\([^>]*\\)/$" . (1 font-lock-string-face))
    ("^\\[\\([^]]*\\)\\]$" . (1 font-lock-string-face)))
  "Expressions to highlight in `easysdcv-mode'.")

(easy-mmode-defmap easysdcv-mode-map
  '(;; Sdcv command.
    ("q" . easysdcv-quit)
    ("j" . easysdcv-next-line)
    ("k" . easysdcv-prev-line)
    ("J" . easysdcv-scroll-up-one-line)
    ("K" . easysdcv-scroll-down-one-line)
    ("d" . easysdcv-next-dictionary)
    ("f" . easysdcv-previous-dictionary)
    ("i" . easysdcv-search-input)
    (";" . easysdcv-search-input+)
    ("p" . easysdcv-search-pointer)
    ("y" . easysdcv-search-pointer+)
    ;; Isearch.
    ("S" . isearch-forward-regexp)
    ("R" . isearch-backward-regexp)
    ("s" . isearch-forward)
    ("r" . isearch-backward)
    ;; Hideshow.
    ("a" . outline-show-all)
    ("A" . outline-hide-body)
    ("v" . outline-show-entry)
    ("V" . outline-hide-entry)
    ;; Misc.
    ("e" . scroll-down)
    (" " . scroll-up)
    ("l" . forward-char)
    ("h" . backward-char)
    ("?" . describe-mode))
  "Keymap for `easysdcv-mode'.")

(define-derived-mode easysdcv-mode nil "sdcv"
  "Major mode to look up word through sdcv.
\\{easysdcv-mode-map}

Turning on Text mode runs the normal hook `easysdcv-mode-hook'."
  (setq font-lock-defaults '(easysdcv-mode-font-lock-keywords t))
  (setq buffer-read-only t)
  (set (make-local-variable 'outline-regexp) "^-->.*\n-->")
  (set (make-local-variable 'outline-level) #'(lambda()
                                                1))
  (outline-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun easysdcv-search-pointer (&optional word)
  "Get current WORD.
Display complete translations in other buffer."
  (interactive)
  ;; Display details translate result
  (easysdcv-search-detail (or word (easysdcv-region-or-word))))

;;;###autoload
(defun easysdcv-search-pointer+ ()
  "Translate word at point.
Show information using tooltip.  This command uses
`easysdcv-dictionary-simple-list'."
  (interactive)
  ;; Display simple translate result.
  (easysdcv-search-simple))

;;;###autoload
(defun easysdcv-search-input (&optional word)
  "Translate current input WORD.
And show information in other buffer."
  (interactive)
  ;; Display details translate result.
  (easysdcv-search-detail (or word (easysdcv-prompt-input))))

;;;###autoload
(defun easysdcv-search-input+ (&optional word)
  "Translate current WORD at point.
And show information using tooltip."
  (interactive)
  ;; Display simple translate result.
  (easysdcv-search-simple (or word (easysdcv-prompt-input))))

(defun easysdcv-quit ()
  "Bury sdcv buffer and restore previous window configuration."
  (interactive)
  (if (window-configuration-p easysdcv-previous-window-configuration)
      (progn
        (set-window-configuration easysdcv-previous-window-configuration)
        (setq easysdcv-previous-window-configuration nil)
        (bury-buffer (easysdcv-get-buffer)))
    (bury-buffer)))

(defun easysdcv-next-dictionary ()
  "Jump to next dictionary."
  (interactive)
  (outline-show-all)
  (if (search-forward-regexp "^-->.*\n-" nil t) ;don't show error when search failed
      (progn
        (call-interactively 'previous-line)
        (recenter 0))
    (message "Reached last dictionary.")))

(defun easysdcv-previous-dictionary ()
  "Jump to previous dictionary."
  (interactive)
  (outline-show-all)
  (if (search-backward-regexp "^-->.*\n-" nil t) ;don't show error when search failed
      (progn
        (forward-char 1)
        (recenter 0))                   ;adjust position
    (message "Reached first dictionary.")))

(defun easysdcv-scroll-up-one-line ()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

(defun easysdcv-scroll-down-one-line ()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(defun easysdcv-next-line (arg)
  "Go to next ARGth line and show item."
  (interactive "P")
  (ignore-errors
    (call-interactively 'next-line arg)
    (save-excursion
      (beginning-of-line nil)
      (when (looking-at outline-regexp)
        (outline-show-entry)))))

(defun easysdcv-prev-line (arg)
  "Go to previous ARGth line."
  (interactive "P")
  (ignore-errors
    (call-interactively 'previous-line arg)))

(defun easysdcv-check ()
  "Check for missing StarDict dictionaries."
  (interactive)
  (let* ((dicts (easysdcv-list-dicts))
         (missing-simple-dicts (easysdcv-missing-dicts easysdcv-dictionary-simple-list dicts))
         (missing-complete-dicts (easysdcv-missing-dicts easysdcv-dictionary-complete-list dicts)))
    (if (not (or missing-simple-dicts missing-complete-dicts))
        (message "The dictionary's settings look correct, sdcv should work as expected.")
      (dolist (dict missing-simple-dicts)
        (message "easysdcv-dictionary-simple-list: dictionary '%s' does not exist, remove it or download the corresponding dictionary file to %s"
                 dict easysdcv-dictionary-data-dir))
      (dolist (dict missing-complete-dicts)
        (message "easysdcv-dictionary-complete-list: dictionary '%s' does not exist, remove it or download the corresponding dictionary file to %s"
                 dict easysdcv-dictionary-data-dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun easysdcv-call-process (&rest arguments)
  "Call `easysdcv-program' with ARGUMENTS.
Result is parsed as json."
  (with-temp-buffer
    (save-excursion
      (let* ((lang-env (concat "LANG=" easysdcv-env-lang))
             (process-environment (cons lang-env process-environment)))
        (apply #'call-process easysdcv-program nil t nil
               (append (list "--non-interactive" "--json-output")
                       (when easysdcv-only-data-dir
                         (list "--only-data-dir"))
                       (when easysdcv-dictionary-data-dir
                         (list "--data-dir" easysdcv-dictionary-data-dir))
                       arguments))))
    (ignore-errors (json-read))))

(defun easysdcv-list-dicts ()
  "List dictionaries present in SDCV."
  (mapcar (lambda (dict) (cdr (assq 'name dict)))
          (easysdcv-call-process "--list-dicts")))

(defun easysdcv-missing-dicts (list &optional dicts)
  "List missing LIST dictionaries in DICTS.
If DICTS is nil, compute present dictionaries with
`easysdcv--list-dicts'."
  (let ((dicts (or dicts (easysdcv-list-dicts))))
    (cl-set-difference list dicts :test #'string=)))

(defun easysdcv-search-detail (&optional word)
  "Search WORD in `easysdcv-dictionary-complete-list'.
The result will be displayed in buffer named with
`easysdcv-buffer-name' in `easysdcv-mode'."
  (message "Searching...")
  (with-current-buffer (get-buffer-create easysdcv-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq easysdcv-current-translate-object word)
    (insert (easysdcv-search-with-dictionary word easysdcv-dictionary-complete-list))
    (easysdcv-goto-sdcv)
    (easysdcv-mode-reinit)))

(defun easysdcv-search-simple (&optional word)
  "Search WORD simple translate result."
  (when (ignore-errors (require 'posframe))
    (let ((result (easysdcv-search-with-dictionary word easysdcv-dictionary-simple-list)))
      ;; Show tooltip at point if word fetch from user cursor.
      (posframe-show
       easysdcv-tooltip-name
       :string result
       :position (if (derived-mode-p 'eaf-mode) (mouse-absolute-pixel-position) (point))
       :timeout easysdcv-tooltip-timeout
       :background-color (face-attribute 'easysdcv-tooltip-face :background)
       :foreground-color (face-attribute 'easysdcv-tooltip-face :foreground)
       :internal-border-width easysdcv-tooltip-border-width
       :tab-line-height 0
       :header-line-height 0)
      (unwind-protect
          (push (read-event " ") unread-command-events)
        (posframe-delete easysdcv-tooltip-name)))))

(defun easysdcv-say-word (word)
  "Listen to WORD pronunciation."
  (if (featurep 'cocoa)
      (call-process-shell-command
       (format "say %s" word) nil 0)
    (let ((player (or (executable-find "mpv")
                      (executable-find "mplayer")
                      (executable-find "mpg123"))))
      (if player
          (start-process
           player
           nil
           player
           (format "http://dict.youdao.com/dictvoice?type=2&audio=%s" (url-hexify-string word)))
        (message "mpv, mplayer or mpg123 is needed to play word voice")))))

(defun easysdcv-search-with-dictionary (word dictionary-list)
  "Search some WORD with DICTIONARY-LIST.
Argument DICTIONARY-LIST the word that needs to be transformed."
  (let* ((word (or word (easysdcv-region-or-word)))
         (translate-result (easysdcv-translate-result word dictionary-list)))

    (when (and (string= easysdcv-fail-notify-string translate-result)
               (setq word (easysdcv-pick-word)))
      (setq translate-result (easysdcv-translate-result word dictionary-list)))

    (when easysdcv-say-word-p
      (easysdcv-say-word word))

    translate-result))

(defun easysdcv-pick-word (&optional _str)
  "Pick word from camelcase string at point.
_STR is ignored and leaved for backwards compatibility."
  (let ((subword (make-symbol "subword")))
    (put subword 'forward-op 'subword-forward)
    (thing-at-point subword t)))

(defun easysdcv-translate-result (word dictionary-list)
  "Call sdcv to search WORD in DICTIONARY-LIST.
Return filtered string of results."
  (let* ((arguments (cons word (mapcan (lambda (d) (list "-u" d)) dictionary-list)))
         (result (mapconcat
                  (lambda (result)
                    (let-alist result
                      (format "-->%s\n-->%s\n%s\n\n" .dict .word .definition)))
                  (apply #'easysdcv-call-process arguments)
                  "")))
    (if (string-empty-p result)
        easysdcv-fail-notify-string
      result)))

(defun easysdcv-goto-sdcv ()
  "Switch to sdcv buffer in other window."
  (setq easysdcv-previous-window-configuration (current-window-configuration))
  (let* ((buffer (easysdcv-get-buffer))
         (window (get-buffer-window buffer)))
    (if (null window)
        ;; Use display-buffer because it follows display-buffer-alist
        (let ((win (display-buffer buffer)))  ; Display the buffer
          (when win
            (select-window win)))
      (select-window window))))

(defun easysdcv-get-buffer ()
  "Get the sdcv buffer.  Create one if there's none."
  (let ((buffer (get-buffer-create easysdcv-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'easysdcv-mode)
        (easysdcv-mode)))
    buffer))

(defvar easysdcv-mode-reinit-hook 'nil
  "Hook for `easysdcv-mode-reinit'.
This hook is called after `easysdcv-search-detail'.")

(defun easysdcv-mode-reinit ()
  "Re-initialize buffer.
Hide all entry but the first one and goto
the beginning of the buffer."
  (ignore-errors
    (setq buffer-read-only t)
    (goto-char (point-min))
    (easysdcv-next-dictionary)
    (outline-show-all)
    (run-hooks 'easysdcv-mode-reinit-hook)
    (message "Finished searching `%s'." easysdcv-current-translate-object)))

(defun easysdcv-prompt-input ()
  "Prompt input for translation."
  (let* ((word (easysdcv-region-or-word))
         (default (if word (format " (default %s)" word) "")))
    (read-string (format "Word%s: " default) nil nil word)))

(defun easysdcv-region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word t)))


(provide 'easysdcv)

;;; easysdcv.el ends here

;;; LocalWords:  sdcv StartDict startdict posframe stardict KDic XDICT CDICT
;;; LocalWords:  FOLDOC WordNet ChiYuan Hideshow reinit
