;;; easysdcv.el --- Interface for the sdcv command (StartDict cli dictionary) -*- lexical-binding: t -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/
;; Copyright (C) 2009 Andy Stewart <lazycat.manatee@gmail.com>

;; Filename: easysdcv.el
;; Description: Interface for sdcv (StartDict console version).
;; Package-Requires: ((emacs "25.1"))
;; Maintainer: James Cherti
;; Original Author: Andy Stewart
;; Created: 2009-02-05 22:04:02
;; Version: 3.6
;; URL: https://github.com/jamescherti/easysdcv.el
;; Keywords: docs, startdict, sdcv

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
;; The `easysdcv` package serves as an Emacs interface for the `sdcv`
;; command-line interface, which is the console version of the StarDict
;; dictionary application.
;;
;; This integration allows users to access and utilize dictionary
;; functionalities directly within the Emacs environment, leveraging the
;; capabilities of `sdcv` to look up words and translations from various
;; dictionary files formatted for StarDict.
;;
;; Below are the commands you can use:
;; - `easysdcv-search-pointer': Searches the word around the cursor and displays
;;   the result in a buffer.
;; - `easysdcv-search-input': Searches the input word and displays the result in
;;   a buffer.
;;

;;; Require

(require 'json)
(require 'cl-lib)
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

(defcustom easysdcv-program "sdcv"
  "Path to sdcv."
  :type 'file
  :group 'easysdcv)

(defcustom easysdcv-dictionary-complete-list nil
  "A list of dictionaries used for translation in easysdcv.
Each entry should specify a dictionary source, allowing multiple dictionaries to
be utilized in the translation process."
  :type '(repeat string)
  :group 'easysdcv)

(defcustom easysdcv-dictionary-data-dir nil
  "The sdcv data directory.
By default, sdcv searches for words in /usr/share/startdict/dict/.
If you customize this value with a local directory, you won't need to copy
dictionary data to the /usr/share directory every time you finish system
installation."
  :type '(choice (const :tag "Default" nil) directory)
  :group 'easysdcv)

(defcustom easysdcv-only-data-dir t
  "Only use the dictionaries in data-dir `easysdcv-dictionary-data-dir'.
Do not search in user and system directories"
  :type 'boolean
  :group 'easysdcv)

(defcustom easysdcv-exact-search nil
  "Do not fuzzy-search for similar words, only return exact matches."
  :type 'boolean
  :group 'easysdcv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar easysdcv-current-translate-object nil
  "The search object.")

(defvar easysdcv-fail-notify-string
  "If there is no explanation available, consider searching with additional
  dictionaries. User notification message for a failed search.")

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

;; Optionally, you might want to define the mode itself here.
(defvar easysdcv-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-derived-mode easysdcv-mode nil "sdcv"
  "Major mode to look up word through sdcv.
\\{easysdcv-mode-map}

Enabling this mode runs the normal hook `easysdcv-mode-hook`."
  (setq font-lock-defaults '(easysdcv-mode-font-lock-keywords t))
  (setq buffer-read-only t)
  (set (make-local-variable 'outline-regexp) "^-->.*\n-->")
  (set (make-local-variable 'outline-level) #'(lambda()
                                                1))
  (outline-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun easysdcv-search-pointer ()
  "Retrieve the word under the cursor and display its definition.
It displays the result in another buffer."
  (interactive)
  (easysdcv--search-detail (easysdcv--get-region-or-word)))

;;;###autoload
(defun easysdcv-search-input (&optional word)
  "Translate the specified input WORD and display the results in another buffer.

If WORD is not provided, the function prompts the user to enter a word.
The details will be shown in the `easysdcv-buffer-name' buffer."
  (interactive)
  (easysdcv--search-detail (or word (easysdcv--prompt-input))))

;;;###autoload
(defun easysdcv-list-dictionaries ()
  "List all available dictionaries in a separate buffer."
  (interactive)
  (let ((dicts (easysdcv--get-list-dicts)))
    (with-output-to-temp-buffer easysdcv-buffer-name
      (dolist (dict dicts)
        (princ (format "%s\n" dict))))))

(defun easysdcv-check ()
  "Check for missing StarDict dictionaries."
  (interactive)
  (let* ((dicts (easysdcv--get-list-dicts))
         (missing-complete-dicts (easysdcv--get-missing-dicts easysdcv-dictionary-complete-list dicts)))
    (if (not missing-complete-dicts)
        (message "The dictionary's settings look correct, sdcv should work as expected.")
      (dolist (dict missing-complete-dicts)
        (message "easysdcv-dictionary-complete-list: dictionary '%s' does not exist, remove it or download the corresponding dictionary file to %s"
                 dict easysdcv-dictionary-data-dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun easysdcv--call-process (&rest arguments)
  "Call `easysdcv-program' with ARGUMENTS.
Result is parsed as json."
  (unless (executable-find easysdcv-program)
    (error (concat "The program '%s' is not found. Please ensure it is "
                   "installed and the path is correctly set "
                   "in `easysdcv-program`.")
           easysdcv-program))
  (with-temp-buffer
    (save-excursion
      (let ((exit-code (apply #'call-process easysdcv-program nil t nil
                              (append (list "--non-interactive" "--json-output")
                                      (when easysdcv-exact-search
                                        (list "--exact-search"))
                                      (when easysdcv-only-data-dir
                                        (list "--only-data-dir"))
                                      (when easysdcv-dictionary-data-dir
                                        (list "--data-dir" easysdcv-dictionary-data-dir))
                                      arguments))))
        (if (not (zerop exit-code))
            (error "Failed to call %s: exit code %d" easysdcv-program
                   exit-code))))
    (ignore-errors (json-read))))

(defun easysdcv--get-list-dicts ()
  "List dictionaries present in SDCV."
  (mapcar (lambda (dict) (cdr (assq 'name dict)))
          (easysdcv--call-process "--list-dicts")))

(defun easysdcv--get-missing-dicts (list &optional dicts)
  "List missing LIST dictionaries in DICTS.
If DICTS is nil, compute present dictionaries with
`easysdcv--get-list-dicts'."
  (let ((dicts (or dicts (easysdcv--get-list-dicts))))
    (cl-set-difference list dicts :test #'string=)))

(defun easysdcv--search-detail (&optional word)
  "Search WORD in `easysdcv-dictionary-complete-list'.
The result will be displayed in buffer named with
`easysdcv-buffer-name' in `easysdcv-mode'."
  (when word
    (message "Searching...")
    (with-current-buffer (get-buffer-create easysdcv-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq easysdcv-current-translate-object word)
      (insert (easysdcv--search-with-dictionary word easysdcv-dictionary-complete-list))
      (easysdcv--goto-sdcv)
      ;; Re-initialize buffer. Hide all entry but the first one and goto the
      ;; beginning of the buffer.
      (ignore-errors
        (setq buffer-read-only t)
        (goto-char (point-min))
        (outline-next-heading)
        (outline-show-all)
        (message "Finished searching `%s'." easysdcv-current-translate-object)))))

(defun easysdcv--search-with-dictionary (word dictionary-list)
  "Search some WORD with DICTIONARY-LIST.
Argument DICTIONARY-LIST the word that needs to be transformed."
  (let* ((word (or word (easysdcv--get-region-or-word)))
         (translate-result (easysdcv--translate-result word dictionary-list)))

    (when (and (string= easysdcv-fail-notify-string translate-result)
               (setq word (thing-at-point 'word t)))
      (setq translate-result (easysdcv--translate-result word dictionary-list)))

    translate-result))

(defun easysdcv--translate-result (word dictionary-list)
  "Call sdcv to search WORD in DICTIONARY-LIST.
Return filtered string of results."
  (let* ((arguments (cons word (mapcan (lambda (d) (list "-u" d)) dictionary-list)))
         (result (mapconcat
                  (lambda (result)
                    (let-alist result
                      (format "-->%s\n-->%s\n%s\n\n" .dict .word .definition)))
                  (apply #'easysdcv--call-process arguments)
                  "")))
    (if (string-empty-p result)
        easysdcv-fail-notify-string
      result)))

(defun easysdcv--goto-sdcv ()
  "Switch to sdcv buffer in other window."
  (let* ((buffer (easysdcv--get-buffer))
         (window (get-buffer-window buffer)))
    (if (null window)
        ;; Use display-buffer because it follows display-buffer-alist
        (let ((win (display-buffer buffer)))  ; Display the buffer
          (when win
            (select-window win)))
      (select-window window))))

(defun easysdcv--get-buffer ()
  "Get the sdcv buffer.  Create one if there's none."
  (let ((buffer (get-buffer-create easysdcv-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'easysdcv-mode)
        (easysdcv-mode)))
    buffer))

(defun easysdcv--prompt-input ()
  "Prompt input for translation."
  (let* ((word (easysdcv--get-region-or-word))
         (default (if word (format " (default %s)" word) "")))
    (read-string (format "Word%s: " default) nil nil word)))

(defun easysdcv--get-region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word t)))

(provide 'easysdcv)

;;; easysdcv.el ends here
