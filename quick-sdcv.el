;;; quick-sdcv.el --- Interface for the sdcv command (StartDict cli dictionary) -*- lexical-binding: t -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/
;; Copyright (C) 2009 Andy Stewart

;; Filename: quick-sdcv.el
;; Description: Interface for sdcv (StartDict console version).
;; Package-Requires: ((emacs "25.1"))
;; Maintainer: James Cherti
;; Original Author: Andy Stewart
;; Created: 2009-02-05 22:04:02
;; Version: 3.6
;; URL: https://github.com/jamescherti/quick-sdcv.el
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
;; The `quick-sdcv` package serves as an Emacs interface for the `sdcv`
;; command-line interface, which is the console version of the StarDict
;; dictionary application.
;;
;; This integration allows users to access and utilize dictionary
;; functionalities directly within the Emacs environment, leveraging the
;; capabilities of `sdcv` to look up words and translations from various
;; dictionary files formatted for StarDict.
;;
;; Below are the commands you can use:
;; - `quick-sdcv-search-at-point': Searches the word around the cursor and
;;   displays the result in a buffer.
;; - `quick-sdcv-search-input': Searches the input word and displays the result
;;   in a buffer.
;;

;;; Require

(require 'json)
(require 'cl-lib)
(require 'outline)
(require 'subword)

;;; Code:

;;; Customize

(defgroup quick-sdcv nil
  "Interface for sdcv (StartDict console version)."
  :group 'edit)

(defcustom quick-sdcv-unique-word-buffers nil
  "If non-nil, create a unique buffer for each word lookup.
This allows multiple definitions to be viewed simultaneously,
with each word appearing in its own buffer.

For instance, if the user searches for the word computer:
- When non-nil, the buffer name will be *SDCV:computer*
- When nil, the buffer name will be *SDCV*

This can be customized with:
- `quick-sdcv-buffer-name-prefix'
- `quick-sdcv-buffer-name-separator'
- `quick-sdcv-buffer-name-suffix'"
  :type 'boolean
  :group 'quick-sdcv)

(defcustom quick-sdcv-buffer-name-prefix "*SDCV"
  "The prefix of the sdcv buffer name."
  :type 'string
  :group 'quick-sdcv)

(defcustom quick-sdcv-buffer-name-separator ":"
  "The separator of the sdcv buffer name."
  :type 'string
  :group 'quick-sdcv)

(defcustom quick-sdcv-buffer-name-suffix "*"
  "The suffix of the sdcv buffer name."
  :type 'string
  :group 'quick-sdcv)

(defcustom quick-sdcv-program "sdcv"
  "Path to sdcv."
  :type 'file
  :group 'quick-sdcv)

(defcustom quick-sdcv-dictionary-complete-list nil
  "A list of dictionaries used for translation in quick-sdcv.
Each entry should specify a dictionary source, allowing multiple dictionaries to
be utilized in the translation process."
  :type '(repeat string)
  :group 'quick-sdcv)

(defcustom quick-sdcv-dictionary-data-dir nil
  "The sdcv data directory.
By default, sdcv searches for words in /usr/share/startdict/dict/.
If you customize this value with a local directory, you won't need to copy
dictionary data to the /usr/share directory every time you finish system
installation."
  :type '(choice (const :tag "Default" nil) directory)
  :group 'quick-sdcv)

(defcustom quick-sdcv-only-data-dir t
  "Only use the dictionaries in data-dir `quick-sdcv-dictionary-data-dir'.
Do not search in user and system directories"
  :type 'boolean
  :group 'quick-sdcv)

(defcustom quick-sdcv-exact-search nil
  "Do not fuzzy-search for similar words, only return exact matches."
  :type 'boolean
  :group 'quick-sdcv)

(defcustom quick-sdcv-dictionary-prefix-symbol "►"
  "Bullet character used in sdcv dictionaries.

This variable specifies the single character used as a bullet in the output of
sdcv dictionaries. The bullet replaces the standard output arrow ('-->')
visually. This can be set to nil to disable the bullet feature entirely,
allowing the output to display without any visual replacements.

The value should be a single character. Setting this variable to an empty string
or nil will disable the bullet feature."
  :group 'quick-sdcv
  :type '(choice (string :tag "Bullet character" :size 1)
                 (const :tag "No bullet" nil)))

;;; Variables

(defvar quick-sdcv--keywords
  `(("^-->.*\n-->"
     (0 (let* ((heading-start (match-beginning 0))
               (heading-end (+ heading-start 3)))
          (compose-region (- heading-end 3) (- heading-end 1)
                          quick-sdcv-dictionary-prefix-symbol)
          (compose-region heading-end (- heading-end 1)
                          " ")
          nil)))))

(defvar quick-sdcv-current-translate-object nil
  "The search object.")

(defvar quick-sdcv-fail-notify-string
  "If there is no explanation available, consider searching with additional
  dictionaries. User notification message for a failed search.")

(defvar quick-sdcv-mode-font-lock-keywords
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
  "Expressions to highlight in `quick-sdcv-mode'.")

;; Optionally, you might want to define the mode itself here.
(defvar quick-sdcv-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-derived-mode quick-sdcv-mode nil "sdcv"
  "Major mode to look up word through sdcv.
\\{quick-sdcv-mode-map}

Enabling this mode runs the normal hook `quick-sdcv-mode-hook`."
  (setq font-lock-defaults '(quick-sdcv-mode-font-lock-keywords t))
  (setq buffer-read-only t)
  (set (make-local-variable 'outline-regexp) "^-->.*\n-->")
  (set (make-local-variable 'outline-level) #'(lambda()
                                                1))
  (setq-local font-lock-multiline t)
  (outline-minor-mode)
  (quick-sdcv--toggle-bullet-fontification t))

;;; Interactive Functions

;;;###autoload
(defun quick-sdcv-search-at-point ()
  "Retrieve the word under the cursor and display its definition.
It displays the result in another buffer."
  (interactive)
  (quick-sdcv--search-detail (quick-sdcv--get-region-or-word)))

;;;###autoload
(defun quick-sdcv-search-input (&optional word)
  "Translate the specified input WORD and display the results in another buffer.

If WORD is not provided, the function prompts the user to enter a word.
The details will be shown in the sdcv buffer."
  (interactive)
  (quick-sdcv--search-detail (or word (quick-sdcv--prompt-input))))

;;;###autoload
(defun quick-sdcv-list-dictionaries ()
  "List all available dictionaries in a separate buffer."
  (interactive)
  (let ((dicts (quick-sdcv--get-list-dicts)))
    (with-output-to-temp-buffer (quick-sdcv--get-buffer-name "list-dict" t)
      (dolist (dict dicts)
        (princ (format "%s\n" dict))))))

(defun quick-sdcv-check ()
  "Check for missing StarDict dictionaries."
  (let* ((dicts (quick-sdcv--get-list-dicts))
         (missing-complete-dicts
          (quick-sdcv--get-missing-dicts
           quick-sdcv-dictionary-complete-list
           dicts)))
    (if (not missing-complete-dicts)
        (message (concat "The dictionary's settings look correct, sdcv "
                         "should work as expected."))
      (dolist (dict missing-complete-dicts)
        (message (concat "quick-sdcv-dictionary-complete-list: dictionary "
                         "'%s' does not exist, remove it or download the "
                         "corresponding dictionary file to %s")
                 dict quick-sdcv-dictionary-data-dir)))))

;;; Utilitiy Functions

(defun quick-sdcv--get-buffer-name (&optional word force-include-word)
  "Return the buffer name for WORD.
If FORCE-INCLUDE-WORD is non-nil, it will always include WORD in the buffer
name."
  (concat quick-sdcv-buffer-name-prefix
          (when (and (or force-include-word
                         quick-sdcv-unique-word-buffers)
                     word)
            (concat quick-sdcv-buffer-name-separator
                    word))
          quick-sdcv-buffer-name-suffix))

(defun quick-sdcv--toggle-bullet-fontification (enabled)
  "Toggle fontification of bullets in the quick-sdcv buffer.

When ENABLED is non-nil, adds font-lock keywords to highlight bullets
using `quick-sdcv--keywords`. If ENABLED is nil, removes the keywords
and deconstructs any bullet regions marked by '-->' in the buffer.

This function also calls `quick-sdcv--fontify-buffer` to apply
fontification to the entire buffer after updating keywords."
  (when quick-sdcv-dictionary-prefix-symbol
    (if enabled
        (font-lock-add-keywords nil quick-sdcv--keywords)
      (save-excursion
        (goto-char (point-min))
        (font-lock-remove-keywords nil quick-sdcv--keywords)
        (while (re-search-forward "^-->.*\n-->" nil t)
          (decompose-region (match-beginning 0) (match-end 0)))))

    ;; Fontify the buffer
    (when font-lock-mode
      (save-restriction
        (widen)
        (when (fboundp 'font-lock-flush)
          (font-lock-flush))
        (when (fboundp 'font-lock-ensure)
          (font-lock-ensure)))
      (with-no-warnings
        (font-lock-fontify-buffer)))))

(defun quick-sdcv--call-process (&rest arguments)
  "Call `quick-sdcv-program' with ARGUMENTS.
Result is parsed as json."
  (unless (executable-find quick-sdcv-program)
    (error (concat "The program '%s' is not found. Please ensure it is "
                   "installed and the path is correctly set "
                   "in `quick-sdcv-program`.")
           quick-sdcv-program))
  (with-temp-buffer
    (save-excursion
      (let ((exit-code (apply #'call-process quick-sdcv-program nil t nil
                              (append (list "--non-interactive" "--json-output")
                                      (when quick-sdcv-exact-search
                                        (list "--exact-search"))
                                      (when quick-sdcv-only-data-dir
                                        (list "--only-data-dir"))
                                      (when quick-sdcv-dictionary-data-dir
                                        (list "--data-dir"
                                              quick-sdcv-dictionary-data-dir))
                                      arguments))))
        (if (not (zerop exit-code))
            (error "Failed to call %s: exit code %d" quick-sdcv-program
                   exit-code))))
    (ignore-errors (json-read))))

(defun quick-sdcv--get-list-dicts ()
  "List dictionaries present in SDCV."
  (mapcar (lambda (dict) (cdr (assq 'name dict)))
          (quick-sdcv--call-process "--list-dicts")))

(defun quick-sdcv--get-missing-dicts (list &optional dicts)
  "List missing LIST dictionaries in DICTS.
If DICTS is nil, compute present dictionaries with
`quick-sdcv--get-list-dicts'."
  (let ((dicts (or dicts (quick-sdcv--get-list-dicts))))
    (cl-set-difference list dicts :test #'string=)))

(defun quick-sdcv--search-detail (&optional word)
  "Search WORD in `quick-sdcv-dictionary-complete-list'.
The result will be displayed in buffer named with
`quick-sdcv-buffer-name' in `quick-sdcv-mode'."
  (when word
    (let* ((buffer-name (quick-sdcv--get-buffer-name word))
           (buffer (get-buffer buffer-name))
           (refresh (or (not buffer)
                        ;; When the words share the same buffer, always refresh
                        (not quick-sdcv-unique-word-buffers))))
      (unless buffer
        (setq buffer (quick-sdcv--get-buffer word)))

      (when buffer
        (with-current-buffer buffer
          (when refresh
            ;; (message "Searching...")
            (setq buffer-read-only nil)
            (erase-buffer)
            (setq quick-sdcv-current-translate-object word)
            (insert (quick-sdcv--search-with-dictionary
                     word
                     quick-sdcv-dictionary-complete-list))

            (setq buffer-read-only t)
            (goto-char (point-min))
            (ignore-errors
              (outline-next-heading))

            ;; (message "Finished searching `%s'."
            ;;          quick-sdcv-current-translate-object)
            )
          (quick-sdcv--goto-sdcv word))))))

(defun quick-sdcv--search-with-dictionary (word dictionary-list)
  "Search some WORD with DICTIONARY-LIST.
Argument DICTIONARY-LIST the word that needs to be transformed."
  (let* ((word (or word (quick-sdcv--get-region-or-word)))
         (translate-result (quick-sdcv--translate-result word dictionary-list)))

    (when (and (string= quick-sdcv-fail-notify-string translate-result)
               (setq word (thing-at-point 'word t)))
      (setq translate-result (quick-sdcv--translate-result word dictionary-list)))

    translate-result))

(defun quick-sdcv--translate-result (word dictionary-list)
  "Call sdcv to search WORD in DICTIONARY-LIST.
Return filtered string of results."
  (let* ((arguments (cons word (mapcan (lambda (d) (list "-u" d)) dictionary-list)))
         (result (mapconcat
                  (lambda (result)
                    (let-alist result
                      (format "-->%s\n-->%s\n%s\n\n" .dict .word .definition)))
                  (apply #'quick-sdcv--call-process arguments)
                  "")))
    (if (string-empty-p result)
        quick-sdcv-fail-notify-string
      result)))

(defun quick-sdcv--goto-sdcv (&optional word)
  "Switch to sdcv buffer of WORD in other window."
  (let* ((buffer (quick-sdcv--get-buffer word))
         (window (when buffer
                   (get-buffer-window buffer))))
    (when buffer
      (if window
          (progn
            (select-window window)
            t)
        ;; Use display-buffer because it follows display-buffer-alist
        (let ((win (display-buffer buffer)))  ; Display the buffer
          (when win
            (select-window win)
            t))))))

(defun quick-sdcv--get-buffer (&optional word)
  "Get the sdcv buffer of WORD. Create one if there's none."
  (let* ((buffer-name (quick-sdcv--get-buffer-name word))
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (unless (eq major-mode 'quick-sdcv-mode)
          (quick-sdcv-mode)))
      buffer)))

(defun quick-sdcv--prompt-input ()
  "Prompt input for translation."
  (let* ((word (quick-sdcv--get-region-or-word))
         (default (if word (format " (default: %s)" word) "")))
    (read-string (format "Word%s: " default) nil nil word)))

(defun quick-sdcv--get-region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word t)))

(provide 'quick-sdcv)

;;; quick-sdcv.el ends here
