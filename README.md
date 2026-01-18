# quick-sdcv.el - Emacs offline dictionary using 'sdcv'
![Build Status](https://github.com/jamescherti/quick-sdcv.el/actions/workflows/ci.yml/badge.svg)
[![MELPA](https://melpa.org/packages/quick-sdcv-badge.svg)](https://melpa.org/#/quick-sdcv)
[![MELPA Stable](https://stable.melpa.org/packages/quick-sdcv-badge.svg)](https://stable.melpa.org/#/quick-sdcv)
![License](https://img.shields.io/github/license/jamescherti/quick-sdcv.el)
![](https://jamescherti.com/misc/made-for-gnu-emacs.svg)

The [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el) package serves as a lightweight Emacs interface for the `sdcv` command-line interface, which is the console version of the StarDict dictionary application.

**This package enables Emacs to function as an offline dictionary.**

This integration allows users to access sdcv dictionary functionalities directly within the Emacs environment, leveraging the capabilities of `sdcv` to look up words and translations from various dictionary files.

Here are the main interactive functions:
- `quick-sdcv-search-at-point`: Searches the word around the cursor and displays the result in a buffer.
- `quick-sdcv-search-input`: Searches the input word and displays the result in a buffer.

If this enhances your workflow, please show your support by **⭐ starring quick-sdcv on GitHub** to help more Emacs users discover its benefits.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [quick-sdcv.el - Emacs offline dictionary using 'sdcv'](#quick-sdcvel---emacs-offline-dictionary-using-sdcv)
  - [Prerequisite](#prerequisite)
  - [Installation](#installation)
  - [Usage](#usage)
  - [Customizations](#customizations)
  - [Usage](#usage-1)
  - [Frequently asked question](#frequently-asked-question)
    - [How to make the sdcv buffer replace the current buffer?](#how-to-make-the-sdcv-buffer-replace-the-current-buffer)
    - [How to make links appear as links in an sdcv buffer?](#how-to-make-links-appear-as-links-in-an-sdcv-buffer)
    - [Evil mode: How to configure the default K key to search for words using quick-sdcv?](#evil-mode-how-to-configure-the-default-k-key-to-search-for-words-using-quick-sdcv)
    - [What is the difference between sdcv and quick-sdcv Emacs packages?](#what-is-the-difference-between-sdcv-and-quick-sdcv-emacs-packages)
  - [Comments from users](#comments-from-users)
  - [Links](#links)

<!-- markdown-toc end -->

## Prerequisite

- The [sdcv](https://github.com/Dushistov/sdcv) command. (It can usually be installed by installing the `sdcv` package.)
- Download dictionaries from: http://download.huzheng.org/ . Once the dictionaries are downloaded, extract them into `/usr/share/stardict/dic/`, or configure the variable `quick-sdcv-dictionary-data-dir` in the Emacs configuration to specify an alternative dictionary path.

## Installation


To install *quick-sdcv* on Emacs from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).

2. Add the following code **at the very beginning of your init.el file, before all other packages**:
```emacs-lisp
(use-package quick-sdcv
  :ensure t
  :custom
  (quick-sdcv-dictionary-prefix-symbol "►")
  (quick-sdcv-ellipsis " ▼"))
```

## Usage

To retrieve the word under the cursor and display its definition in a buffer:
``` elisp
(quick-sdcv-search-at-point)
```

To prompt the user for a word and display its definition in a buffer:
``` elisp
(quick-sdcv-search-input)
```

## Customizations

To create a unique buffer for each word lookup, set the following:
```emacs-lisp
;; Controls whether each word lookup creates a separate buffer.
;; Its default value is nil, but it can be set to t to enable unique buffers.
;;
;; When non-nil, a distinct buffer is created for each word searched. For
;; example, searching for the word "computer" produces a buffer named
;; "*sdcv:computer*". When nil, all lookups share the same buffer, typically
;; named "*sdcv*".
;;
;; The naming of unique buffers can be further customized using the variables:
;; - 'quick-sdcv-buffer-name-prefix'
;; - 'quick-sdcv-buffer-name-separator'
;; - 'quick-sdcv-buffer-name-suffix'
(setq quick-sdcv-unique-buffers t)
```

To perform exact word searches (as opposed to fuzzy searches), use:
```emacs-lisp
;; To perform exact word searches (as opposed to fuzzy searches), use:
(setq quick-sdcv-exact-search t)
```

To change the prefix character used before dictionary names, replacing the default `-->`, set:
```emacs-lisp
;; To change the prefix character used before dictionary names, replacing the
;; default `-->`, set:
(setq quick-sdcv-dictionary-prefix-symbol "►")
```

Customize the *quick-sdcv* dictionaries ellipsis display:
```elisp
;; Customize the *quick-sdcv* dictionaries ellipsis display. In quick-sdcv
;; buffers, `outline-minor-mode' is enabled by default, which allows sections
;; corresponding to individual dictionaries to be folded. The ellipsis (…)
;; indicates a folded section, making it easy to collapse all dictionaries and
;; expand only those of interest
(setq quick-sdcv-ellipsis " ▼")
```

To customize the sdcv history size:
``` elisp
;; Customize the sdcv history size
(setq quick-sdcv-hist-size 100)
```

To specify the path to the sdcv executable:
``` elisp
;; Specify the path to the sdcv executable:
(setq quick-sdcv-program "/path/to/sdcv")
```

To customize the naming convention of the SDCV buffer:
``` elisp
;; Customize the naming convention of the SDCV buffer:
(setq quick-sdcv-buffer-name-prefix "*sdcv"
      quick-sdcv-buffer-name-separator ":"
      quick-sdcv-buffer-name-suffix "*")
```

To specify a list of dictionaries (NOT RECOMMENDED. It is better to let sdcv show all dictionaries):
``` elisp
;; To specify a list of dictionaries (NOT RECOMMENDED. It is better to let sdcv
;; show all dictionaries):
(setq quick-sdcv-dictionary-complete-list '("stardict-WordNet"
                                            "stardict-Webster"
                                            "stardict-eng_eng_main"))
```

## Usage

Below are the commands you can use:

| Command                   | Description
| :---                      | :---
| `quick-sdcv-search-at-point` | Searches the word around the cursor and displays the result in a buffer.
| `quick-sdcv-search-input`   | Searches the input word and displays the result in a buffer.

If the current mark is active, the `quick-sdcv` will translate the region string; otherwise, they will translate the word around the cursor.

## Frequently asked question

### How to make the sdcv buffer replace the current buffer?

To make K search for the word using quick-sdcv when editing Markdown, you can customize the behavior as follows:
```elisp
(add-to-list 'display-buffer-alist '("\\*sdcv"
                                       (display-buffer-same-window)))
```

### How to make links appear as links in an sdcv buffer?

To ensure that links appear as clickable links in the SDCV buffer while using quick-sdcv, add the following hook:
```
(add-hook 'quick-sdcv-mode-hook #'goto-address-mode)
```

### Evil mode: How to configure the default K key to search for words using quick-sdcv?

In Evil-mode, the K key in normal mode typically triggers a help function. While viewing a word's definition in a *quick-sdcv* buffer, pressing `K` in normal mode jumps to the definition of the word at point.

This behavior can be configured in other modes, allowing, for instance, the definition of a word to be displayed by pressing `K` while editing a Markdown or Org file.

For example, to configure `K` to search for a word using *quick-sdcv* when editing Markdown or Org files, use the following customization:
```elisp
(dolist (mode-hook '(markdown-mode-hook org-mode-hook))
  (add-hook mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'quick-sdcv-search-at-point))))
```

### What is the difference between sdcv and quick-sdcv Emacs packages?

The `quick-sdcv` Emacs package is a fork of `sdcv.el` version 3.4, which is available on MELPA. The primary differences between the two packages are as follows:

- **Less dependencies:** Quick-sdcv does not require any external dependencies; sdcv, on the other hand, installs popup, pos-tip, and showtip.
- **Customize the buffer name:**: New variables to customize whether the word is included in the buffer name, as well as the prefix, separator, and suffix of the buffer name (`quick-sdcv-unique-buffers`, quick-sdcv-buffer-name-prefix, quick-sdcv-buffer-name-separator, and quick-sdcv-buffer-name-suffix). When the buffer is dedicated to a specific word, refresh it only when the buffer is created.
- **Improved Outline Minor Mode**: The `quick-sdcv` package fixes the outline minor mode for dictionary folding, enabling users to collapse all definitions for quicker navigation through dictionaries.
- **Default Language Settings**: Various issues have been addressed, including changing the default language setting from Chinese (zh) to nil, providing a more neutral starting point.
- **Buffer Customization**: The `quick-sdcv` package employs `display-buffer`, allowing users to customize the display of the *sdcv* buffer and control its placement through `display-buffer-alist`.
- **Removal of bugs and Warnings**: All Emacs warnings have been eliminated and bugs fixed. (e.g., when `sdcv-search-at-point` cannot locate the word under the cursor)
- **Code Simplification**: The code has been simplified by removing unused variables and omitting features like posframe, text-to-speech using the 'say' command, the quick-sdcv-env-lang variable, and functions such as (quick-sdcv-scroll-up-one-line, quick-sdcv-scroll-down-one-line, quick-sdcv-next-line and quick-sdcv-prev-line) which are similar Emacs features. This simplification makes `quick-sdcv` easier to understand, maintain, and use by focusing solely on dictionary lookup functionality. Features like `posframe` and text-to-speech, which are not essential to core usage, are better suited as separate packages.
- **Keybindings removal**: The default keybindings have been removed from `quick-sdcv-mode` to prevent conflicts with other modes and keeps the mode lightweight and adaptable for users’ preferences.
- **New options**: `quick-sdcv-ellipsis`, quick-sdcv-hist-size, quick-sdcv-exact-search, quick-sdcv-buffer-name-prefix, quick-sdcv-buffer-name-separator, quick-sdcv-buffer-name-suffix, quick-sdcv-verbose
- **Various improvements**: Unset the SDCV_PAGER environment variable, Ensure the buffer and the SDCV output are in UTF-8, Enhance dictionary representation with UTF-8 characters, Implement error handling for cases when the sdcv program is not found.

## Comments from users

- [ecraven](https://github.com/jamescherti/quick-sdcv.el/issues/4#issue-3816307365): Thank you very much for this mode, it is proving very helpful!

## Links

- You can download sdcv dictionaries from http://download.huzheng.org/dict.org/
- [quick-sdcv.el @GitHub](https://github.com/jamescherti/quick-sdcv.el)
- [quick-sdcv.el @MELPA](https://melpa.org/#/sdcv)
- The [sdcv](https://github.com/Dushistov/sdcv) command-line interface (prerequisite).

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.
- [stripspace.el](https://github.com/jamescherti/stripspace.el): Ensure Emacs Automatically removes trailing whitespace before saving a buffer, with an option to preserve the cursor column.
- [persist-text-scale.el](https://github.com/jamescherti/persist-text-scale.el): Ensure that all adjustments made with text-scale-increase and text-scale-decrease are persisted and restored across sessions.
- [pathaction.el](https://github.com/jamescherti/pathaction.el): Execute the pathaction command-line tool from Emacs. The pathaction command-line tool enables the execution of specific commands on targeted files or directories. Its key advantage lies in its flexibility, allowing users to handle various types of files simply by passing the file or directory as an argument to the pathaction tool. The tool uses a .pathaction.yaml rule-set file to determine which command to execute. Additionally, Jinja2 templating can be employed in the rule-set file to further customize the commands.
- [kirigami.el](https://github.com/jamescherti/kirigami.el): The *kirigami* Emacs package offers a unified interface for opening and closing folds across a diverse set of major and minor modes in Emacs, including `outline-mode`, `outline-minor-mode`, `outline-indent-mode`, `org-mode`, `markdown-mode`, `vdiff-mode`, `vdiff-3way-mode`, `hs-minor-mode`, `hide-ifdef-mode`, `origami-mode`, `yafolding-mode`, `folding-mode`, and `treesit-fold-mode`. With Kirigami, folding key bindings only need to be configured **once**. After that, the same keys work consistently across all supported major and minor modes, providing a unified and predictable folding experience.
