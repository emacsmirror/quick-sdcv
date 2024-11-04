# quick-sdcv.el - Emacs interface for the 'sdcv' command-line dictionary
![License](https://img.shields.io/github/license/jamescherti/quick-sdcv.el)
![](https://raw.githubusercontent.com/jamescherti/quick-sdcv.el/main/.images/made-for-gnu-emacs.svg)

The `quick-sdcv` package serves as an Emacs interface for the `sdcv` command-line interface, which is the console version of the StarDict dictionary application.

**This package enables Emacs to function as an offline dictionary.**

This integration allows users to access and utilize dictionary functionalities directly within the Emacs environment, leveraging the capabilities of `sdcv` to look up words and translations from various dictionary files formatted for StarDict.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [quick-sdcv.el - Emacs interface for the 'sdcv' command-line dictionary](#quick-sdcvel---emacs-interface-for-the-sdcv-command-line-dictionary)
    - [Installation](#installation)
        - [Install using straight](#install-using-straight)
    - [Configuration](#configuration)
    - [Usage](#usage)
    - [Frequently asked question](#frequently-asked-question)
        - [What is the difference between sdcv (MELPA) and quick-sdcv Emacs packages?](#what-is-the-difference-between-sdcv-melpa-and-quick-sdcv-emacs-packages)
    - [Links](#links)

<!-- markdown-toc end -->

## Installation

Prerequisite: the `sdcv` command

### Install using straight

To install the `quick-sdcv` using `straight.el`:

1. It if hasn't already been done, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package quick-sdcv
  :ensure t
  :straight (quick-sdcv
             :type git
             :host github
             :repo "jamescherti/quick-sdcv.el"))
```

## Configuration

```elisp
(setq quick-sdcv-dictionary-data-dir "startdict_dictionary_directory") ; Set up the directory for the Stardict dictionary

;; Set up the dictionary list for complete search
(setq quick-sdcv-dictionary-complete-list
      '("ENG-FRA Dictionary"
        "FRA-ENG Dictionary"
        "stardict 1.3 ENG-FRA Dictionary"
        "WordNet"
        "Jargon"))
```

After completing the above configuration, execute the `quick-sdcv-check` Emacs command to confirm that the dictionary settings are correct. Otherwise, `quick-sdcv` will not function correctly due to the absence of dictionary files in `quick-sdcv-dictionary-data-dir`.

You can download sdcv dictionaries from: http://download.huzheng.org/dict.org/

## Usage

Below are the commands you can use:

| Command                   | Description
| :---                      | :---
| `quick-sdcv-search-pointer` | Searches the word around the cursor and displays the result in a buffer.
| `quick-sdcv-search-input`   | Searches the input word and displays the result in a buffer.

If the current mark is active, the `quick-sdcv` will translate the region string; otherwise, they will translate the word around the cursor.

## Frequently asked question

### What is the difference between sdcv (MELPA) and quick-sdcv Emacs packages?

The `quick-sdcv` Emacs package is a fork of `sdcv.el` version 3.4, which is available on MELPA. The primary differences between the two packages are as follows:

- **Improved Outline Minor Mode**: The `quick-sdcv` package fixes the outline minor mode for dictionary folding, enabling users to collapse all definitions for quicker navigation through dictionaries.
- **Default Language Settings**: Various issues have been addressed, including changing the default language setting from Chinese (zh) to nil, providing a more neutral starting point.
- **Buffer Customization**: The `quick-sdcv` package employs `display-buffer`, allowing users to customize the display of the *SDCV* buffer and control its placement through `display-buffer-alist`.
- **Removal of bugs and Warnings**: All Emacs warnings have been eliminated and bugs fixed. (e.g., when `quick-sdcv-search-pointer` cannot locate the word under the cursor)
- **Code Simplification**: The code has been simplified by removing unused variables and omitting features like posframe, text-to-speech using the 'say' command, the quick-sdcv-env-lang variable, and functions such as (quick-sdcv-scroll-up-one-line, quick-sdcv-scroll-down-one-line, quick-sdcv-next-line and quick-sdcv-prev-line) which are similar Emacs features. This simplification makes `quick-sdcv` easier to understand, maintain, and use by focusing solely on dictionary lookup functionality. Features like `posframe` and text-to-speech, which are not essential to core usage, are better suited as separate packages.
- **Keybindings removal**: The default keybindings have been removed from `quick-sdcv-mode` to enhances customizability, prevents conflicts with other modes, and keeps the mode lightweight and adaptable for users’ preferences.
- **Various improvements**: Enhance bullet representation with UTF-8 characters in quick-sdcv, Implement error handling for cases when the sdcv program is not found.
- **New interactive functions**: quick-sdcv-list-dictionaries
- **New defcustom**: quick-sdcv-exact-search

## Links

- You can download sdcv dictionaries from http://download.huzheng.org/dict.org/
- The quick-sdcv.el Emacs package @GitHub: https://github.com/jamescherti/quick-sdcv.el
