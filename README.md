# easysdcv.el - Emacs interface for the 'sdcv' command-line dictionary

The `easysdcv` package serves as an Emacs interface for the `sdcv` command-line interface, which is the console version of the StarDict dictionary application.

This integration allows users to access and utilize dictionary functionalities directly within the Emacs environment, leveraging the capabilities of `sdcv` to look up words and translations from various dictionary files formatted for StarDict.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [easysdcv.el - Emacs interface for the 'sdcv' command-line dictionary](#easysdcvel---emacs-interface-for-the-sdcv-command-line-dictionary)
    - [Installation](#installation)
        - [1. Install sdcv](#1-install-sdcv)
            - [Linux (Debian/Ubuntu-based operating systems)](#linux-debianubuntu-based-operating-systems)
            - [macOS](#macos)
        - [2. Require easysdcv.el](#2-require-easysdcvel)
    - [Configuration](#configuration)
    - [Usage](#usage)
    - [Frequently asked question](#frequently-asked-question)
        - [What is the difference between sdcv (MELPA) and easysdcv Emacs packages?](#what-is-the-difference-between-sdcv-melpa-and-easysdcv-emacs-packages)
    - [Links](#links)

<!-- markdown-toc end -->

## Installation

### 1. Install sdcv

To use this extension, you must install Stardict and sdcv.

#### Linux (Debian/Ubuntu-based operating systems)
```bash
sudo apt-get install sdcv
```

#### macOS
```bash
brew install sdcv
```

### 2. Require easysdcv.el

Place `easysdcv.el` in your load-path.

Then, add the following line to your `.emacs` startup file:

```elisp
(require 'easysdcv)
```

## Configuration

```elisp
(setq easysdcv-dictionary-data-dir "startdict_dictionary_directory") ; Set up the directory for the Stardict dictionary

;; Set up the dictionary list for complete search
(setq easysdcv-dictionary-complete-list
      '("ENG-FRA Dictionary"
        "FRA-ENG Dictionary"
        "stardict 1.3 ENG-FRA Dictionary"
        "WordNet"
        "Jargon"))
```

After completing the above configuration, execute the `easysdcv-check` Emacs command to confirm that the dictionary settings are correct. Otherwise, `easysdcv` will not function correctly due to the absence of dictionary files in `easysdcv-dictionary-data-dir`.

## Usage

Below are the commands you can use:

| Command                   | Description
| :---                      | :---
| `easysdcv-search-pointer` | Searches the word around the cursor and displays the result in a buffer.
| `easysdcv-search-input`   | Searches the input word and displays the result in a buffer.

If the current mark is active, the `easysdcv` will translate the region string; otherwise, they will translate the word around the cursor.

## Frequently asked question

### What is the difference between sdcv (MELPA) and easysdcv Emacs packages?

The `easysdcv` Emacs package is a fork of `sdcv.el` version 3.4, which is available on MELPA. The primary differences between the two packages are as follows:

- **Improved Outline Minor Mode**: The `easysdcv` package fixes the outline minor mode for dictionary folding, enabling users to collapse all definitions for quicker navigation through dictionaries.
- **Default Language Settings**: Various issues have been addressed, including changing the default language setting from Chinese (zh) to nil, providing a more neutral starting point.
- **Buffer Customization**: The `easysdcv` package employs `display-buffer`, allowing users to customize the display of the *SDCV* buffer and control its placement through `display-buffer-alist`.
- **Removal of bugs and Warnings**: All Emacs warnings have been eliminated and bugs fixed. (e.g., when `easysdcv-search-pointer` cannot locate the word under the cursor)
- **Code Simplification**: The code has been simplified by removing unused variables and omitting features like posframe, text-to-speech using the 'say' command, and functions such as (easysdcv-scroll-up-one-line, easysdcv-scroll-down-one-line, easysdcv-next-line and easysdcv-prev-line) which are similar Emacs features. This simplification makes `easysdcv` easier to understand, maintain, and use by focusing solely on dictionary lookup functionality. Features like `posframe` and text-to-speech, which are not essential to core usage, are better suited as separate packages.
- **Keybindings removal**: The default keybindings have been removed from `easysdcv-mode` to enhances customizability, prevents conflicts with other modes, and keeps the mode lightweight and adaptable for users’ preferences.
- **Various improvements**: Implement error handling for cases when the sdcv program is not found.
- **New interactive functions**: easysdcv-list-dictionaries

## Links

- You can download sdcv dictionnaries from http://download.huzheng.org/dict.org/
- The easysdcv.el Emacs package @GitHub: https://github.com/jamescherti/easysdcv.el
