# easysdcv.el - Emacs interface for the 'sdcv' command-line dictionary

The `easysdcv` package serves as an Emacs interface for the `sdcv` command-line interface, which is the console version of the StartDict dictionary application.

This integration allows users to access and utilize dictionary functionalities directly within the Emacs environment, leveraging the capabilities of `sdcv` to look up words and translations from various dictionary files formatted for StarDict.

## Installation

### 1. Install sdcv

To use this extension, you must install Stardict and sdcv.

##### Linux (Debian/Ubuntu-based operating systems)
```bash
sudo apt-get install sdcv
```

##### macOS
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

## Links

- You can download sdcv dictionnaries from http://download.huzheng.org/dict.org/
- The easysdcv.el Emacs package @GitHub: https://github.com/jamescherti/easysdcv.el
