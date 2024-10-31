# easysdcv.el - Emacs interface for the 'sdcv' command-line dictionary

The `easysdcv` package serves as an Emacs interface for the `sdcv` command-line interface, which is the console version of the StartDict dictionary application.

This integration allows users to access and utilize dictionary functionalities directly within the Emacs environment, leveraging the capabilities of `sdcv` to look up words and translations from various dictionary files formatted for StarDict.

## Installation

### 1. Install sdcv

To use this extension, you have to install Stardict and sdcv

##### Linux (Debian/Ubuntu based operating systems)
```Bash
sudo apt-get install sdcv
```

##### MacOS
```Bash
brew install sdcv
```

### 2. Require easysdcv.el

Put `easysdcv.el` to your load-path.

And add the following to your .emacs startup file.

```Elisp
(require 'sdcv)
```

## Configuration

```Elisp
(setq sdcv-dictionary-data-dir "startdict_dictionary_directory") ;setup directory of stardict dictionary

(setq sdcv-dictionary-complete-list     ; setup dictionary list for complete search
      '("Simple ENG-CHN Dictionary"
        "ENG-CHN CHN-ENG Professional Dictionary"
        "XDICT ENG-CHN Dictionary"
        "stardict 1.3 ENG-CHN Dictionary"
        "WordNet"
        "XDICT CHN-ENG Dictionary"
        "Jargon"))
```

After completing the above configuration, execute the `sdcv-check` Emacs command to confirm that the dictionary settings are correct; otherwise, SDCV will not work because there is no dictionary file in `easysdcv-dictionary-data-dir`.

## Usage

Below are the commands you can use:

| Command              | Description                                   |
| :---                 | :---                                          |
| `sdcv-search-pointer` | Searches the word around the cursor and displays the result in a buffer. |
| `sdcv-search-input`   | Searches the input word and displays the result in a buffer. |

If the current mark is active, the SDCV commands will translate the region string; otherwise, they will translate the word around the cursor.

## Links

- You can download sdcv dictionnaries from http://download.huzheng.org/dict.org/
- The easysdcv.el Emacs package @GitHub: https://github.com/jamescherti/easysdcv.el
