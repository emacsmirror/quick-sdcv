# What is sdcv?

(Note: This is a fork of sdcv.el version 3.4. The primary difference is that it does not require posframe.)

Interface for sdcv (StartDict console version).

Translate word by sdcv (console version of Stardict), and display
translation use posframe or buffer.

## Installation

#### 1. Install Stardict and sdcv

To use this extension, you have to install Stardict and sdcv

##### Linux
```Bash
sudo aptitude install stardict sdcv -y
```

##### MacOS
```Bash
brew install stardict sdcv
```

#### 2. Install posframe extension (optional)

You can install get it from:
https://raw.githubusercontent.com/tumashu/posframe/master/posframe.el

#### 3. Require sdcv.el

Put sdcv.el and posframe.el to your load-path.

And add the following to your .emacs startup file.

```Elisp
(require 'sdcv)
```

## Configuration

```Elisp
(setq sdcv-say-word-p t)               ;say word after translation

(setq sdcv-dictionary-data-dir "startdict_dictionary_directory") ;setup directory of stardict dictionary

(setq sdcv-dictionary-simple-list    ; setup dictionary list for simple search
      '("Simple ENG-CHN Dictionary"
        "Simple CHN-ENG Dictionary"))

(setq sdcv-dictionary-complete-list     ; setup dictionary list for complete search
      '("Simple ENG-CHN Dictionary"
        "ENG-CHN CHN-ENG Professional Dictionary"
        "XDICT ENG-CHN Dictionary"
        "stardict 1.3 ENG-CHN Dictionary"
        "WordNet"
        "XDICT CHN-ENG Dictionary"
        "Jargon"))
```

After completing the above configuration, please execute the command ```sdcv-check```
to confirm that the dictionary settings is correct,
otherwise sdcv will not work because there is no dictionary file in sdcv-dictionary-data-dir.

## Usage

Below are commands you can use:

| Command              | Description                                  |
| :---                 | :---                                         |
| sdcv-search-pointer  | Search around word and display with buffer.  |
| sdcv-search-pointer+ | Search around word and display with tooltip. |
| sdcv-search-input    | Search input word and display with buffer.   |
| sdcv-search-input+   | Search input word and display with tooltip.  |

Tips:

If current mark is active, sdcv commands will translate
region string, otherwise translate word around point.

## Dictionary
You can download sdcv dictionary from http://download.huzheng.org/dict.org/

## Screenshot

<img src="sdcv.png">

## Links

- https://github.com/jamescherti/easysdcv.el
