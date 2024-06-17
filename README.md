# Emacs-sane-configuration
An Emacs configuration for the casual editor user.

## Features:
- light-weight
- key bindings similar to VS Code
- well commented so modifications are trivial

## Non-standard key bindings
- `C-o`: open file
- `C-k C-o`: open directory
- `C-<tab> <left>`: previous buffer
- `C-<tab> <right>`: next buffer
- `C-x k`: kill buffer
- `C-x h`: split window with horizontal line
- `C-x v`: split window with vertical line
- `C-x d`: delete current window
- `C-x C-d`: delete current window and kill current buffer
- `C-x <left>`: move to the window on the left
- `C-x <right>`: move to the window on the right
- `C-x <up>`: move to the upper window
- `C-x <down>`: move to the lower window
- `` C-` ``: open shell
- `M-S <down>` and `M-S-<up>`: multiple cursors like Visual Studio/VS Code

## Setup
With a working Emacs installation, all you need to do to use this configuration is to download the [.emacs](https://github.com/bogdanmaciuca/Emacs-sane-configuration/blob/main/.emacs) file in the Emacs home directory and download the [redo+.el](https://www.emacswiki.org/emacs/download/redo%2b.el) file into the scripts folder (Emacs/emacs-29.1/share/emacs/site-lisp) for Emacs 29.1.

## Contributing
Feedback is greatly appreciated and bugs will be fixed relatively quickly (depending on the complexity).
