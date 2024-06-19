;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; ! Download (if necessary) the redo+.el file from https://www.emacswiki.org/emacs/download/redo%2b.el
;; Package list
(setq my-packages
      '(multiple-cursors
	timu-rouge-theme
	syntax-subword))
;; Iterate on packages and install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(cua-mode)

;; Skip startup screen
(setq inhibit-startup-screen t)
;; Close *Completions* automatically
(add-hook 'minibuffer-exit-hook 
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
            (kill-buffer buffer)))))

;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 20)
(global-unset-key (kbd "C-S-o"))
(global-set-key (kbd "C-S-o") 'recentf-open-files)

;; Recent directories
;; Stolen from https://stackoverflow.com/questions/23328037/in-emacs-how-to-maintain-a-list-of-recent-directories
(defun dired-recent ()
  "Open Dired in new buffer, showing the recently used directories."
  (interactive)
  (let ((dirs  (delete-dups
                (mapcar (lambda (f/d)
                          (if (file-directory-p f/d)
                              f/d
                            (file-name-directory f/d)))
                        recentf-list))))
    (dired (cons (generate-new-buffer-name "Recent directories") dirs))))
(global-unset-key (kbd "C-S-k C-S-o"))
(global-set-key (kbd "C-S-k C-S-o") 'dired-recent)
;; Open the recent directories dialogue on startup
(add-hook 'after-init-hook 'dired-recent)

;; Disable line wrapping in programming modes
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; Enable balanced parantheses in programming modes
(add-hook 'prog-mode-hook
	  (lambda ()
	    (electric-pair-local-mode 1)
	    (setq electric-pair-preserve-balance nil)))

;; Scrolling with mouse
(global-unset-key (kbd "<wheel-up>"))
(global-set-key (kbd "<wheel-up>") 'scroll-down-line)
(global-unset-key (kbd "<wheel-down>"))
(global-set-key (kbd "<wheel-down>") 'scroll-up-line)

;; S-<tab>/<backtab> for shifting the currrent/line whole selection back
;; Taken from https://stackoverflow.com/questions/2249955/emacs-shift-tab-to-left-shift-the-block
;; Helper function for tab-region and untab-region
(defun indent-region-custom(numSpaces)
  (progn 
					; default to start and end of current line
    (setq regionStart (line-beginning-position))
    (setq regionEnd (line-end-position))

					; if there's a selection, use that instead of the current line
    (when (use-region-p)
      (setq regionStart (region-beginning))
      (setq regionEnd (region-end))
      )

    (save-excursion ; restore the position afterwards
      (goto-char regionStart) ; go to the start of region
      (setq start (line-beginning-position)) ; save the start of the line
      (goto-char regionEnd) ; go to the end of region
      (setq end (line-end-position)) ; save the end of the line

      (indent-rigidly start end numSpaces) ; indent between start and end
      (setq deactivate-mark nil) ; restore the selected region
      )
    )
  )
(defun untab-region () (interactive) (indent-region-custom -2))
(defun tab-region ()
  (interactive)
  (if (active-minibuffer-window)
      (minibuffer-complete)    ; tab is pressed in minibuffer window -> do completion
    (if (string= (buffer-name) "*shell*")
	(comint-dynamic-complete) ; in a shell, use tab completion
      (if (use-region-p)    ; tab is pressed is any other buffer -> execute with space insertion
	  (indent-region-custom 2) ; region was selected, call indent-region-custom
        (insert "  "))))) ; else insert four spaces as expected
(global-unset-key (kbd "<backtab>"))
(global-set-key (kbd "<backtab>") 'untab-region)

;; Enable line numbers only in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Disable auto-save
(setq auto-save-default nil)
;; Disable backup files
(setq make-backup-files nil)

;; Disable menu bar, tool bar and scroll bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Save
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'save-buffer)
;; Undo, redo
(require 'redo+)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)
(global-unset-key (kbd "C-y"))
(global-set-key (kbd "C-y") 'redo)
(setq redo+-persistent-undo nil)

;; Search
(global-unset-key (kbd "C-f"))
(global-set-key (kbd "C-f") 'isearch-forward)
;; Enable arrow keys for navigating isearch results and ESC for exitting
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<escape>") 'isearch-exit)
;; Replace
(global-unset-key (kbd "C-h"))
(global-set-key (kbd "C-h") 'replace-string)

;; Open buffer
(global-unset-key (kbd "C-o"))
(global-set-key (kbd "C-o") 'find-file)
;; Open directory
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-k C-o"))
(global-set-key (kbd "C-k C-o") 'dired)

;; Navigating buffers
(global-unset-key (kbd "C-<tab> C-<left>"))
(global-set-key (kbd "C-<tab> C-<left>") 'previous-buffer)
(global-unset-key (kbd "C-<tab> C-<right>"))
(global-set-key (kbd "C-<tab> C-<right>") 'next-buffer)
;; Deleting a buffer
(global-unset-key (kbd "C-x k"))
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Splitting windows
(global-unset-key (kbd "C-x h"))
(global-set-key (kbd "C-x h") 'split-window-vertically)
(global-unset-key (kbd "C-x v"))
(global-set-key (kbd "C-x v") 'split-window-horizontally)
;; Deleting a split
(global-unset-key (kbd "C-x d"))
(global-set-key (kbd "C-x d") 'delete-window)
;; Deleting a split and closing a window
(global-unset-key (kbd "C-x C-d"))
(global-set-key (kbd "C-x C-d") 'kill-buffer-and-window)

;; Navigating windows
(global-unset-key (kbd "C-x <left>"))
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-unset-key (kbd "C-x <right>"))
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-unset-key (kbd "C-x <up>"))
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-unset-key (kbd "C-x <down>"))
(global-set-key (kbd "C-x <down>") 'windmove-down)

;; Shell
(global-unset-key (kbd "C-`"))
(global-set-key (kbd "C-`") 'shell)
(defun my-shell-mode-keybindings ()
  (define-key shell-mode-map (kbd "C-`") 'bury-buffer))
(add-hook 'shell-mode-hook 'my-shell-mode-keybindings)

;; Multiple cursors
(require 'multiple-cursors)
(defvar mc-is-enabled nil)
(defvar my-mc-current-line nil)
;; Start hook to set starting line number
(defun my-mc-start-hook ()
  (when (null mc-is-enabled) (progn
      (setq my-mc-current-line (line-number-at-pos))
       (setq mc-is-enabled t))))
(add-hook 'multiple-cursors-mode-hook 'my-mc-start-hook)
;; Exit hook to set line number to nil
(defun my-mc-exit-hook ()
  (setq my-mc-current-line nil)
  (setq mc-is-enabled nil))
(add-hook 'multiple-cursors-mode-disabled-hook 'my-mc-exit-hook)
;; Custom mark-next and mark-prev functions
(defun my-mark-next ()
  (mc/mark-next-like-this 1)
  (setq my-mc-current-line (+ my-mc-current-line 1)))
(defun my-mark-prev ()
  (mc/mark-previous-like-this 1)
  (setq my-mc-current-line (- my-mc-current-line 1)))
;; Custom unmark-next and unmark-prev functions
(defun my-unmark-next ()
  (mc/unmark-next-like-this)
  (setq my-mc-current-line (- my-mc-current-line 1)))
(defun my-unmark-prev ()
  (mc/unmark-previous-like-this)
  (setq my-mc-current-line (+ my-mc-current-line 1)))
;; Custom <up> and <down> functions
(defun my-mc-up ()
  (interactive)
  (cond
   ((null my-mc-current-line) (my-mark-prev))
   ((>= (line-number-at-pos) my-mc-current-line) (my-mark-prev))
   (t (my-unmark-next))))
(defun my-mc-down ()
  (interactive)
  (cond
   ((null my-mc-current-line) (my-mark-next))
   ((<= (line-number-at-pos) my-mc-current-line) (my-mark-next))
   (t (my-unmark-prev))))
;; Key bindings
(global-unset-key (kbd "M-S-<up>"))
(global-set-key (kbd "M-S-<up>") 'my-mc-up)
(global-unset-key (kbd "M-S-<down>"))
(global-set-key (kbd "M-S-<down>") 'my-mc-down)
(define-key mc/keymap (kbd "<escape>") 'mc/keyboard-quit)

;; Forward/backward word
(require 'syntax-subword)
(setq syntax-subward-skip-spaces t)
(global-unset-key (kbd "C-<left>"))
(global-set-key (kbd "C-<left>") 'syntax-subword-backward)
(global-unset-key (kbd "C-<right>"))
(global-set-key (kbd "C-<right>") 'syntax-subword-forward)

;; Theme
(load-theme 'timu-rouge t)
;; Font
(set-frame-font "Consolas 16" nil t)

;; Auto-reloading files changed on disk
(global-auto-revert-mode t)
(setq auto-revert-use-notify nil)

;; C/C++
(setq c-default-style "linux")
(c-set-offset 'arglist-intro '+)        ; Indent the first line of the argument list
(c-set-offset 'arglist-cont-nonempty 0) ; Indent subsequent lines of arguments
(c-set-offset 'arglist-close 0)           ; Align closing brace with function name

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("622034e2b06b087d3e950652c93e465f3df6eab50bfdceddaa78077487e9bc24" default))
 '(package-selected-packages
   '(irony multiple-cursors syntax-subword timu-rouge-theme yasnippet queue highlight-indentation f epc company auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
