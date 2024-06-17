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
;; Forces the messages to 0, and kills the *Messages* buffer - thus disabling it on startup.
(setq-default message-log-max nil)
(when (get-buffer "*Messages*") (kill-buffer "*Messages*"))
;; Disabled *Completions*
(add-hook 'minibuffer-exit-hook 
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
            (kill-buffer buffer)))))

;; Disable line wrapping in programming modes
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; Scrolling with mouse
(global-unset-key (kbd "<wheel-up>"))
(global-set-key (kbd "<wheel-up>") 'scroll-down-line)
(global-unset-key (kbd "<wheel-down>"))
(global-set-key (kbd "<wheel-down>") 'scroll-up-line)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("622034e2b06b087d3e950652c93e465f3df6eab50bfdceddaa78077487e9bc24" default))
 '(package-selected-packages
   '(multiple-cursors syntax-subword timu-rouge-theme yasnippet queue highlight-indentation f epc company auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
