;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use-package integration
(straight-use-package 'use-package)

;; Package Management with straight.el
;; if you use use-package, then this makes each use-package form also invoke straight.el
;; to install the package, unless otherwise specified.
(setq straight-use-package-by-default t)

;; Basic UI Settings
(setq inhibit-startup-message t) ; Hide the startup message
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1) ; Disable the toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(setq visible-bell t) ; Set up the visible bell
(menu-bar-mode -1) ; Disable the menu bar

;; Set custom font
(set-face-attribute 'default nil :font "CaskaydiaCove Nerd Font" :height 150)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; counsel - ivy enhancements for commands like M-x, C-x C-f, etc.
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)  ;; don't start searches with ^
  )

;; Ivy for completion and buffer switching
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  )

;; ivy-prescient - better sorting and filtering for ivy completions
(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1))

;; ivy-rich - more information in ivy completion
(use-package ivy-rich
  :init
  (ivy-rich-mode 1)  ; Enable ivy-rich-mode immediately upon initialization.
  :after counsel  ; Ensure ivy-rich is loaded after the counsel package.
  :config
  ;; Set the display format for ivy completions to use a single line per entry.
  (setq ivy-format-function #'ivy-format-function-line)
  
  ;; Customize the display transformation for `ivy-switch-buffer`.
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list

                   'ivy-switch-buffer
                   '(:columns
                     ;; Define columns for buffer names, indicators, major mode, project, and file path.
                     ((ivy-rich-candidate (:width 40))  ; Buffer name with a fixed width.
                      (ivy-rich-switch-buffer-indicators  ; Display buffer indicators (e.g., modified status).
                       (:width 4 :face error :align right))
                      (ivy-rich-switch-buffer-major-mode  ; Show the major mode of the buffer.
                       (:width 12 :face warning))
                      (ivy-rich-switch-buffer-project  ; Show the project associated with the buffer.
                       (:width 15 :face success))
                      (ivy-rich-switch-buffer-path  ; Display the file path, shortened based on the minibuffer width.
                       (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path
                                            x (ivy-rich-minibuffer-width 0.3)))))))))
  )

(use-package all-the-icons-ivy
  :demand t)  ; Load the package immediately, without waiting for it to be called.

;; Enhanced command feedback
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2)
  )

;; A more informative help system
(use-package helpful
  :bind (([remap describe-function] . counsel-describe-function)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap describe-key] . helpful-key))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  )


;; Vim emulation
(defun exile/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;; :hook (evil-mode . exile/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )

;; Evil keybindings collection
;; contains evil magits
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  )

;; General keybindings management
;; TODO: Customise for my liking and used cmds
(use-package general
  :config
  (general-create-definer exile/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Define Magit keybindings under the 'g' prefix for Git
  (exile/leader-keys
    "g" '(:ignore t :which-key "git (magit)")
    "gs" '(magit-status :which-key "status")
    "gc" '(magit-commit :which-key "commit")
    "gp" '(:ignore t :which-key "push/pull")
    "gpp" '(magit-push :which-key "push")
    "gpl" '(magit-pull :which-key "pull")
    "gb" '(magit-branch :which-key "branch")
    "gf" '(magit-fetch :which-key "fetch")
    "gl" '(magit-log-all :which-key "log")
    "gd" '(magit-diff :which-key "diff"))

  (exile/leader-keys
   "c" '(:ignore t :which-key "copilot")
   "cm" '(exile/copilot-toggle-manual-mode :which-key "toggle manual mode")
   "ce" '(exile/copilot-activate :which-key "enable copilot")
   "cd" '(exile/copilot-deactivate :which-key "disable copilot")
   "co" '(exile/copilot-toggle-for-org :which-key "toggle for org-mode") 
  ))

;; Further enhancing Ivy with icons
;; NOTE: The first time you use this package, you need to run M-x all-the-icons-install-fonts manually
(use-package all-the-icons
  :if (display-graphic-p) ;; check if Emacs is running in a graphical display and not inside a termina;
  :commands (all-the-icons-install-fonts)
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t))
  )


;; Doom modeline for a fancy status bar
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15)
 )

;; Doom themes for aesthetics
(use-package doom-themes
  :init (load-theme 'doom-gruvbox t)
 )

;; Programming utilities

;; magit - git integration
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

;; Rainbow delimiters for colorful parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Exclude line numbers in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; company - auto-completion
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 3
        company-selection-wrap-around t 
        company-tooltip-limit 20
        company-tooltip-minimum-width 15
        company-tooltip-align-annotations t))

;; copilot configuration
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :diminish ;; don't show in mode line (we don't wanna get caught cheating, right? ;)
  :config

  ;; global keybindings
  (general-def
    "TAB" #'exile/company-or-copilot-or-indent ; for tab completion
    )
  (define-key global-map (kbd "M-C-<return>") #'exile/copilot-complete-or-accept) ;; for manual mode
  ;; complete by pressing right or tab but only when copilot completions are
  ;; shown. This means we leave the normal functionality intact.
  )

;; variables for copilot
(defvar exile/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered ")

(defvar exile/copilot-enable-for-org nil
  "Should copilot be enabled for org-mode buffers?")

(defvar exile/company-active nil
  "Flag to indicate whether Company's completion menu is active.")

;; WARNING: this might be a bit of a hack, but it works for now
(defun exile/clear-copilot-during-company ()
  "Clear Copilot's overlay if Company is active."
  (when exile/company-active
    (copilot-clear-overlay)))

(defun exile/company-started (arg)
  "Hook function called when Company completion starts."
  (setq exile/company-active t)
  (add-hook 'pre-command-hook #'exile/clear-copilot-during-company)
  (message "Company completion started."))

(defun exile/company-finished (arg)
  "Hook function called when Company completion finishes or is cancelled."
  (remove-hook 'pre-command-hook #'exile/clear-copilot-during-company)
  (setq exile/company-active nil)
  (message "Company completion finished."))

(with-eval-after-load 'company
  (add-hook 'company-completion-started-hook #'exile/company-started)
  (add-hook 'company-completion-finished-hook #'exile/company-finished)
  (add-hook 'company-completion-cancelled-hook #'exile/company-finished))

(defun exile/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. "
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

(defun exile/company-or-copilot-or-indent ()
"Enhanced completion logic to avoid conflicts between Copilot and Company."
  (interactive)
  (cond
   ;; If Company's completion menu is active, let Company handle the completion.
   (exile/company-active
    (company-complete-selection))
   
   ;; If Copilot's overlay is visible, try to accept a Copilot completion.
   ((copilot--overlay-visible)
    (copilot-accept-completion))
   
   ;; Try to manually trigger Company completion if it's available and not already active.
   ((and (bound-and-true-p company-mode) ;; Ensure company-mode is active
         (not exile/company-active)) ;; Ensure Company's completion menu isn't already showing
    (company-manual-begin))
   
   ;; Fallback to default indentation if neither completion system acts.
   (t
    (indent-for-tab-command))))


(defun exile/copilot-activate ()
  "Activate Copilot globally. If already activated, inform the user."
  (interactive)
  (if copilot-mode
      (message "Copilot is already activated")
    (progn
      (copilot-mode 1)
      (setq exile/copilot-manual-mode nil)
      (message "Copilot activated"))))

(defun exile/copilot-deactivate ()
  "Deactivate Copilot globally. If already deactivated, inform the user."
  (interactive)
  (if copilot-mode
      (progn
        (copilot-mode -1)
        (setq exile/copilot-manual-mode nil)
        (message "Copilot deactivated"))
    (message "Copilot is already deactivated")))

(defun exile/copilot-toggle-manual-mode ()
  "Toggle Copilot's manual mode. Activate Copilot globally if toggling out of manual mode."
  (interactive)
  (setq exile/copilot-manual-mode (not exile/copilot-manual-mode)) ; Toggle the manual mode flag.
  (if exile/copilot-manual-mode
      (message "Copilot manual mode activated")
    (progn
      (unless copilot-mode
        (copilot-mode 1)) ; Activate Copilot globally if not already active.
      (message "Copilot manual mode deactivated, Copilot activated globally"))))

(defun exile/copilot-toggle-for-org ()
  "Toggle copilot activation in org mode. It can sometimes be
annoying, sometimes be useful, that's why this can be handly."
  (interactive)
  (setq exile/copilot-enable-for-org (not exile/copilot-enable-for-org))
  (message "copilot for org is %s" (if exile/copilot-enable-for-org "enabled" "disabled")))

;; End of Emacs Configuration
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-log-types '(((copilot copilot-no-mode-indent)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
