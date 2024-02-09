;; -*- lexical-binding: t -*-


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

;; random variables
(defvar exile/default-font-size 150)


;; set default enocding to utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)


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
(set-frame-parameter (selected-frame) 'alpha '(90 . 90)) ;; Set transparency 
(add-to-list 'default-frame-alist '(alpha . (90 . 90))) ;; Set transparency

;; Set custom font
(set-face-attribute 'default nil :font "CaskaydiaCove Nerd Font" :height exile/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "CaskaydiaCove Nerd Font" :height exile/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "CaskaydiaCove Nerd Font" :height exile/default-font-size :weight 'regular)


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
  (setq which-key-idle-delay 0.3)
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

;; undo tree
(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  :config
  (setq undo-tree-auto-save-history t) ;; Enable auto-saving of undo history
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history"))) ;; Specify where to save undo history files
  (evil-set-undo-system 'undo-tree))

;; Create the directory if it does not exist
(unless (file-exists-p "~/.emacs.d/undo-tree-history")
  (make-directory "~/.emacs.d/undo-tree-history" t))

;; Org mode configuration for habit tracking
(defun exile/org-set-habit ()
  "Set the current Org mode item as a habit."
  (interactive)
  (org-set-property "STYLE" "habit"))


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
   )

  ;; for org capture
  (exile/leader-keys
   "o" '(:ignore t :which-key "org mode")
   "oc" '(org-capture :which-key "capture")
   "oa" '(org-agenda :which-key "agenda")
   "ot" '(counsel-org-tag :which-key "set tags")
   "oe" '(org-set-effort :which-key "set effort based on time")
   "ol" '(org-insert-link :which-key "insert link")
   "oi" '(org-toggle-inline-images :which-key "toggle images")
   "or" '(org-refile :which-key "refile")
   "os" '(org-schedule :which-key "schedule")
   "od" '(org-deadline :which-key "deadline")
   "ou" '(org-time-stamp :which-key " add a time stamp")
   "oh" '(exile/org-set-habit :which-key "set as habit")
   )

  )



(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Further enhancing Ivy with icons
(use-package all-the-icons
  :if (display-graphic-p) ;; check if Emacs is running in a graphical display and not inside a termina;
  :commands (all-the-icons-install-fonts)
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t))
  )

(use-package all-the-icons-dired
  :if (display-graphic-p) ;; check if Emacs is running in a graphical display and not inside a termina;
  :hook (dired-mode . all-the-icons-dired-mode)
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

;; org mode

(defun exile/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
 )

(defun exile/org-font-setup ()
;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (dolist (face '((org-level-1 . 1.6)
                  (org-level-2 . 1.5)
                  (org-level-3 . 1.4)
                  (org-level-4 . 1.4)
                  (org-level-5 . 1.3)
                  (org-level-6 . 1.3)
                  (org-level-7 . 1.2)
                  (org-level-8 . 1.2)))
    (set-face-attribute (car face) nil
			:font "CaskaydiaCove Nerd Font"
			:weight 'regular
			:height (round (* 100 (cdr face)))))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
 )


(use-package org
  :hook (org-mode . exile/org-mode-setup)
  :config
  (setq org-ellipsis " ▼")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
	'("~/.emacs.d/OrgFiles/Tasks.org"
	"~/.emacs.d/OrgFiles/Birthdays.org"
	"~/.emacs.d/OrgFiles/Habits.org"
	"~/.emacs.d/OrgFiles/DrawingTasks.org"
	)
	)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

    (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("errand" . ?E)
       ("home" . ?H)
       ("work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("note" . ?n)
       ("meeting" . ?m)
       ("personal" . ?P)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work")
    ("M" "Meetings" tags-todo "+meeting")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

    (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/.emacs.d/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/.emacs.d/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/.emacs.d/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/.emacs.d/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/.emacs.d/OrgFiles/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
      ("me" "Food" table-line (file+headline "~/.emacs.d/OrgFiles/Metrics.org" "Food")
       "| %U | %^{Food} | %^{Notes} |" :kill-buffer t)))
 
  (exile/org-font-setup)
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "✸" "✿" "✜" "✚" "✦"))
  )

;; center text in org mode
(defun exile/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . exile/org-mode-visual-fill)
  )

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
    "M-C-<return>" #'exile/copilot-trigger ; for manual mode (FIX ME): this doesnt seems to work rn
    )
  )

;; variables for copilot
(defvar exile/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered ")

(defvar exile/company-active nil
  "Flag to indicate whether Company's completion menu is active.")


(defun exile/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when (bound-and-true-p copilot--overlay)
        (let ((pre-copilot-disable-predicates copilot-disable-predicates))
          (setq copilot-disable-predicates (list (lambda () t)))
          (copilot-clear-overlay)
          (run-with-idle-timer
           1.0
           nil
           (lambda ()
             (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error (message "Error handling Copilot overlay: %s" err))))

(defun exile/clear-copilot-during-company ()
  "Clear Copilot's overlay if Company is active."
  (when exile/company-active
    (message "Company completion active, clearing Copilot overlay.")
    (exile/copilot-quit)
    ))

(defun exile/company-started (arg)
  "Hook function called when Company completion starts."
  (setq exile/company-active t)
  (add-hook 'pre-command-hook #'exile/clear-copilot-during-company)
  (message "Company completion started."))

(defun exile/company-finished (arg)
  "Hook function called when Company completion finishes or is cancelled."
  (remove-hook 'pre-command-hook #'exile/clear-copilot-during-company)
  (setq exile/company-active nil))

(with-eval-after-load 'company
  (add-hook 'company-completion-started-hook #'exile/company-started)
  (add-hook 'company-completion-finished-hook #'exile/company-finished)
  (add-hook 'company-completion-cancelled-hook #'exile/company-finished))

(defun exile/copilot-trigger ()
  "Try to trigger completion with Copilot."
  (interactive)
  (if (fboundp 'copilot-complete)
      (copilot-complete)
    (message "Copilot completion function not available.")))
  
(defun exile/company-or-copilot-or-indent ()
"Enhanced completion logic to avoid conflicts between Copilot and Company."
  (interactive)
  (cond
   (exile/company-active
    (company-complete-selection))
   ((copilot--overlay-visible)
    (copilot-accept-completion))
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
  "Toggle Copilot's manual mode."
  (interactive)
  (if exile/copilot-manual-mode
      (progn
        (setq exile/copilot-manual-mode nil)
        (message "Copilot manual mode deactivated"))
    (progn
      (setq exile/copilot-manual-mode t)
      ;; Ensure Copilot is activated if we're switching to manual mode.
      (if copilot-mode
        (global-copilot-mode -1))
      (message "Copilot manual mode activated"))))

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
