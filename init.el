;; -*- lexical-binding: t -*-

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

;; Integrate use-package with straight.el for package installation

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; for debuggin purpose
;; (setq use-package-verbose t)

(defvar exile/default-font-size 150 "Default font size for Emacs.")
(defvar exile/font-name "CaskaydiaCove Nerd Font" "Default font for Emacs.")
(defvar exile/theme 'doom-gruvbox "Default theme for Emacs.")
(defvar exile/org-directory "~/.emacs.d/OrgFiles/" "Directory for Org files.")
(defvar exile/emacs-file (concat exile/org-directory "Emacs.org") "Our Emacs Configuration")
(defvar exile/org-agenda-files '("~/.emacs.d/OrgFiles/Tasks.org"
	"~/.emacs.d/OrgFiles/Birthdays.org"
	"~/.emacs.d/OrgFiles/Habits.org"
	"~/.emacs.d/OrgFiles/DrawingTasks.org"
	))
(defvar exile/padding 10 "Padding for text area.")
;; Define a global variable for the Python executable path
(defvar exile/python-executable-path "C:/Users/rgull/AppData/Local/Programs/Python/Python311/python.exe"
"Path to the Python executable.")

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

;; Set UTF-8 as the default encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Hide the startup message
(setq inhibit-startup-message t)
;;(setq inhibit-splash-screen t)

;; Disable the scroll bar, tool bar, and tooltips
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; Add padding around the text area
(set-fringe-mode exile/padding)

;; Use a visual bell instead of an audible one
(setq visible-bell t)

;; Disable the menu bar
(menu-bar-mode -1)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Exclude line numbers in certain modes
(dolist (mode '(org-mode-hook
               term-mode-hook
	       treemacs-mode-hook
               shell-mode-hook
               eshell-mode-hook))
 (add-hook mode (lambda () (display-line-numbers-mode 0))))

 ;; add agenda as a startup buffer
 ;;(add-hook 'after-init-hook (lambda () (org-agenda-list 1) (delete-other-windows)))

;; Set the default, fixed-pitch, and variable-pitch fonts
(set-face-attribute 'default nil :font exile/font-name :height exile/default-font-size)
(set-face-attribute 'fixed-pitch nil :font exile/font-name :height exile/default-font-size)
(set-face-attribute 'variable-pitch nil :font exile/font-name :height exile/default-font-size :weight 'regular)

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
			:font exile/font-name 
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

;; Org mode configuration for habit tracking
(defun exile/org-set-habit ()
  "Set the current Org mode item as a habit."
  (interactive)
  (org-set-property "STYLE" "habit"))


 ;; General keybindings management
 (use-package general
   :after evil
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

   ;; Define keybindings for copilot under the 'c' prefix
   (exile/leader-keys
    "c" '(:ignore t :which-key "copilot")
    "cm" '(exile/copilot-toggle-manual-mode :which-key "toggle manual mode")
    "ce" '(exile/copilot-activate :which-key "enable copilot")
    "cd" '(exile/copilot-deactivate :which-key "disable copilot")
    )

   ;; for org mode keybindings under the 'o' prefix 
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

   ;; for lsp mode keybindings under the 'l' prefix
   (exile/leader-keys
     "l" '(:ignore t :which-key "lsp")
      "lr" '(lsp-rename :which-key "rename")
      "lf" '(lsp-find-definition :which-key "find definition")
      "lh" '(lsp-describe-thing-at-point :which-key "describe")
      "la" '(lsp-execute-code-action :which-key "execute code action")
      "ls" '(lsp-find-references :which-key "find references")
      "lt" '(lsp-find-type-definition :which-key "find type definition")
      "li" '(lsp-find-implementation :which-key "find implementation")
      "ld" '(lsp-find-declaration :which-key "find declaration")
   )

      ;; for treemacs keybindings under the 't' prefix
   (exile/leader-keys
     "t" '(:ignore t :which-key "treemacs")
     "tf" '(treemacs-find-file :which-key "find file")
     ;; copy file
     "tc" '(treemacs-copy-file :which-key "copy file")
     ;; delete file
     "td" '(treemacs-delete-file :which-key "delete file")
     ;; rename file
     "tr" '(treemacs-rename-file :which-key "rename file")
     ;; create new file
     "tn" '(treemacs-create-file :which-key "create new file")
     ;; create new directory
     "tm" '(treemacs-create-dir :which-key "create new directory")
     ;; refresh treemacs
     "tr" '(treemacs-refresh :which-key "refresh treemacs")
     ;; toggle treemacs
     "tt" '(treemacs :which-key "toggle treemacs")
     )
   
   )

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))  ;; Don't start searches with ^

(use-package ivy
  :diminish
  :bind (("C-s" . swiper) ;; Swiper replaces default Emacs search with Ivy-powered search.
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done) ;; Allows TAB to select an item from the completion list.
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line) ;; Navigate completion list.
         ("C-k" . ivy-previous-line) ;; Navigate completion list.
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill) ;; Kill buffer from switch buffer list.
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)) ;; Kill buffer from reverse search list.
  :config
  (ivy-mode 1) ;; Enable Ivy globally at startup.
  (setq ivy-use-virtual-buffers t) ;; Add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-count-format "(%d/%d) ")) ;; Display the current and total number in the completion list.

(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode 1) ;; Activate Ivy Prescient.
  (prescient-persist-mode 1)) ;; Remember frequently used commands.

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1) ; Enable Ivy Rich globally
  :config
  ;; Customize the display transformation for various Ivy commands to include more details
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 30)) ; Buffer name
                      (ivy-rich-switch-buffer-size (:width 7)) ; Buffer size
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)) ; Modified status
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning)) ; Major mode
                      (ivy-rich-switch-buffer-project (:width 15 :face success)) ; Project name or directory
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))) ; File path that is shortened based on width
                     :predicate
                     (lambda (cand) (get-buffer cand)))))); Only display opened buffers

(use-package all-the-icons-ivy
  :after ivy
  :init
  (all-the-icons-ivy-setup)) ;; Automatically enhances Ivy completion with icons

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  )

;; A more informative help system
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-key)
  :bind (([remap describe-function] . counsel-describe-function)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap describe-key] . helpful-key))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  )

(use-package evil
:init
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
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
(use-package evil-collection
  :config
  (evil-collection-init)
  )

;; undo tree
(use-package undo-tree
  :after evil
  :init
  (global-undo-tree-mode 1)
  :config
  (setq undo-tree-auto-save-history t) ;; Enable auto-saving of undo history
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history"))) ;; Specify where to save undo history files
  (evil-set-undo-system 'undo-tree))

;; Create the directory if it does not exist
(unless (file-exists-p "~/.emacs.d/undo-tree-history")
  (make-directory "~/.emacs.d/undo-tree-history" t))

(use-package hydra
  :defer t
  :config
  (defhydra exile/hydra-text-scale (:timeout 4 :color pink)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("q" nil "quit" :exit t))
)

(use-package all-the-icons
  :if (display-graphic-p) ;; check if Emacs is running in a graphical display and not inside a terminal;
  :commands (all-the-icons-install-fonts)
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t))
  )

;; (use-package all-the-icons-dired
;;   :if (display-graphic-p) ;; check if Emacs is running in a graphical display and not inside a terminal;
;;   :hook (dired-mode . all-the-icons-dired-mode)
;;   )

;; Doom modeline for a fancy status bar
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15)
 )

;; doom themes for aesthetics
(use-package doom-themes
  :init (load-theme 'doom-gruvbox t)
 )

;; magit - git integration
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

;; Rainbow delimiters for colorful parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(defun exile/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
 )

(use-package org
  :commands (org-capture org-argenda org-agenda-list)
  :hook (org-mode . exile/org-mode-setup)
  :config
  (setq org-ellipsis " ▼")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files exile/org-agenda-files)

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
	("me" "Food" table-line (file+headline "~/.emacs.d/OrgFiles/Metrics.org"  "Food")
	 "| %U | %^{Food} | %^{Notes} |" :kill-buffer t)))

  (exile/org-font-setup) ;; Apply font settings for org-mode see: Font Configuration section
  )

(with-eval-after-load 'org
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    ;; Add more languages as needed
    )))
 (setq org-confirm-babel-evaluate nil) ;; Disables confirmation prompt for code block execution.

;; Automatically tangle our Emacs.org config file when we save it
(defun exile/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name exile/emacs-file))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'exile/org-babel-tangle-config)))

(with-eval-after-load 'org
 (require 'org-tempo)
 (eval-after-load 'org
 '(progn
   (add-to-list 'org-structure-template-alist '("bat" . "src bat")))))

(use-package org-bullets
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
  :custom
  company-minimum-prefix-length 3
        company-selection-wrap-around t 
        company-tooltip-limit 20
	companu-idle-delay 0.0
        company-tooltip-minimum-width 15
        company-tooltip-align-annotations t)

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :diminish ;; don't show in mode line (we don't wanna get caught cheating, right? ;)
  :config
  ;; global keybindings
  (general-def
    "TAB" #'exile/company-or-copilot-or-indent ; for tab completion
    "C-<return>" #'exile/copilot-trigger ; for manual mode (FIX ME): this doesnt seems to work rn
    )
  )

;; Flags to control Copilot's behavior
(defvar exile/copilot-manual-mode nil
  "When `t', Copilot only shows completions when manually triggered.")

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
  "Clear Copilot's overlay if Company is active and the Copilot overlay is visible."
  (interactive)
  ;; Check if Company is active and the Copilot overlay is visible
  (when (and exile/company-active (copilot--overlay-visible))
    ;; Clear the Copilot overlay
    (exile/copilot-quit)
    ;; Optionally, print a message for debugging
    (message "Clearing Copilot overlay due to active Company session.")
    ))

(defun exile/company-started (arg)
  "Hook function called when Company completion starts."
  (setq exile/company-active t)
  (add-hook 'pre-command-hook #'exile/clear-copilot-during-company))

(defun exile/company-finished (arg)
  "Hook function called when Company completion finishes or is cancelled."
  (remove-hook 'pre-command-hook #'exile/clear-copilot-during-company)
  (setq exile/company-active nil))

;; Catch the start and finish of company completion
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
  "Use TAB for completion: first try Company, then Copilot, then indent."
  (interactive)
  (cond
   ;; If Company is active and has a selected candidate, complete the selection.
   ((and (bound-and-true-p company-mode)
         (company-manual-begin)
         (company-complete-selection)
	 (message "Company completion selected"))
    ;; If the company has candidates, we've completed the selection.
    nil)

   ;; If Copilot's overlay is visible and Company isn't active, accept the Copilot completion.
   ((and (copilot--overlay-visible)
         (not company-candidates))
    (copilot-accept-completion)
    (message "Copilot completion accepted"))
   ;; Otherwise, just indent the line or region.
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
      (unless copilot-mode
        (global-copilot-mode 1))
      (message "Copilot manual mode activated"))))

(use-package treemacs
  :ensure t
  :defer t
  :init
  ;; Set the path to the Python executable here
  (setq treemacs-python-executable exile/python-executable-path)
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)


(use-package lsp-treemacs
  :after (lsp treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

(defun exile/open-treemacs-on-lsp ()
  (unless (treemacs-get-local-window)
    (treemacs)))

(add-hook 'lsp-mode-hook #'exile/open-treemacs-on-lsp)

(use-package lsp-mode
   :commands (lsp lsp-deferred)
   :init
   (setq lsp-keymap-prefix "C-c l")
   :config
   (lsp-enable-which-key-integration t)
   (setq lsp-enable-snippet nil)
   (setq lsp-prefer-flymake nil)
   (add-hook 'before-save-hook #'lsp-format-buffer nil t)
  )

(use-package lsp-ui
      :hook (lsp-mode . lsp-ui-mode)
      :custom
      (lsp-ui-doc-position 'bottom) ;; Keep doc at the bottom to not obscure code above
      (lsp-ui-doc-delay 0.5) ;; Delay in seconds before doc popup appears
      (lsp-ui-doc-max-width 150) ;; Max width of the doc panel
      (lsp-ui-doc-max-height 30) ;; Max height of the doc panel
      (lsp-ui-doc-enable t) ;; Enable/disable the doc. Set to nil to disable
      (lsp-ui-sideline-show-diagnostics t) ;; Show diagnostics in the sideline
      (lsp-ui-sideline-show-hover nil) ;; Disable hover text in the sideline to reduce noise
      (lsp-ui-sideline-show-code-actions t) ;; Show code actions in sideline
      )

(use-package typescript-mode
 :mode ("\\.ts\\'" "\\.tsx\\'" "\\.js\\'" "\\.jsx\\'")
 :hook (typescript-mode . lsp-deferred)
 :config
  (setq typescript-indent-level 2)
  (setq js-indent-level 2)
 )

(use-package apheleia
  :straight (:host github :repo "radian-software/apheleia")
  :config
  (apheleia-global-mode +1)
  (setq apheleia-formatters
        '((prettier-javascript . ("prettier" "--stdin-filepath" filepath))
          (prettier-typescript . ("prettier" "--stdin-filepath" filepath))))
  (add-hook 'prog-mode-hook #'apheleia-mode))

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :after dired
  )

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(defun exile/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'exile/display-startup-time)
