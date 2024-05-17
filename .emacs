;; -*- mode: elisp-byte-code; -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

;;====================================
;; EMACS BEHAVIOUR
;;====================================
;; Separate custom file from configuration file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Turn off bell
(setq ring-bell-function 'ignore)

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; Scroll one line at the time
(setq scroll-step 1)
(setq scroll-conservatively 10000)

; Pixel scroll
(pixel-scroll-mode)
(setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
(setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
(setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
(setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.

;; hide tool-bar, menu-bar and scroll bar
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(menu-bar-mode -1)

;; To use all the symbols in the keyboard
(require 'iso-transl)

;; Font setup
;;(set-face-attribute 'default nil :height 120)
(set-frame-font "Monospace 14" nil t)

; make ibuffer default
(defalias 'list-buffers 'ibuffer)

; disable parent mode
(setq show-paren-mode t)

; start the emacs server
(server-start)

; Indentation style for CC Mode
(setq c-default-style "k&r")


; key-binding for compilation
(bind-key* (kbd "C-c C-m") #'compile)

; load theme
(load-theme 'solarized-dark)

;; pdf-tools remap vim navigation keybindings
(add-hook 'pdf-view-mode-hook
	  (lambda ()
	    (local-set-key (kbd "j") #'pdf-view-next-line-or-next-page)
	    (local-set-key (kbd "k") #'pdf-view-previous-line-or-previous-page)
	    (local-set-key (kbd "l") #'image-forward-hscroll)
	    (local-set-key (kbd "h") #'image-backward-hscroll)
	    (local-set-key (kbd "J") #'pdf-view-next-page)
	    (local-set-key (kbd "K") #'pdf-view-previous-page)))

;; Vim mode navigation
(bind-key* (kbd "M-h") #'backward-char)
(bind-key* (kbd "M-j") #'next-line)
(bind-key* (kbd "M-k") #'previous-line)
(bind-key* (kbd "M-l") #'forward-char)

;;====================================
;;DIRED
;;====================================
;; (define-key dired-mode-map (kbd "j") 'dired-next-line)
;; (define-key dired-mode-map (kbd "k") 'dired-previous-line)
;; (define-key dired-mode-map (kbd "h") 'dired-up-directory)
;; (define-key dired-mode-map (kbd "l") 'dired-find-file)

;;====================================
;;ORG MODE
;;====================================
(require 'org)
;; Default setting: Use "python3" under Linux and macOS, but "Python" under Windows.
;; The default for R is "R" for all platforms (predefined by Emacs).
(setq org-babel-python-command
      (if (memq system-type '(windows-nt ms-dos))
	  "Python"
	"python3 -q"))

; Org mode languages execution
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   ))

;; Do not ask for confirmation all the time!!
(setq org-confirm-babel-evaluate nil)

;;This often fails, yielding an ugly warning, and isn't of any use in Org-mode anyway
(setq python-shell-completion-native-enable nil)

;; Display images after code evaluation
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(require 'ox-latex)
; use minted package and also babel configuration
(add-to-list 'org-latex-packages-alist
	     '("AUTO" "babel" t ("pdflatex") '("" "minted")))
(setq org-latex-listings 'minted)

; scale the latex preview in org mode
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

; enable visual line mode by default in org files
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;;====================================
;;ORG CAPTURE
;;====================================
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file (concat org-directory "~/org/capture/notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/capture/gtd.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
	("j" "Journal" entry (file+datetree "~/org/capture/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")))

;;====================================
;; ORG REF
;;====================================
;;; org-ref
(setq org-ref-default-bibliography '("/home/migap/org/bibliography.bib"))
(setq org-ref-completion-library 'org-ref-ivy-cite)
(require 'org-ref)
(setq org-latex-prefer-user-labels t) ; to use my own references
(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link) ; key-binding for inserting reference
; For LaTeX export settings
; add shell escape for minted latex package
;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;; 	"bibtex %b"
;; 	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;; 	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; From the org-ref-help file: 
(setq  org-latex-pdf-process '("latexmk -shell-escape -bibtex -pdf %f"))

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
	 (key (car results))
	 (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
	(org-open-file pdf-file)
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

;;====================================
;;ORG OX-HUGO
;;====================================

;; (use-package ox-hugo
;;   :after ox)

;;====================================
;; MAGIT
;;====================================
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))


;;====================================
;;EVIL
;;====================================
(use-package evil
  :init (setq evil-want-C-i-jump nil) ; restore TAB to org-cycle
  :ensure t
  :config
  (evil-mode 1)
  (evil-set-initial-state 'dired-mode 'emacs); disbable evil mode when dired
  (evil-set-initial-state 'deft-mode 'emacs); disbable evil mode when in deft
  (evil-set-initial-state 'elfeed-search-mode 'emacs); disbable evil mode when in elfeed
  (evil-set-initial-state 'elfeed-show-mode 'emacs); disbable evil mode when in elfeed
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1)
  )

;;====================================
;; EVIL-ESCAPE
;;====================================
(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "kj")
  )


;;====================================
;; KEY-CHORD
;;====================================
; Exit insert mode by pressing k and then j quickly
;; (use-package key-chord
;;   :ensure t
;;   :config
;;   (key-chord-mode 1)
;;   (setq key-chord-two-keys-delay 0.2)
;;   ;; Max time delay between two presses of the same key to be considered a key chord.
;;   ;; Should normally be a little longer than `key-chord-two-keys-delay'.
;;   (setq key-chord-one-key-delay 0.3) ; default 0.2
;;   ;(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
;;   (key-chord-define-global "kj" 'evil-normal-state)
;;   )

;;====================================
;; LINUM RELATIVE
;;====================================
(use-package linum-relative
  :config
  (setq linum-relative-current-symbol "") ; instead of 0, displays actual line number in current line
  (add-hook 'LaTeX-mode-hook 'linum-relative-mode); with AUCTeX LaTeX mode
  (add-hook 'c-mode-common-hook 'linum-relative-mode); with AUCTeX LaTeX mode
  (add-hook 'scheme-mode-hook 'linum-relative-mode))

;;====================================
;; YAsnippet
;;====================================
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'octave-mode-hook 'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook 'yas-minor-mode)
  (add-hook 'scheme-mode-hook 'yas-minor-mode)
  (add-hook 'c-mode-common-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode))

;;====================================
;; AUCTeX
;;====================================

(use-package tex
  ; It tries to install tex package and not auctex :ensure t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (setq reftex-plug-into-AUCTeX t) ; makes reftex colaborate with AUCTex
  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	TeX-source-correlate-start-server t)
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
	    #'TeX-revert-document-buffer)
  ;; ##### Enable synctex correlation. From Okular just press
  ;; ##### Shift + Left click to go to the good line.
  (setq TeX-source-correlate-mode t
	TeX-source-correlate-start-server t)
  )

(eval-after-load "latex"
  '(define-key LaTeX-mode-map  (kbd "C-c C-g") 'pdf-sync-forward-search))

;; ;; ### Set Okular as the default PDF viewer.
;; (eval-after-load "tex"
;;   '(setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Okular"))

;; ;;====================================
;; ;; IVY
;; ;;====================================
;; ;; Ivy mode for completition
;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-count-format "(%d/%d) ")
;; (setq ivy-use-selectable-prompt t) ; allows to select "bar" when "barricade" already exists

;; ;;====================================
;; ;; IVY BIBTEX
;; ;;====================================
;; ;; Ivy-bibtex : managing bibliography
;; (setq bibtex-completion-bibliography
;;       '("/home/migap/org/bibliography.bib"
;; 	))
;; (setq bibtex-completion-pdf-field "file")

;; ; citation configuration
;; (setq bibtex-completion-format-citation-functions
;;   '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF) ; in order to insert a link to the files
;;     (latex-mode    . bibtex-completion-format-citation-cite)
;;     (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
;;     (default       . bibtex-completion-format-citation-default)))

;; (global-set-key (kbd "C-c b") 'ivy-bibtex)

;;====================================
;; VERTICO
;;====================================
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;;====================================
;; MARGINALIA
;;====================================
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;;====================================
;; ORDERLESS
;;====================================
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;====================================
;; CONSULT
;;====================================
(use-package consult
  :config
  ;; for evil minibuffer completition
  (setq completion-in-region-function 'consult-completion-in-region)
  )

(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key "M-.")
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n f" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n B" . consult-org-roam-backlinks-recursive)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search))


;;====================================
;; CITAR
;;====================================
(use-package citar
  :custom
  (citar-bibliography '("~/org/bibliography.bib"))
  (org-cite-global-bibliography '("~/org/bibliography.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  ;; :bind
  ;; ("C-c b" . citar-insert-citation)
  ;; (:map org-mode-map
  ;; 	:package org
  ;; 	("C-c b" . org-cite-insert))

  )

(global-set-key (kbd "C-c b") 'citar-insert-citation)
(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;;====================================
;; ORG ROAM
;;====================================

(use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-v2-ack t) ;; acknowledge upgrade and remove warning at startup
  :config
  (setq org-roam-directory
	(file-truename "/home/migap/org/roam/"))
  (setq org-roam-db-location
	(concat org-roam-directory "org-roam.db"))
  (org-roam-setup)
  :bind
  ("C-c n i" . org-roam-node-insert)
  ("C-c n f" . org-roam-node-find)
  ("C-c n l" . org-roam-buffer-toggle)
  )

;;====================================
;; Ispell and hunspell config
;;====================================

(with-eval-after-load "ispell"
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US")
  (setq ispell-program-name "hunspell")
  ;; Configure English, French and Spanish.
  (setq ispell-dictionary "en_US,fr_FR,es_ES")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,fr_FR,es_ES")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/.hunspell_personal"))

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
;(unless (file-exists-p ispell-personal-dictionary)
;  (write-region "" nil ispell-personal-dictionary nil 0))

;;====================================
;; Markdown mode
;;====================================

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;====================================
;; Projectile
;;====================================
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;====================================
;; ERC
;;====================================
;; Set our nickname & real-name as constant variables
;; (setq
;;  erc-nick "migap"     ; Our IRC nick
;;  erc-user-full-name "migap") ; Our /whois name

;; ;; Define a function to connect to a server
;; (defun libera-server ()
;;   (interactive)
;;   (erc-tls :server "irc.libera.chat"
;;        :port   "6697"))

;; Or assign it to a keybinding
;; This example is also using erc's TLS capabilities:
;(global-set-key "\C-cen"
;  (lambda ()
;  (interactive)
;  (erc-tls :server "server2.example.com"
;           :port   "6697")))

;; ;;====================================
;; ;; SLIME
;; ;;====================================

;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "sbcl")

;;====================================
;; UNDO-TREE
;;====================================
(setq undo-tree-auto-save-history nil)


;====================================
;; PDF-TOOLS
;;====================================
(use-package pdf-tools
   :pin manual
   :config
   (pdf-tools-install)
   (setq-default pdf-view-display-size 'fit-width)
   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
   (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
   :custom
   (pdf-annot-activate-created-annotations t "automatically annotate highlights"))


(defun update-other-buffer ()
  (interactive)
  (other-window 1)
  (revert-buffer nil t)
  (other-window -1))

(defun latex-compile-and-update-other-buffer ()
  "Has as a premise that it's run from a latex-mode buffer and the
   other buffer already has the PDF open"
  (interactive)
  (save-buffer)
  (shell-command (concat "pdflatex " (buffer-file-name)))
  (switch-to-buffer (other-buffer))
  (kill-buffer)
  (update-other-buffer))

(defun org-compile-beamer-and-update-other-buffer ()
  "Has as a premise that it's run from an org-mode buffer and the
   other buffer already has the PDF open"
  (interactive)
  (org-beamer-export-to-pdf)
  (update-other-buffer))

(defun org-compile-latex-and-update-other-buffer ()
  "Has as a premise that it's run from an org-mode buffer and the
   other buffer already has the PDF open"
  (interactive)
  (org-latex-export-to-pdf)
  (update-other-buffer))

(eval-after-load 'latex-mode
  '(define-key latex-mode-map (kbd "C-c r") 'latex-compile-and-update-other-buffer))

(define-key org-mode-map (kbd "C-c lr") 'org-compile-latex-and-update-other-buffer)
(define-key org-mode-map (kbd "C-c br") 'org-compile-beamer-and-update-other-buffer)

;;====================================
;; CITRE
;;====================================

(require 'citre)
(require 'citre-config)

(use-package citre
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; Bind your frequently used commands.  Alternatively, you can define them
  ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
  (global-set-key (kbd "C-c t j") 'citre-jump)
  (global-set-key (kbd "C-c t J") 'citre-jump-back)
  (global-set-key (kbd "C-c t p") 'citre-ace-peek)
  (global-set-key (kbd "C-c t u") 'citre-update-this-tags-file)
  :config
  (setq
   ;; Set these if readtags/ctags is not in your PATH.
   ;; citre-readtags-program "/path/to/readtags"
   ;; citre-ctags-program "/path/to/ctags"

   ;; Set these if gtags/global is not in your PATH (and you want to use the
   ;; global backend)
   ;; citre-gtags-program "/path/to/gtags"
   ;; citre-global-program "/path/to/global"

   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root

   ;; Set this if you want to always use one location to create a tags file.
   ;; citre-default-create-tags-file-location 'global-cache

   ;; See the "Create tags file" section above to know these options
   ;; citre-use-project-root-when-creating-tags t
   ;; citre-prompt-language-for-ctags-command t

   ;; By default, when you open any file, and a tags file can be found for it,
   ;; `citre-mode' is automatically enabled.  If you only want this to work for
   ;; certain modes (like `prog-mode'), set it like this.
   ;; citre-auto-enable-citre-mode-modes '(prog-mode)
   ))

;;====================================
;; ELFEED
;;====================================

(global-set-key (kbd "C-x w") 'elfeed)

(load-file "~/repos/mine/dotfiles/elfeed-feeds.el")

;;====================================
;; ACE-WINDOW
;;====================================
; Easily switch between frames and windows
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-keys '(?q ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t)
)

;;====================================
;; GEISER
;;====================================

(use-package geiser
  :ensure t
)
