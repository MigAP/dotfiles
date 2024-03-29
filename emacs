;; -*- mode: elisp-byte-code; -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

;;====================================
;; CUSTOM ELISP FUNCTIONS
;;====================================

(load-file "~/dotfiles/my-elisp.el")

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

;; hide tool-bar 
(tool-bar-mode -1)

;; To use all the symbols in the keyboard 
(require 'iso-transl) 

;; Font setup 
;;(set-face-attribute 'default nil :height 120)
(set-frame-font "Monospace 12" nil t)

; make ibuffer default
(defalias 'list-buffers 'ibuffer) 

; start the emacs server 
(server-start) 

; truncate long lines
(set-default 'truncate-lines t)

; Indentation style for CC Mode
(setq c-default-style "k&r")

; load theme 
(setq x-underline-at-descent-line t) ; better underline position under X
(load-theme 'solarized-dark t)

;; highlight matching paren
(show-paren-mode 1)
;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 0)

; emacs transparency 
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

(defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(95 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c C-t") 'toggle-transparency)

;; pdf-tools remap vim navigation keybindings
(add-hook 'pdf-view-mode-hook
	  (lambda ()
	    (local-set-key (kbd "j") #'pdf-view-next-line-or-next-page)
	    (local-set-key (kbd "k") #'pdf-view-previous-line-or-previous-page)
	    (local-set-key (kbd "l") #'image-forward-hscroll)
	    (local-set-key (kbd "h") #'image-backward-hscroll)
	    (local-set-key (kbd "J") #'pdf-view-next-page)
	    (local-set-key (kbd "K") #'pdf-view-previous-page)))

; eshell configuration 
(defun eshell/e (&rest args)
  (apply #'find-file args))

;;====================================
;; DIRED
;;====================================

; Source: https://protesilaos.com/codelog/2023-06-26-emacs-file-dired-basics/
;; When you first call `find-file' (C-x C-f by default), you do not
;; need to clear the existing file path before adding the new one.
;; Just start typing the whole path and Emacs will "shadow" the
;; current one.  For example, you are at ~/Documents/notes/file.txt
;; and you want to go to ~/.emacs.d/init.el: type the latter directly
;; and Emacs will take you there.
(file-name-shadow-mode 1)

;; Do not outright delete files.  Move them to the system trash
;; instead.  The `trashed' package can act on them in a Dired-like
;; fashion.  I use it and can recommend it to either restore (R) or
;; permanently delete (D) the files.
(setq delete-by-moving-to-trash t)

;; When there are two Dired buffers side-by-side make Emacs
;; automatically suggest the other one as the target of copy or rename
;; operations.  Remember that you can always use M-p and M-n in the
;; minibuffer to cycle through the history, regardless of what this
;; does.  (The "dwim" stands for "Do What I Mean".)
(setq dired-dwim-target t)

;; Automatically hide the detailed listing when visiting a Dired
;; buffer.  This can always be toggled on/off by calling the
;; `dired-hide-details-mode' interactively with M-x or its keybindings
;; (the left parenthesis by default).
;(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; Teach Dired to use a specific external program with either the
;; `dired-do-shell-command' or `dired-do-async-shell-command' command
;; (with the default keys, those are bound to `!' `&', respectively).
;; The first string is a pattern match against file names.  The
;; remaining strings are external programs that Dired will provide as
;; suggestions.  Of course, you can always type an arbitrary program
;; despite these defaults.
(setq dired-guess-shell-alist-user
      '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
        ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
		(".*" "xdg-open")))


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
   (maxima . t)
   (scheme . t) 
   (lisp . t)
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

; completition option for org-mode jum C-c C-j 
(setq org-goto-interface 'outline-path-completion)
(setq org-outline-path-complete-in-steps nil)
;;====================================
;; MOOC Reproducible Research Templates
;;====================================
(require 'cl)
(setq rrmooc/new-org-templates (version<= "9.2" (org-version)))
(when  rrmooc/new-org-templates
  (require 'org-tempo))
(require 'subr-x)
(defun rrmooc/add-org-template (old-style-template)
  (add-to-list 'org-structure-template-alist
	       (if rrmooc/new-org-templates
		   (cons
		    (first old-style-template)
		    (string-trim-right (substring (second old-style-template) 8 -9)))
		 old-style-template)))

(unless rrmooc/new-org-templates
  ;; this template is predefined in the new templating system
  (rrmooc/add-org-template
   '("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")))

(rrmooc/add-org-template
 '("m" "#+begin_src emacs-lisp\n\n#+end_src" "<src lang=\"emacs-lisp\">\n\n</src>"))

(rrmooc/add-org-template
 '("r" "#+begin_src R :results output :session *R* :exports both\n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(rrmooc/add-org-template
 '("R" "#+begin_src R :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(rrmooc/add-org-template
 '("RR" "#+begin_src R :results output graphics :file  (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(rrmooc/add-org-template
 '("p" "#+begin_src python :results output :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(rrmooc/add-org-template
 '("P" "#+begin_src python :results output :session :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(rrmooc/add-org-template
 '("PP" "#+begin_src python :results file :session :var matplot_lib_filename=(org-babel-temp-file \"figure\" \".png\") :exports both\nimport matplotlib.pyplot as plt\n\nimport numpy\nx=numpy.linspace(-15,15)\nplt.figure(figsize=(10,5))\nplt.plot(x,numpy.cos(x)/x)\nplt.tight_layout()\n\nplt.savefig(matplot_lib_filename)\nmatplot_lib_filename\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(if (memq system-type '(windows-nt ms-dos))
    ;; Non-session shell execution does not seem to work under Windows, so we use
    ;; a named session just like for B.
    (rrmooc/add-org-template
     '("b" "#+begin_src shell :session session :results output :exports both\n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))
  (rrmooc/add-org-template
   '("b" "#+begin_src shell :results output :exports both\n\n#+end_src" "<src lang=\"sh\">\n\n</src>")))

(rrmooc/add-org-template
 '("B" "#+begin_src shell :session *shell* :results output :exports both \n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

(rrmooc/add-org-template
 '("g" "#+begin_src dot :results output graphics :file \"/tmp/graph.pdf\" :exports both
digraph G {
node [color=black,fillcolor=white,shape=rectangle,style=filled,fontname=\"Helvetica\"];
A[label=\"A\"]
B[label=\"B\"]
A->B
}\n#+end_src" "<src lang=\"dot\">\n\n</src>"))

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
(setq org-ref-show-citation-on-enter nil)


; For LaTeX export settings 
; add shell escape for minted latex package
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
	 (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
	(org-open-file pdf-file t) ; "t" to open the pdf inside emacs
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

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
  :ensure t
  :config
  (evil-mode 1)
  (evil-set-initial-state 'dired-mode 'emacs); disbable evil mode when dired 
  (evil-set-initial-state 'image-dired-thumbnail-mode 'emacs); disbable evil mode when image-dired 
  (evil-set-initial-state 'image-mode 'emacs); disbable evil mode when in Image mode 
  (evil-set-initial-state 'deft-mode 'emacs); disbable evil mode when in deft  
  (evil-set-initial-state 'elfeed-search-mode 'emacs); disbable evil mode when in elfeed  
  (evil-set-initial-state 'elfeed-show-mode 'emacs); disbable evil mode when in elfeed  
  (evil-set-initial-state 'eww-mode 'emacs); 
  (evil-set-initial-state 'eww-buffers-mode 'emacs); 
  (evil-set-initial-state 'Info-mode 'emacs); 
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1)
  )

;;====================================
;; EVIL-ESCAPE 
;;====================================
(use-package evil-escape
  :ensure t 
  :config
  (setq-default evil-escape-key-sequence "kj")
  (evil-escape-mode)
)

;;====================================
;; KEY-CHORD 
;;====================================
;;Exit insert mode by pressing k and then j quickly
;; (use-package key-chord
;;   :ensure t 
;;   :config
;;   (setq key-chord-two-keys-delay 0.5)
;;   (key-chord-define evil-insert-state-map "kj" 'evil-normal-state) 
;;   (key-chord-mode 1))

;;====================================
;; LINUM RELATIVE
;;====================================
;(require 'linum-relative)
(setq linum-relative-current-symbol "") ; instead of 0, displays actual line number in current line 
;(linum-relative-global-mode)
(add-hook 'matlab-mode-hook 'linum-relative-mode)
(add-hook 'c++-mode-hook 'linum-relative-mode)

;;====================================
;; YAsnippet
;;====================================
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'octave-mode-hook 'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode))

;;====================================
;; AUCTeX
;;====================================

(use-package tex
  ; It tries to install tex package and not auctex :ensure t 
  :config
  (setq TeX-engine 'luatex);
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'LaTeX-mode-hook 'linum-relative-mode); with AUCTeX LaTeX mode
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

;; ### Set Okular as the default PDF viewer.
;; (eval-after-load "tex"
;;   '(setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Okular"))

;;====================================
;; IVY
;;====================================
;; Ivy mode for completition 
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-use-selectable-prompt t) ; allows to select "bar" when "barricade" already exists 

;;====================================
;; IVY BIBTEX
;;====================================
;; Ivy-bibtex : managing bibliography 
(setq bibtex-completion-bibliography
      '("/home/migap/org/bibliography.bib"
        ))
(setq bibtex-completion-pdf-field "file")

; citation configuration 
(setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF) ; in order to insert a link to the files 
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
    (default       . bibtex-completion-format-citation-default)))

(global-set-key (kbd "C-c b") 'ivy-bibtex)


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
;; ELPY
;;====================================

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt"))

(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common nil)))
    (yas-expand)))

(add-hook 'company-mode-hook
          (lambda ()
            (substitute-key-definition
             'company-complete-common
             'company-yasnippet-or-completion
             company-active-map)))


;;====================================
;; visual-fill-column  
;;====================================
; wrap lines when using visual-line-mode (does not work)
(add-hook 'visual-fill-column-mode-hook 'visual-line-mode)
; column at which the text is wrapepd 
(setq visual-fill-column-width 100) 
; center the text 
(setq visual-fill-column-center-text nil)

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
(unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0))

;;====================================
;; Deft
;;====================================
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org/"))

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
(setq
 erc-nick "migap"     ; Our IRC nick
 erc-user-full-name "migap") ; Our /whois name

;; Define a function to connect to a server
(defun libera-server ()
  (interactive)
  (erc-tls :server "irc.libera.chat"
	   :port   "6697"))

;; Or assign it to a keybinding
;; This example is also using erc's TLS capabilities:
;(global-set-key "\C-cen"
;  (lambda ()
;  (interactive)
;  (erc-tls :server "server2.example.com"
;           :port   "6697")))

;;====================================
;; SLIME (lisp)
;;====================================

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(load "/home/migap/.quicklisp/clhs-use-local.el" t) ; CLHS configuration
; (Location: /home/migap/.quicklisp/dists/quicklisp/software/clhs-0.6.3/README) README location for useful info

;;====================================
;; MAXIMA 
;;====================================
(use-package maxima
  :init
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
	maxima-display-maxima-buffer nil)
  (add-to-list 'auto-mode-alist
	       (cons "\\.mac\\'" 'maxima-mode))
  (add-to-list 'interpreter-mode-alist
	       (cons "maxima" 'maxima-mode)))

;;====================================
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
;; NOTMUCH
;;====================================
(autoload 'notmuch "notmuch" "notmuch mail" t)
(setq notmuch-search-oldest-first nil)
(setq message-kill-buffer-on-exit t)

;;====================================
;; MU4E
;;====================================
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e)
;; these are actually the defaults
(setq
 mu4e-maildir       "~/email/etu-unistra/"   ;; top-level Maildir
 mu4e-sent-folder   "/Sent"       ;; folder for sent messages
 mu4e-drafts-folder "/Drafts"     ;; unfinished messages
 mu4e-trash-folder  "/Trash"      ;; trashed messages
 mu4e-refile-folder "/Archives")   ;; saved messages

(setq
 mu4e-get-mail-command "mbsync -a"   ;; or fetchmail, or ...
 mu4e-update-interval 300)             ;; update every 5 minutes


;; don't keep message compose buffers around after sending:
(setq message-kill-buffer-on-exit t)

;; send function:
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)

;; send program:
;; this is exeranal. remember we installed it before.
(setq sendmail-program (executable-find "msmtp"))

;; select the right sender email from the context.
(setq message-sendmail-envelope-from 'header)


;; (setq sendmail-program "/usr/bin/msmtp"
;;       send-mail-function 'smtpmail-send-it
;;       message-sendmail-f-is-evil t
;;       message-sendmail-extra-arguments '("--read-envelope-from")
;;       message-send-mail-function 'message-send-mail-with-sendmail)


;; (setq send-mail-function 'sendmail-send-it
;;       sendmail-program "/usr/bin/msmtp"
;;       mail-specify-envelope-from t
;;       message-sendmail-envelope-from 'header
;;       mail-envelope-from 'header)

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

(load-file "~/webdrive/elfeed-feeds.el")

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
