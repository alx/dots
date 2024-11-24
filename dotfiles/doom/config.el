;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dark+)
(setq doom-font                 (font-spec :family "Monaspace Neon"  :size 20 :weight 'light)
      doom-symbol-font          doom-font
      doom-big-font-increment   4
      doom-variable-pitch-font  (font-spec :family "Monaspace Argon" :size 22 :weight 'light)
      doom-serif-font           (font-spec :family "Monaspace Neon"  :size 20 :weight 'light))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;;
;; Basic Task Management with Org | Checklists, TODOs, and Org-Agenda
;; | Switching to Emacs #5.3
;;
;; https://youtu.be/EgOBBiomfGo?list=PLCWojN2sUurCs-6BBMOfXvkor6swGkC5l
;;
;; add timestamp when task is done
(setq org-log-done t)
;; files included in org-agenda
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;; accept completion from copilot and fallback to company
;; (use-package! copilot
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ("C-<tab>" . 'copilot-accept-completion-by-word)))
;;
;;
;; Modern Org Style
;; https://github.com/minad/org-modern
;;
;; Ten Org-mode Extensions You Need to Install!
;; https://youtu.be/5Y75ICmzQV0
(with-eval-after-load 'org (global-org-modern-mode))

;;
;;
;; Org mode: is it possible to display ONLINE images?
;; https://emacs.stackexchange.com/questions/42281/org-mode-is-it-possible-to-display-online-images
;;
;; Ten Org-mode Extensions You Need to Install!
;; https://youtu.be/5Y75ICmzQV0
(defun org-image-link (protocol link _description)
  "Interpret LINK as base64-encoded image data."
  (cl-assert (string-match "\\`img" protocol) nil
             "Expected protocol type starting with img")
  (let ((buf (url-retrieve-synchronously (concat (substring protocol 3) ":" link))))
    (cl-assert buf nil
               "Download of image \"%s\" failed." link)
    (with-current-buffer buf
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n")
      (buffer-substring-no-properties (point) (point-max)))))

(require 'ol)
(org-link-set-parameters
 "imghttp"
 :image-data-fun #'org-image-link)

(org-link-set-parameters
 "imghttps"
 :image-data-fun #'org-image-link)

;; Generate system prompts for =gptel=

;;   This section will take all the tangled system prompt files to build the associative list for the =gptel-directives= variable in the [[https://github.com/karthink/gptel][gptel]] package.

;;   Structure for =gptel-directives= is

;;   + type: cons list
;;     + key
;;       file basename ; e.g. =bojack=, =dutch-tutor=
;;     + prompt
;;       the non-comment body of the Markdown document - escape all unescaped double-quotes

;;     The magic Emacs Lisp function to create the alist

;;     old block header: #+begin_src emacs-lisp :results none :tangle gptel-build-directives.el :comments org

(defun gjg/parse-prompt-file (prompt-file)
  "Parse a single prompt file and return its description and content."
  (with-temp-buffer
    (insert-file-contents prompt-file)
    (let ((prompt-description "NO DESCRIPTION"))
      ;; nab the description - single-line descriptions only!
      (goto-char (point-min))
      (when (re-search-forward "#\\+description: \\(.*?\\) *--> *$" nil t)
        (setq prompt-description (match-string 1)))
      ;; remove all comments
      (delete-matching-lines "^ *<!--" (point-min) (point-max))
      ;; remove leading blank lines
      (goto-char (point-min))
      (while (and (looking-at "^$") (not (eobp)))
        (delete-char 1))
      ;; return the description and content
      (list prompt-description (buffer-substring-no-properties (point-min) (point-max))))))

(defun gjg/gptel-build-directives (promptdir)
  "Build `gptel-directives' from Markdown files in PROMPTDIR."
  (let* ((prompt-files (directory-files promptdir t "md$")))
    (mapcar (lambda (prompt-file)
              (let ((parsed-prompt (gjg/parse-prompt-file prompt-file)))
                (cons (intern (f-base prompt-file))  ; gptel-directives key
                      (nth 1 parsed-prompt))))       ; prompt content
            prompt-files)))

(use-package! gptel
  :init
  (setq gptel-directives (gjg/gptel-build-directives "~/code/AIPIHKAL/system-prompts/"))
  :config
  (setq
   gptel-model   "gpt4o"
   ;; gptel-backend
   ;; (gptel-make-openai "TogetherAI"
   ;;   :host "api.together.xyz"
   ;;   ;; af8285: obsolete key
   ;;   :key "af8285ed0e6ad8172e550e5318caaf87bfd73295a61c18e612f018b6b984a176"
   ;;   :stream t
   ;;   :models '(;; has many more, check together.ai
   ;;             "meta-llama/Llama-3.2-11B-Vision-Instruct-Turbo"
   ;;             "mistralai/Mixtral-8x7B-Instruct-v0.1"
   ;;             "codellama/CodeLlama-13b-Instruct-hf"
   ;;             "codellama/CodeLlama-34b-Instruct-hf"))
   ))
;;
;; https://github.com/thisirs/openwith
;;
;; Ten Org-mode Extensions You Need to Install!
;; https://youtu.be/5Y75ICmzQV0
;;
;; (when (require 'openwith nil 'noerror)
;;   (setq openwith-associations
;;         (list
;;          (list (openwith-make-extension-regexp
;;                 '("mpg" "mpeg" "mp3" "mp4"
;;                   "avi" "wmv" "wav" "mov" "flv"
;;                   "ogm" "ogg" "mkv"))
;;                "vlc"
;;                '(file))
;;          (list (openwith-make-extension-regexp
;;                 '("xbm" "pbm" "pgm" "ppm" "pnm"
;;                   "png" "gif" "bmp" "tif" "jpeg" "jpg"))
;;                "geeqie"
;;                '(file))
;;          (list (openwith-make-extension-regexp
;;                 '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
;;                "libreoffice"
;;                '(file))
;;          '("\\.lyx" "lyx" (file))
;;          '("\\.chm" "kchmviewer" (file))
;;          (list (openwith-make-extension-regexp
;;                 '("pdf" "ps" "ps.gz" "dvi"))
;;                "okular"
;;                '(file))
;;          ))
;;   (openwith-mode 1))
;;

;; Load elfeed-org
(require 'elfeed-org)

;; Initialize elfeed-org
;; This hooks up elfeed-org to read the configuration when elfeed
;; is started with =M-x elfeed=
(elfeed-org)

;; Optionally specify a number of files containing elfeed
;; configuration. If not set then the location below is used.
;; Note: The customize interface is also supported.
(setq rmh-elfeed-org-files (list "~/org/elfeed.org"))

;; Troubleshooting techniques for Emacs and Emacs Lisp
;; https://stackoverflow.com/questions/2087532/troubleshooting-techniques-for-emacs-and-emacs-lisp
(global-set-key (kbd "C-c C-d")
        (lambda () (interactive)
          (setq debug-on-error (if debug-on-error nil t))
          (message (format "debug-on-error : %s" debug-on-error))))

(use-package elfeed-tube
  :ensure t ;; or :straight t
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

;;
;; Tab mode
;;

;; hide buttons
(setq tab-bar-close-button nil)
(setq tab-bar-new-button nil)

;; tabspaces - Persistent Tabspaces
;; https://github.com/mclear-tools/tabspaces
;; Tabspaces leverages tab-bar.el and project.el (both built into emacs 27+) to create buffer-isolated workspaces (or “tabspaces”) that also integrate with your version-controlled projects. It should work with emacs 27+. It is tested to work with a single frame workflow, but should work with multiple frames as well.
(use-package tabspaces
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo t)
  (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  (tab-bar-new-tab-choice "*doom*"))

;; Emacs workspace management with tab-bar mode
;; https://mihaiolteanu.me/emacs-workspace-management
;;
;;  The tab-bar mode enables one to have as many tabs as one wants
;;  on each Emacs frame. This is similar to what we have in standard
;;  text editors or in browsers. The great thing is that each tab
;;  contains not just a single buffer but a window configuration.
;;  That is, I can split my window into multiple buffers, save it as
;;  a tab and give it a name. Switch to a different tab and I have a
;;  different set of buffers split differently. It is the perfect
;;  place to have a tab containing all my shells, a tab containing
;;  code and org files and one containing the browser, pdf viewers
;;  or music players.


(defun my/name-tab-by-project-or-default ()
  "Return project name if in a project, or default tab-bar name if not.
The default tab-bar name uses the buffer name."
  (let ((project-name (projectile-project-name)))
    (if (string= "-" project-name)
        (tab-bar-tab-name-current)
      (projectile-project-name))))

(setq tab-bar-mode t)
(setq tab-bar-show nil)
(setq tab-bar-new-tab-choice "*doom*")
(setq tab-bar-tab-name-function #'my/name-tab-by-project-or-default)

(map! :leader
      (:prefix-map ("TAB" . "Tabs")
       :desc "Switch tab" "TAB" #'tab-bar-select-tab-by-name
       :desc "New tab" "n" #'tab-bar-new-tab
       :desc "Next tab" "j" #'tab-bar-switch-to-next-tab
       :desc "Previous tab" "k" #'tab-bar-switch-to-prev-tab
       :desc "Rename tab" "r" #'tab-bar-rename-tab
       :desc "Rename tab by name" "R" #'tab-bar-rename-tab-by-name
       :desc "Close tab" "d" #'tab-bar-close-tab
       :desc "Close tab by name" "D" #'tab-bar-close-tab-by-name
       :desc "Close other tabs" "1" #'tab-bar-close-other-tabs))

;; re-define the relevant segment of Doom modeline so that it always shows
;; the name of the tab, whether explicitly set or not
(after! doom-modeline
  (doom-modeline-def-segment workspace-name
  "The current workspace name or number.
Requires `eyebrowse-mode' or `tab-bar-mode' to be enabled."
  (when doom-modeline-workspace-name
    (when-let
        ((name (cond
                ((and (bound-and-true-p eyebrowse-mode)
                      (< 1 (length (eyebrowse--get 'window-configs))))
                 (assq-delete-all 'eyebrowse-mode mode-line-misc-info)
                 (when-let*
                     ((num (eyebrowse--get 'current-slot))
                      (tag (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
                   (if (< 0 (length tag)) tag (int-to-string num))))
                (t
                 (let* ((current-tab (tab-bar--current-tab))
                        (tab-index (tab-bar--current-tab-index))
                        (explicit-name (alist-get 'name current-tab))
                        (tab-name (alist-get 'name current-tab)))
                   (if explicit-name tab-name (+ 1 tab-index)))))))
      (propertize (format " %s " name) 'face
                  (if (doom-modeline--active)
                      'doom-modeline-buffer-major-mode
                    'mode-line-inactive))))))

;; optional: set this to wherever you want the cache to be stored
;; (setq url-cache-directory "~/.cache/emacs/url")

(setq org-display-remote-inline-images 'cache) ;; enable caching

;; or this if you don't want caching
;; (setq org-display-remote-inline-images 'download)

;; or this if you want to disable this plugin
;; (setq org-display-remote-inline-images 'skip)

;; treemacs-follow-mode is a global minor mode which allows
;; the treemacs view to always move its focus to the currently selected file.
;; This mode runs on an idle timer - the exact duration of inactivity (in seconds)
;; before a move is called is determined by treemacs-tag-follow-delay.
(after! treemacs (treemacs-follow-mode 1))

(use-package yankpad
  :ensure t
  :defer 10
  :init
  (setq yankpad-file "~/org/ref_emacs_yankpad.org")
  :config
  (bind-key "<f7>" 'yankpad-map)
  (bind-key "<f12>" 'yankpad-expand)
  ;; If you want to complete snippets using company-mode
  (add-to-list 'company-backends #'company-yankpad)
  ;; If you want to expand snippets with hippie-expand
  (add-to-list 'hippie-expand-try-functions-list #'yankpad-expand))

;;
;; https://emacs.stackexchange.com/questions/4187/strip-text-properties-in-savehist
(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

;; #9409
;; the issue could be that save-interprogram-paste-before-kill means a large clipboard which becomes part of savehist:
;; #1369 (comment)
(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)
(savehist-mode -1)

(use-package org-rainbow-tags
  :ensure t)

;; Killing ansi-term says: "... has a running process"
;; https://emacs.stackexchange.com/questions/17005/killing-ansi-term-says-has-a-running-process
(defun set-no-process-query-on-exit ()
    (let ((proc (get-buffer-process (current-buffer))))
          (when (processp proc)
	          (set-process-query-on-exit-flag proc nil))))

(add-hook 'term-exec-hook 'set-no-process-query-on-exit)

;; copy links OUT of org-mode
;; https://emacs.stackexchange.com/questions/3981/how-to-copy-links-out-of-org-mode
(defun farynaio/org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
          (type (org-element-property :type link))
          (url (org-element-property :path link))
          (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

(define-key org-mode-map (kbd "C-x C-l") 'farynaio/org-link-copy)
