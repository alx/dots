;; Generate system prompts for =gptel=
;;   :PROPERTIES:
;;   :image:    img/coding-ai-hologram-1.jpeg-crop-4-3.png
;;   :END:

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
