


(defun perly-sense-current-line ()
  "Return the vertical position of point"
  (+ (count-lines 1 (point))
     (if (= (current-column) 0) 1 0)
     )
  )


(defun perly-sense-find-source-for-module (module)  
  (let ((file (shell-command-to-string (format "perly_sense find_module_source_file --module=%s" module))))
    (if (not (string-equal file ""))
        (find-file file)
      (message "Module (%s) source file not found" module)
      )
    )
  )


(defun perly-sense-find-source-for-module-at-point ()
  "Find the source file for the module at point."
  (interactive)
  (let ((module (cperl-word-at-point)))
    (if module
        (perly-sense-find-source-for-module module)
      )
    )
  )



(defun perly-sense-display-pod-for-module (module)
  (shell-command (format "perly_sense display_module_pod --module=%s" module)
                 (format "*%s POD*" module)
                 )
  )

(defun perly-sense-display-pod-for-file (file name-buffer)
  (shell-command (format "perly_sense display_file_pod --file=%s" file)
                 (format "*%s POD*" name-buffer)
                 )
  )


(defun perly-sense-display-pod-for-module-at-point ()
  "Display POD for the module at point."
  (interactive)
  (let ((module (cperl-word-at-point)))
    (if module
        (perly-sense-display-pod-for-module module)
      )
    )
  )




(defun perly-sense-smart-docs (word)
  "Display documentation for word."
  (if word
      (perly-sense-display-pod-for-module word)
    (perly-sense-display-pod-for-file (buffer-file-name) (buffer-name))
    )
  )




                                        
; Split result into first line and the rest and return the first line
(defun perly-sense-result-status (result)
  (car (split-string result "[\n]"))
)

; Split result into first line and the rest and return the rest
(defun perly-sense-result-text (result)
  (mapconcat 'identity (cdr (split-string result "[\n]")) "\n")
)

; Split the Split result into first line and the rest and return those two
(defun perly-sense-result-properties (result)
  (split-string (car (split-string result "[\n]")) "[\t]")
  )





(defun perly-sense-display-text-in-buffer (type name text)
  (let ((buffer-name (format "*%s %s*" type name)))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert text)
      (goto-char 1)
      (perly-sense-fontify-pod-buffer buffer-name)
      (display-buffer (current-buffer))
      )
    )
  )


(defun perly-sense-display-doc-message-or-buffer (doc-type name text)
  (cond ((string= doc-type "hint")
         (message "%s" text))
        ((string= doc-type "document")
         (perly-sense-display-text-in-buffer "POD" name text)
         (message nil)
         )
        )
  t
  )





(defun perly-sense-fontify-pod-buffer (buffer-name)
  "Mark up a buffer with text from pod2text."
  (interactive)
  (save-excursion
    (set-buffer buffer-name)
    (goto-char (point-min))
    (while (search-forward-regexp "
 \\{4,\\}" nil t)
      (let* ((point-start (point)))
        (search-forward-regexp "
")
        (backward-char)
        (put-text-property point-start (point) 'face '(:foreground "Gray50"))   ;;TODO: Move to config variable
        )
      )
    )
  )




;found   method  name    levelIndent     docType hint
(defun perly-sense-smart-docs-at-point ()
  "Display documentation for the code at point."
  (interactive)
  (message "Smart docs...")
  (let* (
         (result (shell-command-to-string
                  (format "perly_sense smart_doc --file=%s --row=%s --col=%s"
                          (buffer-file-name) (perly-sense-current-line) (+ 1 (current-column))
                          )
                  ))
         (result-text (perly-sense-result-text result))
         )
    (if (not (string= result ""))
        (let*
            (
             (properties (perly-sense-result-properties result))
             (found    (nth 1 properties))
             (name     (nth 3 properties))
             (doc-type (nth 5 properties))
             )
          (perly-sense-display-doc-message-or-buffer doc-type name result-text)
          )
      (message "Nothing found")
      )
    )
  )





(defun perly-sense-find-file-location (file row col)
  "Find the file and go to the row col location"
  (set-mark-command nil)
  (find-file file)
  (goto-line row)
  (beginning-of-line)
  (forward-char (- col 1))
  )

  




(Defun perly-sense-smart-go-to (word)
  "Go to the original symbol for word."
  (if word
      (perly-sense-find-source-for-module word)
    )
  )


(defun perly-sense-smart-go-to-at-point ()
  "Go to the original symbol in the code at point."
  (interactive)
  (message "Smart goto...")
  (let ((result (shell-command-to-string
               (format "perly_sense smart_go_to --file=%s --row=%s --col=%s"
                       (buffer-file-name) (perly-sense-current-line) (+ 1 (current-column))
                       )
               ))
        )
    (if (string-match "[\t]" result)
        (let ((value (split-string result "[\t]")))
          (let ((file (pop value)))
            (perly-sense-find-file-location file (string-to-number (pop value)) (string-to-number (pop value)))
            (message "Went to: %s" file)
            )
          )
      (message nil)
      )
    )
  )




(defun perly-sense-class-hierarchy-for-class-at-point ()
  "Display the class hierarchy for the current class"
  (interactive)
  (message "Class Hierarchy...")
  (let ((result (shell-command-to-string
                 (format "perly_sense class_hierarchy --file=%s --row=%s --col=%s"
                         (buffer-file-name) (perly-sense-current-line) (+ 1 (current-column))
                         )
                 ))
        )
    (message "%s" result)
    )
  )










(defun perly-sense-display-api-for-class-at-point ()
  "Display the likely API of the class at point."
  (interactive)
  (message "Class API...")
  (let* ((text (shell-command-to-string
               (format "perly_sense class_api --file=%s --row=%s --col=%s"
                       (buffer-file-name) (perly-sense-current-line) (+ 1 (current-column))
                       )
               ))
         (package (perly-sense-result-status text))
         (package-doc (perly-sense-result-text text))
         )
                                        ; TODO: if package eq ""
    (perly-sense-display-text-in-buffer "API" package package-doc)
    (other-window 1)
    (compilation-mode)
    (goto-line 2)
    )
  )











(global-set-key (kbd "\C-p m f") 'perly-sense-find-source-for-module-at-point)
(global-set-key (kbd "\C-p m p") 'perly-sense-display-pod-for-module-at-point)

(global-set-key (kbd "\C-p \C-d") 'perly-sense-smart-docs-at-point)
(global-set-key (kbd "\C-p \C-g") 'perly-sense-smart-go-to-at-point)
(global-set-key (kbd "\C-p \C-c") 'perly-sense-display-api-for-class-at-point)


