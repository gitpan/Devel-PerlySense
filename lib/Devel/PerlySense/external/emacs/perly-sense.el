
;;
;; INSTALLATION
;;
;; See the instructions at:
;; http://search.cpan.org/dist/Devel-PerlySense/lib/Devel/PerlySense.pm#Emacs_installation
;;




;;;; Utilities



(defun alist-value (alist key)
  "Return the value of KEY in ALIST" ;; Surely there must be an existing defun to do this that I haven't found...
  (cdr (assoc key alist)))



(defun alist-num-value (alist key)
  "Return the numeric value of KEY in ALIST"
  (string-to-number (alist-value alist key)))



(defun perly-sense-switch-to-buffer (buffer)
  "Switch to BUFFER (buffer object, or buffer name). If the
buffer is already visible anywhere, re-use that visible buffer."
  (let* ((buffer-window (get-buffer-window buffer)))
    (when buffer-window
      (select-window buffer-window)
      )
    (switch-to-buffer buffer)
    )
  )





;;;; Other modules

;; Regex Tool
(require 'regex-tool)



(defun regex-render-perl (regex sample)
  (with-temp-buffer
    (let*
        ((g-statement      ;; If /g modifier, loop over all matches
          (if (string-match "[|#!?\"'/)>}][cimosx]*?g[cimosxg]*$" regex) "while" "if"))
         (regex-line (format "%s ($line =~
m%s
) {" g-statement regex)))  ;; Insert regex spec on a separate line so it can contain Perl comments
      (insert (format "@lines = <DATA>;
$line = join(\" \", @lines);
print \"(\";
%s
  print \"(\", length($`), \" \", length($&), \" \";
  for $i (1 .. 20) {
    if ($$i) {
      print \"(\", $i, \" . \\\"\", $$i, \"\\\") \";
    }
  }
  print \")\";
}
print \")\";
__DATA__
%s" regex-line sample))
      (call-process-region (point-min) (point-max) "perl" t t)
      (goto-char (point-min))
      (read (current-buffer)))))





;; For their faces
(require 'compile)
(require 'cperl-mode)
(require 'cus-edit)



;;;; Configuration


(defgroup perly-sense nil
  "PerlySense Perl IDE."
  :prefix "perly-sense-"
  :group 'languages
  :version "1.0")



(defgroup perly-sense-faces nil
  "Colors."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "perly-sense-"
  :group 'perly-sense)


(defcustom perly-sense-here-face 'font-lock-string-face
  "*Face for here-docs highlighting."
  :type 'face
  :group 'perly-sense-faces)



(defface perly-sense-heading
  `((t (:inherit 'custom-face-tag)))
;  `((t (:inherit 'bold)))  ;
  "Face for headings."
  :group 'perly-sense-faces)
(defvar perly-sense-heading-face 'perly-sense-heading
  "Face for headings.")





(defface perly-sense-module-name
  `((((class grayscale) (background light))
     (:background "Gray90"))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))
    (((class color) (background light))
     (:foreground "Blue" :background "lightyellow2"))
    (((class color) (background dark))
     (:foreground "yellow" :background ,cperl-dark-background))
    (t (:weight bold)))
  "Face for module names."
  :group 'perly-sense-faces)
(defvar perly-sense-module-name-face 'perly-sense-module-name
  "Face for module names.")

(defface perly-sense-highlighted-module-name
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))
    (((class color) (background light))
     (:foreground "Blue" :background "lightyellow2" :weight bold))
    (((class color) (background dark))
     (:foreground "Blue" :background, cperl-dark-background :weight bold))
    (t (:weight bold)))
  "Face for highlighted module names."
  :group 'perly-sense-faces)
(defvar perly-sense-highlighted-module-name-face 'perly-sense-highlighted-module-name
  "Face for highlighted module names.")

(defvar perly-sense-bookmark-file-face compilation-info-face
  "Face for Bookmark file names.")

(defvar perly-sense-bookmark-line-number-face 'compilation-line-number
  "Face for Bookmark line numbers.")

(defface perly-sense-current-class-method
  `((t (:inherit 'font-lock-function-name-face)))
  "Face for methods in the current class."
  :group 'perly-sense-faces)
(defvar perly-sense-current-class-method-face 'perly-sense-current-class-method
  "Face for methods in the current class.")

(defface perly-sense-current-new-method
  `((t (:inherit 'font-lock-function-name-face :weight bold)))
  "Face for new in the current class."
  :group 'perly-sense-faces)
(defvar perly-sense-current-new-method-face 'perly-sense-current-new-method
  "Face for new in the current class.")

(defface perly-sense-base-class-method
  `((t (:inherit 'font-lock-keyword-face)))
  "Face for methods in the base class."
  :group 'perly-sense-faces)
(defvar perly-sense-base-class-method-face 'perly-sense-base-class-method
  "Face for methods in the base class.")

(defface perly-sense-base-new-method
  `((t (:inherit 'font-lock-keyword-face :weight bold)))
  "Face for new in the base class."
  :group 'perly-sense-faces)
(defvar perly-sense-base-new-method-face 'perly-sense-base-new-method
  "Face for new in the base class.")

(defface perly-sense-cpan-base-class-method
  `((t (:inherit 'font-lock-keyword-face)))
  "Face for methods in base classes outside the Project."
  :group 'perly-sense-faces)
(defvar perly-sense-cpan-base-class-method-face 'perly-sense-cpan-base-class-method
  "Face for methods in base classes outside the Project.")

(defface perly-sense-cpan-base-new-method
  `((t (:inherit 'font-lock-keyword-face :weight bold)))
  "Face for new in base classes outside the Project."
  :group 'perly-sense-faces)
(defvar perly-sense-cpan-base-new-method-face 'perly-sense-cpan-base-new-method
  "Face for new in base classes outside the Project.")





;;;; Defuns



;;;; Defuns



(defun perly-sense-log (msg)
  "log msg in a message and return msg"
;;  (message "LOG(%s)" msg)
  )


(defun perly-sense-current-line ()
  "Return the vertical position of point"
  (+ (count-lines 1 (point))
     (if (= (current-column) 0) 1 0)
     )
  )





;;;;

(defun perly-sense-find-source-for-module (module)
  (let ((file (shell-command-to-string (format "perly_sense find_module_source_file --module=%s" module))))
    (if (not (string-equal file ""))
        (find-file file)
      (message "Module (%s) source file not found" module)
      nil
      )
    )
  )


(defun perly-sense-find-source-for-module-at-point ()
  "Find the source file for the module at point."
  (interactive)
  (let ((module (cperl-word-at-point)))
    (if module
        (progn
          (message "Going to module %s..." module)
          (perly-sense-find-source-for-module module)
          )
      )
    )
  )



; should use something that fontifies
(defun perly-sense-display-pod-for-module (module)
  (shell-command (format "perly_sense display_module_pod --module=%s" module)
                 (format "*%s POD*" module)
                 )
  )

; should use something that fontifies
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




(defun perly-sense-run-file ()
  "Run the current file"
  (interactive)

  ;;If it's the compilation buffer, recompile, else run file
  (if (string= (buffer-name) "*compilation*")
      (progn
        (message "Recompile file...")
        (recompile)
        )
    (progn
      (message "Run File...")
      (let ((result
             (shell-command-to-string
              (format "perly_sense run_file --file=%s" (buffer-file-name)))
             ))
        (let* (
               (result-hash (perly-sense-parse-sexp result))
               (dir-run-from (alist-value result-hash "dir-run-from"))
               (command-run (alist-value result-hash "command-run"))
               (type-source-file (alist-value result-hash "type-source-file"))
               (message-string (alist-value result-hash "message"))
               )
          (if command-run
              (perly-sense-run-file-run-command
               ;;             (perly-sense-run-file-get-command command-run type-source-file)
               command-run
               dir-run-from
               )
            )
          (if message-string
              (message message-string)
            )
          )
        )
      )
    )
  )

(defun perly-sense-run-file-run-command (command dir-run-from)
  "Run command from dir-run-from using the compiler function"
  (with-temp-buffer
    (cd dir-run-from)
    (compile command)
    )
  )



(defun perly-sense-rerun-file ()
  "Rerun the current compilation buffer"
  (interactive)
  (let* ((compilation-buffer (get-buffer "*compilation*")))
    (if compilation-buffer
        (let* ((compilation-window (get-buffer-window compilation-buffer "visible")))
          (progn
            (if compilation-window (select-window compilation-window))
            (switch-to-buffer "*compilation*")
            (recompile))
          )
      (message "Can't re-run: No Run File in progress.")
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




(defun perly-sense-inheritance-docs-at-point ()
  "Display the Inheritance structure for the current Class"
  (interactive)
  (message "Document Inheritance...")
  (let* ((result-alist (perly-sense-command-on-current-file-location "inheritance_doc"))
         (message-string (alist-value result-alist "message"))
         (class-inheritance (alist-value result-alist "class-inheritance"))
         )
    (if (not class-inheritance)
        (message "No Base Class found")
      (message "%s" class-inheritance)
      )
    (if message-string
        (message message-string)
      )
    )
  )



;; todo: remove duplication betweenthis defun and the one above
(defun perly-sense-class-method-docs (class-name method)
  "Display documentation for the 'method' of 'class-name'."
  (interactive)
  (message "Finding docs for method (%s)..." method)
  (let* (
         (result (shell-command-to-string
                  (format "perly_sense method_doc --class_name=%s --method_name=%s --dir_origin=."
                          class-name method
                          )
                  ))
         (result-text (perly-sense-result-text result))
         )
    (if (not (string= result ""))
        (let* ((properties (perly-sense-result-properties result))
               (found    (nth 1 properties))
               (name     (nth 3 properties))
               (doc-type (nth 5 properties)))
          (perly-sense-display-doc-message-or-buffer doc-type name result-text)
          )
      (message "Nothing found")
      )
    )
  )





(defun perly-sense-find-file-location (file row col)
  "Find the file and go to the row/col location. If row and/or
col is 0, the point isn't moved in that dimension."
  (push-mark nil t)
  (when file (find-file file))
  (when (> row 0) (goto-line row))
  (when (> col 0)
    (beginning-of-line)
    (forward-char (- col 1))
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




(defun perly-sense-go-to-base-class-at-point ()
  "Go to the Base Class of the Class at point. If ambigous, let
the the user choose a Class."
  (interactive)
  (message "Goto Base Class...")
  (let* ((result-alist (perly-sense-command-on-current-file-location "base_class_go_to"))
         (message-string (alist-value result-alist "message"))
         (class-list (alist-value result-alist "class-list"))
         (first-class-alist (car class-list))
         (second-class-alist (cdr class-list))
         )
    (if (not first-class-alist)
        (message "No Base Class found")
      (if (not second-class-alist)
          (perly-sense-go-to-class-alist first-class-alist)
        (let ((chosen-class-alist
               (perly-sense-choose-class-alist-from-class-list "Base Class" class-list)))
          (perly-sense-go-to-class-alist chosen-class-alist)
          )
        )
      )
    (if message-string
        (message message-string)
      )
    )
  )



(defun perly-sense-go-to-vc-project ()
  "Go to the project view of the current Version Control, or the
project dir if there is no vc."
  (interactive)
  (message "Goto Version Control...")
  (let ((vc-buffer (get-buffer "*svn-status*")))
    (if vc-buffer
        (perly-sense-switch-to-buffer vc-buffer)
      (let* ((result-alist (perly-sense-command "project_dir"))
             (project-dir (alist-value result-alist "project-dir"))
             )
        (perly-sense-vc-project "svn" project-dir)
        )
      )
    )
  )



(defun perly-sense-vc-project (vcs project-dir)
  "Display the Project view for the VCS (e.g. 'svn') for the
PROJECT-DIR, e.g. run svn-status for PROJECT-DIR.

Currently hard coded for SVN."
  (message "SVN status...")
  (svn-status project-dir)
  )



(defun perly-sense-command (command &optional options)
  "Call perly_sense COMMAND, and return the parsed result as a
sexp"
  (unless options (setq options ""))
  (perly-sense-parse-sexp
   (shell-command-to-string
    (format "perly_sense %s %s" command options))))



(defun perly-sense-command-on-current-file-location (command &optional options)
  "Call perly_sense COMMAND with the current file and row/col,
and return the parsed result as a sexp"
  (unless options (setq options ""))
  (perly-sense-parse-sexp
   (shell-command-to-string
    (format "perly_sense %s --file=%s --row=%s --col=%s %s"
            command
            (buffer-file-name)
            (perly-sense-current-line)
            (+ 1 (current-column))
            options))))



(defun perly-sense-go-to-method-new ()
  "Go to the 'new' method."
  (interactive)
  (message "Goto the 'new' method...")
  (let ((new-location-alist
         (or
          (perly-sense-find-method-in-buffer "new")
          (perly-sense-find-method-in-file "new"))))
    (if new-location-alist
        (perly-sense-go-to-location-alist new-location-alist)
      (message "Could not find any 'new' method")
      )
    )
  )



(defun perly-sense-find-method-in-buffer (method-name)
  "Find a method named METHOD-NAME in the buffer and return an
alist with (keys: row, col), or nil if no method was found."
  (save-excursion
    (beginning-of-buffer)
    (if (and
         (search-forward-regexp (format "\\(^\\| \\)sub +%s\\($\\| \\)" method-name) nil t)
         (search-backward-regexp "sub")
         )
        `(
          ("row" . ,(number-to-string (perly-sense-current-line)))
          ("col" . ,(number-to-string (+ 1 (current-column))))
          )
      nil
      )
    )
  )



(defun perly-sense-find-method-in-file (method-name)
  "Find a method named METHOD-NAME given the current class in the
buffer and return an alist with (keys: file, row, col), or nil if
no method was found."
  (let* ((result-alist (perly-sense-command-on-current-file-location "method_go_to" "--method_name=new"))
         (message-string (alist-value result-alist "message"))
         (file (alist-value result-alist "file"))
         (row (alist-value result-alist "row"))
         (col (alist-value result-alist "col"))
         )
    (if row
        `(
          ("file" . ,file)
          ("row" . ,row)
          ("col" . ,col)
          )
      (when message-string
        (message "no row, message")
        (message "%s" message-string)
        )
      nil
      )
    )
  )




(defun perly-sense-go-to-location-alist (location-alist)
  "Go to the LOCATION-ALIST which may contain the (keys: file,
row, col, class-name).

If file is specified, visit that file first.

If class-name is specified, display that class name in the echo
area."
  (let ((file (alist-value location-alist "file"))
        (row (alist-num-value location-alist "row"))
        (col (alist-num-value location-alist "col"))
        (class-name (alist-value location-alist "class-name"))
        )
    (perly-sense-find-file-location file row col)
    (if class-name
        (message "Went to %s" class-name)
      )
    )
  )



(defun perly-sense-go-to-class-alist (class-alist)
  "Go to the Class class-alist (keys: class-name, file, row)"
  (let ((class-name (alist-value class-alist "class-name"))
        (class-inheritance (alist-value class-alist "class-inheritance"))
        (file (alist-value class-alist "file"))
        (row (alist-num-value class-alist "row")))
    (perly-sense-find-file-location file row 1)
    (message "%s" class-inheritance)
    )
  )



(defun perly-sense-choose-class-alist-from-class-list (what-text class-list)
  "Let the user choose a class-alist from the lass-list of Class
definitions.

Return class-alist with (keys: class-name, file, row)"
  (let* ((class-description-list (mapcar (lambda (class-alist)
                                    (alist-value class-alist "class-description")
                                    ) class-list))
         (chosen-class-description (completing-read
                             (format "%s: " what-text)
                             class-description-list
                             nil
                             "force"
                             nil
                             nil
                             (car class-description-list)
                             ))
         )
    (perly-sense-get-alist-from-list class-list "class-description" chosen-class-description)
    )
  )



(defun perly-sense-get-alist-from-list (list-of-alist key value)
  "Return the first alist in list which aliast's key is value, or
nil if none was found"
  (catch 'found
    (dolist (alist list-of-alist)
      (let ((alist-item-value (alist-value alist key)))
        (if (string= alist-item-value value)
            (throw 'found alist)
          nil)))))



;; todo: remove duplication between this defun and the one above
(defun perly-sense-class-method-go-to (class-name method)
  "Go to the original symbol of 'method' in 'class-name'. Return
t on success, else nil"
  (interactive)
  (message "Go to method (%s)..." method)
  (let ((result (shell-command-to-string
                 (format
                  "perly_sense method_go_to --class_name=%s --method_name=%s --dir_origin=."
                  class-name
                  method
                  )
                 )
                ))
    (if (string-match "[\t]" result)
        (let ((value (split-string result "[\t]")))
          (let ((file (pop value)))
            (perly-sense-find-file-location file (string-to-number (pop value)) (string-to-number (pop value)))
            (message "Went to: %s" file)
            )
          )
      (progn
        (message "Could not find method (%s) (it may be created dynamically, or in XS, or in a subclass)" method)
        nil
        )
      )
    )
  )



(defun perly-sense-class-overview-for-class-at-point ()
  "Display the Class Overview for the current class"
  (interactive)
  (perly-sense-class-overview-with-argstring
   (format
    "perly_sense class_overview --file=%s --row=%s --col=%s"
    (buffer-file-name)
    (perly-sense-current-line)
    (+ 1 (current-column)))))


(defun perly-sense-class-overview-with-argstring (argstring)
  "Call perly_sense class_overview with argstring and display Class Overview with the response"
  (interactive)
  (message "Class Overview...")
  (let ((result (shell-command-to-string
                 (format
                  "perly_sense class_overview %s --width_display=%s"
                  argstring (- (window-width) 2) ))))
    (let* ((result-hash (perly-sense-parse-sexp result))
           (class-name (alist-value result-hash "class-name"))
           (class-overview (alist-value result-hash "class-overview"))
           (message-string (alist-value result-hash "message"))
           (dir (alist-value result-hash "dir"))
           )
;;      (perly-sense-log msg)
      (if class-name
          (perly-sense-display-class-overview class-name class-overview dir)
        )
      (if message-string
          (message message-string)
        )
      )
    )
  )




(defun perly-sense-parse-sexp (result)
;;  (message "RESPONSE AS TEXT |%s|" result)
  (if (string= result "")
      '()
    (let ((response-alist (eval (car (read-from-string result)))))
      response-alist
      )
    )
  )


;;;(perly-sense-parse-result-into-alist "'((\"class-overview\" . \"Hej baberiba [ Class::Accessor ]\") (\"class-name\" . \"Class::Accessor\") (\"message\" . \"Whatever\"))")
;;(perly-sense-parse-result-into-alist "'((\"class-name\" . \"alpha\"))")





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





(defun perly-sense-regex-tool ()
  "Bring up the Regex Tool"
  (interactive)
  (setq regex-tool-backend "Perl")
  (regex-tool)
  (set-buffer "*Regex*")
  (if (= (point-min) (point-max))
      (progn
        (insert "//msi")
        (goto-char 2)
        )

    )
  )



;; PerlySense Class major mode

;;;



(defun perly-sense-display-class-overview (class-name overview-text dir)
  (let ((buffer-name "*Class Overview*"))
    (with-current-buffer (get-buffer-create buffer-name)
(message "dir (%s)" dir)
      (setq default-directory dir)
      (toggle-read-only t)(toggle-read-only)  ; No better way?
      (erase-buffer)
      (insert overview-text)
      (perly-sense-class-mode)
      (perly-sense-fontify-class-overview-buffer buffer-name)
      (perly-sense-search-current-class-name class-name)
      (switch-to-buffer (current-buffer))  ;; before: display-buffer
      (toggle-read-only t)
      )
    )
  )



;; Set point where class-name is mentioned in brackets
(defun perly-sense-search-class-name (class-name)
  (let ((class-name-box (format "[ %s " class-name)))
    (goto-char (point-min))
    (search-forward class-name-box)
    (search-backward "[ ")
    (forward-char)
    )
  )



;; Set point where class-name is mentioned in current [< xxx >]
(defun perly-sense-search-current-class-name (class-name)
  (let ((class-name-box (format "[<%s" class-name)))
    (goto-char (point-min))
    (search-forward class-name-box nil t)
    (search-backward "[<" nil t)
    (forward-char)
    )
  )



(defun perly-sense-fontify-class-overview-buffer (buffer-name)
  "Mark up a buffer with Class Overview text."
  (interactive)
  (save-excursion
    (set-buffer buffer-name)

    (goto-char (point-min))
    (while (search-forward-regexp "\\[ \\w+ +\\]" nil t)
      (put-text-property (match-beginning 0) (match-end 0) 'face perly-sense-module-name-face))

    (goto-char (point-min))
    (while (search-forward-regexp "\\[<\\w+ *>\\]" nil t)
      (put-text-property (match-beginning 0) (match-end 0) 'face perly-sense-highlighted-module-name-face))

    (goto-char (point-min))
    (while (search-forward-regexp "^[^:\n]+:[0-9]+:" nil t)
      (let
          ((file-beginning (match-beginning 0))
           (row-end (- (match-end 0) 1)))
        (search-backward-regexp ":[0-9]+:" nil t)
        (let
            ((file-end (match-beginning 0))
             (row-beginning (+ (match-beginning 0) 1)))
          (put-text-property file-beginning file-end 'face perly-sense-bookmark-file-face)
          (put-text-property row-beginning row-end 'face perly-sense-bookmark-line-number-face)
          )))

    (goto-char (point-min))
    (while (search-forward-regexp "->\\w+" nil t)  ;; ->method
      (put-text-property (match-beginning 0) (match-end 0) 'face perly-sense-current-class-method-face))

    (goto-char (point-min))
    (while (search-forward-regexp "\\\\>\\w+" nil t)  ;; \>method
      (put-text-property (match-beginning 0) (match-end 0) 'face 'font-lock-keyword-face))

    (goto-char (point-min))
    (while (search-forward-regexp "->new\\b" nil t)  ;; ->new
      (put-text-property (match-beginning 0) (match-end 0) 'face perly-sense-current-new-method-face))

    (goto-char (point-min))
    (while (search-forward-regexp "\\\\>new\\b" nil t)  ;; \>new
      (put-text-property (match-beginning 0) (match-end 0) 'face perly-sense-base-new-method-face))



    (goto-char (point-min))
    (while (search-forward-regexp "\\* \\w+ +\\*" nil t)
      (let
          ((heading-beginning (match-beginning 0) )
           (heading-end (match-end 0) ))
        (put-text-property heading-beginning heading-end 'face perly-sense-heading-face)
        (add-text-properties heading-beginning (+ heading-beginning 2) '(invisible t))
        (add-text-properties (- heading-end 2) heading-end '(invisible t))
      ))
    )
  )




(defun perly-sense-compile-goto-error-file-line ()
  "Go to the file + line specified on the row at point, or ask for a
text to parse for a file + line."
  (interactive)
  (let* ((file_row (perly-sense-compile-get-file-line-from-buffer) )
         (file (nth 0 file_row))
         (row (nth 1 file_row)))
    (if file
        (perly-sense-find-file-location file (string-to-number row) 1)
      (let* ((file_row (perly-sense-compile-get-file-line-from-user-input) )
             (file (nth 0 file_row))
             (row (nth 1 file_row)))
        (if file
            (perly-sense-find-file-location file (string-to-number row) 1)
          (message "No 'FILE line N' found")
          )
        )
      )
    )
  )


(defun perly-sense-compile-get-file-line-from-user-input ()
  "Ask for a text to parse for a file + line, parse it using
'perly-sense-compile-get-file-line-from-buffer'. Return what it
returns."
  (with-temp-buffer
    (insert (read-string "FILE, line N text: " (current-kill 0 t)))
    (perly-sense-compile-get-file-line-from-buffer)
    )
  )


(defun perly-sense-compile-get-file-line-from-buffer ()
  "Return a two item list with (file . row) specified on the row at
point, or an empty list () if none was found."
  (save-excursion
    (end-of-line)
    (push-mark)
    (beginning-of-line)
    (if (search-forward-regexp
         "\\(file +`\\|at +\\)\\([/a-zA-Z0-9._ ]+\\)'? +line +\\([0-9]+\\)[.,]"
         (region-end) t)
        (let* ((file (match-string 2))
               (row (match-string 3)))
          (list file row)
          )
      (list)
      )
    )
  )





;;;;;


(defun perly-sense-class-goto-at-point ()
  "Go to the class/method/bookmark at point"
  (interactive)
  (message "Goto at point")
  (let* ((class-name (perly-sense-find-class-name-at-point)))
         (if class-name
             (progn
               (message (format "Going to class (%s)" class-name))
               (perly-sense-find-source-for-module class-name)
               )
           (if (not (perly-sense-class-goto-method-at-point))
               (if (not (perly-sense-class-goto-bookmark-at-point))
                   (message "No Class/Method/Bookmark at point")
                 )
             )
           )
         )
  )



(defun perly-sense-class-goto-method-at-point ()
  "Go to the method declaration for the method at point and
return t, or return nil if no method could be found at point."
  (interactive)
  (let* ((method (perly-sense-class-method-at-point))
         (current-class (perly-sense-class-current-class)))
    (if (and current-class method)
        (progn
          (perly-sense-class-method-go-to current-class method)
          t
          )
      nil
      )
    )
  )



(defun perly-sense-class-docs-at-point ()
  "Display docs for the class/method at point"
  (interactive)
  (message "Docs at point")
  (let* ((class-name (perly-sense-find-class-name-at-point)))
         (if class-name
             (progn
               (message (format "Finding docs for class (%s)" class-name))
               (perly-sense-display-pod-for-module class-name)
               )
           (let* ((method (perly-sense-class-method-at-point))  ;;;'
                  (current-class (perly-sense-class-current-class)))
             (if (and current-class method)
                 (perly-sense-class-method-docs current-class method)
               (message "No Class or Method at point")
               )
             )
           )
         )

  )



(defun perly-sense-class-method-at-point ()
  "Return the method name at (or very near) point, or nil if none was found."
  (save-excursion
    (if (looking-at "[ \n(]") (backward-char)) ;; if at end of method name, move into it
    (if (looking-at "\\w")                ;; we may be on a method name
        (while (looking-at "\\w") (backward-char))   ;; position at beginning of word
      )
    (if (looking-at ">") (backward-char))
    (if (looking-at "[\\\\-]>\\(\\w+\\)")            ;; If on -> or \>, capture method name
        (match-string 1)
      nil
      )
    )
  )



(defun perly-sense-class-goto-bookmark-at-point ()
  "Go to the bookmark at point, if there is any.
Return t if there was any, else nil"
  (interactive)
  (message "Goto bookmark at point")
  (save-excursion
    (beginning-of-line)
    (if (search-forward-regexp "^\\([^:\n]+\\):\\([0-9]+\\):" (point-at-eol) t)
        (let ((file (match-string 1)) (row (string-to-number (match-string 2))))
          (message "file (%s) row (%s)" file row)
          (perly-sense-find-file-location file row 1)
          t
          )
      nil
      )
    )
  )




(defun perly-sense-class-class-overview-at-point ()
  "Display Class Overview for the class/method at point"
  (interactive)
  (message "Class Overview at point")
  (let* ((class-name (perly-sense-find-class-name-at-point)))
    (if class-name
        (progn
          (message (format "Class Overview for class (%s)" class-name))
          (perly-sense-class-overview-with-argstring
           (format "--class_name=%s --dir_origin=." class-name)))
      (message "No Class at point")
      )
    )
  )



(defun perly-sense-class-class-overview-or-goto-at-point ()
  "Display Class Overview for the class/method at point,
or go to the Bookmark at point"
  (interactive)
  (message "Class Overview at point")
  (let* ((class-name (perly-sense-find-class-name-at-point)))
    (if class-name
        (progn
          (message (format "Class Overview for class (%s)" class-name))
          (perly-sense-class-overview-with-argstring
           (format "--class_name=%s --dir_origin=." class-name)))
      (if (not (perly-sense-class-goto-method-at-point))
          (if (not (perly-sense-class-goto-bookmark-at-point))
              (message "No Class/Method/Bookmark at point")
            )
        )
      )
    )
  )



(defun perly-sense-class-current-class ()
  "Return the class currenlty being displayed in the Class Overview buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "\\[<\\(\\w+\\) *>\\]" nil t)
    (match-string 1)
    )
  )



(defun perly-sense-class-quit ()
  "Quit the Class Overview buffer"
  (interactive)
  (message "Quit")
  (kill-buffer nil)
  )



(defun perly-sense-class-find-inheritance ()
  "Navigate to the * Inheritance * in the Class Overview"
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (search-forward "* Inheritance *" nil t)
  (search-forward "[<" nil t)
  (backward-char)
  )



(defun perly-sense-class-find-neighbourhood ()
  "Navigate to the * NeighbourHood * in the Class Overview"
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (search-forward "* NeighbourHood *" nil t)
  (search-forward "[<" nil t)
  (backward-char)
  )



(defun perly-sense-class-find-used ()
  "Navigate to the * Uses * in the Class Overview"
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (search-forward "* Uses *" nil t)
  (beginning-of-line 2)
  (forward-char)
  )



(defun perly-sense-class-find-bookmarks ()
  "Navigate to the * Bookmarks * in the Class Overview"
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (search-forward "* Bookmarks *" nil t)
  (beginning-of-line 2)
  (if (looking-at "-")
      (beginning-of-line 2)
    )
  )



(defun perly-sense-class-find-structure ()
  "Navigate to the * Structure * in the Class Overview"
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (search-forward "* Structure *" nil t)
  (search-forward "-" nil t)
  (beginning-of-line 2)
  )



(defun perly-sense-class-find-api ()
  "Navigate to the * API * in the Class Overview"
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (search-forward "* API *" nil t)
  (beginning-of-line 2)
  )



(defun perly-sense-class-find-api-new ()
  "Navigate to the new method in the Class Overview"
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (search-forward-regexp ".>new\\b" nil t)
  (backward-char 3)
  )



(defun perly-sense-find-class-name-at-point ()
  "Return the class name at point, or nil if none was found"
  (save-excursion
    (if (looking-at "[\\[]")
        (forward-char) ;; So we can search backwards without fear of missing the current char
      )
    (if (search-backward-regexp "[][]" nil t)
        (if (looking-at "[\\[]")
            (progn
              (search-forward-regexp "\\w+" nil t)
              (match-string 0)
              )
          )
      )
    )
  )



(defvar perly-sense-class-mode-map nil
  "Keymap for `PerlySense Class overview major mode'.")
(if perly-sense-class-mode-map
    ()
  (setq perly-sense-class-mode-map (make-sparse-keymap)))
(define-key perly-sense-class-mode-map "q" 'perly-sense-class-quit)
(define-key perly-sense-class-mode-map "I" 'perly-sense-class-find-inheritance)
(define-key perly-sense-class-mode-map "H" 'perly-sense-class-find-neighbourhood)
(define-key perly-sense-class-mode-map "U" 'perly-sense-class-find-used)
(define-key perly-sense-class-mode-map "B" 'perly-sense-class-find-bookmarks)
(define-key perly-sense-class-mode-map "S" 'perly-sense-class-find-structure)
(define-key perly-sense-class-mode-map "A" 'perly-sense-class-find-api)
(define-key perly-sense-class-mode-map "N" 'perly-sense-class-find-api-new)

(define-key perly-sense-class-mode-map "N" 'perly-sense-class-find-api-new)
(define-key perly-sense-class-mode-map (format "%sgn" perly-sense-key-prefix) 'perly-sense-class-find-api-new)

(define-key perly-sense-class-mode-map [return] 'perly-sense-class-class-overview-or-goto-at-point)

(define-key perly-sense-class-mode-map "d" 'perly-sense-class-docs-at-point)
(define-key perly-sense-class-mode-map (format "%s\C-d" perly-sense-key-prefix) 'perly-sense-class-docs-at-point)

(define-key perly-sense-class-mode-map "g" 'perly-sense-class-goto-at-point)
(define-key perly-sense-class-mode-map (format "%s\C-g" perly-sense-key-prefix) 'perly-sense-class-goto-at-point)

(define-key perly-sense-class-mode-map "o" 'perly-sense-class-class-overview-at-point)
(define-key perly-sense-class-mode-map (format "%s\C-o" perly-sense-key-prefix) 'perly-sense-class-class-overview-at-point)






(defvar perly-sense-class-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Treat _ and :: as part of a word
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?: "w" st)
    st)
  "Syntax table for `perly-sense-class-mode'.")


;; (Defvar perly-sense-class-imenu-generic-expression
;;   ...)

;; (defvar perly-sense-class-outline-regexp
;;   ...)

 ;;;###autoload
(define-derived-mode perly-sense-class-mode fundamental-mode "PerlySense Class Overview"
  "A major mode for viewing PerlySense Class overview buffers."
  :syntax-table perly-sense-class-mode-syntax-table
;;   (set (make-local-variable 'comment-start) "# ")
;;   (set (make-local-variable 'comment-start-skip) "#+\\s-*")

;;   (set (make-local-variable 'font-lock-defaults)
;;        '(perly-sense-class-font-lock-keywords))

;;   (set (make-local-variable 'indent-line-function) 'perly-sense-class-indent-line)
;;   (set (make-local-variable 'imenu-generic-expression)
;;        perly-sense-class-imenu-generic-expression)
;;   (set (make-local-variable 'outline-regexp) perly-sense-class-outline-regexp)
  )

;;; Indentation

;; (defun perly-sense-class-indent-line ()
;;   "Indent current line of Perly-Sense-Class code."
;;   (interactive)
;;   (let ((savep (> (current-column) (current-indentation)))
;;         (indent (condition-case nil (max (perly-sense-class-calculate-indentation) 0)
;;                   (error 0))))
;;     (if savep
;;         (save-excursion (indent-line-to indent))
;;       (indent-line-to indent))))

;; (defun perly-sense-class-calculate-indentation ()
;;   "Return the column to which the current line should be indented."
;;   ...)



;; Key bindings
;;;; TODO: move some of these to cperl-mode local bindings

(global-set-key (format "%smf" perly-sense-key-prefix) 'perly-sense-find-source-for-module-at-point)
(global-set-key (format "%smp" perly-sense-key-prefix) 'perly-sense-display-pod-for-module-at-point)

(global-set-key (format "%s\C-d" perly-sense-key-prefix) 'perly-sense-smart-docs-at-point)
(global-set-key (format "%sdi" perly-sense-key-prefix) 'perly-sense-inheritance-docs-at-point)
(global-set-key (format "%s\C-g" perly-sense-key-prefix) 'perly-sense-smart-go-to-at-point)
(global-set-key (format "%sgb" perly-sense-key-prefix) 'perly-sense-go-to-base-class-at-point)
(global-set-key (format "%sgn" perly-sense-key-prefix) 'perly-sense-go-to-method-new)
(global-set-key (format "%sgm" perly-sense-key-prefix) 'perly-sense-find-source-for-module-at-point)
(global-set-key (format "%sgv" perly-sense-key-prefix) 'perly-sense-go-to-vc-project)
(global-set-key (format "%s\C-o" perly-sense-key-prefix) 'perly-sense-class-overview-for-class-at-point)
;; (global-set-key (format "%s\C-c" perly-sense-key-prefix) 'perly-sense-display-api-for-class-at-point)

(global-set-key (format "%s\C-r" perly-sense-key-prefix) 'perly-sense-run-file)
(global-set-key (format "%srr" perly-sense-key-prefix) 'perly-sense-rerun-file)

(global-set-key (format "%sge" perly-sense-key-prefix) 'perly-sense-compile-goto-error-file-line)

(global-set-key (format "%sar" perly-sense-key-prefix) 'perly-sense-regex-tool)





(provide 'perly-sense)





;; EOF
