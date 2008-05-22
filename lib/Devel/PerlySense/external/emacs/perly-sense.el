
;;
;; INSTALLATION
;;
;; See the instructions at:
;; http://search.cpan.org/dist/Devel-PerlySense/lib/Devel/PerlySense.pm#Emacs_installation
;;




;;;; Utilities
;(message "%s" (prin1-to-string thing))


(defun alist-value (alist key)
  "Return the value of KEY in ALIST" ;; Surely there must be an existing defun to do this that I haven't found...
  (cdr (assoc key alist)))



(defun alist-num-value (alist key)
  "Return the numeric value of KEY in ALIST"
  (string-to-number (alist-value alist key)))



(defun ps/switch-to-buffer (buffer)
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
(require 'dropdown-list)



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
  :prefix "ps/"
  :group 'languages
  :version "1.0")



(defcustom ps/dropdown-max-items-to-display '30
  "The maximum number of items to display in a dropdown menu. Any
more items than that, use completing read instead."
  :type 'integer
  :group 'perly-sense)



(defgroup perly-sense-faces nil
  "Colors."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "ps/"
  :group 'perly-sense)


(defcustom ps/here-face 'font-lock-string-face
  "*Face for here-docs highlighting."
  :type 'face
  :group 'perly-sense-faces)



(defface ps/heading
  `((t (:inherit 'custom-face-tag)))
;  `((t (:inherit 'bold)))  ;
  "Face for headings."
  :group 'perly-sense-faces)
(defvar ps/heading-face 'ps/heading
  "Face for headings.")





(defface ps/module-name
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
(defvar ps/module-name-face 'ps/module-name
  "Face for module names.")

(defface ps/highlighted-module-name
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
(defvar ps/highlighted-module-name-face 'ps/highlighted-module-name
  "Face for highlighted module names.")

(defvar ps/bookmark-file-face compilation-info-face
  "Face for Bookmark file names.")

(defvar ps/bookmark-line-number-face 'compilation-line-number
  "Face for Bookmark line numbers.")

(defface ps/current-class-method
  `((t (:inherit 'font-lock-function-name-face)))
  "Face for methods in the current class."
  :group 'perly-sense-faces)
(defvar ps/current-class-method-face 'ps/current-class-method
  "Face for methods in the current class.")

(defface ps/current-new-method
  `((t (:inherit 'font-lock-function-name-face :weight bold)))
  "Face for new in the current class."
  :group 'perly-sense-faces)
(defvar ps/current-new-method-face 'ps/current-new-method
  "Face for new in the current class.")

(defface ps/base-class-method
  `((t (:inherit 'font-lock-keyword-face)))
  "Face for methods in the base class."
  :group 'perly-sense-faces)
(defvar ps/base-class-method-face 'ps/base-class-method
  "Face for methods in the base class.")

(defface ps/base-new-method
  `((t (:inherit 'font-lock-keyword-face :weight bold)))
  "Face for new in the base class."
  :group 'perly-sense-faces)
(defvar ps/base-new-method-face 'ps/base-new-method
  "Face for new in the base class.")

(defface ps/cpan-base-class-method
  `((t (:inherit 'font-lock-keyword-face)))
  "Face for methods in base classes outside the Project."
  :group 'perly-sense-faces)
(defvar ps/cpan-base-class-method-face 'ps/cpan-base-class-method
  "Face for methods in base classes outside the Project.")

(defface ps/cpan-base-new-method
  `((t (:inherit 'font-lock-keyword-face :weight bold)))
  "Face for new in base classes outside the Project."
  :group 'perly-sense-faces)
(defvar ps/cpan-base-new-method-face 'ps/cpan-base-new-method
  "Face for new in base classes outside the Project.")





;;;; Defuns



;;;; Defuns



(defun ps/log (msg)
  "log msg in a message and return msg"
;;  (message "LOG(%s)" msg)
  )


(defun ps/current-line ()
  "Return the vertical position of point"
  (+ (count-lines 1 (point))
     (if (= (current-column) 0) 1 0)
     )
  )





;;;;

(defun ps/find-source-for-module (module)
  (let ((file (shell-command-to-string (format "perly_sense find_module_source_file --module=%s" module))))
    (if (not (string-equal file ""))
        (find-file file)
      (message "Module (%s) source file not found" module)
      nil
      )
    )
  )


(defun ps/find-source-for-module-at-point ()
  "Find the source file for the module at point."
  (interactive)
  (let ((module (cperl-word-at-point)))
    (if module
        (progn
          (message "Going to module %s..." module)
          (ps/find-source-for-module module)
          )
      )
    )
  )



; should use something that fontifies
(defun ps/display-pod-for-module (module)
  (let* ((result-alist
          (ps/command
           "display_module_pod"
           (format "--module=%s" module)))
         (message-string (alist-value result-alist "message"))
         (pod            (alist-value result-alist "pod"))
         )
    (if (not (string= pod ""))
        (ps/display-text-in-buffer "POD" module pod))
    (message "Nothing found")
    (when message-string
      (message "%s" message-string))
    )
  )

  

(defun ps/display-pod-for-module-at-point ()
  "Display POD for the module at point."
  (interactive)
  (let ((module (cperl-word-at-point)))
    (if module
        (ps/display-pod-for-module module)
      )
    )
  )



(defun ps/display-text-in-buffer (type name text)
  (let ((buffer-name (format "*%s %s*" type name)))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert text)
      (goto-char 1)
      (ps/fontify-pod-buffer buffer-name)
      (display-buffer (current-buffer))
      )
    )
  )


(defun ps/display-doc-message-or-buffer (doc-type name text)
  (cond ((string= doc-type "hint")
         (message "%s" text))
        ((string= doc-type "document")
         (ps/display-text-in-buffer "POD" name text)
         (message nil)
         )
        )
  t
  )





(defun ps/fontify-pod-buffer (buffer-name)
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




(defun ps/run-file ()
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
              (format "perly_sense run_file \"--file=%s\"" (buffer-file-name)))
             ))
        (let* (
               (result-hash (ps/parse-sexp result))
               (dir-run-from (alist-value result-hash "dir_run_from"))
               (command-run (alist-value result-hash "command_run"))
               (type-source-file (alist-value result-hash "type_source_file"))
               (message-string (alist-value result-hash "message"))
               )
          (if command-run
              (ps/run-file-run-command
               ;;             (ps/run-file-get-command command-run type-source-file)
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

(defun ps/run-file-run-command (command dir-run-from)
  "Run command from dir-run-from using the compiler function"
  (with-temp-buffer
    (cd dir-run-from)
    (compile command)
    )
  )



(defun ps/rerun-file ()
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





(defun ps/smart-docs-at-point ()
  "Display documentation for the code at point."
  (interactive)
  (message "Smart docs...")
  (let* ((result-alist (ps/command-on-current-file-location "smart_doc"))
         (message-string (alist-value result-alist "message"))
         (found          (alist-value result-alist "found"))
         (name           (alist-value result-alist "name"))
         (doc-type       (alist-value result-alist "doc_type"))
         (text           (alist-value result-alist "text"))
         )
    (if (not (string= text ""))
        (ps/display-doc-message-or-buffer doc-type name text)
      (message "Nothing found")
      )
    (when message-string
      (message "%s" message-string))
    )
  )




;; todo: remove duplication between this defun and the one above
(defun ps/class-method-docs (class-name method)
  "Display documentation for the 'method' of 'class-name'."
  (interactive)
  (message "Finding docs for method (%s)..." method)
  (let* ((result-alist
          (ps/command
           "method_doc"
           (format "--class_name=%s --method_name=%s --dir_origin=." class-name method)))
         (message-string (alist-value result-alist "message"))
         (found          (alist-value result-alist "found"))
         (name           (alist-value result-alist "name"))
         (doc-type       (alist-value result-alist "doc_type"))
         (text           (alist-value result-alist "text"))
         )
    (if (not (string= text ""))
        (ps/display-doc-message-or-buffer doc-type name text)
      (message "Nothing found")
      )
    (when message-string
      (message "%s" message-string))
    )
  )



(defun ps/inheritance-docs-at-point ()
  "Display the Inheritance structure for the current Class"
  (interactive)
  (message "Document Inheritance...")
  (let* ((result-alist (ps/command-on-current-file-location "inheritance_doc"))
         (message-string (alist-value result-alist "message"))
         (class-inheritance (alist-value result-alist "class_inheritance"))
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



(defun ps/use-docs-at-point ()
  "Display the used modules for the current Class"
  (interactive)
  (message "Document Uses...")
  (let* ((result-alist (ps/command-on-current-file-location "use_doc"))
         (message-string (alist-value result-alist "message"))
         (class-use (alist-value result-alist "class_use"))
         )
    (if (not class-use)
        (message "No use statements found")
      (message "%s" class-use)
      )
    (if message-string
        (message message-string)
      )
    )
  )



(defun ps/find-file-location (file row col)
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




(defun ps/smart-go-to-at-point ()
  "Go to the original symbol in the code at point."
  (interactive)
  (message "Smart goto...")
  (let* ((result-alist (ps/command-on-current-file-location "smart_go_to"))
         (message-string (alist-value result-alist "message"))
         (file (alist-value result-alist "file"))
         (row (alist-value result-alist "row"))
         (col (alist-value result-alist "col"))
         )
    (if file
        (progn
          (ps/find-file-location file (string-to-number row) (string-to-number col))
          (message "Went to: %s:%s" file row))
      (message "Nothing found")
      )
    (when message-string
      (message "%s" message-string))
    )
  )




(defun ps/go-to-base-class-at-point ()
  "Go to the Base Class of the Class at point. If ambigous, let
the the user choose a Class."
  (interactive)
  (message "Goto Base Class...")
  (let* ((result-alist (ps/command-on-current-file-location "base_class_go_to"))
         (message-string (alist-value result-alist "message"))
         (class-list (alist-value result-alist "class_list"))
         (first-class-alist (car class-list))
         (second-class-alist (cdr class-list))
         )
    (if (not first-class-alist)
        (message "No Base Class found")
      (if (not second-class-alist)
          (ps/go-to-class-alist first-class-alist)
        (let ((chosen-class-alist
               (ps/choose-class-alist-from-class-list "Base Class" class-list)))
          (if chosen-class-alist
              (ps/go-to-class-alist chosen-class-alist)
            )
          )
        )
      )
    (if message-string
        (message message-string)
      )
    )
  )



(defun ps/go-to-use-section ()
  "Set mark and go to the end of the 'use Module' section."
  (interactive)
  (message "Goto the 'use Module' section...")
  (let* ((use-position (ps/find-use-module-section-position)))
    (if (not use-position)
        (message "No 'use Module' section found")
      (push-mark)
      (goto-char use-position)
      (next-line-nomark)
      (beginning-of-line)
      )
    )
  )



(defun ps/find-use-module-section-position ()
  "Return the position of the end of the last use Module
statement in the file, or nil if none was found."
  (save-excursion
    (goto-char (point-max))
    (if (search-backward-regexp "^ *use +[a-zA-Z][^;]+;" nil t)
        (progn
          (search-forward-regexp ";")
          (point))
      nil
      )
    )
  )



(defun ps/goto-test-other-files ()
  "Go to other test files. When in a Perl module, let user choose
amongst test files to go to. When in a test file, let user choose
amongst source files to go to.

You must have Devel::CoverX::Covered installed and have a
cover_db for your project in the project dir."
  (interactive)

  (let* ((result-alist (ps/command-on-current-file-location "test_other_files"))
         (message (alist-value result-alist "message")))
    (if message
        (message "%s" message)
      (let* ((other-files-list (alist-value result-alist "other_files"))
             (project-dir (alist-value result-alist "project-dir"))
             (chosen-file (ps/choose-from-strings-alist "File: " other-files-list)))
        (if chosen-file
            (find-file (expand-file-name chosen-file project-dir)))
        )))
  )



(defun ps/choose-from-strings-alist (prompt items-alist)
  "Let user choose amongst the strings in items-alist.

If appropriate (given the number of items in items-alist), use a
dropdown-list, otherwise a completing read with 'prompt'.

Return the chosen string, or nil if the user canceled.
"
  (if (< (length items-alist) ps/dropdown-max-items-to-display)
      (let* ((n (dropdown-list items-alist)))
        (if n
            (nth n items-alist)
          nil
          ))
    (completing-read
     (format "%s: " prompt)
     items-alist
     nil
     "force"
     nil
     nil
     (car items-alist)
     )
    )
  )



(defun ps/go-to-vc-project ()
  "Go to the project view of the current Version Control, or the
project dir if there is no vc."
  (interactive)
  (message "Goto Version Control...")
  (let ((vc-buffer (get-buffer "*svn-status*")))  ;; (or *cvs-status*, etc)
    (if vc-buffer
        (ps/switch-to-buffer vc-buffer)
      (let* ((result-alist (ps/command "vcs_dir"))
             (project-dir (alist-value result-alist "project_dir"))
             (vcs-name (alist-value result-alist "vcs_name"))
             )
        (if (not (string= project-dir ""))
            (ps/vc-project vcs-name project-dir)
          (message "No Project dir found")
          )
        )
      )
    )
  )



(defun ps/vc-project (vcs project-dir)
  "Display the Project view for the VCS (e.g. 'svn', 'none') for
the PROJECT-DIR, e.g. run svn-status for PROJECT-DIR."
  (cond
   ((string= vcs "svn")
    (message "SVN status...")
    (svn-status project-dir))
   (t
    (message "No VCS...")
    (dired project-dir))
   )
  )



(defun ps/edit-move-use-statement ()
  "If point is on a line with a single 'use Module' statement,
set mark and move that statement to the end of the 'use
Module' section at the top of the file."
  (interactive)
  (let ((message
         (catch 'message
           (save-excursion
             (end-of-line)
             (if (not (search-backward-regexp "^ *use +[a-zA-Z][^\n]*?; *?$" (point-at-bol) t))
                 (throw 'message "No 'use Module' statement on this line.")
               (kill-region (match-beginning 0) (match-end 0))
               (delete-char 1)
               (push-mark)
               )
             )
           (let* ((use-position (ps/find-use-module-section-position)))
             (if (not use-position)
                 (throw 'message "No 'use Module' section found, nowhere to put the killed use statement.")
               (goto-char use-position)
               (newline-and-indent)
               (yank) (pop-mark)
               (beginning-of-line)
               (lisp-indent-line)
               )
             )
           "Set mark and moved use statement. Hit C-u C-m to return."
           )
         ))
    (if message (message "%s" message))
    )
  )



;; Thanks to Jonathan Rockway at
;; http://blog.jrock.us/articles/Increment%20test%20counter.pod
(defun ps/edit-test-count (&optional amount)
  "Increase the Test::More test count by AMOUNT"
  (interactive "p")
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "tests\s+=>\s*[0-9]+" nil t)
        (progn
          (backward-char)
          (let ((inc-response (ps/increment-number-at-point amount)))
            (message "Test count: %s + %s = %s" (nth 0 inc-response) (nth 1 inc-response) (nth 2 inc-response))
            )
          )
      (message "Could not find a test count"))))



(defun ps/set-test-count (current-count new-count)
  "Set the Test::More test count from CURRENT-COUNT to NEW-COUNT."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "tests\s+=>\s*[0-9]+" nil t)
        (let ((amount (- new-count current-count)))
          (backward-char)
          (let ((inc-response (ps/increment-number-at-point amount)))
            (message "Test count: %s + %s = %s" (nth 0 inc-response) (nth 1 inc-response) (nth 2 inc-response))
            )
          )
      (message "Could not find a test count"))))



;; Thanks to Phil Jackson at
;; http://www.shellarchive.co.uk/Shell.html#sec21
(defun ps/increment-number-at-point (&optional amount)
  "Increment the number under point by AMOUNT.

Return a list with the items (original number, amount, new
number), or nil if there was no number at point."
  (interactive "p")
  (let ((num (number-at-point)))
    (if (numberp num)
      (let ((newnum (+ num amount))
            (p (point)))
        (save-excursion
          (skip-chars-backward "-.0123456789")
          (delete-region (point) (+ (point) (length (number-to-string num))))
          (insert (number-to-string newnum)))
        (goto-char p)
        (list num amount newnum)
        )
      nil)))



(defun ps/assist-sync-test-count ()
  "Synchronize Test::More test count with the one reported by the
current test run, if any"
  (interactive)
  (let
      ((message
        (catch 'message
          (save-excursion
            (let ((expected-count (ps/expected-test-count))
                  (current-count (ps/current-test-count)))
              (if (eq expected-count nil)
                  (throw 'message "No *compilation* buffer with a test run found."))
              (if (eq current-count nil)
                  (throw 'message "No test count found in the current buffer"))
              (if (= expected-count current-count)
                  (throw 'message
                         (format
                          "Current test count is the same as the expected count (%s)"
                          expected-count))
                (ps/set-test-count current-count expected-count)
                nil))))))
    (if message (message "%s" message))))



(defun ps/expected-test-count ()
  "Return the expected number of tests, or nil if that couldn't be deduced."
  (if (not (get-buffer "*compilation*"))
      nil
    (catch 'count-string
      (save-excursion
        (set-buffer "*compilation*")
        (goto-char (point-min))
        (if (re-search-forward "Files=[0-9]+, Tests=\\([0-9]+\\)" nil t)
            (throw 'count-string (string-to-number (match-string 1))))
        (if (re-search-forward "Looks like you planned \\([0-9]+\\) tests but ran \\([0-9]+\\) extra" nil t)
            (let* ((planned-count (string-to-number (match-string 1)))
                   (extra-count (string-to-number (match-string 2)))
                   (actual-count (+ planned-count extra-count))
                   )
              (throw 'count-string actual-count)
              )
          )
        (if (re-search-forward "planned [0-9]+ tests but \\(only \\)?ran \\([0-9]+\\)" nil t)
            (throw 'count-string (string-to-number (match-string 2))))
        (if (re-search-forward "Failed [0-9]+/\\([0-9]+\\) tests" nil t)
            (throw 'count-string (string-to-number (match-string 1))))
        (throw 'count-string nil)))))



(defun ps/current-test-count ()
  "Return the test count of the current buffer, or nil if that couldn't be deduced."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "tests\s+=>\s*[0-9]+" nil t)
        (let ((num (number-at-point)))
          (if (numberp num)
              num
            nil)))))



(defun ps/command-on-current-file-location (command &optional options)
  "Call perly_sense COMMAND with the current file and row/col,
and return the parsed result as a sexp"
  (unless options (setq options ""))
  (ps/parse-sexp
   (ps/shell-command-to-string
    (format "perly_sense %s \"--file=%s\" --row=%s --col=%s %s --width_display=%s"
            command
            (buffer-file-name)
            (ps/current-line)
            (+ 1 (current-column))
            options
            (- (window-width) 2)))))



(defun ps/command (command &optional options)
  "Call 'perly_sense COMMAND OPTIONS' and some additional default
options, and return the parsed result as a sexp"
  (unless options (setq options ""))
  (ps/parse-sexp
   (ps/shell-command-to-string
    (format "perly_sense %s %s --width_display=%s"
            command
            options
            (- (window-width) 2)))))



(defun ps/shell-command-to-string (command)
  "Run command and return the response"
  (let ((response (shell-command-to-string command)))
;;    (message "Called (%s), got (%s)" command response)
    response
    )
  )



(defun ps/go-to-method-new ()
  "Go to the 'new' method."
  (interactive)
  (message "Goto the 'new' method...")
  (let ((new-location-alist
         (or
          (ps/find-method-in-buffer "new")
          (ps/find-method-in-file "new"))))
    (if new-location-alist
        (ps/go-to-location-alist new-location-alist)
      (message "Could not find any 'new' method")
      )
    )
  )



(defun ps/find-method-in-buffer (method-name)
  "Find a method named METHOD-NAME in the buffer and return an
alist with (keys: row, col), or nil if no method was found."
  (save-excursion
    (beginning-of-buffer)
    (if (and
         (search-forward-regexp (format "\\(^\\| \\)sub +%s\\($\\| \\)" method-name) nil t)
         (search-backward-regexp "sub")
         )
        `(
          ("row" . ,(number-to-string (ps/current-line)))
          ("col" . ,(number-to-string (+ 1 (current-column))))
          )
      nil
      )
    )
  )



(defun ps/find-method-in-file (method-name)
  "Find a method named METHOD-NAME given the current class in the
buffer and return an alist with (keys: file, row, col), or nil if
no method was found."
  (let* ((result-alist (ps/command-on-current-file-location "method_go_to" "--method_name=new"))
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




(defun ps/go-to-location-alist (location-alist)
  "Go to the LOCATION-ALIST which may contain the (keys: file,
row, col, class-name).

If file is specified, visit that file first.

If class-name is specified, display that class name in the echo
area."
  (let ((file (alist-value location-alist "file"))
        (row (alist-num-value location-alist "row"))
        (col (alist-num-value location-alist "col"))
        (class-name (alist-value location-alist "class_name"))
        )
    (ps/find-file-location file row col)
    (if class-name
        (message "Went to %s" class-name)
      )
    )
  )



(defun ps/go-to-class-alist (class-alist)
  "Go to the Class class-alist (keys: class-name, file, row)"
  (let ((class-name (alist-value class-alist "class_name"))
        (class-inheritance (alist-value class-alist "class_inheritance"))
        (file (alist-value class-alist "file"))
        (row (alist-num-value class-alist "row")))
    (ps/find-file-location file row 1)
    (message "%s" class-inheritance)
    )
  )



(defun ps/choose-class-alist-from-class-list (what-text class-list)
  "Let the user choose a class-alist from the lass-list of Class
definitions.

Return class-alist with (keys: class-name, file, row), or nil if
none was chosen."
  (ps/choose-class-alist-from-class-list-with-dropdown what-text class-list)
  )



(defun ps/choose-class-alist-from-class-list-with-dropdown (what-text class-list)
  "Let the user choose a class-alist from the lass-list of Class
definitions using a dropdown list.

Return class-alist with (keys: class-name, file, row), or nil if
none was chosen."
  (let* ((class-description-list (mapcar (lambda (class-alist)
                                    (alist-value class-alist "class_description")
                                    ) class-list))
         (n (dropdown-list class-description-list))
         )
    (if n
        (let ((chosen-class-description (nth n class-description-list)))
          (ps/get-alist-from-list
           class-list "class-description" chosen-class-description)
          )
      nil
      )
    )
  )



(defun ps/choose-class-alist-from-class-list-with-completing-read (what-text class-list)
  "Let the user choose a class-alist from the lass-list of Class
definitions using completing read.

Return class-alist with (keys: class-name, file, row)"
  (let* ((class-description-list (mapcar (lambda (class-alist)
                                    (alist-value class-alist "class_description")
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
    (ps/get-alist-from-list class-list "class-description" chosen-class-description)
    )
  )



(defun ps/get-alist-from-list (list-of-alist key value)
  "Return the first alist in list which aliast's key is value, or
nil if none was found"
  (catch 'found
    (dolist (alist list-of-alist)
      (let ((alist-item-value (alist-value alist key)))
        (if (string= alist-item-value value)
            (throw 'found alist)
          nil)))))



;; todo: remove duplication between this defun and the one above
(defun ps/class-method-go-to (class-name method)
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
            (ps/find-file-location file (string-to-number (pop value)) (string-to-number (pop value)))
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



(defun ps/class-overview-for-class-at-point ()
  "Display the Class Overview for the current class"
  (interactive)
  (ps/class-overview-with-argstring
   (format
    "perly_sense class_overview --file=%s --row=%s --col=%s"
    (buffer-file-name)
    (ps/current-line)
    (+ 1 (current-column)))))


(defun ps/class-overview-with-argstring (argstring)
  "Call perly_sense class_overview with argstring and display Class Overview with the response"
  (interactive)
  (message "Class Overview...")
  (let ((result (shell-command-to-string
                 (format
                  "perly_sense class_overview %s --width_display=%s"
                  argstring (- (window-width) 2) ))))
    (let* ((result-hash (ps/parse-sexp result))
           (class-name (alist-value result-hash "class_name"))
           (class-overview (alist-value result-hash "class_overview"))
           (message-string (alist-value result-hash "message"))
           (dir (alist-value result-hash "dir"))
           )
;;      (ps/log msg)
      (if class-name
          (ps/display-class-overview class-name class-overview dir)
        )
      (if message-string
          (message message-string)
        )
      )
    )
  )




(defun ps/parse-sexp (result)
;;  (message "RESPONSE AS TEXT |%s|" result)
  (if (string= result "")
      '()
    (let ((response-alist (eval (car (read-from-string result)))))
      response-alist
      )
    )
  )


;;;(ps/parse-result-into-alist "'((\"class-overview\" . \"Hej baberiba [ Class::Accessor ]\") (\"class-name\" . \"Class::Accessor\") (\"message\" . \"Whatever\"))")
;;(ps/parse-result-into-alist "'((\"class-name\" . \"alpha\"))")





(defun ps/regex-tool ()
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



(defun ps/display-class-overview (class-name overview-text dir)
  (let ((buffer-name "*Class Overview*"))
    (with-current-buffer (get-buffer-create buffer-name)
(message "dir (%s)" dir)
      (setq default-directory dir)
      (toggle-read-only t)(toggle-read-only)  ; No better way?
      (erase-buffer)
      (insert overview-text)
      (ps/class-mode)
      (ps/fontify-class-overview-buffer buffer-name)
      (ps/search-current-class-name class-name)
      (switch-to-buffer (current-buffer))  ;; before: display-buffer
      (toggle-read-only t)
      )
    )
  )



;; Set point where class-name is mentioned in brackets
(defun ps/search-class-name (class-name)
  (let ((class-name-box (format "[ %s " class-name)))
    (goto-char (point-min))
    (search-forward class-name-box)
    (search-backward "[ ")
    (forward-char)
    )
  )



;; Set point where class-name is mentioned in current [< xxx >]
(defun ps/search-current-class-name (class-name)
  (let ((class-name-box (format "[<%s" class-name)))
    (goto-char (point-min))
    (search-forward class-name-box nil t)
    (search-backward "[<" nil t)
    (forward-char)
    )
  )



(defun ps/fontify-class-overview-buffer (buffer-name)
  "Mark up a buffer with Class Overview text."
  (interactive)
  (save-excursion
    (set-buffer buffer-name)

    (goto-char (point-min))
    (while (search-forward-regexp "\\[ \\w+ +\\]" nil t)
      (put-text-property (match-beginning 0) (match-end 0) 'face ps/module-name-face))

    (goto-char (point-min))
    (while (search-forward-regexp "\\[<\\w+ *>\\]" nil t)
      (put-text-property (match-beginning 0) (match-end 0) 'face ps/highlighted-module-name-face))

    (goto-char (point-min))
    (while (search-forward-regexp "^[^:\n]+:[0-9]+:" nil t)
      (let
          ((file-beginning (match-beginning 0))
           (row-end (- (match-end 0) 1)))
        (search-backward-regexp ":[0-9]+:" nil t)
        (let
            ((file-end (match-beginning 0))
             (row-beginning (+ (match-beginning 0) 1)))
          (put-text-property file-beginning file-end 'face ps/bookmark-file-face)
          (put-text-property row-beginning row-end 'face ps/bookmark-line-number-face)
          )))

    (goto-char (point-min))
    (while (search-forward-regexp "->\\w+" nil t)  ;; ->method
      (put-text-property (match-beginning 0) (match-end 0) 'face ps/current-class-method-face))

    (goto-char (point-min))
    (while (search-forward-regexp "\\\\>\\w+" nil t)  ;; \>method
      (put-text-property (match-beginning 0) (match-end 0) 'face 'font-lock-keyword-face))

    (goto-char (point-min))
    (while (search-forward-regexp "->new\\b" nil t)  ;; ->new
      (put-text-property (match-beginning 0) (match-end 0) 'face ps/current-new-method-face))

    (goto-char (point-min))
    (while (search-forward-regexp "\\\\>new\\b" nil t)  ;; \>new
      (put-text-property (match-beginning 0) (match-end 0) 'face ps/base-new-method-face))



    (goto-char (point-min))
    (while (search-forward-regexp "\\* \\w+ +\\*" nil t)
      (let
          ((heading-beginning (match-beginning 0) )
           (heading-end (match-end 0) ))
        (put-text-property heading-beginning heading-end 'face ps/heading-face)
        (add-text-properties heading-beginning (+ heading-beginning 2) '(invisible t))
        (add-text-properties (- heading-end 2) heading-end '(invisible t))
      ))
    )
  )




(defun ps/compile-goto-error-file-line ()
  "Go to the file + line specified on the row at point, or ask for a
text to parse for a file + line."
  (interactive)
  (let* ((file_row (ps/compile-get-file-line-from-buffer) )
         (file (nth 0 file_row))
         (row (nth 1 file_row)))
    (if file
        (ps/find-file-location file (string-to-number row) 1)
      (let* ((file_row (ps/compile-get-file-line-from-user-input) )
             (file (nth 0 file_row))
             (row (nth 1 file_row)))
        (if file
            (ps/find-file-location file (string-to-number row) 1)
          (message "No 'FILE line N' found")
          )
        )
      )
    )
  )


(defun ps/compile-get-file-line-from-user-input ()
  "Ask for a text to parse for a file + line, parse it using
'ps/compile-get-file-line-from-buffer'. Return what it
returns."
  (with-temp-buffer
    (insert (read-string "FILE, line N text: " (current-kill 0 t)))
    (ps/compile-get-file-line-from-buffer)
    )
  )


(defun ps/compile-get-file-line-from-buffer ()
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


(defun ps/class-goto-at-point ()
  "Go to the class/method/bookmark at point"
  (interactive)
  (message "Goto at point")
  (let* ((class-name (ps/find-class-name-at-point)))
         (if class-name
             (progn
               (message (format "Going to class (%s)" class-name))
               (ps/find-source-for-module class-name)
               )
           (if (not (ps/class-goto-method-at-point))
               (if (not (ps/class-goto-bookmark-at-point))
                   (message "No Class/Method/Bookmark at point")
                 )
             )
           )
         )
  )



(defun ps/class-goto-method-at-point ()
  "Go to the method declaration for the method at point and
return t, or return nil if no method could be found at point."
  (interactive)
  (let* ((method (ps/class-method-at-point))
         (current-class (ps/class-current-class)))
    (if (and current-class method)
        (progn
          (ps/class-method-go-to current-class method)
          t
          )
      nil
      )
    )
  )



(defun ps/class-docs-at-point ()
  "Display docs for the class/method at point"
  (interactive)
  (message "Docs at point")
  (let* ((class-name (ps/find-class-name-at-point)))
         (if class-name
             (progn
               (message (format "Finding docs for class (%s)" class-name))
               (ps/display-pod-for-module class-name)
               )
           (let* ((method (ps/class-method-at-point))  ;;;'
                  (current-class (ps/class-current-class)))
             (if (and current-class method)
                 (ps/class-method-docs current-class method)
               (message "No Class or Method at point")
               )
             )
           )
         )

  )



(defun ps/class-method-at-point ()
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



(defun ps/class-goto-bookmark-at-point ()
  "Go to the bookmark at point, if there is any.
Return t if there was any, else nil"
  (interactive)
  (message "Goto bookmark at point")
  (save-excursion
    (beginning-of-line)
    (if (search-forward-regexp "^\\([^:\n]+\\):\\([0-9]+\\):" (point-at-eol) t)
        (let ((file (match-string 1)) (row (string-to-number (match-string 2))))
          (message "file (%s) row (%s)" file row)
          (ps/find-file-location file row 1)
          t
          )
      nil
      )
    )
  )




(defun ps/class-class-overview-at-point ()
  "Display Class Overview for the class/method at point"
  (interactive)
  (message "Class Overview at point")
  (let* ((class-name (ps/find-class-name-at-point)))
    (if class-name
        (progn
          (message (format "Class Overview for class (%s)" class-name))
          (ps/class-overview-with-argstring
           (format "--class_name=%s --dir_origin=." class-name)))
      (message "No Class at point")
      )
    )
  )



(defun ps/class-class-overview-or-goto-at-point ()
  "Display Class Overview for the class/method at point,
or go to the Bookmark at point"
  (interactive)
  (message "Class Overview at point")
  (let* ((class-name (ps/find-class-name-at-point)))
    (if class-name
        (progn
          (message (format "Class Overview for class (%s)" class-name))
          (ps/class-overview-with-argstring
           (format "--class_name=%s --dir_origin=." class-name)))
      (if (not (ps/class-goto-method-at-point))
          (if (not (ps/class-goto-bookmark-at-point))
              (message "No Class/Method/Bookmark at point")
            )
        )
      )
    )
  )



(defun ps/class-current-class ()
  "Return the class currenlty being displayed in the Class Overview buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "\\[<\\(\\w+\\) *>\\]" nil t)
    (match-string 1)
    )
  )



(defun ps/class-quit ()
  "Quit the Class Overview buffer"
  (interactive)
  (message "Quit")
  (kill-buffer nil)
  )



(defun ps/class-find-inheritance ()
  "Navigate to the * Inheritance * in the Class Overview"
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (search-forward "* Inheritance *" nil t)
  (search-forward "[<" nil t)
  (backward-char)
  )



(defun ps/class-find-neighbourhood ()
  "Navigate to the * NeighbourHood * in the Class Overview"
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (search-forward "* NeighbourHood *" nil t)
  (search-forward "[<" nil t)
  (backward-char)
  )



(defun ps/class-find-used ()
  "Navigate to the * Uses * in the Class Overview"
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (search-forward "* Uses *" nil t)
  (beginning-of-line 2)
  (forward-char)
  )



(defun ps/class-find-bookmarks ()
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



(defun ps/class-find-structure ()
  "Navigate to the * Structure * in the Class Overview"
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (search-forward "* Structure *" nil t)
  (search-forward "-" nil t)
  (beginning-of-line 2)
  )



(defun ps/class-find-api ()
  "Navigate to the * API * in the Class Overview"
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (search-forward "* API *" nil t)
  (beginning-of-line 2)
  )



(defun ps/class-find-api-new ()
  "Navigate to the new method in the Class Overview"
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (search-forward-regexp ".>new\\b" nil t)
  (backward-char 3)
  )



(defun ps/find-class-name-at-point ()
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



(defvar ps/class-mode-map nil
  "Keymap for `PerlySense Class overview major mode'.")
(if ps/class-mode-map
    ()
  (setq ps/class-mode-map (make-sparse-keymap)))
(define-key ps/class-mode-map "q" 'ps/class-quit)
(define-key ps/class-mode-map "I" 'ps/class-find-inheritance)
(define-key ps/class-mode-map "H" 'ps/class-find-neighbourhood)
(define-key ps/class-mode-map "U" 'ps/class-find-used)
(define-key ps/class-mode-map "B" 'ps/class-find-bookmarks)
(define-key ps/class-mode-map "S" 'ps/class-find-structure)
(define-key ps/class-mode-map "A" 'ps/class-find-api)
(define-key ps/class-mode-map "N" 'ps/class-find-api-new)

(define-key ps/class-mode-map "N" 'ps/class-find-api-new)
(define-key ps/class-mode-map (format "%sgn" ps/key-prefix) 'ps/class-find-api-new)

(define-key ps/class-mode-map [return] 'ps/class-class-overview-or-goto-at-point)

(define-key ps/class-mode-map "d" 'ps/class-docs-at-point)
(define-key ps/class-mode-map (format "%s\C-d" ps/key-prefix) 'ps/class-docs-at-point)

(define-key ps/class-mode-map "g" 'ps/class-goto-at-point)
(define-key ps/class-mode-map (format "%s\C-g" ps/key-prefix) 'ps/class-goto-at-point)

(define-key ps/class-mode-map "o" 'ps/class-class-overview-at-point)
(define-key ps/class-mode-map (format "%s\C-o" ps/key-prefix) 'ps/class-class-overview-at-point)





(defvar ps/class-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Treat _ and :: as part of a word
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?: "w" st)
    st)
  "Syntax table for `ps/class-mode'.")


;; (Defvar ps/class-imenu-generic-expression
;;   ...)

;; (defvar ps/class-outline-regexp
;;   ...)

 ;;;###autoload
(define-derived-mode ps/class-mode fundamental-mode "PerlySense Class Overview"
  "A major mode for viewing PerlySense Class overview buffers."
  :syntax-table ps/class-mode-syntax-table
;;   (set (make-local-variable 'comment-start) "# ")
;;   (set (make-local-variable 'comment-start-skip) "#+\\s-*")

;;   (set (make-local-variable 'font-lock-defaults)
;;        '(ps/class-font-lock-keywords))

;;   (set (make-local-variable 'indent-line-function) 'ps/class-indent-line)
;;   (set (make-local-variable 'imenu-generic-expression)
;;        ps/class-imenu-generic-expression)
;;   (set (make-local-variable 'outline-regexp) ps/class-outline-regexp)
  )

;;; Indentation

;; (defun ps/class-indent-line ()
;;   "Indent current line of Ps/Class code."
;;   (interactive)
;;   (let ((savep (> (current-column) (current-indentation)))
;;         (indent (condition-case nil (max (ps/class-calculate-indentation) 0)
;;                   (error 0))))
;;     (if savep
;;         (save-excursion (indent-line-to indent))
;;       (indent-line-to indent))))

;; (defun ps/class-calculate-indentation ()
;;   "Return the column to which the current line should be indented."
;;   ...)



;; Key bindings
;;;; TODO: move some of these to cperl-mode local bindings

(global-set-key (format "%smf" ps/key-prefix) 'ps/find-source-for-module-at-point)
(global-set-key (format "%smp" ps/key-prefix) 'ps/display-pod-for-module-at-point)

(global-set-key (format "%s\C-d" ps/key-prefix) 'ps/smart-docs-at-point)
(global-set-key (format "%sdi" ps/key-prefix) 'ps/inheritance-docs-at-point)
(global-set-key (format "%sdu" ps/key-prefix) 'ps/use-docs-at-point)
(global-set-key (format "%sdo" ps/key-prefix) 'ps/class-overview-for-class-at-point)

(global-set-key (format "%s\C-g" ps/key-prefix) 'ps/smart-go-to-at-point)
(global-set-key (format "%sgb" ps/key-prefix) 'ps/go-to-base-class-at-point)
(global-set-key (format "%sgu" ps/key-prefix) 'ps/go-to-use-section)
(global-set-key (format "%sgn" ps/key-prefix) 'ps/go-to-method-new)
(global-set-key (format "%sgm" ps/key-prefix) 'ps/find-source-for-module-at-point)
(global-set-key (format "%sgv" ps/key-prefix) 'ps/go-to-vc-project)

(global-set-key (format "%semu" ps/key-prefix) 'ps/edit-move-use-statement)
(global-set-key (format "%setc" ps/key-prefix) 'ps/edit-test-count)

(global-set-key (format "%sat" ps/key-prefix) 'ps/assist-sync-test-count)

(global-set-key (format "%s\C-o" ps/key-prefix) 'ps/class-overview-for-class-at-point)
;; (global-set-key (format "%s\C-c" ps/key-prefix) 'ps/display-api-for-class-at-point)

(global-set-key (format "%s\C-r" ps/key-prefix) 'ps/run-file)
(global-set-key (format "%srr" ps/key-prefix) 'ps/rerun-file)

(global-set-key (format "%sge" ps/key-prefix) 'ps/compile-goto-error-file-line)
(global-set-key (format "%sgto" ps/key-prefix) 'ps/goto-test-other-files)

(global-set-key (format "%sar" ps/key-prefix) 'ps/regex-tool)





(load "perly-sense-visualize-coverage")
(if ps/load-flymake (load "perly-sense-flymake"))



(provide 'perly-sense)





;; EOF
