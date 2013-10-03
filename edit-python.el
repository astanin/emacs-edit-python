;;; edit-python.el --- facilities to edit Python code

;; Copyright 2013 Sergey Astanin

;; Author: Sergey Astanin
;; Version: 0.1
;; Package-Requires: ((s "1.8.0") (dash "2.2.0") (ido "1.57") (python "0.24.2") (eproject "1.5"))
;; URL: https://github.com/astanin/edit-python.el
;; License: MIT

;;; Commentary:

;; Quick hacks to improve Python editing in Emacs.
;;
;; Adding imports: `edit-python-import-from` inserts an unqualified
;; "from module import name" for the identifier at point;
;; `edit-python-import-qualified` inserts a qualified "import module"
;; or "import module as alias".  Both functions look up files of the
;; current project to suggest module names.
;;

(require 's)
(require 'dash)
(require 'ido)
(require 'python)


(defun ep/end-of-python-string ()
  "Given the point is at the beginning of a Python string literal,
   move it to the end of string. `python-nav-end-of-statement` is
   probably supposed to do the same, but the result is not reliable."
  (let ((s (buffer-substring-no-properties (point) (+ (point) 3))))
    (cond
     ((or (s-starts-with? "'''" s)
          (s-starts-with? "\"\"\"" s))
      (search-forward s nil 'no-error 2))
     ((s-starts-with? "'" s)
      (re-search-forward "[^\\]'"))
     ((s-starts-with? "\"" s)
      (re-search-forward "[^\\]\"")))))

(defun ep/beginning-of-imports ()
  "Move the point to the beginning of file-level imports"
  (beginning-of-buffer)
  ;; skip comments (shebang, coding)
  (while (eq ?# (char-after))
    (forward-line)
    (skip-chars-forward " \t\r\n"))
  ;; skip module docstring if any
  (while (or (eq ?\" (char-after))
             (eq ?'  (char-after)))
    (ep/end-of-python-string)
    (skip-chars-forward " \t\r\n")))


(defun ep/end-of-imports ()
  "Move the beyond the last import"
  (ep/beginning-of-imports)
  (while (re-search-forward "from[ \t].*[ \t]import\\|^import " (line-end-position) 't)
    (python-nav-end-of-statement)
    (skip-chars-forward " \t\r\n"))
  (skip-chars-backward " \t\r\n"))


(defun ep/python-filename-to-module-name (module-full-path)
  (let* ((buffer-filename (buffer-file-name))
         (shared (file-name-directory
                  (s-shared-start buffer-filename module-full-path)))
         (relative (s-chop-prefix shared module-full-path)))
    (s-replace "/" "."
               (s-chop-suffix ".py" relative))))


(defun ep/python-file-toplevel-defs (module-full-path)
  (with-temp-buffer
    (insert-file-contents module-full-path)
    (let ((defs '())
          (def-re "^def[ \\t]+\\([A-Za-z][A-Za-z0-9_]*\\)")
          (assignment-re "^\\([A-Za-z][A-Za-z0-9_]*\\)[ \\t]*="))
      (beginning-of-buffer)
      (while (re-search-forward def-re nil 't)
        (let ((defname (match-string-no-properties 1)))
          (setq defs (append defs `(,defname)))))
      (beginning-of-buffer)
      (while (re-search-forward assignment-re nil 't)
        (let ((varname (match-string-no-properties 1)))
          (setq defs (append defs `(,varname)))))
      defs)))


(defun ep/insert-from-import (module name)
  (save-excursion
    (let* ((re-import-prefix (s-replace "." "\\."
                                        (concat "from " module " import")))
           (re-existing-import (concat "^" re-import-prefix ".*" name))
           (re-from-statement  (concat "^" re-import-prefix))
           (already-imported? (save-excursion
                                (beginning-of-buffer)
                                (re-search-forward re-existing-import nil 't)))
           (from-import-position (save-excursion
                                   (beginning-of-buffer)
                                   (re-search-forward re-from-statement nil 't))))
      (unless already-imported?
        (if from-import-position
            (progn  ;; append to the exiting from-import
              (goto-char from-import-position)
              (end-of-line)
              (insert (concat ", " name)))
            (progn  ;; insert a new from-import
              (ep/end-of-imports)
              (newline)
              (insert (concat "from " module " import " name))))))))


(defun ep/insert-qualified-import (module alias)
  (save-excursion
    (let* ((import-str (concat "import " module
                             (when (not (s-blank? alias))
                               (concat " as " alias))))
           (re-import (s-replace "." "\\."
                                 (concat "^import " module
                                         (when (not (s-blank? alias))
                                           (concat ".*[ \t]as[ \t]+" alias)))))
           (already-imported? (save-excursion
                                 (beginning-of-buffer)
                                 (re-search-forward re-import nil 't))))
      (unless already-imported?
        (ep/end-of-imports)
        (newline)
        (insert import-str)))))


(defun ep/beginning-of-identifier (&optional qualified)
  (let ((id-chars (if qualified "[:alnum:]_." "[:alnum:]_")))
    (skip-chars-backward id-chars)))


(defun ep/end-of-identifier (&optional qualified)
  (let ((id-chars (if qualified "[:alnum:]_." "[:alnum:]_")))
    (skip-chars-forward id-chars)))


(defun ep/identifier-at-point (&optional qualified)
  "Return a string with the name of the Python identifier at point."
  (buffer-substring-no-properties
   (save-excursion
     (ep/beginning-of-identifier qualified)
     (point))
   (save-excursion
     (ep/end-of-identifier qualified)
     (point))))


(defun ep/suggest-modules-for-name (name)
  "Return a list of project-local module names where the `name` is defined."
  (let* (;; python files of the current project
         (pyfiles (remove-if-not
                   '(lambda (f) (s-ends-with? ".py" f))
                   (eproject-list-project-files)))
         ;; guess the names of their python modules
         (pymods  (mapcar
                   'ep/python-filename-to-module-name
                   pyfiles))
         ;; a list of top-level definitions per every file
         (modvars (mapcar
                   'ep/python-file-toplevel-defs
                   pyfiles))
         ;; an alist of module-name => list of top-level defs and vars
         (mods-and-vars         (mapcar* #'list
                                         pymods modvars))
         ;; remove where name is not mentioned
         (mods-with-the-name    (remove-if-not '(lambda (m) (member name (cadr m)))
                                               mods-and-vars))
         ;; a list of candidate module names to import
         (modnames    (remove-if #'s-blank?
                                 (mapcar #'car mods-with-the-name))))
    modnames))


(defun edit-python-import-from ()
  "Add \"from module import name\" to the top of the file,
  where `name` is the identifier at point.

  Relies on eproject to get a list of project files."
  (interactive)
  (let* ((name     (ep/identifier-at-point))
         (modnames (ep/suggest-modules-for-name name))
         (prompt   (concat "import " name " from: "))
         (module   (ido-completing-read prompt modnames)))
    (when module
      (ep/insert-from-import module name))))


(defun edit-python-import-qualified ()
  "Add a qualified import statement \"import x.y\" or \"import
  x.y as alias\" to the top of the file, and prepend the module
  prefix to the unqualified identifier at point if it's
  unqualified."
  (interactive)
  (let* ((qname      (ep/identifier-at-point 'qualified))
         (name-parts (s-split "\\." qname 'omit-nulls))
         (qualified? (> (length name-parts) 1))
         (prefix     (s-join "." (-take (- (length name-parts) 1) name-parts)))
         (alias      (when (and (not (s-blank? prefix))
                                (not (s-contains? "\." prefix)))
                       prefix))  ; qname's prefix looks like a module's alias
         (name       (-last-item name-parts))  ; unqualified identifier
         (modnames   (ep/suggest-modules-for-name name))
         (modnames   (if qualified?  ; prefix could be the module's name
                       (cons prefix modnames)
                       modnames))
         (prompt     (concat "import ... "))
         (module     (ido-completing-read prompt modnames))
         (prompt-as  (concat "import " module " as ... "))
         (suggest-as (when alias `(,alias "")))
         (alias      (ido-completing-read prompt-as suggest-as))
         (prefix     (if (not (s-blank? alias)) alias module)))
    (ep/insert-qualified-import module alias)
    (unless qualified?
      (ep/beginning-of-identifier)
      (insert prefix)
      (insert "."))))


(provide 'edit-python)


;;; edit-python.el ends here
