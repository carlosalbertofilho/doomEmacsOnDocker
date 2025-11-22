;;; flycheck-norminette.el --- Flycheck integration for 42 Norminette -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho
;;
;; Author: Carlos Filho
;; Version: 1.0
;; Package-Requires: ((emacs "24.3") (flycheck "32"))
;; Keywords: 42, norminette, flycheck, c
;;
;; This file integrates 42 School's Norminette with Flycheck for real-time syntax checking.

;;; Commentary:
;;
;; Provides Flycheck integration for the 42 School Norminette checker.
;; Automatically checks C files against 42 coding standards and highlights errors.
;;
;; Usage:
;;   (require 'flycheck-norminette)
;;   (flycheck-norminette-setup)
;;
;; Configuration:
;;   Customize `flycheck-norminette-executable' to set the norminette path

;;; Code:

(require 'flycheck)

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(defgroup flycheck-norminette nil
  "Flycheck integration for 42 Norminette."
  :group 'flycheck
  :prefix "flycheck-norminette-")

(defcustom flycheck-norminette-executable "norminette"
  "Path to the norminette executable."
  :type 'string
  :group 'flycheck-norminette)

;; =============================================================================
;; FLYCHECK CHECKER DEFINITION
;; =============================================================================

(flycheck-define-checker c-norminette
  "A C syntax checker using 42 School's Norminette.

Checks C source files for compliance with 42 coding standards.
See URL `https://github.com/42School/norminette' for more information."
  :command ("norminette" source)
  :error-patterns
  ((error line-start
          "Error: " (id (one-or-more (not (any space))))
          (one-or-more space)
          "(line:" (zero-or-more space) line "," (zero-or-more space) 
          "col:" (zero-or-more space) column "):"
          (zero-or-more space)
          (message (one-or-more not-newline))
          line-end)
   (error line-start
          (file-name) ": Error!" line-end)
   (warning line-start
            "Notice: " (id (one-or-more (not (any space))))
            (one-or-more space)
            "(line:" (zero-or-more space) line "," (zero-or-more space)
            "col:" (zero-or-more space) column "):"
            (zero-or-more space)
            (message (one-or-more not-newline))
            line-end)
   (info line-start
         (file-name) ": OK!" line-end))
  :error-filter
  (lambda (errors)
    (dolist (err errors)
      (let ((msg (flycheck-error-message err)))
        (when msg
          ;; Remove ANSI color codes (e.g., [0m, [95m, etc.)
          (setq msg (replace-regexp-in-string "\033\\[\\([0-9;]*\\)m" "" msg))
          ;; Remove other ANSI escape sequences
          (setq msg (replace-regexp-in-string "\\[\\([0-9;]*\\)m" "" msg))
          ;; Remove asterisks used for highlighting (*!text*!)
          (setq msg (replace-regexp-in-string "\\*!\\([^*]*\\)\\*!" "\\1" msg))
          ;; Clean up extra spaces
          (setq msg (replace-regexp-in-string "  +" " " msg))
          (setq msg (string-trim msg))
          
          ;; Update the message
          (setf (flycheck-error-message err) msg)
          
          ;; Add context to common errors
          (cond
           ((string-match "INVALID_HEADER" msg)
            (setf (flycheck-error-message err)
                  (concat msg " (Use SPC c h to insert/update header)")))
           ((string-match "TOO_MANY_FUNCS" msg)
            (setf (flycheck-error-message err)
                  (concat msg " (Max 5 functions per file)")))
           ((string-match "TOO_MANY_LINES" msg)
            (setf (flycheck-error-message err)
                  (concat msg " (Max 25 lines per function)")))
           ((string-match "SPACE_REPLACE_TAB" msg)
            (setf (flycheck-error-message err)
                  (concat msg " (Use tabs for indentation)")))
           ((string-match "SPC_BEFORE_NL" msg)
            (setf (flycheck-error-message err)
                  (concat msg " (Remove trailing whitespace)")))))))
    ;; Filter out "OK!" messages in normal operation
    (seq-filter
     (lambda (err)
       (not (and (eq 'info (flycheck-error-level err))
                 (string-match "No norminette errors found"
                              (flycheck-error-message err)))))
     errors))
  :modes (c-mode)
  :predicate
  (lambda ()
    (and buffer-file-name
         (or (string-match-p "\\.c\\'" buffer-file-name)
             (string-match-p "\\.h\\'" buffer-file-name))
         (executable-find flycheck-norminette-executable))))

;; =============================================================================
;; SETUP FUNCTION
;; =============================================================================

;;;###autoload
(defun flycheck-norminette-setup ()
  "Setup Flycheck Norminette integration.

Adds norminette checkers to Flycheck and configures them for C files."
  (interactive)
  ;; Add our checkers to Flycheck
  (add-to-list 'flycheck-checkers 'c-norminette)

  ;; Set norminette as the preferred checker for C files
  (flycheck-add-next-checker 'c/c++-clang 'c-norminette)
  
  ;; Configure flycheck for better norminette experience
  (setq-default
   ;; Show errors immediately
   flycheck-check-syntax-automatically '(save mode-enabled)
   ;; Display errors in a more readable format
   flycheck-display-errors-delay 0.3)
  
  (message "Flycheck Norminette integration enabled!"))

;; =============================================================================
;; INTERACTIVE COMMANDS
;; =============================================================================

;;;###autoload
(defun flycheck-norminette-check-buffer ()
  "Run norminette check on the current buffer."
  (interactive)
  (if (executable-find flycheck-norminette-executable)
      (flycheck-buffer)
    (user-error "Norminette executable not found. Is it installed?")))

;;;###autoload
(defun flycheck-norminette-toggle ()
  "Toggle norminette checking on/off for the current buffer."
  (interactive)
  (if flycheck-mode
      (progn
        (flycheck-mode -1)
        (message "Norminette checking disabled"))
    (progn
      (flycheck-mode 1)
      (message "Norminette checking enabled"))))

;; =============================================================================
;; AUTO-SETUP
;; =============================================================================

;;;###autoload
(defun flycheck-norminette-auto-enable ()
  "Automatically enable norminette checking for C files.

Add this to your C mode hook to enable automatic checking."
  (when (and buffer-file-name
             (or (string-match-p "\\.c\\'" buffer-file-name)
                 (string-match-p "\\.h\\'" buffer-file-name))
             (executable-find flycheck-norminette-executable))
    (flycheck-mode 1)
    (flycheck-select-checker 'c-norminette)))

(provide 'flycheck-norminette)
;;; flycheck-norminette.el ends here
