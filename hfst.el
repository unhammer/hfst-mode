; hfst-mode.el -- major mode for editing HFST files

; For Helsinki Finite State Transducer Tools (HFST), see
; http://www.ling.helsinki.fi/kieliteknologia/tutkimus/hfst/

;; Copyright (C) 2010 Kevin Brubeck Unhammer
;; Based on sfst.el Copyright (C) 2006 Sebastian Nagel 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(defconst hfst-version "2010-01-11" "Version of hfst-mode")

;;;============================================================================
;;;
;;; Define the formal stuff for a major mode named hfst.
;;;

(defvar hfst-mode-hook nil)

(defvar hfst-mode-syntax-table
  (let ((hfst-mode-syntax-table (make-syntax-table)))
    ; comments start with !
    (modify-syntax-entry ?!  "<" hfst-mode-syntax-table)
    ; and last until end-of-line
    (modify-syntax-entry ?\n ">!" hfst-mode-syntax-table)
    ;; todo: % is the escape character (I think)
    (modify-syntax-entry ?% "\\" hfst-mode-syntax-table)
    ;; dots appear as symbols in flag diacritics:
    (modify-syntax-entry ?. "_" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?\" "." hfst-mode-syntax-table)
;;     (modify-syntax-entry ?\\ "\\" hfst-mode-syntax-table)
;;    (modify-syntax-entry ?\" "." hfst-mode-syntax-table)
;;     (modify-syntax-entry ?\' "$" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?< "\"" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?< "." hfst-mode-syntax-table)
;;     (modify-syntax-entry ?> "." hfst-mode-syntax-table)
;;     (modify-syntax-entry ?{ "(}" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?} "){" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?( "()" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?) ")(" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?[ "(]" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?] ")[" hfst-mode-syntax-table)
    hfst-mode-syntax-table)
  "Syntax table for hfst-mode")

(defconst hfst-font-lock-keywords
  '(;; keywords TODO: alphabet doesn't match if on first line!
    ("\\(?:\\Sw\\|^\\)\\(Alphabet\\|Multichar_Symbols\\|Sets\\|Rules\\|Definitions\\|Diacritics\\|Rule-variables\\|where\\|in\\|matched\\)\\Sw"
     (1 'font-lock-keyword-face nil t))
    ;; todo: lexicon names always start with a capital letter, but can
    ;; you have eg. Æ? or just A-Z?
    ("\\(LEXICON\\) +\\(\\(\\sw\\|\\s_\\)+\\)"
     (1 'font-lock-keyword-face nil t)
     (2 font-lock-variable-name-face))
    ;; flag diacritics:
    ("@\\sw\\.\\(\\(\\sw\\|\\s_\\)+\\)@" 
     (1 'font-lock-variable-name-face nil t))
    ;; End symbol:
    ("\\(#\\)"
     (1 'font-lock-warning-face nil t))
    ;; escape symbol:
    ("\\(%\\)\\([^ \t\n]+\\)"
     (1 'font-lock-warning-face nil t)
     (2 'font-lock-unfontify-region-function nil t))
    ;; operators:
    ;; todo: is the + just another symbol?
    ("\\(\\(?:<=>?\\|<?=>\\)\\|[:+^>|-/;_]\\)"
    ; [_/\\\\^]->\\|[\\^_]?\\|^_\\|||\\|<<\\|__
     (1 'font-lock-function-name-face nil t)))
  "Expressions to highlight in hfst-mode.")

(defun hfst-font ()
  "Set font-lock variables for hfst mode."
  (make-local-variable 'font-lock-keywords-case-fold-search) ; For GNU Emacs.
  (setq font-lock-keywords-case-fold-search nil)
  (put major-mode 'font-lock-keywords-case-fold-search nil) ; For XEmacs.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(hfst-font-lock-keywords nil nil)))

(defun hfst-mode ()
  "Major mode for editing files describing finite state
transducers to be used with hfst, Helsinki Finite State
Transducer Tools, see
http://www.ling.helsinki.fi/kieliteknologia/tutkimus/hfst/."
  (interactive)
  (kill-all-local-variables)
;;  (use-local-map hfst-mode-map)
  (setq major-mode 'hfst-mode)
  (setq mode-name "HFST")
  (setq parse-sexp-ignore-comments t)
  (set-syntax-table hfst-mode-syntax-table)
;;   (make-local-variable 'indent-line-function)
;;   (setq indent-line-function 'hfst-indent-line)
  (make-local-variable 'comment-start)  
  (setq comment-start "! ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "! *")
  (make-local-variable 'completion-ignore-case)
  (setq completion-ignore-case nil)
  (hfst-font)
  (run-mode-hooks 'hfst-mode-hook))

;;; Run hooks -----------------------------------------------------------------
(run-hooks 'hfst-load-hook)

(provide 'hfst)

;;;============================================================================

;;; hfst.el ends here