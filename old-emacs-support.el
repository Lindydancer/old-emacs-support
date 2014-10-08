;;; old-emacs-support.el --- Features needed to use old Emacs versions.

;; Copyright (C) 2014 Anders Lindgren
;; Copyright (C) 1985-1987, 1993-2013 Free Software Foundation, Inc.

;; Author: Anders Lindgren
;; Version: 0.0.2
;; URL: https://github.com/Lindydancer/old-emacs-support

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides features needed to use my packages on older
;; Emacs versions. Supported Emacs versions are 22.x, 23.x, and 24.x.
;;
;; In addition, this package can patch known problems in earlier Emacs
;; versions, for things that are related to my packages.
;;
;; I have opted to implement backward compatibility support in a
;; separate package, rather than let each package handle it themselves.
;;
;; One reason for this is to make packages simpler to read. Another is
;; to make life easier for FSF in case they would include my packages
;; in future Emacs distributions.

;;; Code:

;; ------------------------------------------------------------
;; user-error
;;

;; Unlike `error', a `user-error' does not invoke the debugger, even
;; when `debug-on-error' is non-nil.

(unless (fboundp 'user-error)
  (defun user-error (format &rest args)
    "\
Signal a pilot error, making error message by passing all args to `format'.
In Emacs, the convention is that error messages start with a capital
letter but *do not* end with a period.  Please follow this convention
for the sake of consistency.
This is just like `error' except that `user-error's are expected to be the
result of an incorrect manipulation on the part of the user, rather than the
result of an actual problem."
    (signal 'user-error (list (apply #'format format args)))))


(unless (get 'user-error 'error-conditions)
  (put 'user-error 'error-conditions '(user-error error)))
(unless (get 'user-error 'error-message)
  (put 'user-error 'error-message ""))


(add-to-list 'debug-ignored-errors 'user-error)


;; ------------------------------------------------------------
;; Window and frame functions
;;

;; In newer Emacs versions, a window can be adjusted down to the pixel
;; level. In this case `window-width' includes partial-width columns
;; whereas `window-text-width' does not.
(unless (fboundp 'window-text-width)
  (defalias 'window-text-width 'window-width))


(unless (fboundp 'frame-border-width)
  (defun frame-border-width (&optional frame)
    "Return border width of FRAME in pixels."
    (let ((pair (assq 'internal-border-width (frame-parameters frame))))
      (if pair
          (cdr pair)
        0))))


(unless (fboundp 'frame-scroll-bar-width)
  (defun frame-scroll-bar-width (&optional frame)
    "Return scroll bar width of FRAME in pixels."
    (let ((pair (assq 'scroll-bar-width (frame-parameters frame))))
      (if pair
          (cdr pair)
        0))))


(unless (fboundp 'frame-fringe-width)
  (defun frame-fringe-width (&optional frame)
    "Return fringe width of FRAME in pixels."
    (let ((left-pair (assq 'left-fringe (frame-parameters frame)))
          (right-pair (assq 'right-fringe (frame-parameters frame))))
      (+ (or (cdr-safe left-pair) 0)
	 (or (cdr-safe right-pair) 0)))))


(unless (fboundp 'frame-text-width)
  (defun frame-text-width (&optional frame)
    "Return text area width of FRAME in pixels."
    (* (frame-width frame)
       (frame-char-width frame))))


(unless (fboundp 'frame-text-height)
  (defun frame-text-height (&optional frame)
    "Return text area height of FRAME in pixels."
    (* (frame-height frame)
       (frame-char-height frame))))


;; ------------------------------------------------------------
;; With silent modifications
;;
;; Used by the font-lock-studio.
;;
;; Originates from "subr.el" of Emacs.

(unless (fboundp 'with-silent-modifications)
  (defmacro with-silent-modifications (&rest body)
    "Execute BODY, pretending it does not modify the buffer.
If BODY performs real modifications to the buffer's text, other
than cosmetic ones, undo data may become corrupted.

This macro will run BODY normally, but doesn't count its buffer
modifications as being buffer modifications.  This affects things
like `buffer-modified-p', checking whether the file is locked by
someone else, running buffer modification hooks, and other things
of that nature.

Typically used around modifications of text-properties which do
not really affect the buffer's content."
    (declare (debug t) (indent 0))
    (let ((modified (make-symbol "modified")))
      `(let* ((,modified (buffer-modified-p))
              (buffer-undo-list t)
              (inhibit-read-only t)
              (inhibit-modification-hooks t)
              deactivate-mark
              ;; Avoid setting and removing file locks and checking
              ;; buffer's uptodate-ness w.r.t the underlying file.
              buffer-file-name
              buffer-file-truename)
         (unwind-protect
             (progn
               ,@body)
           (unless ,modified
             (restore-buffer-modified-p nil)))))))


;; ------------------------------------------------------------
;; Font Lock Mode
;;

;; In Emacs 24.4, you should no longer call `font-lock-fontify-buffer'
;; from lisp in order to refontify the buffer after installing new
;; keywords.

(eval-after-load "font-lock"
  '(unless (fboundp 'font-lock-flush)
     (defun font-lock-flush (&optional beg end)
       "Declare the region BEG...END's fontification as out-of-date.
If the region is not specified, it defaults to the whole buffer."
       (when font-lock-mode
         (font-lock-fontify-buffer)))))


;; ------------------------------------------------------------
;; Base modes -- `prog-mode', `text-mode', and `special-mode'.
;;
;; Makes `prog-mode' and `special-mode' available as parent modes and
;; ensures that `derived-mode-p' work.
;;
;; Note: The following does not modify the modes to start the parent
;; mode. In particular, functions added to the hook of a parent mode
;; is not runed when the "derived" mode is started.


;; --------------------------------------------------
;; Prog mode
;;

(unless (fboundp 'prog-mode)
  (defvar prog-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [?\C-\M-q] 'prog-indent-sexp)
      map)
    "Keymap used for programming modes.")

  (defun prog-indent-sexp ()
    "Indent the expression after point."
    (interactive)
    (let ((start (point))
          (end (save-excursion (forward-sexp 1) (point))))
      (indent-region start end nil)))

  (define-derived-mode prog-mode fundamental-mode "Prog"
    "Major mode for editing programming language source code."
    (set (make-local-variable 'require-final-newline)
         mode-require-final-newline)
    (set (make-local-variable 'parse-sexp-ignore-comments) t)
    ;; Any programming language is always written left to right.
    (setq bidi-paragraph-direction 'left-to-right)))

(dolist (mode '(ada-mode
                antlr-mode
                asm-mode
                autoconf-mode
                awk-mode
                bat-mode
                c-mode
                c++-mode
                cfengine2-mode
                cfengine3-mode
                cperl-mode
                dcl-mode
                emacs-lisp-mode
                f90-mode
                fortran-mode
                gdb-script-mode
                icon-mode
                idl-mode
                idlwave-mode
                java-mode
                js-mode
                ld-script-mode
                lisp-mode
                m2-mode
                m4-mode
                makefile-mode
                meta-common-mode
                mixal-mode
                objc-mode
                octave-mode
                opascal-mode
                pascal-mode
                perl-mode
                pike-mode
                prolog-mode
                ps-mode
                python-mode
                ruby-mode
                scheme-mode
                sh-mode
                simula-mode
                sql-mode
                tcl-mode
                vera-mode
                verilog-mode
                vhdl-mode))
  (unless (get mode 'derived-mode-parent)
    (put mode 'derived-mode-parent 'prog-mode)))


;; --------------------------------------------------
;; Text mode
;;

(dolist (mode '(bib-mode
                change-log-mode
                dns-mode
                doctor-mode
                dun-mode
                log-edit-mode
                mail-mode
                message-mode
                mh-show-mode
                nroff-mode
                nxml-mode
                outline-mode
                paragraph-indent-text-mode
                rst-mode
                scribe-mode
                sgml-mode
                tex-mode
                texinfo-mode
                todo-edit-mode))
  (unless (get mode 'derived-mode-parent)
    (put mode 'derived-mode-parent 'text-mode)))


;; --------------------------------------------------
;; Special mode
;;
;; Used as base for the major mode used by the font-lock-studio
;; interface buffer.
;;
;; Originates from "simple.el" of Emacs.

(unless (boundp 'special-mode-map)
  (defvar special-mode-map
    (let ((map (make-sparse-keymap)))
      (suppress-keymap map)
      (define-key map "q" 'quit-window)
      (define-key map " " 'scroll-up-command)
      (define-key map [?\S-\ ] 'scroll-down-command)
      (define-key map "\C-?" 'scroll-down-command)
      (define-key map "?" 'describe-mode)
      (define-key map "h" 'describe-mode)
      (define-key map ">" 'end-of-buffer)
      (define-key map "<" 'beginning-of-buffer)
      (define-key map "g" 'revert-buffer)
      map))

  (put 'special-mode 'mode-class 'special)
  (define-derived-mode special-mode nil "Special"
    "Parent major mode from which special major modes should inherit."
    (setq buffer-read-only t)))


(dolist (mode '(5x5-mode
                apropos-mode
                blackbox-mode
                bookmark-bmenu-mode
                custom-theme-choose-mode
                diary-fancy-display-mode
                display-time-world-mode
                ebrowse-member-mode
                ebrowse-tree-mode
                epa-info-mode
                epa-key-list-mode
                epa-key-mode
                erc-list-menu-mode
                ert-results-mode
                ert-simple-view-mode
                eudc-mode
                gomoku-mode
                help-mode
                ibuffer-mode
                idlwave-help-mode
                landmark-mode
                life-mode
                locate-mode
                log-view-mode
                messages-buffer-mode
                mspools-mode
                net-utils-mode
                occur-mode
                org-export-stack-mode
                proced-mode
                profiler-report-mode
                rmail-summary-mode
                select-tags-table-mode
                snake-mode
                solitaire-mode
                tabulated-list-mode
                tar-mode
                todo-archive-mode
                todo-categories-mode
                todo-filtered-items-mode
                todo-mode
                vc-annotate-mode
                vc-dir-mode
                xesam-mode))
  (unless (get mode 'derived-mode-parent)
    (put mode 'derived-mode-parent 'special-mode)))


;; ------------------------------------------------------------
;; Follow-mode (Emacs bug#16426)
;;
;; In Emacs 24.3, Follow-mode was refactorized. Unfortunately, this
;; also introduced a bug visible in, for example, *grep*.
;;
;; Originally, `follow-adjust-window' was called with `(point)' as the
;; second argument. Unfortunately, this is the value of the point of
;; the current buffer, not of the buffer of the selected window.
;;
;; Originates from "follow.el" of Emacs.

(when (and (equal emacs-major-version 24)
           (equal emacs-minor-version 3))
  (eval-after-load "follow"
    '(defun follow-post-command-hook ()
       "\
Ensure that the windows in Follow mode are adjacent after each command."
       (unless (input-pending-p)
         (let ((follow-inside-post-command-hook t)
               (win (selected-window))
               dest)
           ;; Work in the selected window, not in the current buffer.
           (with-current-buffer (window-buffer win)
             (setq dest (point))
             (unless (and (symbolp this-command)
                          (get this-command 'follow-mode-use-cache))
               (setq follow-windows-start-end-cache nil)))
           (follow-adjust-window win dest))))))


;; ------------------------------------------------------------
;; The end.
;;

(provide 'old-emacs-support)


;; ------------------------------------------------------------
;; Inhibit byte compilation.
;;

;; This file is intended to be used with a range of different Emacs
;; versions. For each version, different compiler warnings are issued.
;; Instead of fighting each of them, byte compilation for this file is
;; disabled.

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; old-emacs-support.el ends here
