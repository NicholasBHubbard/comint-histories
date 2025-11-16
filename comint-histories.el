;;; comint-histories.el --- Many comint histories -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Nicholas Hubbard <nicholashubbard@posteo.net>
;;
;; Licensed under the same terms as Emacs and under the MIT license.

;; SPDX-License-Identifier: MIT

;; Author: Nicholas Hubbard <nicholashubbard@posteo.net>
;; URL: https://github.com/NicholasBHubbard/comint-histories
;; Package-Requires: ((emacs "25.1") (f "0.21.0"))
;; Version: 2.2
;; Created: 2025-01-02
;; By: Nicholas Hubbard <nicholashubbard@posteo.net>
;; Keywords: convenience, processes, terminals

;;; Commentary:

;; This package provides functionality for defining multiple histories for
;; comint inputs.  This is useful for dividing up histories for different
;; programs run through comint buffers.  Users can define custom histories via
;; the comint-histories-add-history macro.  Histories can be customized in
;; various ways including their length, if they should persist across sessions,
;; filters to prevent inputs from being added to the history, and more.

;; Please see https://github.com/NicholasBHubbard/comint-histories for more
;; information.

;;; Code:

(require 'comint)
(require 'cl-lib)
(require 'seq)
(require 'f)

(defcustom comint-histories-persist-dir
  (f-join user-emacs-directory "comint-histories")
  "Directory for storing saved histories."
  :type 'string
  :group 'comint-histories)

(defcustom comint-histories-global-filters nil
  "Filters to be implicitly added to all history :filters."
  :type '(repeat sexp)
  :group 'comint-histories)

(defvar-local comint-histories--last-selected-history nil
  "Internal variable to keep track of the buffers selected history.")

(defvar comint-histories--histories nil
  "Internal alist of plists containing all defined histories.")

(defmacro comint-histories-add-history (name &rest props)
  "Declare a comint-histories history named NAME with properties PROPS.

Usage: (comint-histories-add-history history-name
          [:keyword [option]]...)

:predicates     List of functions that take zero args who's conjunction
                determines the selection of this history.

:filters        List of regexp strings and functions that take one arg. If the
                input matches any of the regexp's, or any of the functions
                return non-nil when applied to the input, then the input is not
                added to the history.

:persist        If non-nil, then save and load the history to/from a file.
                Defaults to T.

:defer-load     If non-nil, then don't load the history from disk until it is
                selected. Only relevant to persistent histories. Defaults to T.

:length         Maximum length of the history ring. Defaults to 100.

:no-dups        Do not allow duplicate entries from entering the history. When
                adding a duplicate item to the history, the older entry is
                removed first. Defaults to NIL.

:reselect-after If non-nil, when this history is selected, immediately reselect
                the history after a call to `comint-send-input'. Defaults to
                NIL.

:rtrim          If non-nil, then trim beginning whitespace from the input before
                adding attempting to add it to the history. Defaults to T.

:ltrim          If non-nil, then trim ending whitespace from the input before
                attempting to add it to the history. Defaults to T.

If a history with name NAME does not already exist in
`comint-histories--histories', then the new one will be added to the end of
`comint-histories--histories' (giving it lowest selection precedence), and it's
history file will be loaded if :persist is non-nil. Otherwise, if a history with
name NAME does already exist in `comint-histories--histories', then it's
settings will be updated to the new definition, but it's existing history ring
will not be updated other than resizing it to the new :length.

If a history with name NAME already exists in `comint-histories--histories',
then update the props of the existing history to reflect PROPS. Note that in
this case the order of `comint-histories--histories' is preserved, and the
actual saved history for this history is not modified outside changing its
length if :length was changed in PROPS."
  (declare (indent defun))
  (let ((name (symbol-name name))
        (history (list :history nil
                       :predicates nil
                       :filters nil
                       :persist t
                       :defer-load t
                       :loaded nil
                       :length 100
                       :no-dups nil
                       :reselect-after nil
                       :rtrim t
                       :ltrim t))
        (valid-props '(:predicates
                       :filters
                       :persist
                       :defer-load
                       :reselect-after
                       :length
                       :no-dups
                       :rtrim
                       :ltrim)))
    (while props
      (let ((prop (car props))
            (val (cadr props)))
        (if (not (memq prop valid-props))
            (user-error "Invalid history property: %s" prop)
          (setq history (plist-put history prop (eval val)))
          (setq props (cddr props)))))
    (when (null (plist-get history :predicates))
      (user-error ":predicates cannot be NIL"))
    (let ((history- (cons name history)))
      `(let ((history (quote ,history-)))
         (if-let ((existing-history (assoc (car history)
                                           comint-histories--histories)))
             (let ((existing-ring (plist-get (cdr existing-history) :history))
                   (new-length (plist-get (cdr history) :length)))
               (ring-resize existing-ring new-length)
               (setq history (cons (car history)
                                   (plist-put (cdr history)
                                              :history existing-ring)))
               (setf (cdr (assoc (car history) comint-histories--histories))
                     (cdr history)))
           (setf (plist-get (cdr history) :history)
                 (make-ring (plist-get (cdr history) :length)))
           (add-to-list 'comint-histories--histories history t)
           (when (and (plist-get (cdr history) :persist)
                      (not (plist-get (cdr history) :defer-load))
                      (f-file? (comint-histories--history-file history t)))
             (comint-histories--load-history-from-disk history t)
             (setf (plist-get (cdr history) :loaded) t)))))))

(defun comint-histories-search-history (arg &optional history)
  "Search the HISTORY with `completing-read' and insert the selection.

If HISTORY is NIL then if ARG (or prefix arg) prompt for a history else
automatically select the history."
  (interactive "P")
  (let ((history (or history
                     (if arg
                         (let ((history-name
                                (completing-read
                                 "histories: "
                                 (mapcar #'car comint-histories--histories)
                                 nil t)))
                           (assoc history-name comint-histories--histories))
                       (comint-histories--select-history)))))
    (if (not history)
        (user-error "No history could be selected")
      (let ((history-val (completing-read
                          (format "history (%s): " (car history))
                          (ring-elements (plist-get (cdr history) :history))
                          nil t)))
        (goto-char (point-max))
        (delete-region (comint-line-beginning-position) (point))
        (insert history-val)))))

(defun comint-histories-get-prompt ()
  "Return the latest comint prompt in the current buffer as a string."
  (when (derived-mode-p 'comint-mode)
    (save-excursion
      (goto-char (point-max))
      (let ((prompt-end (comint-line-beginning-position))
            (inhibit-field-text-motion t))
        (beginning-of-line)
        (buffer-substring-no-properties (point) prompt-end)))))

(defun comint-histories--history-filter-function (history)
  "Return function to be the `comint-input-filter' based on HISTORYs :filters."
  (lambda (input)
    (cl-every
     (lambda (filter)
       (not (if (functionp filter)
                (funcall filter input)
              (string-match-p filter input))))
     (append comint-histories-global-filters
             (plist-get (cdr history) :filters)))))

(defun comint-histories--history-file (history &optional dont-create)
  "Return the history-file for HISTORY, maybe creating it if it doesn't exist."
  (let* ((dir (or (bound-and-true-p comint-histories-persist-dir)
                  (f-join user-emacs-directory "comint-histories")))
         (file (f-join dir (car history))))
    (when (and (not dont-create) (not (f-directory? dir)))
      (f-mkdir dir))
    (when (and (not dont-create) (not (f-file? file)))
      (f-touch file))
    file))

(defun comint-histories--load-history-from-disk (history &optional insert)
  "Load the history-ring from HISTORY's persistent file returning it as a list.

If INSERT is non-nil then insert the history into HISTORY's history ring."
  (let* ((history-file (comint-histories--history-file history))
         (history-text (f-read-text history-file 'utf-8))
         (length (plist-get (cdr history) :length))
         (lines (seq-take
                 (split-string history-text (format "%c" #x1F) t)
                 length)))
    (when insert
      (let ((comint-input-ring (plist-get (cdr history) :history))
            (comint-input-filter
             (comint-histories--history-filter-function history))
            (comint-input-ring-size (plist-get (cdr history) :length)))
        (dolist (x (reverse lines))
          (comint-add-to-input-history x))))
    lines))

(defun comint-histories--save-history-to-disk (history)
  "Save HISTORY's history-ring to it's persistent file."
  (let* ((history-file (comint-histories--history-file history))
         (existing-history (ring-elements (plist-get (cdr history) :history)))
         (loaded-history (comint-histories--load-history-from-disk history))
         (all-history (append existing-history loaded-history))
         (text ""))
    (when (plist-get (cdr history) :no-dups)
      (setq all-history (delete-dups all-history)))
    (dolist (x (seq-take all-history (plist-get (cdr history) :length)))
      (setq text (concat text (format "%s%c" x #x1F))))
    (f-write-text text 'utf-8 history-file)))

(defun comint-histories--select-history ()
  "Select a history from `comint-histories--histories'.

A history is selected if all of it's :predicates return non-nil when invoked
with zero arguments. If a new history is selected then set `comint-input-ring'
to that histories history ring."
  (let ((selected-history))
    (catch 'loop
      (dolist (history comint-histories--histories)
        (when (cl-every (lambda (fn) (funcall fn))
                        (plist-get (cdr history) :predicates))
          (setq selected-history history)
          (throw 'loop t))))
    (when (and selected-history
               (not (equal (car selected-history)
                           (car comint-histories--last-selected-history))))
      (when (and (not (plist-get (cdr selected-history) :loaded))
                 (plist-get (cdr selected-history) :persist)
                 (f-file? (comint-histories--history-file selected-history t)))
        (comint-histories--load-history-from-disk selected-history t)
        (setf (plist-get (cdr selected-history) :loaded) t))
      (setq-local comint-histories--last-selected-history selected-history)
      (setq-local comint-input-ring (plist-get (cdr selected-history) :history))
      (setq-local comint-input-ring-size
                  (plist-get (cdr selected-history) :length))
      (setq-local comint-input-filter
                  (comint-histories--history-filter-function selected-history)))
    selected-history))

(defun comint-histories-get-input ()
  "Return the content of the comint input buffer."
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc)
        (user-error "Current buffer has no process")
      (save-excursion
        (goto-char (point-max))
        (let ((beg-point (comint-line-beginning-position))
              (end-point (point)))
          (buffer-substring-no-properties beg-point end-point))))))

(defun comint-histories-index-move (hist-idx move-idx)
  "Move the history at index HIST-IDX to index MOVE-IDX in the history list.

If HIST-IDX is NIL then it is assumed to be the maximum index of
`comint-histories--histories'.

Note that indices start at 0."
  (let* ((histories-length (length comint-histories--histories))
         (max-index (- histories-length 1))
         (hist-idx (or hist-idx max-index)))
    (if (= 0 histories-length)
        (user-error "History list is empty")
      (if (= 1 histories-length)
          (user-error "History list only has 1 history")
        (if (or (> 0 hist-idx) (> hist-idx max-index))
            (user-error "HIST-IDX not in range")
          (if (or (> 0 move-idx) (> move-idx max-index))
              (user-error "MOVE-IDX not in range")
            (let* ((history (nth hist-idx comint-histories--histories))
                   (history-list
                    (cl-remove
                     (car history)
                     comint-histories--histories
                     :test #'equal
                     :key #'car))
                   (padded (cons nil history-list))
                   (c (nthcdr move-idx padded)))
              (setcdr c (cons history (cdr c)))
              (setq comint-histories--histories (cdr padded))
              (mapcar #'car comint-histories--histories))))))))

(defun comint-histories--save-histories-to-disk ()
  "Save persistent histories in `comint-histories--histories' to disk."
  (dolist (history comint-histories--histories)
    (when (plist-get (cdr history) :persist)
      (comint-histories--save-history-to-disk history))))

(defun comint-histories--before-add-to-comint-input-ring (args)
  "Advise function to run before `comint-add-to-input-history'.

Uses the most recently selected history as the history. Trims the input if the
:ltrim or :rtrim history options are set. If the :no-dups option is set then
removes duplicate items from `comint-input-ring'."
  (if-let ((history comint-histories--last-selected-history)
           (cmd (car args)))
      (let ((ltrim (plist-get (cdr history) :ltrim))
            (rtrim (plist-get (cdr history) :rtrim))
            (no-dups (plist-get (cdr history) :no-dups)))
        (when ltrim
          (setq cmd (replace-regexp-in-string "^[\n\r ]+" "" cmd)))
        (when rtrim
          (setq cmd (replace-regexp-in-string "[\n\r ]+$" "" cmd)))
        (when no-dups
          (while-let ((idx (ring-member comint-input-ring cmd)))
            (ring-remove comint-input-ring idx)))
        (list cmd))
    args))

(defun comint-histories--around-comint-send-input (orig-fn &rest args)
  "Advise function to run around `comint-add-to-input-history'."
  (comint-histories--select-history)
  (apply orig-fn args)
  (when (plist-get (cdr comint-histories--last-selected-history) :reselect-after)
    (comint-histories--select-history)))

(define-minor-mode comint-histories-mode
  "Toggle `comint-histories-mode'."
  :global t
  :group 'comint-histories
  :require 'comint-histories
  (if comint-histories-mode
      (progn
        (add-hook 'comint-mode-hook #'comint-histories--select-history)
        (advice-add 'comint-send-input :around
                    #'comint-histories--around-comint-send-input)
        (advice-add 'comint-add-to-input-history :filter-args
                    #'comint-histories--before-add-to-comint-input-ring)
        (add-hook 'kill-emacs-hook #'comint-histories--save-histories-to-disk))
    (remove-hook 'comint-mode-hook #'comint-histories--select-history)
    (advice-remove 'comint-send-input
                   #'comint-histories--around-comint-send-input)
    (advice-remove 'comint-add-to-input-history
                   #'comint-histories--before-add-to-comint-input-ring)
    (remove-hook 'kill-emacs-hook #'comint-histories--save-histories-to-disk)))

(provide 'comint-histories)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; comint-histories.el ends here
