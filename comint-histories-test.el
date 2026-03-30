;;; comint-histories-test.el --- Tests for comint-histories -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'comint)
(require 'cl-lib)
(require 'f)
(require 'comint-histories)

;;; --- Test helpers ---

(defvar comint-histories-test--saved-histories nil)
(defvar comint-histories-test--saved-persist-dir nil)
(defvar comint-histories-test--saved-global-filters nil)

(defmacro comint-histories-test--with-clean-state (&rest body)
  "Run BODY with a fresh comint-histories state.
Saves and restores all global state. Uses a temp dir for persistence."
  (declare (indent 0))
  `(let ((comint-histories-test--saved-histories comint-histories--histories)
         (comint-histories-test--saved-persist-dir comint-histories-persist-dir)
         (comint-histories-test--saved-global-filters comint-histories-global-filters)
         (tmpdir (make-temp-file "comint-histories-test-" t)))
     (unwind-protect
         (progn
           (setq comint-histories--histories nil)
           (setq comint-histories-persist-dir tmpdir)
           (setq comint-histories-global-filters nil)
           ,@body)
       (setq comint-histories--histories comint-histories-test--saved-histories)
       (setq comint-histories-persist-dir comint-histories-test--saved-persist-dir)
       (setq comint-histories-global-filters comint-histories-test--saved-global-filters)
       (delete-directory tmpdir t))))

(defun comint-histories-test--make-history (name &rest props)
  "Create a history entry for NAME with PROPS, bypassing the macro.
Returns (NAME . plist) suitable for pushing to `comint-histories--histories'."
  (let ((plist (list :history (make-ring (or (plist-get props :length) 100))
                     :predicates (or (plist-get props :predicates) (list #'always))
                     :filters (or (plist-get props :filters) nil)
                     :persist (if (plist-member props :persist)
                                  (plist-get props :persist)
                                t)
                     :defer-load (if (plist-member props :defer-load)
                                     (plist-get props :defer-load)
                                   t)
                     :loaded nil
                     :length (or (plist-get props :length) 100)
                     :no-dups (plist-get props :no-dups)
                     :reselect-after (plist-get props :reselect-after)
                     :rtrim (if (plist-member props :rtrim)
                                (plist-get props :rtrim)
                              t)
                     :ltrim (if (plist-member props :ltrim)
                                (plist-get props :ltrim)
                              t))))
    (cons name plist)))

;;; --- Filter function tests ---

(ert-deftest comint-histories-test-filter-no-filters ()
  "Filter function with no filters accepts everything."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history "test"))
           (fn (comint-histories--history-filter-function hist)))
      (should (funcall fn "anything"))
      (should (funcall fn ""))
      (should (funcall fn "ls -la")))))

(ert-deftest comint-histories-test-filter-regex ()
  "Regex filter excludes matching inputs."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "test" :filters '("^cd ")))
           (fn (comint-histories--history-filter-function hist)))
      (should-not (funcall fn "cd /tmp"))
      (should (funcall fn "echo hello"))
      (should (funcall fn "lcd something")))))

(ert-deftest comint-histories-test-filter-function ()
  "Function filter excludes when function returns non-nil."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "test" :filters (list (lambda (s) (< (length s) 3)))))
           (fn (comint-histories--history-filter-function hist)))
      (should-not (funcall fn "ab"))
      (should (funcall fn "abc"))
      (should (funcall fn "long command")))))

(ert-deftest comint-histories-test-filter-multiple ()
  "Multiple filters all apply (any match excludes)."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "test" :filters (list "^cd " "^ls$"
                                        (lambda (s) (string-prefix-p "exit" s)))))
           (fn (comint-histories--history-filter-function hist)))
      (should-not (funcall fn "cd /tmp"))
      (should-not (funcall fn "ls"))
      (should-not (funcall fn "exit"))
      (should (funcall fn "echo hello"))
      (should (funcall fn "ls -la")))))

(ert-deftest comint-histories-test-filter-global ()
  "Global filters apply to all histories."
  (comint-histories-test--with-clean-state
    (setq comint-histories-global-filters '("^secret"))
    (let* ((hist (comint-histories-test--make-history
                  "test" :filters '("^cd ")))
           (fn (comint-histories--history-filter-function hist)))
      (should-not (funcall fn "cd /tmp"))
      (should-not (funcall fn "secret stuff"))
      (should (funcall fn "echo hello")))))

;;; --- History selection tests ---

(ert-deftest comint-histories-test-select-first-match ()
  "Selection returns the first history whose predicates all pass."
  (comint-histories-test--with-clean-state
    (let* ((hist-a (comint-histories-test--make-history
                    "a" :predicates (list #'ignore) :persist nil))
           (hist-b (comint-histories-test--make-history
                    "b" :predicates (list #'always) :persist nil))
           (hist-c (comint-histories-test--make-history
                    "c" :predicates (list #'always) :persist nil)))
      (setq comint-histories--histories (list hist-a hist-b hist-c))
      (with-temp-buffer
        (let ((result (comint-histories--select-history)))
          (should (equal (car result) "b")))))))

(ert-deftest comint-histories-test-select-no-match ()
  "Selection returns nil when no predicates match."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "a" :predicates (list #'ignore) :persist nil)))
      (setq comint-histories--histories (list hist))
      (with-temp-buffer
        (should-not (comint-histories--select-history))))))

(ert-deftest comint-histories-test-select-multiple-predicates ()
  "All predicates must pass for a history to be selected."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "a" :predicates (list #'always #'ignore) :persist nil)))
      (setq comint-histories--histories (list hist))
      (with-temp-buffer
        (should-not (comint-histories--select-history))))))

(ert-deftest comint-histories-test-select-sets-buffer-locals ()
  "Selection sets comint-input-ring and related buffer-local vars."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "a" :predicates (list #'always) :persist nil :length 50)))
      (setq comint-histories--histories (list hist))
      (with-temp-buffer
        (comint-histories--select-history)
        (should (eq comint-input-ring (plist-get (cdr hist) :history)))
        (should (= comint-input-ring-size 50))
        (should (functionp comint-input-filter))))))

(ert-deftest comint-histories-test-select-no-switch-when-same ()
  "Selection does not re-set buffer locals when same history already selected."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "a" :predicates (list #'always) :persist nil)))
      (setq comint-histories--histories (list hist))
      (with-temp-buffer
        (comint-histories--select-history)
        (let ((old-filter comint-input-filter))
          (comint-histories--select-history)
          ;; Same object, not a new closure
          (should (eq comint-input-filter old-filter)))))))

;;; --- Persistence tests ---

(ert-deftest comint-histories-test-save-load-roundtrip ()
  "Saving and loading a history preserves entries."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "roundtrip" :persist t :length 10)))
      (ring-insert (plist-get (cdr hist) :history) "cmd-1")
      (ring-insert (plist-get (cdr hist) :history) "cmd-2")
      (ring-insert (plist-get (cdr hist) :history) "cmd-3")
      (comint-histories--save-history-to-disk hist)
      ;; Create a fresh history with empty ring
      (let* ((hist2 (comint-histories-test--make-history
                     "roundtrip" :persist t :length 10))
             (loaded (comint-histories--load-history-from-disk hist2 t))
             (ring (plist-get (cdr hist2) :history))
             (elts (ring-elements ring)))
        (should (= 3 (length loaded)))
        (should (member "cmd-1" elts))
        (should (member "cmd-2" elts))
        (should (member "cmd-3" elts))))))

(ert-deftest comint-histories-test-save-respects-length ()
  "Saving truncates to :length."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "truncate" :persist t :length 3)))
      (ring-insert (plist-get (cdr hist) :history) "a")
      (ring-insert (plist-get (cdr hist) :history) "b")
      (ring-insert (plist-get (cdr hist) :history) "c")
      (comint-histories--save-history-to-disk hist)
      (let ((hist2 (comint-histories-test--make-history
                    "truncate" :persist t :length 3)))
        (comint-histories--load-history-from-disk hist2 t)
        (should (= 3 (ring-length (plist-get (cdr hist2) :history))))))))

(ert-deftest comint-histories-test-save-dedup ()
  "Saving with :no-dups removes duplicates."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "dedup" :persist t :no-dups t :length 10)))
      (ring-insert (plist-get (cdr hist) :history) "dup")
      (ring-insert (plist-get (cdr hist) :history) "dup")
      (ring-insert (plist-get (cdr hist) :history) "unique")
      (comint-histories--save-history-to-disk hist)
      (let ((hist2 (comint-histories-test--make-history
                    "dedup" :persist t :length 10)))
        (let ((loaded (comint-histories--load-history-from-disk hist2)))
          (should (= 2 (length loaded))))))))

(ert-deftest comint-histories-test-save-merges-disk-and-memory ()
  "Saving merges in-memory ring with on-disk history."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "merge" :persist t :length 10)))
      ;; Write some initial history to disk
      (ring-insert (plist-get (cdr hist) :history) "from-disk")
      (comint-histories--save-history-to-disk hist)
      ;; Now create new history with only in-memory entries
      (let ((hist2 (comint-histories-test--make-history
                    "merge" :persist t :length 10)))
        (ring-insert (plist-get (cdr hist2) :history) "from-memory")
        (comint-histories--save-history-to-disk hist2)
        ;; Load and verify both are present
        (let ((hist3 (comint-histories-test--make-history
                      "merge" :persist t :length 10)))
          (let ((loaded (comint-histories--load-history-from-disk hist3)))
            (should (member "from-disk" loaded))
            (should (member "from-memory" loaded))))))))

(ert-deftest comint-histories-test-defer-load ()
  "Deferred histories load on first selection, not at creation."
  (comint-histories-test--with-clean-state
    ;; Write a history file
    (let* ((hist (comint-histories-test--make-history
                  "deferred" :persist t :length 10)))
      (ring-insert (plist-get (cdr hist) :history) "deferred-cmd")
      (comint-histories--save-history-to-disk hist))
    ;; Create a new history that hasn't been loaded
    (let* ((hist (comint-histories-test--make-history
                  "deferred" :persist t :defer-load t :length 10
                  :predicates (list #'always))))
      (setq comint-histories--histories (list hist))
      (should-not (plist-get (cdr hist) :loaded))
      (should (= 0 (ring-length (plist-get (cdr hist) :history))))
      ;; Selecting should trigger load
      (with-temp-buffer
        (comint-histories--select-history)
        (should (plist-get (cdr hist) :loaded))
        (should (= 1 (ring-length (plist-get (cdr hist) :history))))))))

(ert-deftest comint-histories-test-history-file-path ()
  "History file path uses persist-dir and history name."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history "my-hist"))
           (file (comint-histories--history-file hist t)))
      (should (string= file (f-join comint-histories-persist-dir "my-hist"))))))

;;; --- Add history macro tests ---

(ert-deftest comint-histories-test-add-history-basic ()
  "Adding a history creates it with correct defaults."
  (comint-histories-test--with-clean-state
    (comint-histories-add-history test-basic
      :predicates (list #'always)
      :persist nil)
    (should (= 1 (length comint-histories--histories)))
    (let ((hist (car comint-histories--histories)))
      (should (equal "test-basic" (car hist)))
      (should (= 100 (plist-get (cdr hist) :length)))
      (should (eq t (plist-get (cdr hist) :ltrim)))
      (should (eq t (plist-get (cdr hist) :rtrim)))
      (should (null (plist-get (cdr hist) :no-dups)))
      (should (null (plist-get (cdr hist) :reselect-after)))
      (should (ring-p (plist-get (cdr hist) :history))))))

(ert-deftest comint-histories-test-add-history-custom-props ()
  "Adding a history respects custom properties."
  (comint-histories-test--with-clean-state
    (comint-histories-add-history test-custom
      :predicates (list #'always)
      :persist nil
      :length 50
      :no-dups t
      :ltrim nil
      :rtrim nil)
    (let ((hist (car comint-histories--histories)))
      (should (= 50 (plist-get (cdr hist) :length)))
      (should (eq t (plist-get (cdr hist) :no-dups)))
      (should (null (plist-get (cdr hist) :ltrim)))
      (should (null (plist-get (cdr hist) :rtrim))))))

(ert-deftest comint-histories-test-add-history-update-existing ()
  "Re-adding a history updates props but preserves ring contents."
  (comint-histories-test--with-clean-state
    (comint-histories-add-history test-update
      :predicates (list #'always)
      :persist nil
      :length 50)
    (let ((hist (assoc "test-update" comint-histories--histories)))
      (ring-insert (plist-get (cdr hist) :history) "preserved"))
    ;; Re-add with different length
    (comint-histories-add-history test-update
      :predicates (list #'always)
      :persist nil
      :length 200)
    (should (= 1 (length comint-histories--histories)))
    (let ((hist (assoc "test-update" comint-histories--histories)))
      (should (= 200 (plist-get (cdr hist) :length)))
      (should (member "preserved"
                      (ring-elements (plist-get (cdr hist) :history)))))))

(ert-deftest comint-histories-test-add-history-preserves-order ()
  "Re-adding a history preserves its position in the list."
  (comint-histories-test--with-clean-state
    (comint-histories-add-history first
      :predicates (list #'always) :persist nil)
    (comint-histories-add-history second
      :predicates (list #'always) :persist nil)
    (comint-histories-add-history third
      :predicates (list #'always) :persist nil)
    ;; Re-add second
    (comint-histories-add-history second
      :predicates (list #'always) :persist nil :length 200)
    (should (equal (mapcar #'car comint-histories--histories)
                   '("first" "second" "third")))))

(ert-deftest comint-histories-test-add-history-invalid-prop ()
  "Invalid property signals an error."
  (should-error
   (macroexpand '(comint-histories-add-history bad
                   :predicates (list #'always)
                   :bogus t))
   :type 'user-error))

(ert-deftest comint-histories-test-add-history-no-predicates ()
  "Missing :predicates signals an error."
  (should-error
   (macroexpand '(comint-histories-add-history bad
                   :persist nil))
   :type 'user-error))

;;; --- Input ring advice tests ---

(ert-deftest comint-histories-test-advice-ltrim ()
  "Left trim removes leading whitespace."
  (with-temp-buffer
    (let* ((hist (comint-histories-test--make-history
                  "trim" :ltrim t :rtrim nil))
           (comint-histories--last-selected-history hist))
      (should (equal (comint-histories--before-add-to-comint-input-ring
                      '("  hello"))
                     '("hello"))))))

(ert-deftest comint-histories-test-advice-rtrim ()
  "Right trim removes trailing whitespace."
  (with-temp-buffer
    (let* ((hist (comint-histories-test--make-history
                  "trim" :ltrim nil :rtrim t))
           (comint-histories--last-selected-history hist))
      (should (equal (comint-histories--before-add-to-comint-input-ring
                      '("hello  "))
                     '("hello"))))))

(ert-deftest comint-histories-test-advice-both-trim ()
  "Both trims applied together."
  (with-temp-buffer
    (let* ((hist (comint-histories-test--make-history
                  "trim" :ltrim t :rtrim t))
           (comint-histories--last-selected-history hist))
      (should (equal (comint-histories--before-add-to-comint-input-ring
                      '("  hello  "))
                     '("hello"))))))

(ert-deftest comint-histories-test-advice-no-trim ()
  "No trimming when both disabled."
  (with-temp-buffer
    (let* ((hist (comint-histories-test--make-history
                  "trim" :ltrim nil :rtrim nil))
           (comint-histories--last-selected-history hist))
      (should (equal (comint-histories--before-add-to-comint-input-ring
                      '("  hello  "))
                     '("  hello  "))))))

(ert-deftest comint-histories-test-advice-trim-newlines ()
  "Trimming removes newlines and carriage returns."
  (with-temp-buffer
    (let* ((hist (comint-histories-test--make-history
                  "trim" :ltrim t :rtrim t))
           (comint-histories--last-selected-history hist))
      (should (equal (comint-histories--before-add-to-comint-input-ring
                      '("\n\r hello\n\r "))
                     '("hello"))))))

(ert-deftest comint-histories-test-advice-no-dups ()
  "Dedup removes existing duplicate from ring before adding."
  (with-temp-buffer
    (let* ((hist (comint-histories-test--make-history
                  "dedup" :no-dups t :ltrim nil :rtrim nil))
           (ring (plist-get (cdr hist) :history))
           (comint-histories--last-selected-history hist)
           (comint-input-ring ring))
      (ring-insert ring "dup")
      (ring-insert ring "other")
      (comint-histories--before-add-to-comint-input-ring '("dup"))
      ;; The old "dup" should be removed from the ring
      (should (= 1 (ring-length ring)))
      (should (equal "other" (ring-ref ring 0))))))

(ert-deftest comint-histories-test-advice-passthrough-no-history ()
  "When no history is selected, args pass through unchanged."
  (with-temp-buffer
    (let ((comint-histories--last-selected-history nil))
      (should (equal (comint-histories--before-add-to-comint-input-ring
                      '("  hello  "))
                     '("  hello  "))))))

;;; --- Index move tests ---

(ert-deftest comint-histories-test-index-move-basic ()
  "Move a history from one position to another."
  (comint-histories-test--with-clean-state
    (let ((a (comint-histories-test--make-history "a" :persist nil))
          (b (comint-histories-test--make-history "b" :persist nil))
          (c (comint-histories-test--make-history "c" :persist nil)))
      (setq comint-histories--histories (list a b c))
      (comint-histories-index-move 2 0)
      (should (equal (mapcar #'car comint-histories--histories)
                     '("c" "a" "b"))))))

(ert-deftest comint-histories-test-index-move-nil-hist-idx ()
  "NIL hist-idx defaults to max index."
  (comint-histories-test--with-clean-state
    (let ((a (comint-histories-test--make-history "a" :persist nil))
          (b (comint-histories-test--make-history "b" :persist nil))
          (c (comint-histories-test--make-history "c" :persist nil)))
      (setq comint-histories--histories (list a b c))
      (comint-histories-index-move nil 0)
      (should (equal (mapcar #'car comint-histories--histories)
                     '("c" "a" "b"))))))

(ert-deftest comint-histories-test-index-move-empty-error ()
  "Moving in an empty list signals an error."
  (comint-histories-test--with-clean-state
    (should-error (comint-histories-index-move 0 0) :type 'user-error)))

(ert-deftest comint-histories-test-index-move-single-error ()
  "Moving in a single-element list signals an error."
  (comint-histories-test--with-clean-state
    (setq comint-histories--histories
          (list (comint-histories-test--make-history "a" :persist nil)))
    (should-error (comint-histories-index-move 0 0) :type 'user-error)))

(ert-deftest comint-histories-test-index-move-out-of-range ()
  "Out of range indices signal errors."
  (comint-histories-test--with-clean-state
    (let ((a (comint-histories-test--make-history "a" :persist nil))
          (b (comint-histories-test--make-history "b" :persist nil)))
      (setq comint-histories--histories (list a b))
      (should-error (comint-histories-index-move 5 0) :type 'user-error)
      (should-error (comint-histories-index-move 0 5) :type 'user-error))))

;;; --- Reselect-after / timing tests ---

(ert-deftest comint-histories-test-reselect-sets-pending-flag-and-timer ()
  "After send-input, :reselect-after sets the pending flag and starts a timer."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "reselect" :predicates (list #'always)
                  :persist nil :reselect-after t)))
      (setq comint-histories--histories (list hist))
      (with-temp-buffer
        (comint-histories--select-history)
        (comint-histories--around-comint-send-input #'ignore)
        (should comint-histories--pending-reselect)
        (should (timerp comint-histories--reselect-timer))
        ;; Clean up timer
        (cancel-timer comint-histories--reselect-timer)))))

(ert-deftest comint-histories-test-no-pending-without-reselect-after ()
  "Without :reselect-after, no pending flag is set."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "no-reselect" :predicates (list #'always)
                  :persist nil :reselect-after nil)))
      (setq comint-histories--histories (list hist))
      (with-temp-buffer
        (comint-histories--select-history)
        (comint-histories--around-comint-send-input #'ignore)
        (should-not comint-histories--pending-reselect)))))

(ert-deftest comint-histories-test-output-filter-clears-pending-and-timer ()
  "Output filter clears the pending flag, cancels timer, and reselects."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "reselect" :predicates (list #'always)
                  :persist nil :reselect-after t)))
      (setq comint-histories--histories (list hist))
      (with-temp-buffer
        (comint-histories--select-history)
        (comint-histories--around-comint-send-input #'ignore)
        (should comint-histories--pending-reselect)
        (should (timerp comint-histories--reselect-timer))
        ;; Output arrives, should clear both
        (comint-histories--output-filter "some output")
        (should-not comint-histories--pending-reselect)
        (should-not comint-histories--reselect-timer)))))

(ert-deftest comint-histories-test-output-filter-noop-without-pending ()
  "Output filter does nothing when no reselection is pending."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "a" :predicates (list #'always) :persist nil)))
      (setq comint-histories--histories (list hist))
      (with-temp-buffer
        (comint-histories--select-history)
        (let ((old-filter comint-input-filter))
          (comint-histories--output-filter "some output")
          ;; Nothing should change
          (should (eq comint-input-filter old-filter)))))))

(ert-deftest comint-histories-test-reselect-switches-history-on-output ()
  "Reselect-after switches to a different history when predicates change."
  (comint-histories-test--with-clean-state
    (let* ((switch-flag nil)
           (hist-a (comint-histories-test--make-history
                    "a" :predicates (list (lambda () (not switch-flag)))
                    :persist nil :reselect-after t))
           (hist-b (comint-histories-test--make-history
                    "b" :predicates (list (lambda () switch-flag))
                    :persist nil)))
      (setq comint-histories--histories (list hist-a hist-b))
      (with-temp-buffer
        ;; Initially selects "a"
        (comint-histories--select-history)
        (should (equal "a" (car comint-histories--last-selected-history)))
        ;; Simulate send-input (sets pending flag)
        (comint-histories--around-comint-send-input #'ignore)
        (should comint-histories--pending-reselect)
        ;; Predicates now favor "b" (simulating changed process state)
        (setq switch-flag t)
        ;; Output arrives, triggering reselection
        (comint-histories--output-filter "new prompt")
        (should-not comint-histories--pending-reselect)
        (should (equal "b" (car comint-histories--last-selected-history)))
        (should (eq comint-input-ring (plist-get (cdr hist-b) :history)))))))

;;; --- Mode enable/disable tests ---

(ert-deftest comint-histories-test-mode-enable-disable ()
  "Enabling and disabling the mode adds and removes hooks/advice."
  (comint-histories-test--with-clean-state
    (unwind-protect
        (progn
          (comint-histories-mode 1)
          (should (memq #'comint-histories--select-history
                        comint-mode-hook))
          (should (memq #'comint-histories--output-filter
                        comint-output-filter-functions))
          (should (memq #'comint-histories--save-histories-to-disk
                        kill-emacs-hook))
          (should (advice-member-p
                   #'comint-histories--around-comint-send-input
                   'comint-send-input))
          (should (advice-member-p
                   #'comint-histories--before-add-to-comint-input-ring
                   'comint-add-to-input-history))
          (comint-histories-mode -1)
          (should-not (memq #'comint-histories--select-history
                            comint-mode-hook))
          (should-not (memq #'comint-histories--output-filter
                            comint-output-filter-functions))
          (should-not (memq #'comint-histories--save-histories-to-disk
                            kill-emacs-hook))
          (should-not (advice-member-p
                       #'comint-histories--around-comint-send-input
                       'comint-send-input))
          (should-not (advice-member-p
                       #'comint-histories--before-add-to-comint-input-ring
                       'comint-add-to-input-history)))
      ;; Ensure mode is off even if test fails
      (comint-histories-mode -1))))

;;; --- History file separator tests ---

(ert-deftest comint-histories-test-separator-in-entries ()
  "Entries containing newlines survive save/load roundtrip."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "newlines" :persist t :length 10)))
      (ring-insert (plist-get (cdr hist) :history) "line1\nline2")
      (ring-insert (plist-get (cdr hist) :history) "simple")
      (comint-histories--save-history-to-disk hist)
      (let* ((hist2 (comint-histories-test--make-history
                     "newlines" :persist t :length 10))
             (loaded (comint-histories--load-history-from-disk hist2)))
        (should (= 2 (length loaded)))
        (should (member "line1\nline2" loaded))
        (should (member "simple" loaded))))))

;;; --- Edge case tests ---

(ert-deftest comint-histories-test-empty-histories-list ()
  "Selection with empty histories list returns nil."
  (comint-histories-test--with-clean-state
    (with-temp-buffer
      (should-not (comint-histories--select-history)))))

(ert-deftest comint-histories-test-filter-empty-string ()
  "Filter function handles empty string input."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "test" :filters '(".")))
           (fn (comint-histories--history-filter-function hist)))
      ;; "." matches any char, empty string should pass
      (should (funcall fn ""))
      (should-not (funcall fn "x")))))

(ert-deftest comint-histories-test-persist-empty-ring ()
  "Saving an empty ring creates a valid (empty) file."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "empty" :persist t :length 10)))
      (comint-histories--save-history-to-disk hist)
      (let* ((hist2 (comint-histories-test--make-history
                     "empty" :persist t :length 10))
             (loaded (comint-histories--load-history-from-disk hist2)))
        (should (= 0 (length loaded)))))))

(ert-deftest comint-histories-test-send-input-no-histories ()
  "Around advice works when no histories are defined."
  (comint-histories-test--with-clean-state
    (with-temp-buffer
      (let ((called nil))
        (comint-histories--around-comint-send-input
         (lambda (&rest _) (setq called t)))
        (should called)
        (should-not comint-histories--pending-reselect)))))

;;; --- Live comint buffer tests ---

(defmacro comint-histories-test--with-comint-buffer (name program args &rest body)
  "Run BODY with a live comint buffer running PROGRAM with ARGS bound to NAME.
The buffer and process are cleaned up afterward."
  (declare (indent 3))
  `(let ((,name (apply #'make-comint-in-buffer
                       "comint-histories-test" nil ,program nil ,args)))
     (unwind-protect
         (with-current-buffer ,name
           (let ((proc (get-buffer-process ,name)))
             (while (not (eq (process-status proc) 'run))
               (accept-process-output proc 0.1)))
           ,@body)
       (let ((proc (get-buffer-process ,name)))
         (when proc
           (delete-process proc)))
       (when (buffer-live-p ,name)
         (kill-buffer ,name)))))

(defmacro comint-histories-test--with-shell-buffer (name &rest body)
  "Run BODY with a live sh comint buffer bound to NAME."
  (declare (indent 1))
  `(comint-histories-test--with-comint-buffer ,name
       "sh" '("-i")
     ,@body))

(ert-deftest comint-histories-test-get-input-live-buffer ()
  "get-input returns the text in the comint input area."
  (comint-histories-test--with-shell-buffer buf
    (goto-char (point-max))
    (insert "test-input")
    (should (equal "test-input" (comint-histories-get-input)))))

(ert-deftest comint-histories-test-get-input-empty ()
  "get-input returns empty string when no input typed."
  (comint-histories-test--with-shell-buffer buf
    (goto-char (point-max))
    (should (equal "" (comint-histories-get-input)))))

(ert-deftest comint-histories-test-get-input-no-process ()
  "get-input signals error when buffer has no process."
  (with-temp-buffer
    (should-error (comint-histories-get-input) :type 'user-error)))

(ert-deftest comint-histories-test-get-prompt-non-comint ()
  "get-prompt returns nil in non-comint buffers."
  (with-temp-buffer
    (should-not (comint-histories-get-prompt))))

(ert-deftest comint-histories-test-full-send-input-chain ()
  "Full advice chain: send-input adds trimmed entry to selected history ring."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "shell" :predicates (list #'always)
                  :persist nil :ltrim t :rtrim t)))
      (setq comint-histories--histories (list hist))
      (unwind-protect
          (progn
            (comint-histories-mode 1)
            (comint-histories-test--with-shell-buffer buf
              ;; Select history for this buffer
              (comint-histories--select-history)
              (goto-char (point-max))
              (insert "  echo hello  ")
              (comint-send-input)
              (accept-process-output (get-buffer-process buf) 0.5)
              ;; The trimmed input should be in the ring
              (let ((elts (ring-elements (plist-get (cdr hist) :history))))
                (should (member "echo hello" elts)))))
        (comint-histories-mode -1)))))

(ert-deftest comint-histories-test-full-send-input-filter-excludes ()
  "Full advice chain: filtered input is not added to history ring."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "shell" :predicates (list #'always)
                  :persist nil :filters '("^secret"))))
      (setq comint-histories--histories (list hist))
      (unwind-protect
          (progn
            (comint-histories-mode 1)
            (comint-histories-test--with-shell-buffer buf
              (comint-histories--select-history)
              (goto-char (point-max))
              (insert "secret password")
              (comint-send-input)
              (accept-process-output (get-buffer-process buf) 0.5)
              (should (= 0 (ring-length
                            (plist-get (cdr hist) :history))))))
        (comint-histories-mode -1)))))

(ert-deftest comint-histories-test-full-send-input-no-dups ()
  "Full chain: :no-dups removes older duplicate on send."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "shell" :predicates (list #'always)
                  :persist nil :no-dups t)))
      (setq comint-histories--histories (list hist))
      (unwind-protect
          (progn
            (comint-histories-mode 1)
            (comint-histories-test--with-shell-buffer buf
              (comint-histories--select-history)
              ;; Send same command twice
              (goto-char (point-max))
              (insert "dup-cmd")
              (comint-send-input)
              (accept-process-output (get-buffer-process buf) 0.5)
              (goto-char (point-max))
              (insert "dup-cmd")
              (comint-send-input)
              (accept-process-output (get-buffer-process buf) 0.5)
              (let ((ring (plist-get (cdr hist) :history)))
                (should (= 1 (ring-length ring)))
                (should (equal "dup-cmd" (ring-ref ring 0))))))
        (comint-histories-mode -1)))))

(ert-deftest comint-histories-test-full-reselect-after-on-output ()
  "Full chain: :reselect-after triggers reselection when output arrives."
  (comint-histories-test--with-clean-state
    (let* ((switch-flag nil)
           (hist-a (comint-histories-test--make-history
                    "a" :predicates (list (lambda () (not switch-flag)))
                    :persist nil :reselect-after t))
           (hist-b (comint-histories-test--make-history
                    "b" :predicates (list (lambda () switch-flag))
                    :persist nil)))
      (setq comint-histories--histories (list hist-a hist-b))
      (unwind-protect
          (progn
            (comint-histories-mode 1)
            (comint-histories-test--with-shell-buffer buf
              (comint-histories--select-history)
              (should (equal "a" (car comint-histories--last-selected-history)))
              (goto-char (point-max))
              (insert "echo trigger")
              (setq switch-flag t)
              (comint-send-input)
              ;; Wait for sh to produce output, triggering output filter
              (accept-process-output (get-buffer-process buf) 1)
              (should (equal "b"
                             (car comint-histories--last-selected-history)))))
        (comint-histories-mode -1)))))

(ert-deftest comint-histories-test-reselect-timer-fallback ()
  "Timer fallback triggers reselection when no output arrives."
  (comint-histories-test--with-clean-state
    (let* ((switch-flag nil)
           (hist-a (comint-histories-test--make-history
                    "a" :predicates (list (lambda () (not switch-flag)))
                    :persist nil :reselect-after t))
           (hist-b (comint-histories-test--make-history
                    "b" :predicates (list (lambda () switch-flag))
                    :persist nil)))
      (setq comint-histories--histories (list hist-a hist-b))
      (with-temp-buffer
        (comint-histories--select-history)
        (should (equal "a" (car comint-histories--last-selected-history)))
        ;; Simulate send-input setting the pending flag + timer
        (comint-histories--around-comint-send-input #'ignore)
        (should comint-histories--pending-reselect)
        (should (timerp comint-histories--reselect-timer))
        ;; Change predicates to favor "b"
        (setq switch-flag t)
        ;; Let the timer fire (0.5s timer + margin)
        (sleep-for 1)
        (should-not comint-histories--pending-reselect)
        (should (equal "b" (car comint-histories--last-selected-history)))))))

(ert-deftest comint-histories-test-mode-hook-selects-on-new-buffer ()
  "comint-mode-hook selects history when a new comint buffer is created."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "auto" :predicates (list #'always) :persist nil)))
      (setq comint-histories--histories (list hist))
      (unwind-protect
          (progn
            (comint-histories-mode 1)
            (comint-histories-test--with-shell-buffer buf
              ;; comint-mode-hook should have fired --select-history
              (should comint-histories--last-selected-history)
              (should (equal "auto"
                             (car comint-histories--last-selected-history)))))
        (comint-histories-mode -1)))))

;;; --- Search history tests (mocked completing-read) ---

(ert-deftest comint-histories-test-search-history-inserts-selection ()
  "search-history inserts the selected history item at point."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "search" :predicates (list #'always) :persist nil)))
      (ring-insert (plist-get (cdr hist) :history) "cmd-one")
      (ring-insert (plist-get (cdr hist) :history) "cmd-two")
      (setq comint-histories--histories (list hist))
      (comint-histories-test--with-shell-buffer buf
        (comint-histories--select-history)
        (goto-char (point-max))
        (insert "existing-text")
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "cmd-one")))
          (comint-histories-search-history nil))
        (should (equal "cmd-one"
                       (buffer-substring
                        (comint-line-beginning-position) (point-max))))))))

(ert-deftest comint-histories-test-search-history-with-explicit-history ()
  "search-history works when passed an explicit history."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "explicit" :predicates (list #'always) :persist nil)))
      (ring-insert (plist-get (cdr hist) :history) "explicit-cmd")
      (setq comint-histories--histories (list hist))
      (comint-histories-test--with-shell-buffer buf
        (comint-histories--select-history)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "explicit-cmd")))
          (comint-histories-search-history nil hist))
        (should (equal "explicit-cmd"
                       (buffer-substring
                        (comint-line-beginning-position) (point-max))))))))

(ert-deftest comint-histories-test-search-history-no-selection-error ()
  "search-history errors when no history can be selected."
  (comint-histories-test--with-clean-state
    (comint-histories-test--with-shell-buffer buf
      (should-error (comint-histories-search-history nil)
                    :type 'user-error))))

;;; --- Eager load tests ---

(ert-deftest comint-histories-test-eager-load-on-add ()
  "With :defer-load nil, add-history loads from disk immediately."
  (comint-histories-test--with-clean-state
    ;; Write a history file first
    (let* ((hist (comint-histories-test--make-history
                  "eager" :persist t :length 10)))
      (ring-insert (plist-get (cdr hist) :history) "eager-cmd")
      (comint-histories--save-history-to-disk hist))
    ;; Now add-history with defer-load nil should load it
    (comint-histories-add-history eager
      :predicates (list #'always)
      :persist t
      :defer-load nil)
    (let* ((hist (assoc "eager" comint-histories--histories))
           (ring (plist-get (cdr hist) :history)))
      (should (plist-get (cdr hist) :loaded))
      (should (member "eager-cmd" (ring-elements ring))))))

(ert-deftest comint-histories-test-eager-load-no-file ()
  "With :defer-load nil but no file on disk, history starts empty."
  (comint-histories-test--with-clean-state
    (comint-histories-add-history no-file
      :predicates (list #'always)
      :persist t
      :defer-load nil)
    (let* ((hist (assoc "no-file" comint-histories--histories))
           (ring (plist-get (cdr hist) :history)))
      (should (= 0 (ring-length ring))))))

;;; --- Save all histories tests ---

(ert-deftest comint-histories-test-save-all-persistent ()
  "save-histories-to-disk saves all persistent histories."
  (comint-histories-test--with-clean-state
    (let* ((hist-a (comint-histories-test--make-history
                    "save-a" :persist t :length 10))
           (hist-b (comint-histories-test--make-history
                    "save-b" :persist t :length 10))
           (hist-c (comint-histories-test--make-history
                    "save-c" :persist nil :length 10)))
      (ring-insert (plist-get (cdr hist-a) :history) "a-cmd")
      (ring-insert (plist-get (cdr hist-b) :history) "b-cmd")
      (ring-insert (plist-get (cdr hist-c) :history) "c-cmd")
      (setq comint-histories--histories (list hist-a hist-b hist-c))
      (comint-histories--save-histories-to-disk)
      ;; Persistent ones should have files
      (should (f-file? (f-join comint-histories-persist-dir "save-a")))
      (should (f-file? (f-join comint-histories-persist-dir "save-b")))
      ;; Non-persistent should not
      (should-not (f-file? (f-join comint-histories-persist-dir "save-c"))))))

;;; --- History file management tests ---

(ert-deftest comint-histories-test-history-file-creates-dir ()
  "history-file creates the persist directory if it doesn't exist."
  (comint-histories-test--with-clean-state
    (let* ((subdir (f-join comint-histories-persist-dir "nested"))
           (comint-histories-persist-dir subdir)
           (hist (comint-histories-test--make-history "create-test")))
      (should-not (f-directory? subdir))
      (comint-histories--history-file hist)
      (should (f-directory? subdir))
      (should (f-file? (f-join subdir "create-test"))))))

(ert-deftest comint-histories-test-history-file-dont-create ()
  "history-file with dont-create does not create files."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history "no-create"))
           (file (comint-histories--history-file hist t)))
      (should (stringp file))
      (should-not (f-file? file)))))

;;; --- Multiple buffer isolation tests ---

(ert-deftest comint-histories-test-buffer-local-isolation ()
  "Different buffers independently select different histories."
  (comint-histories-test--with-clean-state
    (let* ((flag-a t)
           (hist-a (comint-histories-test--make-history
                    "a" :predicates (list (lambda () flag-a)) :persist nil))
           (hist-b (comint-histories-test--make-history
                    "b" :predicates (list (lambda () (not flag-a))) :persist nil)))
      (setq comint-histories--histories (list hist-a hist-b))
      (let (buf-a-selection buf-b-selection)
        (with-temp-buffer
          (setq flag-a t)
          (comint-histories--select-history)
          (setq buf-a-selection (car comint-histories--last-selected-history)))
        (with-temp-buffer
          (setq flag-a nil)
          (comint-histories--select-history)
          (setq buf-b-selection (car comint-histories--last-selected-history)))
        (should (equal "a" buf-a-selection))
        (should (equal "b" buf-b-selection))))))

(ert-deftest comint-histories-test-pending-reselect-buffer-local ()
  "Pending reselect flag is buffer-local, not shared."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "a" :predicates (list #'always)
                  :persist nil :reselect-after t)))
      (setq comint-histories--histories (list hist))
      (let (buf-a buf-b)
        (setq buf-a (generate-new-buffer " *test-a*"))
        (setq buf-b (generate-new-buffer " *test-b*"))
        (unwind-protect
            (progn
              (with-current-buffer buf-a
                (comint-histories--select-history)
                (comint-histories--around-comint-send-input #'ignore)
                (should comint-histories--pending-reselect))
              (with-current-buffer buf-b
                (should-not comint-histories--pending-reselect)))
          (with-current-buffer buf-a
            (when (timerp comint-histories--reselect-timer)
              (cancel-timer comint-histories--reselect-timer)))
          (kill-buffer buf-a)
          (kill-buffer buf-b))))))

;;; --- Ring overflow tests ---

(ert-deftest comint-histories-test-ring-overflow ()
  "Ring drops oldest entries when exceeding :length."
  (comint-histories-test--with-clean-state
    (let* ((hist (comint-histories-test--make-history
                  "small" :persist nil :length 3)))
      (let ((ring (plist-get (cdr hist) :history)))
        (ring-insert ring "first")
        (ring-insert ring "second")
        (ring-insert ring "third")
        (ring-insert ring "fourth")
        (should (= 3 (ring-length ring)))
        (let ((elts (ring-elements ring)))
          (should-not (member "first" elts))
          (should (member "fourth" elts)))))))

(ert-deftest comint-histories-test-persist-ring-overflow ()
  "Persistence respects length even when disk has more entries."
  (comint-histories-test--with-clean-state
    ;; Save 5 entries
    (let* ((hist (comint-histories-test--make-history
                  "overflow" :persist t :length 5)))
      (dolist (cmd '("a" "b" "c" "d" "e"))
        (ring-insert (plist-get (cdr hist) :history) cmd))
      (comint-histories--save-history-to-disk hist))
    ;; Load into a smaller ring
    (let* ((hist2 (comint-histories-test--make-history
                   "overflow" :persist t :length 3))
           (loaded (comint-histories--load-history-from-disk hist2)))
      (should (= 3 (length loaded))))))

;;; --- Advice passthrough tests ---

(ert-deftest comint-histories-test-advice-nil-cmd ()
  "Advice passes through when cmd is nil."
  (with-temp-buffer
    (let* ((hist (comint-histories-test--make-history
                  "test" :ltrim t :rtrim t))
           (comint-histories--last-selected-history hist))
      (should (equal (comint-histories--before-add-to-comint-input-ring '(nil))
                     '(nil))))))

(ert-deftest comint-histories-test-advice-multiple-dups ()
  "Dedup removes all duplicate occurrences from ring."
  (with-temp-buffer
    (let* ((hist (comint-histories-test--make-history
                  "dedup" :no-dups t :ltrim nil :rtrim nil))
           (ring (plist-get (cdr hist) :history))
           (comint-histories--last-selected-history hist)
           (comint-input-ring ring))
      (ring-insert ring "dup")
      (ring-insert ring "other")
      (ring-insert ring "dup")
      (ring-insert ring "dup")
      (comint-histories--before-add-to-comint-input-ring '("dup"))
      (should (= 1 (ring-length ring)))
      (should (equal "other" (ring-ref ring 0))))))

(ert-deftest comint-histories-test-advice-trim-only-whitespace ()
  "Trimming a whitespace-only string results in empty string."
  (with-temp-buffer
    (let* ((hist (comint-histories-test--make-history
                  "trim" :ltrim t :rtrim t))
           (comint-histories--last-selected-history hist))
      (should (equal (comint-histories--before-add-to-comint-input-ring
                      '("   \n\r  "))
                     '(""))))))

;;; --- Index move additional tests ---

(ert-deftest comint-histories-test-index-move-to-end ()
  "Move a history to the last position."
  (comint-histories-test--with-clean-state
    (let ((a (comint-histories-test--make-history "a" :persist nil))
          (b (comint-histories-test--make-history "b" :persist nil))
          (c (comint-histories-test--make-history "c" :persist nil)))
      (setq comint-histories--histories (list a b c))
      (comint-histories-index-move 0 2)
      (should (equal (mapcar #'car comint-histories--histories)
                     '("b" "c" "a"))))))

(ert-deftest comint-histories-test-index-move-returns-order ()
  "index-move returns the new ordering as a list of names."
  (comint-histories-test--with-clean-state
    (let ((a (comint-histories-test--make-history "a" :persist nil))
          (b (comint-histories-test--make-history "b" :persist nil)))
      (setq comint-histories--histories (list a b))
      (let ((result (comint-histories-index-move 1 0)))
        (should (equal result '("b" "a")))))))

;;; --- Selection with dynamic predicates ---

(ert-deftest comint-histories-test-select-predicate-uses-buffer-state ()
  "Predicates can use buffer-local state for selection."
  (comint-histories-test--with-clean-state
    (let* ((hist-a (comint-histories-test--make-history
                    "a" :predicates
                    (list (lambda ()
                            (equal (buffer-local-value
                                    'major-mode (current-buffer))
                                   'comint-mode)))
                    :persist nil))
           (hist-b (comint-histories-test--make-history
                    "b" :predicates (list #'always) :persist nil)))
      (setq comint-histories--histories (list hist-a hist-b))
      ;; In a non-comint buffer, hist-a's predicate fails
      (with-temp-buffer
        (let ((result (comint-histories--select-history)))
          (should (equal "b" (car result))))))))

(ert-deftest comint-histories-test-select-switches-when-predicates-change ()
  "Selection switches history when predicates start returning differently."
  (comint-histories-test--with-clean-state
    (let* ((flag t)
           (hist-a (comint-histories-test--make-history
                    "a" :predicates (list (lambda () flag)) :persist nil))
           (hist-b (comint-histories-test--make-history
                    "b" :predicates (list (lambda () (not flag))) :persist nil)))
      (setq comint-histories--histories (list hist-a hist-b))
      (with-temp-buffer
        (comint-histories--select-history)
        (should (equal "a" (car comint-histories--last-selected-history)))
        (should (eq comint-input-ring (plist-get (cdr hist-a) :history)))
        (setq flag nil)
        (comint-histories--select-history)
        (should (equal "b" (car comint-histories--last-selected-history)))
        (should (eq comint-input-ring (plist-get (cdr hist-b) :history)))))))

;;; --- Persistence with filters ---

(ert-deftest comint-histories-test-load-applies-filters ()
  "Loading from disk applies filters, excluding filtered entries."
  (comint-histories-test--with-clean-state
    ;; Manually write a history file with entries that should be filtered
    (let* ((hist (comint-histories-test--make-history
                  "filtered" :persist t :length 10))
           (file (comint-histories--history-file hist)))
      (f-write-text (format "good-cmd%cfiltered-cmd%cgood-too%c"
                            #x1F #x1F #x1F)
                    'utf-8 file))
    ;; Load with a filter that excludes "filtered-"
    (let* ((hist2 (comint-histories-test--make-history
                   "filtered" :persist t :length 10
                   :filters '("^filtered-"))))
      (comint-histories--load-history-from-disk hist2 t)
      (let ((elts (ring-elements (plist-get (cdr hist2) :history))))
        (should (member "good-cmd" elts))
        (should (member "good-too" elts))
        (should-not (member "filtered-cmd" elts))))))

;;; --- Global filter interaction ---

(ert-deftest comint-histories-test-global-filter-on-load ()
  "Global filters apply when loading history from disk."
  (comint-histories-test--with-clean-state
    (setq comint-histories-global-filters '("^banned"))
    (let* ((hist (comint-histories-test--make-history
                  "gfilter" :persist t :length 10))
           (file (comint-histories--history-file hist)))
      (f-write-text (format "ok-cmd%cbanned-cmd%c" #x1F #x1F)
                    'utf-8 file))
    (let* ((hist2 (comint-histories-test--make-history
                   "gfilter" :persist t :length 10)))
      (comint-histories--load-history-from-disk hist2 t)
      (let ((elts (ring-elements (plist-get (cdr hist2) :history))))
        (should (member "ok-cmd" elts))
        (should-not (member "banned-cmd" elts))))))

(provide 'comint-histories-test)

;;; comint-histories-test.el ends here
