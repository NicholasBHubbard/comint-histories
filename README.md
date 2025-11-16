# comint-histories

## Installation

Available on MELPA.

## Overview

comint-histories allows you to to create separate histories for different comint inputs. This is useful if you use comint-mode to run different programs and you want each program to have its own history. comint-histories also allows you to save your histories across sessions and create custom filters to keep junk out of the histories.

Say, for example, you use `M-x shell` as your shell, but you also use `M-x shell` to run python repls and use gdb. You could have three histories, one for shell inputs, one for python inputs, and another for gdb inputs. Each of these histories could have different lengths and policies for what input is allowed in the history. You may also sometimes run your python repl from its inferior-mode (`M-x run-python`), and you want those inputs added to the same python history as when you run python through `M-x shell`. This is all possible with comint-histories.

For the most part comint-histories leverages the existing components of comint-mode, including the `comint-input-ring`.

To use comint-histories you must turn on the global minor mode `comint-histories-mode`:

```
(comint-histories-mode 1)
```

The two primary components of comint-histories are `comint-histories-add-history` and `comint-histories-search-history`. The next sections will explain these in detail.

## Functions

To define a new history you must use `comint-histories-add-history`. Here is an example usage:

```
(comint-histories-add-history ielm
  :predicates '((lambda () (derived-mode-p 'inferior-emacs-lisp-mode)))
  :filters '((lambda (input) (<= (length input) 3)))
  :persist t
  :length 2000
  :rtrim t
  :ltrim t)
```

This creates a history for `ielm` inputs with a max length of 2000 items, saves across sessions, rejects inputs with length less or equal to 3, and trims whitespace off the front and end of the input before processing.

Here is the docstring for `comint-histories-add-history`:

```
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
```

#### comint-histories-search-history

This is the only interactive function provided by comint-histories and allows you to browse a history with `completing-read` to select and insert a history item. If called with prefix arg, then the user is prompted to select a history with `completing-read`, otherwise automatic selection is made.

Though comint-histories leverages the `comint-input-ring`, which means functions like `comint-history-isearch-backward` work seamlessly, a `completing-read` interface is preferred by the author of comint-histories.

Many packages will sort the candidates for `completing-read`, however, you almost certainly do not want your histories sorted as they are already in order of newest entries to oldest. For this reason, a binding similar to the following is recommended for using `comint-histories-search-history`.

```
(define-key comint-mode-map (kbd "C-r") #'(lambda () (interactive)
                                            (let ((ivy-sort-functions-alist nil)
                                                  (ivy-prescient-enable-sorting nil)
                                                  (vertico-sort-function nil)
                                                  (vertico-sort-override-function nil)
                                                  (vertico-prescient-enable-sorting nil)
                                                  (selectrum-should-sort nil)
                                                  (selectrum-prescient-enable-sorting nil))
                                              (call-interactively #'comint-histories-search-history))))
```

#### comint-histories-get-prompt

A helper function that returns the prompt of the comint buffer. This function exists because it is very likely that users will want to define history `:predicates` based on the comint prompt.

#### comint-histories-get-input

A helper function that returns the contents of the comint input buffer. This function exists because it is likely that users will want to define history `:predicates` based on the contents of the comint input buffer. Here is an example of a history that saves your `cd` shell commands that use full paths.

```
(comint-histories-add-history shell-cds
  :predicates '((lambda () (derived-mode-p 'shell-mode))
                (lambda () (string-match-p "^cd [/~]" (comint-histories-get-input)))))
```

#### comint-histories-index-move

Move a history in the history list by index (starting at 0). This function is useful for when you add a new history mid Emacs session, and you wish to place it in a certain position in the history list. Remember that the order of the history list is relevant for history selection, and `comint-histories-add-history` always adds to the end of the history list. This function takes two arguments, HIST-IDX and MOVE-IDX, and moves the history at index HIST-IDX to index MOVE-IDX within the history list. If HIST-IDX is nil, then it is set to the maximum index in the history list.

## Variables

#### comint-histories-global-filters

A list of global filters to be used as a filter for every history. Here is an example usage that prevents inputs of length less or equal to 3 from entering any history:

```
(add-to-list 'comint-histories-global-filters #'(lambda (input) (<= (length input) 3)))
```

#### comint-histories-persist-dir

Directory to place history files for persistent histories.

## Example configuration

Here is a modified version of the authors current configuration for comint-histories:

```
(use-package comint-histories
  :demand t
  :bind
  (:map comint-mode-map
        ("C-r" . (lambda () (interactive)
                   (let ((ivy-sort-functions-alist nil)
                         (ivy-prescient-enable-sorting nil)
                         (vertico-sort-function nil)
                         (vertico-sort-override-function nil)
                         (vertico-prescient-enable-sorting nil)
                         (selectrum-should-sort nil)
                         (selectrum-prescient-enable-sorting nil))
                     (call-interactively #'comint-histories-search-history)))))
  :custom
  (comint-histories-global-filters '((lambda (x) (<= (length x) 3)) string-blank-p))
  :config
  (comint-histories-mode 1)

  (comint-histories-add-history gdb
    :predicates '((lambda () (string-match-p "^(gdb)" (comint-histories-get-prompt))))
    :length 2000
    :no-dups t)

  (comint-histories-add-history python
    :predicates '((lambda () (or (derived-mode-p 'inferior-python-mode)
                                 (string-match-p "^>>>" (comint-histories-get-prompt)))))
    :length 2000)

  (comint-histories-add-history ielm
    :predicates '((lambda () (derived-mode-p 'inferior-emacs-lisp-mode)))
    :length 2000
    :no-dups t)

  (comint-histories-add-history shell
    :predicates '((lambda () (derived-mode-p 'shell-mode)))
    :filters '("^ls" "^cd")
    :length 2000
    :no-dups t))
```
