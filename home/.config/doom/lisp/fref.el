;;; fref.el --- File Reference Library, functions to encode file names -*- lexical-binding: t; -*-

;; Copyright (C) 2026 noncog

;; Author: noncog
;; Maintainer: noncog
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/home/.config/doom/lisp/fref.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;  Disclaimer:
;;
;; This file is based on features from Denote.el by Protesilaos.
;; Author: Protesilaos <info@protesilaos.com>
;; Maintainer: Protesilaos <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote
;;
;;; Commentary:
;;
;; File Reference is a library of functions to encode file properties into file names.
;;
;; Inspired by Denote, but simplified down to just the core idea of file name encoding.
;;
;; This library was created to abstract the core ideas of Denote's file naming conventions
;; from its notes management and extensions. Although significant effort has been made to
;; minimize the core, it still contains their opinionated workflow designed for notes
;; management. The goal of this library is to build a modular setup that other libraries
;; and packages can depend upon.
;;
;; This library may prove to be less effective than converting to the Denote ecosystem in
;; the future. The purpose of this library could be applied to Denote itself and achieve
;; the same effect of a lean core worth building upon.
;;
;;; Code:

(defgroup fref ()
  "File-naming scheme library."
  :group 'files)

(defcustom fref-filename-components '(identifier signature title tags)
  "Specify the desired filename components and order.

The value is a list of the following symbols:

- `identifier': This is the combination of the date and time.  When it
  is the first on the list, it looks like \"20260624T071923\" and does
  not have a component separator of its own due its unambiguous format.
  When it is placed anywhere else in the file name, it is prefixed with
  \"@@\", so it looks like \"@@20260624T071923\".

- `signature': This is an arbitrary string that can be used to qualify
  the file in some way.The string is always prefixed with the \"==\"
  to remain unambiguous.

- `title': This is an arbitrary string which describes the file.  It is
  always prefixed with \"--\" to be unambiguous.

- `tags': This is a series of one or more words that succinctly
  group the file.  Multiple tags are separated by an underscore
  prefixed to each of them.  The file name component is always prefixed
  with \"__\".

All four symbols must appear exactly once.  Duplicates are ignored.  Any
missing symbol is added automatically.

Some examples:
    (setq fref-filename-components
       \\='(identifier signature title tags))
    => 20260624T071923==hello--this-is-the-title__project_review.org

    (setq fref-filename-components
       \\='(signature identifier title tags))
    => ==hello@@20260624T071923--this-is-the-title__project_review.org

    (setq fref-filename-components
       \\='(title signature identifier tags))
    => --this-is-the-title==hello@@20260624T071923__project_review.org

    (setq fref-filename-components
       \\='(tags title signature identifier))
    => __project_review--this-is-the-title==hello@@20260624T071923.org

Before deciding on this, please consider the longer-term implications
of file names with varying patterns. Consistency makes things
predictable and thus easier to find. So pick one order and never touch
it again. When in doubt, leave the default file-naming scheme as-is."
  :group 'fref
  :type '(list
          (const :tag "Identifier component (date and time)" identifier)
          (const :tag "File signature (text to qualify a file)" signature)
          (const :tag "The title of the file" title)
          (const :tag "Tags of the file" tags)))

(defcustom fref-component-slug-functions
  '((identifier . identity)
    (title . fref-slug-title)
    (signature . fref-slug-signature)
    (tag . fref-slug-tag))
  "Specify functions used to format the components of the file name.

The value is an alist where each element is a cons cell of the
form (COMPONENT . FUNCTION).

- The COMPONENT is an unquoted symbol among `identifier', `title',
  `signature', `tag' (notice `tag' is non-plural, see below),
  which refers to the corresponding component of the file name.

- The FUNCTION is to be used to format the given component.
  This function should take a string as its parameter and return
  the string formatted for the file name.

Note that the `identifier' component calls the function `identity'
which returns the provided value unmodified.

In the case of the `tag' component, the function receives a SINGLE
string representing a single tag and returns it formatted for the
file name. Joining the tags together is handled by fref formatting
functions.

By default, if a component function is not specified, fref uses
`identity', `fref-slug-title', `fref-slug-signature' and `fref-slug-tag'.

Remember that deviating from the default file-naming scheme will make
things harder to search for in the future.  The formatting scheme and
restrictions imposed by default are there for a reason.  This setting
provides you with an entry to modifying the scheme but be warned about
possible issues that may follow."
  :group 'fref
  :type '(alist :key (choice (const title)
                             (const signature)
                             (const tag))
          :value function))

(defcustom fref-identifier-delimiter-always-present nil
  "Specify if file names always contain the identifier delimiter."
  :group 'fref
  :type 'boolean)


;;; Constants
(defconst fref-id-format "%Y%m%dT%H%M%S"
  "Format of ID in a file name.")

(defconst fref-id-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)"
  "Regular expression to match the `fref-id-format'.")

(defconst fref-file-id-regexp "@@\\([^.]+?\\)\\(==.*\\|--.*\\|__.*\\|@@.*\\|\\..*\\)*$"
  "Regular expression to match the ID in a file name.")

(defconst fref-signature-regexp "==\\([^.]+?\\)\\(==.*\\|--.*\\|__.*\\|@@.*\\|\\..*\\)*$"
  "Regular expression to match the SIGNATURE in a file name.")

(defconst fref-title-regexp  "--\\([^.]+?\\)\\(==.*\\|__.*\\|@@.*\\|\\..*\\)*$"
  "Regular expression to match the TITLE in a file name.")

(defconst fref-tags-regexp "__\\([^.]+?\\)\\(==.*\\|--.*\\|__.*\\|@@.*\\|\\..*\\)*$"
  "Regular expression to match the TAGS in a file name.")

;;; Getters

(defun fref-extract-id (string)
  "Return the ID component from STRING if found, else return nil."
  (when (string-match fref-id-regexp string)
    (match-string-no-properties 0 string)))

(defun fref-file-id (file)
  "Return the ID component from FILE name, else return nil."
  (let ((filename (file-name-nondirectory file)))
    (cond ((string-match (concat "\\`" fref-id-regexp) filename)
           (match-string-no-properties 0 filename))
          ((string-match fref-file-id-regexp filename)
           (match-string-no-properties 1 filename)))))

(defun fref-file-title (file)
  "Return the TITLE component from FILE name, else return nil."
  (let ((filename (file-name-nondirectory file)))
    (when (string-match fref-title-regexp filename)
      (match-string 1 filename))))

(defun fref-file-tags (file)
  "Return the TAGS component from FILE name as a string, else return nil.

Also see `fref-get-tags-list' for returning TAGS as a list instead."
  (let ((filename (file-name-nondirectory file)))
    (when (string-match fref-tags-regexp filename)
      (match-string 1 filename))))

(defun fref-file-tags-list (file)
  "Return the TAGS component from FILE name as a list of strings, else return nil."
  (when-let* ((tags (fref-get-tags file)))
    (split-string tags "_" :omit-nulls)))

(defun fref-file-signature (file)
  "Return the SIGNATURE component from FILE name, else return nil."
  (let ((filename (file-name-nondirectory file)))
    (when (string-match fref-signature-regexp filename)
      (match-string 1 filename))))

;;; Slug Functions

(defun fref-slug-title (title)
  "Return TITLE formatted for use in a file name.

Uses Unicode normalization to properly handle international characters
and diacritical marks. Implementation adapted from org-roam and vulpea.

Credit: USAMI Kenta (@zonuexe)
See: https://github.com/org-roam/org-roam/pull/1460"
  (let ((slug-trim-chars
         ;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
         ;; For why these specific glyphs: https://github.com/org-roam/org-roam/pull/1460
         '( #x300 #x301 #x302 #x303 #x304 #x306 #x307
            #x308 #x309 #x30A #x30B #x30C #x31B #x323
            #x324 #x325 #x327 #x32D #x32E #x330 #x331)))
    (thread-last title
                 (string-glyph-decompose)
                 (seq-remove (lambda (char) (memq char slug-trim-chars)))
                 (apply #'string)
                 (string-glyph-compose)
                 (replace-regexp-in-string "[^[:alnum:]]" "-") ;; convert all non-alphanumeric
                 (replace-regexp-in-string "-\\{2,\\}" "-")    ;; remove sequential hyphens
                 (replace-regexp-in-string "^-\\|-$" "")       ;; remove leading/trailing hyphens
                 (downcase))))

(defun fref-slug-tag (tag)
  "Return TAG formatted for use in a file name."
  ;; NOTE: Consider supporting tags format supported by org (underscores).
  (downcase (replace-regexp-in-string "[^[:alnum:]]" "" tag)))

(defun fref-slug-signature (signature)
  "Return SIGNATURE formatted for use in a file name."
  (thread-last signature
               (replace-regexp-in-string "[^[:alnum:]]" "=") ;; convert all non-alphanumeric
               (replace-regexp-in-string "=\\{2,\\}" "=")    ;; remove sequential equals
               (replace-regexp-in-string "^=\\|=$" "")       ;; remove leading/trailing equal
               (downcase)))

(defun fref--trim-right-token-chars (string component)
  "Remove =, -, _ and @ characters from the end of STRING.
The removal is done only if necessary according to COMPONENT."
  (if (eq component 'title)
      (string-trim-right string "[=@_]+")
    (string-trim-right string "[=@_-]+")))

(defun fref--remove-consecutive-token-chars (string component)
  "Replace consecutive =, -, _ and @ with a single one in STRING.
The removal is done only if necessary according to COMPONENT."
  (let ((string (replace-regexp-in-string
              "_\\{2,\\}" "_"
              (replace-regexp-in-string
               "=\\{2,\\}" "="
               (replace-regexp-in-string
                "@\\{2,\\}" "@" string)))))
    ;; -- are allowed in titles when the default slug function is disabled.
    (if (eq component 'title)
        string
      (replace-regexp-in-string
       "-\\{2,\\}" "-" string))))

(defun fref-slug-component (component string)
  "Return STRING formatted as an fref file name COMPONENT.

Apply the function specified in `fref-component-slug-functions' to
COMPONENT which is one of `identifier', `title', `signature', `tag'.

Since these may be user supplied functions, also apply additional
processing to ensure the formatted value aligns with file naming
scheme.  If the resulting string still contains consecutive symbols
such as -, _, =, or @, they are replaced by a single occurence of
the character if necessary according to the COMPONENT.  If COMPONENT
is `tag' underscores are removed from STRING as they are used as the tag
separator in file names.

Also enforce the rules of the file-naming scheme."
  ;; TODO: SHould this be when-let*?
  (let* ((slug-fn (alist-get component fref-component-slug-functions))
         (slug-str (pcase component
                     ('title (funcall (or slug-fn #'fref-slug-title) string))
                     ('tag (replace-regexp-in-string
                            "_" "" (funcall (or slug-fn #'fref-slug-tag) string)))
                     ;; FIXME: There is an error here in control flow related to invalid identifiers.
                     ('identifier (fref-extract-id (funcall (or slug-fn #'identity) string)))
                     ('signature (funcall (or slug-fn #'denote-sluggify-signature) string)))))
    (thread-first
      (replace-regexp-in-string "\\." "" slug-str)     ;; Remove periods.
      (fref--remove-consecutive-token-chars component) ;; Replace consecutive  =, -, _ and @ chars.
      (fref--trim-right-token-chars component))))      ;; Trim end of string =, -, _ and @ chars.

(defun fref-slug-tags (tags)
  "Return TAGS as a list of strings formatted as an fref file name component."
  (mapcar (lambda (tag)
            (fref-slug-component 'tag tag))
          tags))

(defun fref-format-file-name (dir id tags title extension signature)
  "Format file name.
DIR, ID, TAGS, TITLE, EXTENSION and SIGNATURE are
expected to be supplied by `fref' or equivalent command.

DIR is a string pointing to a directory.  It ends with a forward slash
 (the function `denote-directories' makes sure this is the case when
returning the value of the variable `denote-directory'). DIR cannot
be nil or an empty string.

ID is a string holding the identifier of the note.  It can be an
empty string, in which case its respective file name component is
not added to the base file name.

DIR and ID form the base file name.

TAGS is a list of strings that is reduced to a single string
by `denote-keywords-combine'.  KEYWORDS can be an empty list or
a nil value, in which case the relevant file name component is
not added to the base file name.

TITLE and SIGNATURE are strings.  They can be an empty string, in
which case their respective file name component is not added to
the base file name.

EXTENSION is a string that contains a dot followed by the file
type extension.  It can be an empty string or a nil value, in
which case it is not added to the base file name."
  (cond
   ((null dir)
    (error "DIR must not be nil"))
   ((string-empty-p dir)
    (error "DIR must not be an empty string"))
   ((not (string-suffix-p "/" dir))
    (error "DIR does not end with a / as directories ought to")))
  (let ((filename "")
        (components (seq-union fref-filename-components
                               '(identifier signature title tags))))
    (dolist (component components)
      (cond ((and (eq component 'identifier) id (not (string-empty-p id)))
             (setq filename (concat filename "@@" (fref-slug-component 'identifier id))))
            ((and (eq component 'title) title (not (string-empty-p title)))
             (setq filename (concat filename "--" (fref-slug-component 'title title))))
            ((and (eq component 'tags) tags)
             (setq filename (concat filename "__"
                                    (string-join (fref-slug-tags tags) "_"))))
            ((and (eq component 'signature) signature (not (string-empty-p signature)))
             (setq filename (concat filename "==" (fref-slug-component 'signature signature))))))
    (when (string-empty-p filename)
      (error "There should be at least one file name component"))
    (setq filename (concat filename extension))
    ;; Do not prepend identifier with @@ if it is the first component and has the format 00000000T000000.
    (when (and (not fref-identifier-delimiter-always-present)
               (string-prefix-p "@@" filename)
               (string-match-p (concat "\\`" fref-id-regexp "\\'") id))
      (setq filename (substring filename 2)))
    (concat dir filename)))

(provide 'fref)
;;; fref.el ends here
;;
;; Dev Notes:
;;
;; fref should be the thing to follow links and be changeable by user in finding methods.
;; should the things that return things by abstracted to one function and you ask for what you want?

;; I need to solve the separator issue and come up with allowing the user to find parts and put them together better.
;; TODO: Consider letting use other id format and changing slug functions to match with defcustom.
;; The issue is that there isn't great means to programmatically generate a regular expression to match the values. Otherwise would be easy.
;; I need both internal to a component and component separators.
;;
;; Now that we have this we can do the following uses:
;; - denote-org-capture
;; - denote--prepare-note
;;   - this is called by denote itself.
;;   - which calls denote--creation-prepare-note-data which returns a list for this
;; - denote--rename-file
;; - denote-change-file-type-and-front-matter

;; - denote-rename-file
;; - denote-dired-rename-files


;; denote-rename-
;; file-title
;; file-keywords
;; file-date
;; file-identifier
;; file-signature
;; file-using-front-matter  (file)
;; buffer--format
;; buffer
;; buffer--fallback
;; buffer-rename-function-or-fallback
;; file-and-buffer
;; file-prompt
;; file
;;
;; For renames and stuff for sure need:
;; denote--component-has-value-p
;; denote-rewrite-front-matter.
;; denote--format-front-matter.
;;
;; Need to add retrieving file creation time on file systems and os that support it.
