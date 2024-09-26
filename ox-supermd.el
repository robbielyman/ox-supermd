;;; ox-supermd.el --- SuperMD backend for Org Export Engine -*- lexical-binding: t; -*-

;; Author: Robbie Lyman <rb.lymn@gmail.com>
;; Keywords: org, text, superMD, zine
;;
;; URL: https://github.com/robbielyman/ox-supermd
;; Version: 0.0.1
;; Package-Requires: ((org "8.0"))

;; This file is NOT part of Emacs.
;;
;;; License:
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;;; Commentary:
;; This library implements a SuperMD backend (for the Zine static site generator)
;; for Org exporter, based on the `md' backend.
;; See the Org manual for more information.

;;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ox-md)


;;; User-Configurable Variables

(defgroup org-export-supermd nil
  "Options specific to SuperMD export backend."
  :tag "Org SuperMD"
  :group 'org-export
  :version "0.0.1"
  :package-version '(Org . "8.0"))


;;;; Define Backend

(org-export-define-derived-backend
 'supermd 'md
 :filters-alist '((:filter-parse-tre . org-md-separate-element))
 :menu-entry
 '(?s "Export to SuperMD"
      ((?S "To temporary buffer"
           (lambda (a s v b) (org-supermd-export-as-supermd a s v)))
       (?s "To file" (lambda (a s v b) (org-supermd-export-to-supermd a s v)))
       (?o "To file and open"
           (lambda (a s v b)
             (if a (org-supermd-export-to-supermd t s v)
               (org-open-file (org-supermd-export-to-supermd nil s v)))))))
 :translate-alis '((bold . org-supermd-bold)
                   (center-block . org-md--convert-to-html)
                   (code . org-supermd-verbatim)
                   (drawer . org-md--identity)
                   (dynamic-block . org-md--identity)
                   (example-block . org-supermd-example-block)
                   (fixed-width . org-supermd-example-block)
                   (headline . org-md-headline)
                   (horizontal-rule . org-md-horizontal-rule)
                   (inline-src-block . org-supermd-verbatim)
                   (inlinetask . org-md--convert-to-html)
                   (inner-template . org-md-inner-template)
                   (italic . org-supermd-italic)
                   (item . org-supermd-item)
                   (keyword . org-md-keyword)
                   (latex-environment . org-supermd-latex-environment)
                   (latex-fragment . org-supermd-latex-fragment)
                   (line-break . org-md-line-break)
                   (link . org-md-link)
                   (node-property . org-md-node-property)
                   (paragraph . org-md-paragraph)
                   (plain-list . org-md-plain-list)
                   (plain-text . org-md-plain-text)
                   (property-drawer . org-md-property-drawer)
                   (quote-block . org-md-quote-block)
                   (section . org-md-section)
                   (special-block . org-md--convert-to-html)
                   (src-block . org-supermd-example-block)
                   (table . org-md--convert-to-html)
                   (template . org-md-template)
                   (verbatim . org-supermd-verbatim))
 :options-alist '())


;;; Transcode Functions

;;;; Bold

(defun org-supermd-bold (_bold contents _info)
  "Transcode BOLD object into SuperMD format.
CONTENTS is the text within bold markup.
INFO is a plist used as a communication channel."
  (format "**%s**" contents))

;;;; Code and Verbatim

(defun org-supermd-verbatim (verbatim _contents _info)
  "Transcode VERBATIM object into SuperMD format.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  (let ((value (org-element-property :value verbatim)))
    (format (cond ((not (string-match "`" value)) "`%s`")
                  ((or (string-prefix-p "`" value)
                       (string-suffix-p "`" value))
                   "`` %s ``")
                  (t "``%s``"))
            value)))

;;;; Example Block, Src Block and Export Block

(defun org-supermd-example-block (example-block _contents info)
  "Transcode EXAMPLE-BLOCK element into SuperMD format.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  (replace-regexp-in-string
   "^" "    "
   (org-remove-indentation
    (org-export-format-code-default example-block info))))

(defun org-supermd-export-block (export-block contents info)
  "Transcode an EXPORT-BLOCK element from Org into SuperMD.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (member (org-element-property :type export-block) '("MARKDOWN" "MD"))
      (org-remove-indentation (org-element-property :value export-block))
    ;; Also include HTML export blocks.
    ;; TODO: possibly a bug.
    (org-export-with-backend 'html export-block contents info)))

;;;; Headline

;;;; Italic

(defun org-supermd-italic (_italic contents _info)
  "Transcode ITALIC object into SuperMD format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format "*%s*" contents))

;;;; Item

(defun org-supermd-item (item contents info)
  "Transcode ITEM element into SuperMD format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-element-parent item)))
         (struct (org-element-property :structure item))
         (bullet (if (not (eq type 'ordered)) "-"
                   (concat (number-to-string
                            (car (last (org-list-get-item-number
                                        (org-element-property :begin item)
                                        struct
                                        (org-list-prevs-alist struct)
                                        (org-list-parents-alist struct)))))
                           "."))))
    (concat bullet
            (make-string (max 1 (- 4 (length bullet))) ? )
            (pcase (org-element-property :checkbox item)
              (`on "[X] ")
              (`trans "[-] ")
              (`off "[ ] "))
            (let ((tag (org-element-property :tag item)))
              (and tag (format "**%s:** " (org-export-data tag info))))
            (and contents
                 (org-trim (replace-regexp-in-string "^" "    " contents))))))

;;;; LaTeX Environment

(defun org-supermd-latex-environment (latex-environment _content info)
  "Transcode a LATEX-ENVIRONMENT object from Org to SuperMD.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (plist-get info :with-latex)
    (let ((latex-frag (org-remove-indentation
                      (org-element-property :value latex-environment)))
          (label (org-html--reference latex-environment info t)))
      (if (org-string-nw-p label)
          (replace-regexp-in-string "\\`.*"
                                    (format "\\&\n\\\\label{%s}" label)
                                    latex-frag)
        latex-frag))))

;;;; LaTeX Fragment

(defun org-supermd-latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object from Org to SuperMD.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (plist-get info :with-latex)
    (let ((frag (org-element-property :value latex-fragment)))
      (cond
       ((string-match-p "^\\\\(" frag)
        (concat "$" (substring frag 2 -2) "$"))
       ((string-match-p "^\\\\\\[" frag)
        (concat "$$" (substring frag 2 -2) "$$"))
       (t frag))))) ; either already $-delimited or a macro


;;; Interactive function

;;;###autoload
(defun org-supermd-export-as-supermd (&optional async subtreep visible-only)
  "Export current buffer to a SuperMD buffer.

If narrowing is active in the current buffer, only export its narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties first.

When optional argument VISIBLE-ONLY is non-nil,
don't export contents of hidden elements.

Export is done in a buffer named \"*Org SuperMD Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is non-nil."
  (interactive)
  (org-export-to-buffer 'supermd "*Org SuperMD Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-supermd-convert-region-to-supermd ()
  "Assume the current egion has Org syntax, and convert it to SuperMD.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in a SuperMD buffer
and use this command to convert it."
  (interactive)
  (org-export-replace-region-by 'supermd))

(defalias 'org-export-region-to-supermd #'org-supermd-convert-region-to-supermd)

(defun org-export-to-supermd (&optional async subtreep visible-only)
  "Export current buffer to a SuperMD file.

If narrowing is active in the current buffer, only export its narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interfac.

When optional argument SUBTREEP is non-nil, export the sub-tree at point,
extracting information from the headline properties first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".smd" subtreep)))
    (org-export-to-file 'supermd outfile async subtreep visible-only)))

;;;###autoload
(defun org-supermd-publish-to-supermd (plist filename pub-dir)
  "Publish an org file to SuperMD.

FILENAME is the filename of the Org file to be published.
PLIST is the property list for the given project.
PUB-DIR is the publishing directory.

Return the output file name."
  (org-publish-org-to 'supermd filename ".smd" plist pub-dir))

(provide 'ox-supermd)
;;; ox-supermd.el ends here

