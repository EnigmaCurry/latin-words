;; latin-words.el --- A Latin-English dictionary -*- lexical-binding: t; -*-
;;
;; Author: EnigmaCurry
;; URL: https://github.com/EnigmaCurry/latin-words
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: dictionary, latin
;; SPDX-License-Identifier: MIT AND CC-BY-SA-3.0
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is a Latin to English Dictionary based upon the work "A Latin
;; Dictionary" by Charlton T. Lewis and Charles Short, published by
;; the Perseus Digital Library at Tufts University CC-BY-SA 3.0
;;

;;; Usage:
;;
;; ;; Get description string for Latin word "adbibo":
;; (latin-word-get-description "adbibo")
;;
;; ;; Get definition object for Latin word "adbibo":
;; (latin-word-get-definition "adbibo")
;;    
;; ;; Get a "random" deterministic Latin word based on numeric seed:
;; (latin-word-get-by-seed 1234567890)

;; See LICENSE.txt

(defvar latin-words-directory
  (expand-file-name "data" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing the Latin dictionary files.")

(defun latin-word-of-the-day ()
    (latin-word-get-by-seed (string-to-number (substring (secure-hash 'sha256 (format-time-string "%Y%m%d")) 0 16) 16)))
    (latin-word-of-the-day)

(defun latin-word-get-by-seed (seed)
  "Retrieve a Latin word by a given SEED from JSON files in `latin-words-directory`."
  (let* ((default-directory latin-words-directory)
         (command "jq -r '.[] | .key' *.json | sort -u")
         (words
          (split-string (shell-command-to-string command) "\n" t))
         (num-words (length words)))
    (if (> num-words 0)
        (let* ((index (mod seed num-words))
               (word (nth index words)))
          (message "%s" word)
          word)
      (message "No words found."))))

(defun latin-word-get-definition (word)
  "Lookup WORD in the Latin dictionary in the latin-words-directory."
  (let*
      ((first-letter (upcase (substring word 0 1)))
       (file-name
        (concat
         latin-words-directory "/" "ls_" first-letter ".json"))
       ;; Updated jq command to flatten "senses"
       (jq-command
        (format
         "jq -r --arg word '%s' '.[] | select(.key == $word) | .senses |= (if type == \"array\" then flatten else . end)' %s"
         word file-name))
       (result (shell-command-to-string jq-command)))
    (if (string-blank-p result)
        (json-parse-string "{}")
      (json-parse-string result))))

(defun latin-word-get-description (word)
  (let* ((entry (latin-word-get-definition word))
         (word (gethash "key" entry))
         (part-of-speech (or (gethash "part_of_speech" entry) ""))
         (gender (or (gethash "gender" entry) ""))
         (senses (mapconcat 'identity (latin-word-flatten-vector (or (gethash "senses" entry) ())) "\n\n"))
         (title-orthography (or (gethash "title_orthography" entry) ""))
         (main-notes (or (gethash "main_notes" entry) ""))
         (description (with-temp-buffer
                        (let* ((start (point)))
                          (insert (upcase word))
                          (when (not (string-blank-p title-orthography))          
                            (insert " ")
                            (insert title-orthography)
                            )
                          (when (not (string-blank-p part-of-speech))
                            (insert (concat " (" part-of-speech (when (not (string-blank-p gender)) (concat " " gender)) ")")))
                          (when (not (string-blank-p main-notes))          
                            (insert " ")
                            (insert main-notes)
                            )
                          (when (not (string-blank-p senses))
                            (newline)
                            (newline)
                            (insert senses)
                            )
                          (fill-region start (point))
                          (buffer-string)
                          ))))
    description
    ))


(defun latin-word-flatten-vector (vec)
  "Flatten VEC, which may contain sub-vectors."
  (apply #'vector
         (cl-mapcan (lambda (item)
                      (if (vectorp item)
                          (flatten-vector item)
                        (list item)))
                    vec)))

(provide 'latin-words)
