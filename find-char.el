;;; find-char.el --- Find char      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>
;; Version: 1.0.0

;;; License: GPLv3 or later

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;;
;;;; Developer's note
;;
;;;;; How does find-char work in general
;;
;; 1. get char `find-char'
;; 2. get a list of matched positions `find-char--fetch-pos' -> `find-char--pos-list'
;; 3. init session (transient map session): set `find-char--begin-pos', etc, stuff in `find-char-maybe-init'
;; 4. find the position in of current point in `find-char--pos-list': in forward/backward: `find-char--find-index'
;; 5. forward/backward index and jump
;; 6. when finished (transient map exit): clean up: st `find-char--begin-pos' to nil, etc, in `find-char-cleanup'

;;; Code:
;;

;;;; Variables

(defvar find-char-search-range 6000
  "Range in where find-char searches for char.
100 means search from point - 50 to point + 50.")

(defvar find-char-stop-at 'beginning
  "Where to stop at when find match. Could be 'beginning or 'end.")


;;;; Private variables

(defvar find-char--last-str nil
  "Last search string of find-char.")

(defvar find-char--pos-list nil
  "List of current matched positions.")

(defvar find-char--begin-pos nil
  "Where user begined jumping.
Used for quick deleting.")

(defvar find-char--overlay-lst nil
  "Stores highlight overlays.")


;;;; Core helper

(defun find-char--normalize-pos (pos len)
  "Return the right position of POS with match length LEN.
POS is returned by `re-search-forward' so it is at the end of the match.
If `find-char-stop-at' is 'beginning, normalize it to beginning,
else normalize to end."
  (if (eq find-char-stop-at 'beginning)
      (max 1 (- pos len))
    pos))

(defun find-char--fetch-pos (str)
  "Return a list of (end of) positions where STR appears.
`find-char--last-str' is set to STR."
  (setq find-char--last-str str)
  (save-excursion
    ;; respect field
    (let ((end (min (point-max) (+ (point) (/ find-char-search-range 2))))
          (str-len (length str))
          pos list)
      (goto-char (max (point-min) (- (point) (/ find-char-search-range 2))))
      (while (setq pos (re-search-forward str end t))
        (push (find-char--normalize-pos pos str-len) list))
      (reverse list))))

(defun find-char--maybe-update-pos-list (str)
  "Update `find-char--pos-list' if `find-char--last-str' doesn't equal to STR."
  (when (not find-char--pos-list)
    ;; `find-char--pos-list' is set to nil at the end of each session
    (setq find-char--pos-list (find-char--fetch-pos str))
    (find-char--highlight-match str)))

(defun find-char--find-index (pos list)
  "Find the index of POS in LIST.
Might return float when the point is in between two pos.
E.g., 12.5 for point between 12 and 13."
  ;;          |0st match   |1nd     |2       |3
  ;;        ^-0.5th <- start from here
  ;;          |0  ^0.5     |1       |2       |3
  ;;               we are at 0.5th if point is greater than 0th match pos
  ;;          |0           |1  ^1.5 |        |3
  ;;                           and so on
  (let ((index -0.5)
        (max-possible-index (- (length list) 0.5)))
    (while (and (<= index max-possible-index) (> pos (nth (round (+ index 0.5)) list)))
      (setq index (1+ index)))
    (if (= pos (nth (round (+ index 0.5)) list))
        (round (+ index 0.5))
      index)))

;;;; Jump

(defun find-char-forward (&optional str)
  "Search forward for STR and jump to it.
Also highlight other matches.
If STR is nil, use `find-char--last-str'."
  (interactive)
  (find-char--maybe-update-pos-list (or str find-char--last-str))
  (find-char-maybe-init)
  (catch 'early-return
    (let* ((len (length find-char--pos-list))
           (index (let ((index (find-char--find-index (point) find-char--pos-list)))
                    (if (> index len) ; pos is greater than the largest matched position
                        (throw 'early-return nil))
                    (if (integerp index)
                        (1+ index)
                      (ceiling index)))))
      ;; normalize index
      (if (= index len)
          (setq index (1- index)))

      ;; jump
      (goto-char (nth index find-char--pos-list))
      ;; report           0-base to 1-base
      (message "-> %d/%d" (1+ index) len))))

(defun find-char-backward (&optional str)
  "Search backward for STR and jump to it.
Also highlight other matches.
If STR is nil, use `find-char--last-str'."
  (interactive)
  (find-char--maybe-update-pos-list (or str find-char--last-str))
  (find-char-maybe-init)
  (catch 'early-return
    (let ((len (length find-char--pos-list))
          (index (let ((index (find-char--find-index (point) find-char--pos-list)))
                   (if (< index 0) ; pos is smaller than the smallest matched position
                       (throw 'early-return nil))
                   (if (integerp index)
                       (1- index)
                     (floor index)))))
      ;; normalize index
      (if (< index 0)
          (setq index 0))
      ;; jump
      (goto-char (nth index find-char--pos-list))
      ;; report           0-base to 1-base
      (message "-> %d/%d" (1+ index) len))))

;;;; Session management

(defun find-char-cleanup ()
  "Clean up session."
  (find-char--clear-highlight)
  (setq find-char--begin-pos nil)
  (setq find-char--pos-list nil))

(defun find-char-maybe-init ()
  "Init session."
  ;; set begin pos if not set yet (at begging of a session)
  (when (not find-char--begin-pos)
    (push-mark) ; region highlight uses this
    (setq find-char--begin-pos (point)))
  ;; TODO start point highlight
  )

;;;; Highlight

(defun find-char--highlight-match (str)
  "Highlight matched STRs."
  (if find-char--pos-list
      (let ((len (length str)))
        (dolist (pos find-char--pos-list)
          (let (beg end)
            (if (eq find-char-stop-at 'beginning)
                (progn (setq beg pos)
                       (setq end (+ pos len)))
              (setq beg (- pos len))
              (setq end pos))
            (let ((ov (make-overlay beg end nil t)))
              (overlay-put ov 'font-lock-face 'lazy-highlight)
              (push ov find-char--overlay-lst)))))))

(defun find-char--clear-highlight ()
  "Clear all match highlights."
  (mapc #'delete-overlay find-char--overlay-lst)
  (setq find-char--overlay-lst nil))

;;;; Convenient commands

(defun find-char-quick-delete ()
  "Delete region between where user started jumping and point."
  (interactive)
  (if find-char--begin-pos
      (delete-region find-char--begin-pos (point))
    (message "Begin position not set")))

(defun find-char-toggle-region-highlight ()
  "Toggle highlight of the region between starting position and point."
  (interactive)
  (if find-char--begin-pos
      (if mark-active
          (deactivate-mark)
        (activate-mark))
    (message "Begin position not set")))

(defun find-char-copy-region ()
  "Push region to kill ring."
  (interactive)
  (kill-ring-save find-char--begin-pos (point)))

;;; Userland

(defvar find-char-transient-map (let ((map (make-sparse-keymap)))
                                  (define-key map (kbd "'") #'find-char-forward)
                                  (define-key map (kbd ";") #'find-char-backward)
                                  (define-key map (kbd "C-'") #'find-char-forward)
                                  (define-key map (kbd "C-;") #'find-char-backward)
                                  (define-key map (kbd "C-d") #'find-char-quick-delete)
                                  (define-key map (kbd "C-v") #'find-char-toggle-region-highlight)
                                  (define-key map (kbd "C-w") #'find-char-copy-region)
                                  (define-key map [remap keyboard-quit] (lambda () (interactive)
                                                                          (keyboard-quit)
                                                                          (find-char-cleanup)))
                                  map)
  "Transient map for find-char.")

(defun find-char (arg)
  "Prompt for CHAR and find it and jump to it.
Any modifier is ignored, i.e., \"C-f\" in considered as \"f\".
You can combine countes (ARG) with this function. C-3 M-x ‘find-char’ jumps to the third match.
If ARG is negative (C--), search backward.
(Don’t use C-u, C-u is passed as 4)."
  (interactive "p")
  (let* ((char-str (char-to-string (read-char "Find: ")))
         (char-upcase-p (equal char-str (upcase char-str)))
         (func-in-dir (if (> arg 0)
                          #'find-char-forward
                        #'find-char-backward))
         (func-in-opposite-dir (if (> arg 0)
                                   #'find-char-backward
                                 #'find-char-forward)))
    (dotimes (_ (abs arg)) (funcall func-in-dir char-str))
    (set-transient-map (let ((tmp-map (make-sparse-keymap)))
                         (set-keymap-parent tmp-map find-char-transient-map)
                         (define-key tmp-map (kbd char-str) func-in-dir)
                         (define-key tmp-map (kbd (if char-upcase-p
                                                      (downcase char-str)
                                                    (upcase char-str)))
                           func-in-opposite-dir)
                         tmp-map)
                       t
                       #'find-char-cleanup)))

(defun find-char-backward-cmd ()
  "Equivalent of C-- M-x `find-char'."
  (interactive)
  (find-char 4))

(provide 'find-char)

;;; find-char.el ends here
