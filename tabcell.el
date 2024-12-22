;;; tabcell.el --- Active cell tracking for tabulated lists -*- lexical-binding: t -*-

;; Author: Your Name <your.email@example.com>
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;; Keywords: convenience, tabulated-list
;; URL: https://github.com/yourusername/tabcell

;;; Commentary:

;; tabcell.el provides a minor mode for active cell tracking and navigation
;; within Emacs' `tabulated-list-mode`. It highlights the currently active cell
;; and provides keyboard shortcuts for efficient movement between rows and columns.

;; Features:
;; - Highlights the active cell in a tabulated list.
;; - Supports keyboard navigation within the table.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)

(add-to-list 'load-path default-directory)

;; -------------------------------------------------------------------------------- cell-mode

(defun tabcall-debug (format-string &rest args)
  "Log debugging information. FORMAT-STRING and ARGS behave like `message`."
  (apply #'message (concat "[DEBUG] " format-string) args))

(defvar-local tabcell-active-cell '(0 . 0)
  "The currently active cell in the tabular list, represented as (row . column).")

(defvar-local tabcell-active-cell-overlay nil
  "Overlay used to highlight the currently active cell.")

(defvar-local tabcell-line-cells nil
  "Overlay used to highlight the currently active cell.")

(defvar tabcell-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Horizontal movements
    (define-key map (kbd "<home>") #'tabcell-move-home)
    (define-key map (kbd "<end>") #'tabcell-move-end)    
    (define-key map (kbd "<right>") #'tabcell-list-next-column)
    (define-key map (kbd "<left>") #'tabcell-list-previous-column)
    ;; Vertical movements
    (define-key map (kbd "C-<home>") #'tabcell-move-column-home)
    (define-key map (kbd "C-<end>") #'tabcell-move-column-end)
    (define-key map (kbd "<down>") #'tabcell-move-down)
    (define-key map (kbd "<up>") #'tabcell-move-up)
    (define-key map [mouse-1] #'tabcell-track-active-cell)    
    (define-key map [drag-mouse-1] #'ignore)    
    map)
  "Keymap for `tabcell-mode`. Provides key bindings for navigating the tabular list.")

(defun tabcell-cumulative-sum-with-step (vector formats &optional start step)
  "Return a vector with cumulative sums of VECTOR, adding STEP at each step.
If STEP is not provided, it defaults to 0."
  (let ((result (make-vector (1+ (length vector)) (or start 0)))
        (sum (or start 0))
        (step (or step 0))) ;; Default to 0 if STEP is not specified
    (dotimes (i (length vector))
      (setq sum (+ sum (nth i vector) step))
      (aset result (1+ i)
            (max sum (nth 1 (elt formats i)))))
    (tabcall-debug "tabcell-cumulative-sum-with-step: %S" result)
    result))

(defun tabcell-update-active-cell-overlay ()
  "Update the overlay to highlight the currently active cell."
  (when (and tabcell-active-cell-overlay
             (overlayp tabcell-active-cell-overlay))
    (delete-overlay tabcell-active-cell-overlay)) ;; Remove previous overlay
  (let* ((row (car tabcell-active-cell))
         (column (cdr tabcell-active-cell))
         (entry (tabulated-list-get-entry))
         (cell-start nil)
         (cell-end nil))
    (when (and entry column) ;; Calculate cell boundaries
      (let ((cell-content (aref entry column))
            (line-start (line-beginning-position))
            (field-len 0)
            (columns (tabcell-cumulative-sum-with-step
                      (cl-map 'list (lambda (field)
                                      (length field))
                              entry)
		      tabulated-list-format
		      tabulated-list-padding
                      1)))
        (save-excursion
          (let* ((begin (aref columns column))
                 (end (aref columns (1+ column)))
                 (field-len (max (length cell-content)
                                 (nth 1 (elt tabulated-list-format column)))))
            (setq cell-start (+ line-start begin))
            (setq cell-end (+ cell-start field-len))
            (setq tabcell-active-cell-overlay (make-overlay cell-start cell-end))
            (tabcall-debug "tabcell-update-active-cell-overlay: start=%d, end=%d, row=%d, column=%d" cell-start cell-end row column)))
          (overlay-put tabcell-active-cell-overlay 'face 'highlight)))))

(defun tabcell-get-active-cell ()
  "Return the active cell (row . column) based on the cursor position."
  (let ((row (line-number-at-pos))
        (column (current-column))) ;; Use cursor position to calculate column
    (let ((offset 0) (col 0))
      (while (< (1+ offset) (- column tabulated-list-padding))
        (setq offset (+ offset 1 (nth 1 (elt tabulated-list-format col))))
        (setq col (1+ col)))
      (tabcall-debug "tabcell-get-active-cell: row=%d, column=%d" row col)
      (cons row col))))

(defun tabcell-track-active-cell (event)
  "Track the currently active cell and update its overlay."
  (interactive (list last-input-event))
  (let* ((coord  (posn-col-row (event-start event)))
           (bpoint (posn-point  (event-start event)))
           (row (1+ (cdr coord))))
      (goto-char bpoint)
      (let ((prev (or (previous-single-property-change
                       (point) 'tabulated-list-column-name)
                      1))
            (column (tabcell-cell-columns (car coord))))
        (unless (< prev (pos-bol))
          (goto-char prev)
          (when (tabcell-list-previous-column)
            (tabcell-list-next-column))
          (setq tabcell-active-cell (cons row column))
          (tabcall-debug "tabcell-track-active-cell: row=%d, column=%d" row column)))))

(define-minor-mode tabcell-mode
  "Minor mode to enable active cell tracking in `tabcell-mode`."
  :lighter " tc"
  :keymap tabcell-mode-map  
  (if (not (derived-mode-p 'tabulated-list-mode))
      (progn
        (setq tabcell-mode nil)
        (tabcall-debug "tabcell-mode: Cannot be enabled outside tabulated-list-mode."))
    (if tabcell-mode
        (progn
          ;; Activate the mode
          (setq tabcell-active-cell '(1 . 0))
          (setq tabcell-active-cell-overlay nil)
          (tabcall-debug "tabcell-mode: Activated."))
      ;; Deactivate the mode
      (when tabcell-active-cell-overlay
        (delete-overlay tabcell-active-cell-overlay)
        (setq tabcell-active-cell-overlay nil)
        (tabcall-debug "tabcell-mode: Deactivated.")))))

(defun tabcell-find-index-in-vector (num vec)
  "Find the index of the first element in VEC greater than or equal to NUM.
If no such element is found, return the length of VEC."
  (let ((len (length vec)))
    (catch 'found
      (dotimes (index len)
        (when (>= (aref vec index) num)
          (throw 'found index)))
      len))) ;; Return the length of the vector if no element is found

(defun tabcell-cell-columns (column)
  "Retrieve column index within the current row."
  (let ((index (tabcell-find-index-in-vector column tabcell-line-cells)))
    (tabcall-debug "tabcell-cell-columns: column=%d, index=%d" column index)
    index))

(defun tabcell-fill-line-cells ()
  "Populate `tabcell-line-cells` with column boundaries of the current row."
  (let* ((entry (tabulated-list-get-entry))
         (columns (tabcell-cumulative-sum-with-step
                   (cl-map 'list (lambda (field)
                                   (length field))
                           entry)
		   tabulated-list-format
		   tabulated-list-padding
                   1)))
    (setq tabcell-line-cells columns)
    (tabcall-debug "tabcell-fill-line-cells: %S" columns)))

(defun tabcell-move-down ()
  "Move to the cell below, maintaining the current column."
  (interactive)
  (let ((current-cell tabcell-active-cell))
    (forward-line 1) ;; Move to the next row
    (if (tabulated-list-get-id) ;; Check if the row is valid
        (setq tabcell-active-cell (cons (1+ (car current-cell)) (cdr current-cell)))
      (progn
        (forward-line -1) ;; Move back if there are no more rows
        (tabcall-debug "tabcell-move-down: No more rows below.")))
    (tabcell-update-cursor-position)))

(defun tabcell-move-up ()
  "Move to the cell above, maintaining the current column."
  (interactive)
  (let ((current-cell tabcell-active-cell))
    (when (> (car current-cell) 1)
      (forward-line -1) ;; Move to the previous row
      (setq tabcell-active-cell (cons (1- (car current-cell)) (cdr current-cell)))
      (tabcell-update-cursor-position))
    (tabcall-debug "tabcell-move-up: current-cell=%S" tabcell-active-cell)))

(defun tabcell-move-home ()
  "Move to the first cell in the current row."
  (interactive)
  (let ((current-cell tabcell-active-cell))
    (setq tabcell-active-cell (cons (car current-cell) 0)) ;; Move to the first column
    (tabcell-update-cursor-position)
    (tabcall-debug "tabcell-move-home: current-cell=%S" tabcell-active-cell)))

(defun tabcell-move-end ()
  "Move to the last cell in the current row."
  (interactive)
  (let ((current-cell tabcell-active-cell))
    (goto-char (point-max)) ;; Go to the end of the buffer
    (beginning-of-line)
    (setq tabcell-active-cell (cons (line-number-at-pos) (cdr current-cell)))
    (tabcell-update-cursor-position)
    (tabcall-debug "tabcell-move-end: current-cell=%S" tabcell-active-cell)))

(defun tabcell-move-column-home ()
  "Move to the first cell in the current row."
  (interactive)
  (let ((current-cell tabcell-active-cell))
    (setq tabcell-active-cell (cons 1 0)) ;; Move to the first column
    (tabcell-update-cursor-position)
    (goto-char (point-min))
    (tabcall-debug "tabcell-move-column-home: current-cell=%S" tabcell-active-cell)))

(defun tabcell-move-column-end ()
  "Move to the last cell in the current row."
  (interactive)
  (let ((current-column (cdr tabcell-active-cell)))
    (goto-char (point-max))
    (while (not (tabulated-list-get-id))
      (forward-line -1))
    (setq tabcell-active-cell (cons (line-number-at-pos) current-column))
    (tabcell-update-cursor-position)
    (tabcall-debug "tabcell-move-column-end: current-cell=%S" tabcell-active-cell)))

(defun tabcell-update-cursor-position ()
  "Move the cursor to the active cell based on `tabcell-active-cell`."
  (let ((row (car tabcell-active-cell))
        (column (cdr tabcell-active-cell)))
    ;; Move to the correct row
    (goto-char (point-min)) ;; Go to the beginning of the buffer
    (forward-line (1- row))
    (goto-char (line-beginning-position)) ;; Start of the buffer
    (forward-char (1- (tabcell-calculate-column-offset column)))
    (tabcell-update-active-cell-overlay)
    (tabcall-debug "tabcell-update-cursor-position: row=%d, column=%d, point=%d" row column (point))))

(defun tabcell-calculate-column-offset (column)
  "Calculate the offset for COLUMN based on `tabulated-list-format`."
  (if (> column (length tabulated-list-format))
      (line-end-position)
    (let ((offset 0))
      (dotimes (i column)
        (setq offset (+ offset  (nth 1 (elt tabulated-list-format i)))))
      (+ column offset tabulated-list-padding))))

;; -------------------------------------------------------------------------------- mouvements

(defun tabcell-highlight (cell-start cell-end)
  "Highlight the specified range from CELL-START to CELL-END."
  (when (and tabcell-active-cell-overlay
             (overlayp tabcell-active-cell-overlay))
    (delete-overlay tabcell-active-cell-overlay))
  (setq tabcell-active-cell-overlay (make-overlay cell-start cell-end))
  (overlay-put tabcell-active-cell-overlay 'face 'highlight)
  (tabcall-debug "tabcell-highlight: cell-start=%d, cell-end=%d" cell-start cell-end))

(defun tabcell-list-next-column (&optional arg)
  "Go to the start of the next column after point on the current line.
If ARG is provided, move that many columns."
  (interactive "p")
  (let ((steps (or arg 1)))
    (dotimes (_ steps t)
      (let* ((next (or (next-single-property-change
                        (point) 'tabulated-list-column-name)
                       (point-max)))
             (prox (or (next-single-property-change
                        next 'tabulated-list-column-name)
                       (point-max))))
        (if (not (< next (line-end-position)))
            nil
          (goto-char next)
          (tabcell-highlight next prox)
          (let* ((cell (tabcell-get-active-cell))
                 (row (car cell))
                 (column (cdr cell)))
            (setq tabcell-active-cell (cons row column))
            (tabcall-debug "tabcell-list-next-column: row=%d, column=%d" row column)))))))

(defun tabcell-list-previous-column (&optional arg)
  "Go to the start of the previous column before point on the current line.
If ARG is provided, move that many columns."
  (interactive "p")
  (let ((steps (or arg 1)))
    (dotimes (_ steps t)
      (let* ((prev (or (previous-single-property-change
                        (point) 'tabulated-list-column-name)
                       1))
             (prox (or (next-single-property-change
                        prev 'tabulated-list-column-name)
                       (point-max))))
        (if (< prev (line-beginning-position))
            nil
          (goto-char prev)
          (tabcell-highlight prev prox)
          (let* ((cell (tabcell-get-active-cell))
                 (row (car cell))
                 (column (cdr cell)))
            (setq tabcell-active-cell (cons row column))
            (tabcall-debug "tabcell-list-previous-column: row=%d, column=%d" row column)))))))

;; Add advice to handle container refresh
(defun tabcell-refresh (&rest _)
  "Advice to refresh tabulated-list content for tabcell mode."
  (when (derived-mode-p 'tabcell-mode)
    (setq-local tabcell-line-cells nil)
    (tabcell-update-cursor-position)
    (tabcall-debug "tabcell-refresh: Refreshed container.")))

(provide 'tabcell)
