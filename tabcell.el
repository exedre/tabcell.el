;;; tabcell.el --- Active cell tracking for tabulated lists -*- lexical-binding: t -*-

;; Author: Emmanuele Somma <emmanuele@exedre.org>
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;; Keywords: convenience, tabulated-list
;; URL: https://github.com/exedre/tabcell.el

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

(defvar tabcell-debug nil
  "Log debugging information. FORMAT-STRING and ARGS behave like `message`.")

(defun tabcall-debug (format-string &rest args)
  "Log debugging information. FORMAT-STRING and ARGS behave like `message`."
  (if tabcell-debug
      (apply #'message (concat "[DEBUG] " format-string) args)))

(defvar-local tabcell-active-cell '(1 . 0)
  "The currently active cell in the tabular list, represented as (row . column).")

(defvar-local tabcell-active-cell-overlay nil
  "Overlay used to highlight the currently active cell.")

(defvar-local tabcell-line-cells nil
  "Cells for current line.")

(defvar tabcell-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Horizontal movements
    (define-key map (kbd "<home>") #'tabcell-move-line-home)
    (define-key map (kbd "<end>") #'tabcell-move-line-end)    
    (define-key map (kbd "C-<right>") #'tabcell-move-line-end)
    (define-key map (kbd "C-<left>") #'tabcell-move-line-home)    
    (define-key map (kbd "<right>") #'tabcell-next-column)
    (define-key map (kbd "<left>") #'tabcell-previous-column)
    ;; Vertical movements
    (define-key map (kbd "C-<home>") #'tabcell-move-sheet-home)
    (define-key map (kbd "C-<end>") #'tabcell-move-sheet-end)
    (define-key map (kbd "C-<up>") #'tabcell-move-column-home)
    (define-key map (kbd "C-<down>") #'tabcell-move-column-end)
    (define-key map (kbd "<down>") #'tabcell-move-line-down)
    (define-key map (kbd "<up>") #'tabcell-move-line-up)
    (define-key map (kbd "<prior>") #'tabcell-move-sheet-prior)
    (define-key map (kbd "<next>") #'tabcell-move-sheet-next)
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
  (tabcall-debug "tabcell-track-active-cell-in: RC=%S" tabcell-active-cell)  
  (let* ((coord  (posn-col-row (event-start event)))
           (bpoint (posn-point  (event-start event)))
           (row (1+ (cdr coord))))
    (goto-char bpoint)
    (let ((prev (or (previous-single-property-change
                     (point) 'tabulated-list-column-name)
                    1)))
      (unless (< prev (pos-bol))
        (goto-char prev)
	(when (tabcell-previous-column)
	  (tabcell-next-column))
	(tabcall-debug "tabcell-track-active-cell-out: RC=%S" tabcell-active-cell)))))
      

(define-minor-mode tabcell-mode
  "Minor mode to enable active cell tracking in `tabcell-mode`."
  :lighter " tc"
  :keymap nil
  (if tabcell-mode
      (progn
	;; Add keymap
        (set (make-local-variable 'minor-mode-overriding-map-alist)
             (cons (cons 'tabcell-mode tabcell-mode-map)
                   minor-mode-overriding-map-alist))
        ;; Activate the mode
        (setq tabcell-active-cell '(1 . 0))
        (setq tabcell-active-cell-overlay nil)
        (tabcall-debug "tabcell-mode: Activated."))
    ;; Deactivate the mode
    (when tabcell-active-cell-overlay
      (delete-overlay tabcell-active-cell-overlay)
      (setq tabcell-active-cell-overlay nil)
      (setq minor-mode-overriding-map-alist
            (assq-delete-all 'tabcell-mode minor-mode-overriding-map-alist))
      (tabcall-debug "tabcell-mode: Deactivated."))))


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


(defun tabcell-update-cursor-position ()
  "Move the cursor to the active cell based on `tabcell-active-cell`."
  (let ((row (car tabcell-active-cell))
        (column (cdr tabcell-active-cell)))
    (goto-char (point-min))
    (forward-line (1- row))
    (goto-char (line-beginning-position))
    (forward-char (1- (tabcell-calculate-column-offset column)))
    (tabcell-next-column)
    (tabcall-debug "tabcell-update-cursor-position: row=%d, column=%d, point=%d" row column (point))))


(defun tabcell-calculate-column-offset (column)
  "Calculate the offset for COLUMN based on `tabulated-list-format`."
  (if (> column (length tabulated-list-format))
      (line-end-position)
    (let ((offset 0))
      (dotimes (i column)
        (setq offset (+ offset  (nth 1 (elt tabulated-list-format i)))))
      (+ column offset tabulated-list-padding))))


(defun tabcell-highlight (cell-start cell-end)
  "Highlight the specified range from CELL-START to CELL-END."
  (when (and tabcell-active-cell-overlay
             (overlayp tabcell-active-cell-overlay))
    (delete-overlay tabcell-active-cell-overlay))
  (setq tabcell-active-cell-overlay (make-overlay cell-start cell-end))
  (overlay-put tabcell-active-cell-overlay 'face 'highlight)
  (tabcall-debug "tabcell-highlight: cell-start=%d, cell-end=%d" cell-start cell-end))


;; -------------------------------------------------------------------------------- mouvements

(defun tabcell-move (direction)
  "Move the active cell in the specified DIRECTION.
DIRECTION should be a symbol: 'up, 'down, 'left, 'right, 'home, 'end,
'sheet-home, 'sheet-end, 'page-up, or 'page-down."
  (let ((current-cell tabcell-active-cell)
        (row (car tabcell-active-cell))
        (column (cdr tabcell-active-cell)))
    (pcase direction
      ('up (when (> row 1)
             (setq tabcell-active-cell (cons (1- row) column))))
      ('down (when (< row (line-number-at-pos (point-max)))
               (setq tabcell-active-cell (cons (1+ row) column))))
      ('left (when (> column 0)
               (setq tabcell-active-cell (cons row (1- column)))))
      ('right (when (< column (1- (length tabulated-list-format)))
                (setq tabcell-active-cell (cons row (1+ column)))))
      ('home (setq tabcell-active-cell (cons row 0)))
      ('end (setq tabcell-active-cell (cons row (1- (length tabulated-list-format)))))
      ('sheet-home (setq tabcell-active-cell (cons 1 0)))
      ('sheet-end (setq tabcell-active-cell (cons (1- (line-number-at-pos (point-max)))
						  (1- (length tabulated-list-format)))))
      ('page-up (let ((new-row (max 1 (- row (window-body-height)))))
                  (setq tabcell-active-cell (cons new-row column))))
      ('page-down (let ((new-row (min (1- (line-number-at-pos (point-max)))
				      (+ row (window-body-height)))))
                    (setq tabcell-active-cell (cons new-row column)))))
    (tabcell-update-cursor-position)
    (tabcall-debug "tabcell-move: direction=%s current-cell=%S" direction tabcell-active-cell)))


(defun tabcell-move-line-up ()
  "Move to the cell above."
  (interactive)
  (tabcell-move 'up))

(defun tabcell-move-line-down ()
  "Move to the cell below."
  (interactive)
  (tabcell-move 'down))

(defun tabcell-move-line-home ()
  "Move to the first cell in the current row."
  (interactive)
  (tabcell-move 'home))

(defun tabcell-move-line-end ()
  "Move to the last cell in the current row."
  (interactive)
  (tabcell-move 'end))

(defun tabcell-move-column-home ()
  "Move to the first cell in the current column."
  (interactive)
  (tabcell-move 'sheet-home))

(defun tabcell-move-column-end ()
  "Move to the last cell in the current column."
  (interactive)
  (tabcell-move 'sheet-end))

(defun tabcell-move-sheet-home ()
  "Move to the first cell in the sheet."
  (interactive)
  (tabcell-move 'sheet-home))

(defun tabcell-move-sheet-end ()
  "Move to the last cell in the sheet."
  (interactive)
  (tabcell-move 'sheet-end))

(defun tabcell-move-sheet-prior ()
  "Move one page up in the sheet."
  (interactive)
  (tabcell-move 'page-up))

(defun tabcell-move-sheet-next ()
  "Move one page down in the sheet."
  (interactive)
  (tabcell-move 'page-down))


(defun tabcell-next-column (&optional arg)
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
            (tabcall-debug "tabcell-list-column: row=%d, column=%d" row column)))))))


(defun tabcell-previous-column (&optional arg)
  "Go to the start of the previous column before point on the current line.
If ARG is provided, move that many columns."
  (interactive "p")
  (let ((steps (or arg 1)))
    (dotimes (_ steps t)
      (when (> (1- (point)) tabulated-list-padding)
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
            (tabcall-debug "tabcell-previous-column: row=%d, column=%d" row column))))))))


;; Add advice to handle container refresh

(defun tabcell-refresh (&rest _)
  "Advice to refresh tabulated-list content for tabcell mode."
  (when (derived-mode-p 'tabcell-mode)
    (setq-local tabcell-line-cells nil)
    (tabcell-update-cursor-position)
    (tabcall-debug "tabcell-refresh: Refreshed container.")))

(provide 'tabcell)
