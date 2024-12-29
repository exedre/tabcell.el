;;; tabcell.el --- Active cell tracking for tabulated lists -*- lexical-binding: t -*-

;; Author: Emmanuele Somma <emmanuele@exedre.org>
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;; Keywords: convenience, tabulated-list
;; URL: https://github.com/exedre/tabcell.el

;;; Commentary:

;; tabcell.el provides a minor mode for active cell tracking and navigation
;; within Emacs' `tabulated-list-mode`.  It highlights the currently active cell
;; and provides keyboard shortcuts for efficient movement between rows and columns.

;; Features:
;; - Highlights the active cell in a tabulated list.
;; - Supports keyboard navigation within the table.

;; Installation:
;; 1. Place `tabcell.el` in your Emacs load path.
;; 2. Add the following to your Emacs configuration:
;;
;;    (require 'tabcell)
;;
;; Usage:
;; - Enable `tabcell-mode` in any `tabulated-list-mode` buffer:
;;    (tabcell-mode 1)
;;
;; - Key bindings:
;;   | Key         | Action                         |
;;   |-------------+--------------------------------|
;;   | <up>        | Move to the cell above         |
;;   | <down>      | Move to the cell below         |
;;   | <left>      | Move to the previous column    |
;;   | <right>     | Move to the next column        |
;;   | C-<home>    | Move to the first cell         |
;;   | C-<end>     | Move to the last cell          |
;;
;; For detailed usage and documentation, visit:
;; https://github.com/exedre/tabcell.el


;; Features:
;; - Highlights the active cell in a tabulated list.
;; - Supports keyboard navigation within the table.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)

(add-to-list 'load-path default-directory)

;; -------------------------------------------------------------------------------- cell-mode

(defvar-local tabcell-mode nil
  "Tabulated list cell mode.")

(defvar-local tabcell--active-cell '(1 . 0)
  "The currently active cell in the tabular list, represented as (row . column).")

(defvar-local tabcell--active-cell-overlay nil
  "Overlay used to highlight the currently active cell.")

(defvar-local tabcell--line-cells nil
  "Cells for current line.")

(defvar-local tabcell--buffer nil
  "Record the working buffer.")

(defvar tabcell-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Horizontal movements
    (define-key map (kbd "<home>") #'tabcell--move-line-home)
    (define-key map (kbd "<end>") #'tabcell--move-line-end)
    (define-key map (kbd "C-<right>") #'tabcell--move-line-end)
    (define-key map (kbd "C-<left>") #'tabcell--move-line-home)
    (define-key map (kbd "<right>") #'tabcell--next-column)
    (define-key map (kbd "<left>") #'tabcell--previous-column)
    ;; Vertical movements
    (define-key map (kbd "C-<home>") #'tabcell--move-sheet-home)
    (define-key map (kbd "C-<end>") #'tabcell--move-sheet-end)
    (define-key map (kbd "C-<up>") #'tabcell--move-column-home)
    (define-key map (kbd "C-<down>") #'tabcell--move-column-end)
    (define-key map (kbd "<down>") #'tabcell--move-line-down)
    (define-key map (kbd "<up>") #'tabcell--move-line-up)
    (define-key map (kbd "<prior>") #'tabcell--move-sheet-prior)
    (define-key map (kbd "<next>") #'tabcell--move-sheet-next)
    (define-key map [mouse-1] #'tabcell-track-active-cell)
    (define-key map [drag-mouse-1] #'ignore)
    map)
  "Keymap for `tabcell-mode`.  Provides key bindings for navigating the tabular list.")

;;
;; (@* "Entry")
;;

(defun tabcell--enable ()
  "Enable `tabcell' in current buffer."
  (if (derived-mode-p 'tabulated-list-mode)
      (progn
        (setq tabcell--buffer (current-buffer))	
	;; Add keymap	
        (set (make-local-variable 'minor-mode-overriding-map-alist)
             (cons (cons 'tabcell-mode tabcell-mode-map)
                   minor-mode-overriding-map-alist))
        ;; Activate the mode
        ;(add-hook 'pre-command-hook #'tabcell--pre-command nil t)
        ;(add-hook 'post-command-hook #'tabcell--post-command nil t)
	;(add-hook 'after-change-functions #'tabcell--after-change nil t))
        (setq tabcell--active-cell-overlay nil)	
        (setq tabcell--active-cell (tabcell-get-active-cell))
	(unless tabcell--line-cells
	  (tabcell--fill-line-cells))
	(tabcell--update-cursor-position))
    (tabcell-mode -1)
    (user-error "[WARNING] You can't enable tabcell in buffer that aren't derived from `tabulated-list-mode`"))) 

(defun tabcell--disable ()
  "Disable `tabcell' in current buffer."
  (when tabcell--active-cell-overlay
    (delete-overlay tabcell--active-cell-overlay)
    (setq tabcell--active-cell-overlay nil))
  ;(remove-hook 'pre-command-hook #'tabcell--pre-command t)
  ;(remove-hook 'post-command-hook #'tabcell--post-command t)
  ;(remove-hook 'after-change-functions #'tabcell--after-change t))
  (setq minor-mode-overriding-map-alist
        (assq-delete-all 'tabcell-mode minor-mode-overriding-map-alist)))

;;;###autoload
(define-minor-mode tabcell-mode
  "Minor mode 'tabcell-mode'."
  :lighter " TCell"
  :group tabcell
  (if tabcell-mode (tabcell--enable) (tabcell--disable)))

;;
;; (@* "Faces" )
;;


(defface tabcell-select-face
  '((t :box (:line-width -1 :color "#65A7E2" :style nil)))
  "Face when selecting the current box."
  :group 'tabcell)

;;
;; (@* "Macro" )
;;

(defmacro tabcell-current-buffer (&rest body)
  "Execute BODY with in the current working buffer."
  (declare (indent 0) (debug t))
  `(when tabcell--buffer (with-current-buffer tabcell--buffer (progn ,@body))))

;;
;; (@* "Util" )
;;


(defun tabcell--column-offset (column)
  "Retrieve COLUMN index within the current row."
  (when (integerp column) 
    (unless tabcell--line-cells
      (tabcell--fill-line-cells))
    (tabcell-find-index-in-vector column tabcell--line-cells)))


(defun tabcell-get-active-cell ()
  "Return the active cell (row . column) based on the cursor position."
  (interactive)
  (let* ((row (line-number-at-pos))
        (column (current-column)) ;; Use cursor position to calculate column    
	(cell (cons row (tabcell--column-offset column))))
    cell))


(defun tabcell-track-active-cell (event)
  "EVENT to track the currently active cell and update its overlay."
  (interactive (list last-input-event))
  (let* ((coord  (posn-col-row (event-start event)))
           (bpoint (posn-point  (event-start event)))
           (row (1+ (cdr coord))))
    (goto-char bpoint)
    (tabcell--this-column)))


(defun tabcell-find-index-in-vector (num vec)
  "Find the index of the first element in VEC greater than or equal to NUM.
If no such element is found, return the length of VEC."
  (let ((len (length vec)))
    (catch 'found
      (dotimes (index len)
        (when (>= (car (nth index vec)) num)
          (throw 'found index)))
      len))) ;; Return the length of the vector if no element is found


(defun tabcell--fill-line-cells ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq-local tabcell--line-cells nil)
    (catch 'eol
      (while t
	(let* ((next (or (next-single-property-change
			  (point) 'tabulated-list-column-name)
			 (point-max)))
               (prox (or (next-single-property-change
			  next 'tabulated-list-column-name)
			 (point-max))))
	  (when (not (< next (line-end-position)))
            (throw 'eol tabcell--line-cells))
	  (goto-char (1- prox))
	  (setq tabcell--line-cells
		(push  (cons next (1- prox)) tabcell--line-cells))))))
  (setq tabcell--line-cells
	(reverse tabcell--line-cells)))



(defun tabcell--update-cursor-position ()
  "Move the cursor to the active cell based on `tabcell--active-cell`."
  (unless tabcell--line-cells
    (tabcell--fill-line-cells))
  (let* ((row (car tabcell--active-cell))
         (column (cdr tabcell--active-cell))
	 (target (car (nth column tabcell--line-cells))))
    (goto-char (point-min))
    (forward-line (1- row))
    (forward-char target)
    (tabcell--this-column)))


(defun tabcell--delete-overlays ()
  (when (and tabcell--active-cell-overlay
             (overlayp tabcell--active-cell-overlay))
    (delete-overlay tabcell--active-cell-overlay)))


(defun tabcell--highlight (cell-start cell-end)  
  "Highlight the specified range from CELL-START to CELL-END."
  (tabcell--delete-overlays)
  (setq tabcell--active-cell-overlay (make-overlay cell-start cell-end))
  (overlay-put tabcell--active-cell-overlay 'face 'highlight))


(defun tabcell--move (direction)
  "Move the active cell in the specified DIRECTION.
DIRECTION should be a symbol: 'up, 'down, 'left, 'right, 'home, 'end,
'sheet-home, 'sheet-end, 'page-up, or 'page-down."
  (let ((current-cell tabcell--active-cell)
        (row (car tabcell--active-cell))
        (column (cdr tabcell--active-cell)))
    (pcase direction
      ('up
       (when (> row 1)
	 (setq tabcell--active-cell (cons (1- row) column))))
      ('down
       (when (<= row (tabcell--row-max))
         (setq tabcell--active-cell (cons (1+ row) column))))
      ('left
       (when (> column 0)
         (setq tabcell--active-cell (cons row (1- column)))))
      ('right
       (when (<= column (tabcell--column-max))
         (setq tabcell--active-cell (cons row (1+ column)))))
      ('home
       (setq tabcell--active-cell (cons row (tabcell--column-max))))
      ('end
       (setq tabcell--active-cell (cons row (tabcell--column-min))))
      ('sheet-home
       (setq tabcell--active-cell (cons (tabcell--row-min)
					(tabcell--column-min))))
      ('sheet-end
       (setq tabcell--active-cell (cons (tabcell--row-max)
					(tabcell--column-max))))
      ('page-up (let ((new-row (max 1 (- row (window-body-height)))))
                  (setq tabcell--active-cell (cons new-row column))))
      ('page-down (let ((new-row (min (1- (line-number-at-pos (point-max)))
				      (+ row (window-body-height)))))
                    (setq tabcell--active-cell (cons new-row column)))))
    (tabcell--update-cursor-position)))


(defun tabcell--move-line-up ()
  "Move to the cell above."
  (interactive)
  (tabcell--move 'up))

(defun tabcell--move-line-down ()
  "Move to the cell below."
  (interactive)
  (tabcell--move 'down))

(defun tabcell--move-line-home ()
  "Move to the first cell in the current row."
  (interactive)
  (tabcell--move 'home))

(defun tabcell--move-line-end ()
  "Move to the last cell in the current row."
  (interactive)
  (tabcell--move 'end))

(defun tabcell--move-column-home ()
  "Move to the first cell in the current column."
  (interactive)
  (tabcell--move 'sheet-home))

(defun tabcell--move-column-end ()
  "Move to the last cell in the current column."
  (interactive)
  (tabcell--move 'sheet-end))

(defun tabcell--move-sheet-home ()
  "Move to the first cell in the sheet."
  (interactive)
  (tabcell--move 'sheet-home))

(defun tabcell--move-sheet-end ()
  "Move to the last cell in the sheet."
  (interactive)
  (tabcell--move 'sheet-end))

(defun tabcell--move-sheet-prior ()
  "Move one page up in the sheet."
  (interactive)
  (tabcell--move 'page-up))

(defun tabcell--move-sheet-next ()
  "Move one page down in the sheet."
  (interactive)
  (tabcell--move 'page-down))


(defun tabcell--column-max () (1- (length tabulated-list-format)))	      
(defun tabcell--row-max    () (1- (line-number-at-pos (point-max))))
(defun tabcell--column-min () 0)
(defun tabcell--row-min    () 1)


(defun tabcell--update-column-position (start end)
  "Highlight the range from START to END and update the active cell."
  (goto-char start)  
  (tabcell--highlight start end)
  (let* ((cell (tabcell-get-active-cell))
         (row (car cell))
         (column (cdr cell)))
    (setq tabcell--active-cell (cons row column))))


(defun tabcell--this-column (&optional arg)
  "Stay at the current column on the current line.
If ARG is provided, perform no movement but re-highlight."
  (interactive "p")
  (let ((steps (or arg 1)))
    (dotimes (_ steps t)
      (let* ((next (or (next-single-property-change
                        (point) 'tabulated-list-column-name)
                       (point-max)))
             (prev (or (previous-single-property-change
                        (1+ (point)) 'tabulated-list-column-name)
                       (point-min))))
        (if (not (<= next (line-end-position)))
            nil
          (tabcell--update-column-position prev next))))))


(defun tabcell--next-column (&optional arg)
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
          (tabcell--update-column-position next prox))))))


(defun tabcell--previous-column (&optional arg)
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
            (tabcell--update-column-position prev prox)))))))


;; Add advice to handle container refresh

(defun tabcell-refresh (&optional offset &rest _)
  "Advice to refresh tabulated-list content for tabcell mode."
  (when (tabcell-mode)
    (tabcell--update-cursor-position)))


(provide 'tabcell)

;;; tabcell.el ends here
