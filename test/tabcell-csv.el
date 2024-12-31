;;; tabcell-csv.el --- Example for loading large CSV into tabcell -*- lexical-binding: t -*-

(require 'tabcell)
(require 'csv)

(defun tabcell-load-csv (file-path)
  "Load a mock CSV file into a tabulated list buffer with tabcell-mode enabled.
FILE-PATH is the path to the CSV file to load."
  (interactive "fSelect CSV file: ")
  (let ((buffer (generate-new-buffer "*Tabcell CSV*")))
    (with-current-buffer buffer
      ;; Enable tabulated-list-mode and configure format
      (tabulated-list-mode)
      (let* ((data (with-temp-buffer
                     (insert-file-contents file-path)
                     (csv-parse-buffer t)))
             (header (mapcar #'car (car data)))
             (rows (cdr data))
             (num-columns (length header))
             (num-rows (length rows)))
        ;; Configure tabulated-list-format
        (setq tabulated-list-padding 2)
        (setq tabulated-list-format
              (vconcat `[("ID" 5 t)] (mapcar (lambda (col)
                                 (list col 5 t))
                               header)))
        ;; Configure tabulated-list-entries
        (setq tabulated-list-entries
	      (cl-loop for row in rows
		       for index from 0
		       for fields = (apply 'vector  (cons (number-to-string index) (mapcar #'cdr row)))
		       collect (list index fields)))
        (tabulated-list-init-header)
        (tabulated-list-print)
        ;; Enable tabcell-mode
        ;; Print statistics
        (message "CSV Loaded: %d columns, %d rows" num-columns num-rows)
        (message "Column Names: %s" (string-join header ", "))))
      (switch-to-buffer buffer)
      (tabcell-mode nil)))

(provide 'tabcell-csv)
;;; tabcell-csv.el ends here
