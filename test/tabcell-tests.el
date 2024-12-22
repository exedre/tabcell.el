;;; tabcell-tests.el --- Tests for tabcell.el -*- lexical-binding: t -*-

(require 'ert)
(require 'tabcell)

;;; Define common tabulated-list formats for tests
(defvar tabcell-test-tabulated-list-format
  [("Col1" 10 t) ("Col2" 20 t) ("Col3" 30 t)]
  "A standard format for tabulated-list-mode used in tabcell tests.")

;;; Test for `tabcell-cumulative-sum-with-step`
(ert-deftest tabcell-test-cumulative-sum-with-step ()
  "Test `tabcell-cumulative-sum-with-step` for normal and edge cases."
  ;; Normal case
  (let ((vector '(10 20 30)))
    (should (equal (tabcell-cumulative-sum-with-step vector tabcell-test-tabulated-list-format 2 5)
                   [2 17 42 67])))
  ;; Case with no step
  (let ((vector '(10 20 30)))
    (should (equal (tabcell-cumulative-sum-with-step vector tabcell-test-tabulated-list-format )
                   [10 30 60])))
  ;; Edge case: Empty vector
  (let ((vector '()))
    (should (equal (tabcell-cumulative-sum-with-step vector tabcell-test-tabulated-list-format )
                   [0]))))

;;; Test for `tabcell-get-active-cell`
(ert-deftest tabcell-test-get-active-cell ()
  "Test `tabcell-get-active-cell` with various cursor positions."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Col1" 10 t) ("Col2" 10 t) ("Col3" 10 t)])
    (tabulated-list-init-header)
    (insert "Row1Col1 Row1Col2 Row1Col3\n")
    (insert "Row2Col1 Row2Col2 Row2Col3\n")
    (goto-char (point-min))
    (forward-line 1)
    (forward-char 15)
    (should (equal (tabcell-get-active-cell) '(2 . 1)))))

;;; Test for `tabcell-find-index-in-vector`
(ert-deftest tabcell-test-find-index-in-vector ()
  "Test `tabcell-find-index-in-vector` with different vectors and targets."
  ;; Normal case
  (should (equal (tabcell-find-index-in-vector 15 [10 20 30]) 1))
  ;; Case where the target is smaller than all elements
  (should (equal (tabcell-find-index-in-vector 5 [10 20 30]) 0))
  ;; Case where the target is larger than all elements
  (should (equal (tabcell-find-index-in-vector 35 [10 20 30]) 3))
  ;; Edge case: Empty vector
  (should (equal (tabcell-find-index-in-vector 10 []) 0)))

;;; Test for `tabcell-fill-line-cells`
(ert-deftest tabcell-test-fill-line-cells ()
  "Test `tabcell-fill-line-cells` for proper column boundary calculations."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Col1" 10 t) ("Col2" 15 t) ("Col3" 20 t)])
    (tabulated-list-init-header)
    (insert "Row1Col1 Row1Col2 Row1Col3\n")
    (goto-char (point-min))
    (forward-line 1)
    (tabcell-fill-line-cells)
    (should (equal tabcell-line-cells [0 10 25 45]))))

;;; Test for `tabcell-update-active-cell-overlay`
(ert-deftest tabcell-test-update-active-cell-overlay ()
  "Test `tabcell-update-active-cell-overlay` to ensure the overlay updates correctly."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Col1" 10 t) ("Col2" 15 t) ("Col3" 20 t)])
    (tabulated-list-init-header)
    (insert "Row1Col1 Row1Col2 Row1Col3\n")
    (setq tabcell-active-cell '(1 . 2))
    (tabcell-update-active-cell-overlay)
    (let ((ov tabcell-active-cell-overlay))
      (should (overlayp ov))
      (should (equal (overlay-start ov) (+ (line-beginning-position) 25)))
      (should (equal (overlay-end ov) (+ (line-beginning-position) 45))))))

;;; Test for `tabcell-move-down`
(ert-deftest tabcell-test-move-down ()
  "Test `tabcell-move-down` to ensure it moves the active cell correctly."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Col1" 10 t) ("Col2" 10 t)])
    (tabulated-list-init-header)
    (insert "Row1Col1 Row1Col2\n")
    (insert "Row2Col1 Row2Col2\n")
    (setq tabcell-active-cell '(1 . 1))
    (goto-char (point-min))
    (forward-line 1)
    (tabcell-move-down)
    (should (equal tabcell-active-cell '(2 . 1)))))

;;; Test for edge cases in `tabcell-mode`
(ert-deftest tabcell-test-mode-edge-cases ()
  "Test `tabcell-mode` activation and deactivation in different contexts."
  (with-temp-buffer
    (tabcell-mode 1)
    (should (equal tabcell-active-cell '(1 . 0)))
    (tabcell-mode -1)
    (should (not tabcell-active-cell-overlay))))

(provide 'tabcell-tests)
;;; tabcell-tests.el ends here
