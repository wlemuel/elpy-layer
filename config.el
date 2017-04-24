;;; config.el --- Python Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; (spacemacs|defvar-company-backends python-mode)
;; (spacemacs|defvar-company-backends inferior-python-mode)
;; (spacemacs|defvar-company-backends pip-requirements-mode)

;; (spacemacs|define-jump-handlers python-mode)

(defvar python-enable-yapf-format-on-save nil
  "If non-nil, automatically format code with YAPF on save.")

(defvar python-fill-column 79
  "Fill column value for python buffers")

(defvar python-tab-width 4
  "Tab width value for python buffers")

(defvar python-sort-imports-on-save nil
  "If non-nil, automatically sort imports on save.")
