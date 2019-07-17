;;; packages.el --- react layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defconst vue-packages
  '(web-mode
    add-node-modules-path
    company
    emmet-mode
    evil-matchit
    flycheck
    prettier-js
    smartparens
    yasnippet))

(defun vue/post-init-web-mode ()
  (define-derived-mode vue-mode web-mode "vuejs")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  (spacemacs/set-leader-keys-for-major-mode 'vue-mode "a" #'helm-google-suggest)
  (add-hook 'vue-mode-local-vars-hook #'spacemacs//vue-setup-backend))

(defun vue/post-init-add-node-modules-path ()
  (add-hook 'vue-mode-hook #'add-node-modules-path))

(defun vue/post-init-company ()
  (add-hook 'vue-mode-local-vars-hook #'spacemacs//vue-setup-company))

(defun vue/post-init-emmet-mode ()
  (add-hook 'vue-mode-hook #'spacemacs//vue-turn-on-emmet))

(defun vue/post-init-evil-matchit ()
  (add-hook 'vue-mode-hook 'turn-on-evil-matchit-mode))

(defun vue/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'vue-mode))
  (spacemacs/enable-flycheck 'vue-mode))

(defun vue/pre-init-prettier-js ()
  (add-to-list 'spacemacs--prettier-modes 'vue-mode))

(defun vue/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'vue-mode-hook #'smartparens-strict-mode)
    (add-hook 'vue-mode-hook #'smartparens-mode)))


(defun vue/post-init-yasnippet ()
  (add-hook 'vue-mode-hook #'spacemacs/load-yasnippet))
