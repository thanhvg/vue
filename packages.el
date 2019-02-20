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
  '(
    vue-mode
    add-node-modules-path
    company
    emmet-mode
    evil-matchit
    flycheck
    prettier-js
    smartparens
    yasnippet
    ))

(defun vue/init-vue-mode ()
  "Initialize my package"
  (use-package vue-mode
    :defer t
    :mode (("\\.vue\\'"  . vue-mode))
    :config
    (setq mmm-submode-decoration-level 0)
    :init
    (add-hook 'mmm-mode-hook
              (lambda ()
                (set-face-background 'mmm-default-submode-face nil)))


    (spacemacs/set-leader-keys-for-major-mode 'vue-mode "a" #'vue-mode-edit-all-indirect)
    (spacemacs/set-leader-keys-for-major-mode 'vue-mode "p" #'vue-mode-edit-indirect-at-point)
    (spacemacs/set-leader-keys-for-major-mode 'vue-mode "v" #'vue-mode-reparse)

    (add-hook 'vue-mode-local-vars-hook #'spacemacs//vue-setup-backend)))

(defun vue/post-init-add-node-modules-path ()
  (spacemacs/add-to-hooks #'add-node-modules-path '(css-mode-hook
                                                    vue-mode-hook
                                                    vue-html-mode-hook
                                                    js-mode-hook)))

(defun vue/post-init-company ()
  (add-hook 'vue-mode-local-vars-hook #'spacemacs//vue-setup-company))

(defun vue/post-init-emmet-mode ()
  ;; (add-hook 'vue-html-mode 'emmet-mode))
  ;; (spacemacs/add-to-hooks 'emmet-mode '(vue-html-mode-hook)))
  ;; (spacemacs/add-to-hooks 'emmet-mode '(mmm-vue-html-mode-hook)))
  (add-hook 'mmm-vue-html-mode-enter-hook #'spacemacs//vue-turn-on-emmet)
  (add-hook 'mmm-vue-html-mode-exit-hook #'spacemacs//vue-turn-off-emmet))

(defun vue/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'vue-html-mode
               '((evilmi-simple-get-tag evilmi-simple-jump)
                 (evilmi-javascript-get-tag evilmi-javascript-jump)
                 (evilmi-html-get-tag evilmi-html-jump))))
  (add-hook `vue-mode-hook `turn-on-evil-matchit-mode))

(defun vue/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (dolist (checker '(javascript-eslint))
      (flycheck-add-mode checker 'vue-mode)
      (flycheck-add-mode checker 'js-mode)
      (flycheck-add-mode checker 'vue-html-mode)))
  (spacemacs/enable-flycheck 'vue-mode))

(defun vue/pre-init-prettier-js ()
  (dolist (mode '(vue-mode))
    (add-to-list 'spacemacs--prettier-modes mode)))

(defun vue/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'vue-mode-hook #'smartparens-strict-mode)
    (add-hook 'vue-mode-hook #'smartparens-mode)))


(defun vue/post-init-yasnippet ()
  (spacemacs/add-to-hooks 'spacemacs/force-yasnippet-off '(mmm-vue-html-mode-enter-hook)
  (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(mmm-js-mode-enter-hook))))
