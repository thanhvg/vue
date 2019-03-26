;;; funcs.el --- react layer funcs file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; backend
(defun spacemacs//vue-setup-backend ()
  "Conditionally setup react backend."
  (pcase vue-backend
    (`dumb (spacemacs//vue-setup-dumb))
    (`lsp (spacemacs//vue-setup-lsp))))

(defun spacemacs//vue-setup-company ()
  "Conditionally setup company based on backend."
  (pcase vue-backend
    (`dumb (spacemacs//vue-setup-dumb-company))
    (`lsp (spacemacs//vue-setup-lsp-company))))


;; lsp
(defun spacemacs//vue-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        ;; error checking from lsp langserver sucks, turn it off
        ;; so eslint won't be overriden
        (setq-local lsp-prefer-flymake :none)
        (lsp))
    ;; (flycheck-select-checker 'javascript-eslint))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dotfile."))))

(defun spacemacs//vue-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes vue-mode
          :variables company-minimum-prefix-length 2
          :append-hooks nil
          :call-hooks t)
        (company-mode)
        (fix-lsp-company-prefix))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; dumb

(defun spacemacs//vue-setup-dumb-imenu () 
  (setq imenu-generic-expression '(("html" "^<template>$" 0)
                                   ("js" "^<script>$" 0)
                                   ("js" "^\\s-*\\(data\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(mounted\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(beforeMount\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(beforeDestroy\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(created\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(computed\\):\\s-?{" 1)
                                   ("js" "^\\s-*\\(watched\\):\\s-?{" 1)
                                   ("js" "^\\s-*\\(methods\\):\\s-?{" 1)
                                   ("js" "^\\s-*\\(props\\):\\s-?{" 1)
                                   ("css" "^<css>$" 0))
        imenu-create-index-function #'imenu-default-create-index-function))

(defun spacemacs//vue-setup-dumb ()
  (add-to-list 'spacemacs-jump-handlers-vue-mode 'dumb-jump-go)
  (add-to-list 'spacemacs-jump-handlers-vue-html-mode 'dumb-jump-go)
  (spacemacs//vue-setup-dumb-imenu))

(defun spacemacs//vue-setup-dumb-company ()
  (spacemacs|add-company-backends :backends company-capf
                                  :modes vue-mode
                                  :variables company-minimum-prefix-length 1)
  (company-mode))


;; Emmet
(defun spacemacs//vue-turn-on-emmet ()
  (emmet-mode 1)
  (setq-local emmet-indent-after-insert nil))

(defun spacemacs//vue-turn-off-emmet ()
  (emmet-mode -1))

;; Others

(defun spacemacs//vue-setup-yasnippet ()
  (yas-activate-extra-mode 'js-mode))

(defun spacemacs//vue-setup-eslint ()
  (flycheck-select-checker 'javascript-eslint))


