;; -*- mode: guix-scheme -*-

(use-modules
 (bj desktop))

(define tools-manifest
  (specifications->manifest
   '(;;"emacs-native-comp"
     "font-fira-code"
     )))

(concatenate-manifests
 (list
  tools-manifest))
