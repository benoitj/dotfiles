;;; -*- lexical-binding: t -*-

(use-package plantuml-mode
  :config
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-output-type "svg")
  (setq plantuml-java-args '("-jar")))
