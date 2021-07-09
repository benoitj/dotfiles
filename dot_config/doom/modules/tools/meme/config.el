;;; tools/meme/config.el -*- lexical-binding: t; -*-

(use-package! meme
  :commands (meme-file meme))

(map! :leader
      (:prefix-map ("o M" . "meme")
       (:desc "Meme" "M" #'meme)
       (:desc "file" "f" #'meme-file)))
