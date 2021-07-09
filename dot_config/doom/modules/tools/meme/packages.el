;; -*- no-byte-compile: t; -*-
;;; tools/meme/packages.el

(package! imgur)

(package! meme :recipe
  (:host github
   :repo "larsmagne/meme"
   :files ("*.el" "images"))
  :pin "77b0f76")
