;; -*- mode: guix-scheme-*-

(define-module (bj desktop)
 #:use-module (guix packages)
 #:use-module (guix git-download)
 #:use-module (gnu packages suckless))

(define-public my-dmenu
  (package
   (inherit dmenu)
   (name "my-dmenu")
   (version "latest")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/benoitj/dmenu")
       (commit "d34439f61aeffa23f83c075fedbb68e7c3e72cee")))
     (file-name
      (git-file-name name version))
     (sha256
      (base32
       "127wqlnrbi5zwz598vqdv7pa6r981ks9j9s8s1rmwr9l1m3rnffs"))))))
