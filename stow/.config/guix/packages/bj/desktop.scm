;; -*- mode: guix-scheme-*-

(define-module (bj desktop)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages suckless)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages xorg)
  #:use-module (guix utils)
  )

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

(define-public my-st
  (package
    (inherit st)
    (name "my-st")
    (version "latest")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/benoitj/st")
         (commit "e3c45af03fd7e90f39b79607318d6c446053b669")))
       (file-name
        (git-file-name name version))
       (sha256
        (base32
         "0p13afvxg2jhs6xqi733dhi3i0x72yyd79bqnxsivbh24s8ijsp0"))))))

(define-public my-dwm
  (package
    (inherit dwm)
    (name "my-dwm")
    (version "latest")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/benoitj/dwm")
         (commit "1f87e532c3d02fd5798c2d0d7ebb495545cacd29")))
       (file-name
        (git-file-name name version))
       (sha256
        (base32
         "1zqpnixpdm92abl7walxy210l5z79m1hhdyi1sp221y7lln1inir"))))))


(define-public my-dwmstatus
  (package
    (name "my-dwmstatus")
    (version "latest")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/benoitj/dwmstatus")
         (commit "9b91a90061b3c9bb28f5d9f44b823e7bc9b2fc8c")))
       (file-name
        (git-file-name name version))
       (sha256
        (base32
         "0wqw4h20whrq40mvyx9x6vv03vizv6x0vi55hb8bdrzrf442kil6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output)
             (string-append "FREETYPEINC="
                            (assoc-ref %build-inputs "freetype")
                            "/include/freetype2"))
       #:phases
       (modify-phases %standard-phases (delete 'configure))))
    (inputs
     `(("freetype" ,freetype)
       ("libxft" ,libxft)
       ("libx11" ,libx11)
       ("libxinerama" ,libxinerama)))
    (home-page "https://git.suckless.org/dwmstatus")
    (synopsis "DWM status bar")
    (description
     "dwm status bar.")
    (license license:x11)))
