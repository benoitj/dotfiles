;; -*- mode: guix-scheme-*-

(define-module
  (bj desktop)
  #:use-module
  (guix packages)
  #:use-module
  (guix git-download)
  #:use-module
  (gnu packages suckless)
  #:use-module
  (gnu packages web-browsers)
  #:use-module
  (guix build-system gnu)
  #:use-module
  ((guix licenses)
   #:prefix license:)
  #:use-module
  (gnu packages fonts)
  #:use-module
  (gnu packages fontutils)
  #:use-module
  (gnu packages xorg)
  #:use-module
  (guix utils)
  #:use-module
  (guix download)
  #:use-module
  (guix build-system python)
  #:use-module
  (gnu packages python-xyz)
  #:use-module
  (gnu packages python-web)
  #:use-module
  (gnu packages qt)
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
       (commit "04aa83a124c4eee375d31692553ba767c477f02d")))
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
       (commit "3f56475629e46e245ef9a486b31e10997486c050")))
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
    `(#:tests? #f                       ; no tests
      #:make-flags
      (list
       (string-append "CC=" ,(cc-for-target))
       (string-append "PREFIX=" %output)
       (string-append "FREETYPEINC="
                      (assoc-ref %build-inputs "freetype")
                      "/include/freetype2"))
      #:phases
      (modify-phases %standard-phases
                     (delete 'configure))))
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

(define-public my-qutebrowser
  (package
   (name "my-qutebrowser")
   (version "2.0.2")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append "https://github.com/qutebrowser/"
                     "qutebrowser/releases/download/v" version "/"
                     "qutebrowser-" version ".tar.gz"))
     (sha256
      (base32 "0fxkazz4ykmkiww27l92yr96hq00qn5vvjmknxcy4cl97d2pxa28"))))
   (build-system python-build-system)
   (native-inputs
    `(("python-attrs" ,python-attrs)))
                                        ; for tests
   (inputs
    `(("python-colorama" ,python-colorama)
      ("python-cssutils" ,python-cssutils)
      ("python-jinja2" ,python-jinja2)
      ("python-markupsafe" ,python-markupsafe)
      ("python-pygments" ,python-pygments)
      ("python-pypeg2" ,python-pypeg2)
      ("python-pyyaml" ,python-pyyaml)
      ("python-importlib-resources" ,python-importlib-resources)
      ;; FIXME: python-pyqtwebengine needs to come before python-pyqt so
      ;; that it's __init__.py is used first.
      ("python-pyqtwebengine" ,python-pyqtwebengine)
      ("python-pyqt" ,python-pyqt)
      ;; While qtwebengine is provided by python-pyqtwebengine, it's
      ;; included here so we can wrap QTWEBENGINEPROCESS_PATH.
      ("qtwebengine" ,qtwebengine)))
   (arguments
    `( ;; FIXME: With the existance of qtwebengine, tests can now run.  But
      ;; they are still disabled because test phase hangs.  It's not readily
      ;; apparent as to why.
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
                     (add-before 'check 'set-env-offscreen
                                 (lambda _
                                   (setenv "QT_QPA_PLATFORM" "offscreen")
                                   #t))
                     (add-after 'install 'install-more
                                (lambda*
                                    (#:key outputs #:allow-other-keys)
                                  (let*
                                      ((out
                                        (assoc-ref outputs "out"))
                                       (app
                                        (string-append out "/share/applications"))
                                       (hicolor
                                        (string-append out "/share/icons/hicolor")))
                                    (install-file "doc/qutebrowser.1"
                                                  (string-append out "/share/man/man1"))
                                    (for-each
                                     (lambda
                                         (i)
                                       (let
                                           ((src
                                             (format #f "icons/qutebrowser-~dx~d.png" i i))
                                            (dest
                                             (format #f "~a/~dx~d/apps/qutebrowser.png"
                                                     hicolor i i)))
                                         (mkdir-p
                                          (dirname dest))
                                         (copy-file src dest)))
                                     '(16 24 32 48 64 128 256 512))
                                    (install-file "icons/qutebrowser.svg"
                                                  (string-append hicolor "/scalable/apps"))
                                    (substitute* "misc/org.qutebrowser.qutebrowser.desktop"
                                                 (("Exec=qutebrowser")
                                                  (string-append "Exec=" out "/bin/qutebrowser")))
                                    (install-file "misc/org.qutebrowser.qutebrowser.desktop" app)
                                    #t)))
                     (add-after 'wrap 'wrap-qt-process-path
                                (lambda*
                                    (#:key inputs outputs #:allow-other-keys)
                                  (let*
                                      ((out
                                        (assoc-ref outputs "out"))
                                       (bin
                                        (string-append out "/bin/qutebrowser"))
                                       (qt-process-path
                                        (string-append
                                         (assoc-ref inputs "qtwebengine")
                                         "/lib/qt5/libexec/QtWebEngineProcess")))
                                    (wrap-program bin
                                                  `("QTWEBENGINEPROCESS_PATH" =
                                                    (,qt-process-path)))
                                    #t))))))
   (home-page "https://qutebrowser.org/")
   (synopsis "Minimal, keyboard-focused, vim-like web browser")
   (description "qutebrowser is a keyboard-focused browser with a minimal
GUI.  It is based on PyQt5 and QtWebEngine.")
   (license license:gpl3+)))
