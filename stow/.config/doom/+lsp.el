;;; +lsp.el -*- lexical-binding: t; -*-


(defun dwim-open-test-or-class ()
  "This command will open the matching test file, or class file
  matching names of java files. This supports java maven project
  and will navigate to the src/main/java or src/test/java
  folders"
  (interactive)
  (find-file (bj/get-test-or-class-path (buffer-file-name))))

(defun bj/get-test-or-class-path (file-path)
  ""
  (when (string-match "\\(.*/src/.*/java/.*\\)/\\([A-Z].*.java\\)" file-path)
    (let ((folder (match-string 1 file-path))
          (file-name (match-string 2 file-path)))
      (concat (bj/toggle-class-or-test-path folder) "/" (bj/toggle-class-or-test file-name)))))

;;(bj/get-test-or-class-path "/home/benoit/src/perso/test-java/api/src/main/java/com/apiDemo/controller/BanksController.java")


(defun bj/toggle-class-or-test-path (path)
  ""

  (if (string-match "\\(.*/src/\\)\\(main\\|test\\)\\(/java/.*\\)" path)
      (let ((new-main-or-test (if (equal "main" (match-string 2 path)) "test" "main"))
            (prefix (match-string 1 path))
            (postfix (match-string 3 path)))
        (concat prefix new-main-or-test postfix))
    path))

;;(bj/toggle-class-or-test-path "/home/benoit/src/perso/test-java/api/src/test/java/com/apiDemo/controller")

(defun bj/toggle-class-or-test (file-name)
  (let ((basename (bj/get-java-basename file-name)))
    (concat (bj/toggle-class-or-test-basename basename) ".java")))

;;(bj/toggle-class-or-test "MyClass.java")

(defun bj/get-java-basename (file-name)
  ""
  (if (string-match "\\(.*\\)\\.java" file-name)
      (match-string 1 file-name)
    nil))
;;(bj/get-java-basename "MyClass.java")


(defun bj/toggle-class-or-test-basename (file-name)
  ""
  (if (string-match "\\(.*\\)Test" file-name)
      (match-string 1 file-name)
    (concat file-name "Test")))



;;(bj/toggle-class-or-test-basename "MyClass")
