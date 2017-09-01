;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Muriithi Frederick Muriuki <fredmanglis@gmail.com>

(define-module (gn packages ipfs)
  #:use-module (guix store)
  #:use-module (gnu packages base)
  #:use-module (gnu packages golang)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu))

(define-public go-logging
  (package
   (name "go-logging")
   (version "1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://github.com/op/go-logging/archive/v"
	   version
	   ".tar.gz"))
     (sha256
      (base32
       "1g6rfcql1xbwbmh6b3rjmmpl5fxp4z677v1b2g6gn7hcrbrj954l"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("go" ,go)))
   (arguments
    `(#:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (add-before
	'build
	'setup-go-workspace
	(lambda* _
	  (mkdir-p (string-append
		    (getcwd)
		    "/../gopath/src/github.com/op/go-logging"))
	  (copy-recursively
	   (getcwd)
	   (string-append
	    (getcwd)
	    "/../gopath/src/github.com/op/go-logging"))))
       (replace 'build
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((cwd (getcwd))
		    (gopath (string-append (getcwd) "/../gopath")))
	       (setenv "GOPATH" gopath)
	       (zero? (system* "go" "install" "github.com/op/go-logging")))))
       (replace 'check
	 (lambda* _
	   (zero? (system* "go" "test" "github.com/op/go-logging"))))
       (replace 'install
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (assoc-ref outputs "out"))
			  (gopath (string-append (getcwd) "/../gopath")))
		      (with-directory-excursion gopath
			(copy-recursively "." out))))))))
   (home-page "https://github.com/op/go-logging")
   (synopsis "Golang logging library")
   (description "Package logging implements a logging infrastructure for Go.
Its output format is customizable and supports different logging backends like
syslog, file and memory.  Multiple backends can be utilized with different log
levels per backend and logger.")
   (license license:bsd-3)))

(define-public go-logging-whyrusleeping
  (let ((commit "0457bb6b88fc1973573aaf6b5145d8d3ae972390")
	(revision "1"))
    (package
     (name "go-logging")
     (version (string-append "0.0.0-" revision (string-take commit 7)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/whyrusleeping/go-logging.git")
	     (commit commit)))
       (sha256
	(base32
	 "1bl180mhg03hdqhyr5sfjcg16ns2ppal625g9ag5m10l2pvlwnqn"))))
     (build-system gnu-build-system)
     (native-inputs
      `(("go" ,go)))
     (arguments
      `(#:phases
	(modify-phases
	 %standard-phases
	 (delete 'configure)
	 (add-before
	  'build
	  'fix-typo
	  (lambda* _
	    (substitute* "example_test.go"
			(("log.Debug") "log.Debugf"))))
	 (add-before
	  'build
	  'setup-go-workspace
	  (lambda* _
	    (mkdir-p (string-append
		      (getcwd)
		      "/../gopath/src/github.com/whyrusleeping/go-logging"))
	    (copy-recursively
	     (getcwd)
	     (string-append
	      (getcwd)
	      "/../gopath/src/github.com/whyrusleeping/go-logging"))))
	 (replace 'build
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let* ((cwd (getcwd))
			   (gopath (string-append (getcwd) "/../gopath")))
		      (setenv "GOPATH" gopath)
		      (zero? (system* "go" "install" "github.com/whyrusleeping/go-logging")))))
	 (replace 'check
		  (lambda* _
		    (zero? (system* "go" "test" "github.com/whyrusleeping/go-logging"))))
	 (replace 'install
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (assoc-ref outputs "out"))
			  (gopath (string-append (getcwd) "/../gopath")))
		      (with-directory-excursion gopath
			(copy-recursively "." out))))))))
     (home-page "https://github.com/whyrusleeping/go-logging")
     (synopsis "Golang logging library")
     (description "Package logging implements a logging infrastructure for Go.
Its output format is customizable and supports different logging backends like
syslog, file and memory.  Multiple backends can be utilized with different log
levels per backend and logger.")
     (license license:bsd-3))))

(define-public go-log
  (let ((commit "48d644b006ba26f1793bffc46396e981801078e3")
	(revision "1"))
    (package
     (name "go-log")
     (version (string-append "0.0.0-" revision "." (string-take commit 7)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/ipfs/go-log.git")
	     (commit commit)))
       (sha256
	(base32
	 "0q2bk2s2v626ikm2pjalq4qg4n53yyf1bb81jbljb23iijxrqsbr"))))
     (build-system gnu-build-system)
     (native-inputs
      `(("go" ,go)
	("go-logging" ,go-logging-whyrusleeping)))
     (arguments
      `(#:phases
	(modify-phases
	 %standard-phases
	 (delete 'configure)
	 (add-before
	  'build
	  'setup-go-workspace
	  (lambda* _
	    (mkdir-p (string-append
		      (getcwd)
		      "/../gopath/src/github.com/ipfs/go-log"))
	    (copy-recursively
	     (getcwd)
	     (string-append
	      (getcwd)
	      "/../gopath/src/github.com/ipfs/go-log"))))
	 (replace 'build
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((gopath
			    (string-append
			     (getcwd)
			     "/../gopath:"
			     ,(with-store store
			       (package-output store go-logging-whyrusleeping))))
			   )
		      (setenv "GOPATH" gopath)
		      (zero? (system* "go" "install" "github.com/ipfs/go-log")))))
	 (replace 'check
		  (lambda* _
		    (zero? (system* "go" "test" "github.com/ipfs/go-log"))))
	 (replace 'install
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (assoc-ref outputs "out"))
			  (gopath (string-append (getcwd) "/../gopath"))
			  (pwd (getcwd)))
		      (and (chdir gopath)
			   (copy-recursively
			    (string-append gopath "/pkg")
			    (string-append out "/pkg"))
			   (chdir pwd))))))))
     (home-page "https://github.com/ipfs/go-log")
     (synopsis "Logging library used by go-ipfs")
     (description "A logging library used by go-ipfs.  It currently uses a
modified version of @code{go-logging} to implement the standard printf-style
log output.")
     (license license:expat))))

(define-public go-ipfs
  (package
    (name "go-ipfs")
    (version "0.4.11-pre")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ipfs/go-ipfs/archive/v"
			   version
			   ".tar.gz"))
       (sha256
	(base32
	 "0d3nfnlcp5p8a6cxm5jh6y8f527ylnhd02f5yivylgp2zpxckzz0"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("make" ,gnu-make)
       ("go" ,go)
       ("go-logging" ,go-logging-whyrusleeping)
       ("go-log" ,go-log)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (delete 'configure)
	 (add-before
	     'build
	     'update-makefile
	   (lambda* _
	     (substitute* "Makefile"
	       (("/bin/sh") (which "sh")))))
	 (add-before
	     'build
	     'setup-go-workspace
	   (lambda* _
	     (mkdir-p (string-append
		       (getcwd)
		       "/../gopath/src/github.com/ipfs/go-ipfs"))
	     (copy-recursively
	      (getcwd)
	      (string-append
	       (getcwd)
	       "/../gopath/src/github.com/ipfs/go-ipfs"))))
	 (replace 'build
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((cwd (getcwd))
		    (gopath (string-append (getcwd) "/../gopath"))
		    (src (string-append gopath "/src"))
		    (ipfs-src (string-append
			       src
			       "/github.com/ipfs/go-ipfs")))
	       (setenv "GOPATH" gopath)
	       ;; (system* "go" "build" "github.com/ipfs/go-ipfs")
	       (chdir ipfs-src)
	       (and (system* "make" "build") (chdir cwd))))))))
    (home-page "https://ipfs.io/")
    (synopsis "Peer-to-peer hypermedia protocol")
    (description "IPFS (the InterPlanetary File System) is a new hypermedia
distribution protocol, addressed by content and identities.  IPFS enables the
creation of completely distributed applications.   It aims to make the web
faster, safer, and more open.")
    (license license:expat)))
