;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Muriithi Frederick Muriuki <fredmanglis@gmail.com>

(define-module (gn packages ipfs)
  #:use-module (gnu packages base)
  #:use-module (gnu packages golang)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu))

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
       ;;("go-gx" ,go-gx)
       ))
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
