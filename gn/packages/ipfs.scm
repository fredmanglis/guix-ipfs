;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Muriithi Frederick Muriuki <fredmanglis@gmail.com>

(define-module (gn packages ipfs)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages golang)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (texinfo string-utils))

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
			  (gopath (string-append (getcwd) "/../gopath")))
		      (with-directory-excursion
		       gopath
		       (copy-recursively "." out))))))))
     (home-page "https://github.com/ipfs/go-log")
     (synopsis "Logging library used by go-ipfs")
     (description "A logging library used by go-ipfs.  It currently uses a
modified version of @code{go-logging} to implement the standard printf-style
log output.")
     (license license:expat))))

(define-public go-randbuf
  (let ((commit "674640a50e6a1331d97d8efcfc43977ffe64897c")
	(revision "1"))
    (package
     (name "go-randbuf")
     (version (string-append "0.0.0-" revision "." (string-take commit 7)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/jbenet/go-randbuf.git")
	     (commit commit)))
       (sha256
	(base32
	 "0mqlmpl9jfg66n61jvg7pl2i65i6sm3v0mx47q8nldz2ywm146ig"))))
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
		      "/../gopath/src/github.com/jbenet/go-randbuf"))
	    (copy-recursively
	     (getcwd)
	     (string-append
	      (getcwd)
	      "/../gopath/src/github.com/jbenet/go-randbuf"))))
	 (replace 'build
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let* ((cwd (getcwd))
			   (gopath (string-append (getcwd) "/../gopath")))
		      (setenv "GOPATH" gopath)
		      (zero? (system* "go" "install" "github.com/jbenet/go-randbuf")))))
	 (replace 'check
		  (lambda* _
		    (zero? (system* "go" "test" "github.com/jbenet/go-randbuf"))))
	 (replace 'install
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (assoc-ref outputs "out"))
			  (gopath (string-append (getcwd) "/../gopath")))
		      (with-directory-excursion
		       gopath
		       (copy-recursively "." out))))))))
     (home-page "https://github.com/jbenet/go-randbuf")
     (synopsis "Generate a random []byte of size n")
     (description "Generate a random []byte of size n")
     (license license:expat))))

(define-public go-msgio
  (let ((commit "242a3f4ed2d0098bff2f25b1bd32f4254e803b23")
	(revision "1"))
    (package
     (name "go-msgio")
     (version (string-append "0.0.0-" revision "." (string-take commit 7)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/jbenet/go-msgio.git")
	     (commit commit)))
       (sha256
	(base32
	 "111w6y4kyls4p0azzv42m2dkalkpyji5wrawxyzpa5h100qqh3g9"))))
     (build-system gnu-build-system)
     (native-inputs
      `(("go" ,go)
	("go-randbuf" ,go-randbuf)))
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
		    "/../gopath/src/github.com/jbenet/go-msgio"))
	  (copy-recursively
	   (getcwd)
	   (string-append
	    (getcwd)
	    "/../gopath/src/github.com/jbenet/go-msgio"))))
       (replace 'build
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((cwd (getcwd))
		    (gopath
		     (string-append
		      (getcwd)
		      "/../gopath:"
		      ,(with-store
			store
			(package-output store go-randbuf)))))
	       (setenv "GOPATH" gopath)
	       (zero? (system* "go" "install" "github.com/jbenet/go-msgio")))))
       (replace 'check
	 (lambda* _
	   (zero? (system* "go" "test" "github.com/jbenet/go-msgio"))))
       (replace 'install
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (assoc-ref outputs "out"))
			  (gopath (string-append (getcwd) "/../gopath")))
		      (with-directory-excursion
		       gopath
		       (copy-recursively "." out))))))))
     (home-page "https://github.com/jbenet/go-msgio")
     (synopsis "Simple package to read/write length-delimited slices")
     (description "This is a simple package that helps read and write
length-delimited slices.  It is helpful for building wire protocols.")
     (license license:expat))))

(define-public go-gogo-protobuf
  (package
   (name "go-gogo-protobuf")
   (version "0.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/gogo/protobuf/archive/v"
			 version
			 ".tar.gz"))
     (sha256
      (base32
       "0w7glv0kwpavfrmzin01jvzqaj1cbykwh14q81bg2jldka5m1yv4"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("go" ,go)
      ;; C++ implementation of protocol buffers from https://developers.google.com/protocol-buffers/
      ))
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
		    "/../gopath/src/github.com/gogo/protobuf"))
	  (copy-recursively
	   (getcwd)
	   (string-append
	    (getcwd)
	    "/../gopath/src/github.com/gogo/protobuf"))))
       (replace 'build
		(lambda* (#:key outputs #:allow-other-keys)
		  (let* ((cwd (getcwd))
			 (gopath
			  (string-append
			   (getcwd)
			   "/../gopath"
			   ;; "/../gopath:"
			   ;; ,(with-store
			   ;; 	store
			   ;; 	(package-output store go-randbuf))
			   )))
		    (setenv "GOPATH" gopath)
		    ;; figure out what needs be built here. Maybe split it into
		    ;; separate definitions, all inheriting from a common parent
		    ;; each building a separate item: proto, gogoproto, etc.
		    (and
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/codec"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/gogoproto"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/gogoreplace"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/io"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/jsonpb"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/proto"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/protoc-gen-combo"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/protoc-gen-gofast"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/protoc-gen-gogo"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/protoc-gen-gogofast"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/protoc-gen-gogofaster"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/protoc-gen-gogoslick"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/protoc-gen-gogotypes"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/protoc-gen-gostring"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/protoc-min-version"))))))
       (replace 'check
		(lambda* _
		  (and
		   (zero? (system* "go" "test"
				   "github.com/gogo/protobuf/codec"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/io"))
		     (zero? (system* "go" "install"
				     "github.com/gogo/protobuf/jsonpb"))
		     (zero? (system* "go" "test"
				     "github.com/gogo/protobuf/proto")))))
       (replace 'install
		(lambda* (#:key outputs #:allow-other-keys)
		  (let ((out (assoc-ref outputs "out"))
			(gopath (string-append (getcwd) "/../gopath")))
		    (with-directory-excursion
		     gopath
		     (copy-recursively "." out))))))))
   (home-page "https://github.com/gogo/protobuf")
   (synopsis " Protocol Buffers for Go with Gadgets")
   (description "This is a fork of https://github.com/golang/protobuf with
extra code generation features.  The code generation is used to achieve:
@itemize
@item fast marshalling and unmarshalling
@item more canonical Go structures
@item goprotobuf compatibility
@item less typing by optionally generating extra helper code
@item peace of mind by optionally generating test and benchmark code
@item other serialization formats
@end itemize")
   (license #f)))

(define-public go-ed25519
  (let ((commit "5312a61534124124185d41f09206b9fef1d88403")
	(revision "1"))
    (package
     (name "go-ed25519")
     (version (string-append "0.0.0-" revision "." (string-take commit 7)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/agl/ed25519.git")
	     (commit commit)))
       (sha256
	(base32
	 "1v8mhkf1m3ga5262s75vabskxvsw5rpqvi5nwhxwiv7gfk6h823i"))))
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
		    "/../gopath/src/github.com/agl/ed25519"))
	  (copy-recursively
	   (getcwd)
	   (string-append
	    (getcwd)
	    "/../gopath/src/github.com/agl/ed25519"))))
       (replace 'build
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((cwd (getcwd))
		    (gopath
		     (string-append
		      (getcwd)
		      "/../gopath:"
		      ,(with-store
			store
			(package-output store go-randbuf)))))
	       (setenv "GOPATH" gopath)
	       (zero? (system* "go" "install" "github.com/agl/ed25519")))))
       (replace 'check
	 (lambda* _
	   (zero? (system* "go" "test" "github.com/agl/ed25519"))))
       (replace 'install
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (assoc-ref outputs "out"))
			  (gopath (string-append (getcwd) "/../gopath")))
		      (with-directory-excursion
		       gopath
		       (copy-recursively "." out)))))
       (delete 'reset-gzip-timestamps))))
     (home-page "https://github.com/agl/ed25519")
     (synopsis "ed25519 public-key signature system for Go")
     (description "ed25519 public-key signature system for Go")
     (license license:bsd-3))))

(define-public go-btcd-minimal
  (package
   (name "go-btcd")
   (version "0.12.0-beta")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/btcsuite/btcd/archive/BTCD_"
			 (string-upcase
			  (transform-string
			   (transform-string version "." "_")
			   "-"
			   "_"))
			 ".tar.gz"))
     (sha256
      (base32
       "18s65raxxjr7gx7af3g8i8jicl0k60dpihavn7z1mrsa0wcgrsiy"))))
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
		    "/../gopath/src/github.com/btcsuite/btcd"))
	  (copy-recursively
	   (getcwd)
	   (string-append
	    (getcwd)
	    "/../gopath/src/github.com/btcsuite/btcd"))))
       (replace 'build
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((cwd (getcwd))
		    (gopath
		     (string-append
		      (getcwd)
		      "/../gopath:"
		      ,(with-store
			store
			(package-output store go-randbuf)))))
	       (setenv "GOPATH" gopath)
	       (zero? (system* "go" "install" "github.com/btcsuite/btcd")))))
       (replace 'check
	 (lambda* _
	   (zero? (system* "go" "test" "github.com/btcsuite/btcd"))))
       (replace 'install
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (assoc-ref outputs "out"))
			  (gopath (string-append (getcwd) "/../gopath")))
		      (with-directory-excursion
		       gopath
		       (copy-recursively "." out))))))))
   (home-page "https://github.com/btcsuite/btcd/")
   (synopsis "Alternative full node bitcoin implementation written in Go (golang)")
   (description "btcd is an alternative full node bitcoin implementation written
in Go (golang).  It properly downloads, validates, and serves the block chain
using the exact rules (including consensus bugs) for block acceptance as Bitcoin
Core.  It does not cause a fork in the block chain.   It includes a full block
validation testing framework which contains all of the 'official' block
acceptance tests (and some additional ones) that is run on every pull request to
help ensure it properly follows consensus. Also, it passes all of the JSON test
data in the Bitcoin Core code.

It also properly relays newly mined blocks, maintains a transaction pool, and
relays individual transactions that have not yet made it into a block. It
ensures all individual transactions admitted to the pool follow the rules
required by the block chain and also includes more strict checks which filter
transactions based on miner requirements ('standard' transactions).

One key difference between btcd and Bitcoin Core is that btcd does NOT include
wallet functionality and this was a very intentional design decision.")
   (license license:isc)))

(define-public go-btcec
  (package
   (inherit go-btcd-minimal)
   (name "go-btcec")
   (arguments
    (substitute-keyword-arguments
     (package-arguments go-btcd-minimal)
     ((#:phases phases)
      `(modify-phases
	,phases
	(replace 'build
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((cwd (getcwd))
		    (gopath
		     (string-append
		      (getcwd)
		      "/../gopath")))
	       (setenv "GOPATH" gopath)
	       (zero? (system* "go" "install" "github.com/btcsuite/btcd/btcec")))))
       (replace 'check
	 (lambda* _
	   (zero? (system* "go" "test" "github.com/btcsuite/btcd/btcec"))))))))))

(define-public go-libp2p-crypto
  (let ((commit "e89e1de117dd65c6129d99d1d853f48bc847cf17")
	(revision "1"))
    (package
     (name "go-libp2p-crypto")
     (version (string-append "0.0.0-" revision "." (string-take commit 7)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/libp2p/go-libp2p-crypto.git")
	     (commit commit)))
       (sha256
	(base32
	 "09624wrfwcxxjgdlwfl5l3zwiiv9fr1d2cs009cca6zsb5mziaq5"))))
     (build-system gnu-build-system)
     (native-inputs
      `(("go" ,go)
	("go-gogo-protobuf" ,go-gogo-protobuf)
	("go-ed25519" ,go-ed25519)
	("go-btcec" ,go-btcec)))
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
		    "/../gopath/src/github.com/libp2p/go-libp2p-crypto"))
	  (copy-recursively
	   (getcwd)
	   (string-append
	    (getcwd)
	    "/../gopath/src/github.com/libp2p/go-libp2p-crypto"))))
       (replace 'build
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((cwd (getcwd))
		    (gopath
		     (string-append
		      (getcwd)
		      "/../gopath:"
		      ,(with-store
			store
			(package-output store go-gogo-protobuf))
		      ,(with-store
			store
			(package-output go-ed25519))
		      ,(with-store
			store
			(package-output go-btcec)))))
	       (setenv "GOPATH" gopath)
	       (zero? (system* "go" "install" "github.com/libp2p/go-libp2p-crypto")))))
       (replace 'check
	 (lambda* _
	   (zero? (system* "go" "test" "github.com/libp2p/go-libp2p-crypto"))))
       (replace 'install
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (assoc-ref outputs "out"))
			  (gopath (string-append (getcwd) "/../gopath")))
		      (with-directory-excursion
		       gopath
		       (copy-recursively "." out))))))))
     (home-page "https://github.com/libp2p/go-libp2p-crypto")
     (synopsis "Various cryptographic utilities used by ipfs")
     (description "Various cryptographic utilities used by ipfs")
     (license license:expat))))

(define-public go-crypto
  (let ((commit "81e90905daefcd6fd217b62423c0908922eadb30")
	(revision "1"))
    (package
     (name "go-crypto")
     (version (string-append "0.0.0-" revision "." (string-take commit 7)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/golang/crypto.git")
	     (commit commit)))
       (sha256
	(base32
	 "05f6dm2dkd22db1pydd54wmm0qrn16n9mgy5favcxc9lf83hawn6"))))
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
		    "/../gopath/src/golang.org/x/crypto"))
	  (copy-recursively
	   (getcwd)
	   (string-append
	    (getcwd)
	    "/../gopath/src/golang.org/x/crypto"))))
       (replace 'build
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((cwd (getcwd))
		    (gopath
		     (string-append
		      (getcwd)
		      "/../gopath")))
	       (setenv "GOPATH" gopath)
	       (and
		(zero? (system* "go" "install" "golang.org/x/crypto/acme"))
		(zero? (system* "go" "install" "golang.org/x/crypto/acme/autocert"))
		(zero? (system* "go" "install" "golang.org/x/crypto/bcrypt"))
		(zero? (system* "go" "install" "golang.org/x/crypto/blake2b"))
		(zero? (system* "go" "install" "golang.org/x/crypto/blake2s"))
		(zero? (system* "go" "install" "golang.org/x/crypto/blowfish"))
		(zero? (system* "go" "install" "golang.org/x/crypto/bn256"))
		(zero? (system* "go" "install" "golang.org/x/crypto/cast5"))
		(zero? (system* "go" "install" "golang.org/x/crypto/chacha20poly1305"))
		(zero? (system* "go" "install" "golang.org/x/crypto/chacha20poly1305/internal/chacha20"))
		(zero? (system* "go" "install" "golang.org/x/crypto/cryptobyte"))
		(zero? (system* "go" "install" "golang.org/x/crypto/curve25519"))
		(zero? (system* "go" "install" "golang.org/x/crypto/ed25519"))
		(zero? (system* "go" "install" "golang.org/x/crypto/ed25519/internal/edwards25519"))
		(zero? (system* "go" "install" "golang.org/x/crypto/hkdf"))
		(zero? (system* "go" "install" "golang.org/x/crypto/md4"))
		(zero? (system* "go" "install" "golang.org/x/crypto/nacl/auth"))
		(zero? (system* "go" "install" "golang.org/x/crypto/nacl/box"))
		(zero? (system* "go" "install" "golang.org/x/crypto/nacl/secretbox"))
		(zero? (system* "go" "install" "golang.org/x/crypto/ocsp"))
		(zero? (system* "go" "install" "golang.org/x/crypto/openpgp"))
		(zero? (system* "go" "install" "golang.org/x/crypto/openpgp/armor"))
		(zero? (system* "go" "install" "golang.org/x/crypto/openpgp/clearsign"))
		(zero? (system* "go" "install" "golang.org/x/crypto/openpgp/elgamal"))
		(zero? (system* "go" "install" "golang.org/x/crypto/openpgp/errors"))
		(zero? (system* "go" "install" "golang.org/x/crypto/openpgp/packet"))
		(zero? (system* "go" "install" "golang.org/x/crypto/openpgp/s2k"))
		(zero? (system* "go" "install" "golang.org/x/crypto/otr"))
		(zero? (system* "go" "install" "golang.org/x/crypto/pbkdf2"))
		(zero? (system* "go" "install" "golang.org/x/crypto/pkcs12"))
		(zero? (system* "go" "install" "golang.org/x/crypto/pkcs12/internal/rc2"))
		(zero? (system* "go" "install" "golang.org/x/crypto/poly1305"))
		(zero? (system* "go" "install" "golang.org/x/crypto/ripemd160"))
		(zero? (system* "go" "install" "golang.org/x/crypto/salsa20"))
		(zero? (system* "go" "install" "golang.org/x/crypto/salsa20/salsa"))
		(zero? (system* "go" "install" "golang.org/x/crypto/scrypt"))
		(zero? (system* "go" "install" "golang.org/x/crypto/sha3"))
		(zero? (system* "go" "install" "golang.org/x/crypto/ssh"))
		(zero? (system* "go" "install" "golang.org/x/crypto/ssh/agent"))
		(zero? (system* "go" "install" "golang.org/x/crypto/ssh/knownhosts"))
		;; Missing package golang.org/x/sys/unix
		;; (zero? (system* "go" "install" "golang.org/x/crypto/ssh/terminal"))
		(zero? (system* "go" "install" "golang.org/x/crypto/ssh/test"))
		(zero? (system* "go" "install" "golang.org/x/crypto/tea"))
		(zero? (system* "go" "install" "golang.org/x/crypto/twofish"))
		(zero? (system* "go" "install" "golang.org/x/crypto/xtea"))
		(zero? (system* "go" "install" "golang.org/x/crypto/xts")))
	       )))
       (replace 'check
	 (lambda* _
	   (and
		(zero? (system* "go" "test" "golang.org/x/crypto/acme"))
		(zero? (system* "go" "test" "golang.org/x/crypto/acme/autocert"))
		(zero? (system* "go" "test" "golang.org/x/crypto/bcrypt"))
		(zero? (system* "go" "test" "golang.org/x/crypto/blake2b"))
		(zero? (system* "go" "test" "golang.org/x/crypto/blake2s"))
		(zero? (system* "go" "test" "golang.org/x/crypto/blowfish"))
		(zero? (system* "go" "test" "golang.org/x/crypto/bn256"))
		(zero? (system* "go" "test" "golang.org/x/crypto/cast5"))
		(zero? (system* "go" "test" "golang.org/x/crypto/chacha20poly1305"))
		(zero? (system* "go" "test" "golang.org/x/crypto/chacha20poly1305/internal/chacha20"))
		(zero? (system* "go" "test" "golang.org/x/crypto/cryptobyte"))
		(zero? (system* "go" "test" "golang.org/x/crypto/curve25519"))
		(zero? (system* "go" "test" "golang.org/x/crypto/ed25519"))
		(zero? (system* "go" "test" "golang.org/x/crypto/hkdf"))
		(zero? (system* "go" "test" "golang.org/x/crypto/md4"))
		(zero? (system* "go" "test" "golang.org/x/crypto/nacl/auth"))
		(zero? (system* "go" "test" "golang.org/x/crypto/nacl/box"))
		(zero? (system* "go" "test" "golang.org/x/crypto/nacl/secretbox"))
		(zero? (system* "go" "test" "golang.org/x/crypto/ocsp"))
		(zero? (system* "go" "test" "golang.org/x/crypto/openpgp"))
		(zero? (system* "go" "test" "golang.org/x/crypto/openpgp/armor"))
		(zero? (system* "go" "test" "golang.org/x/crypto/openpgp/clearsign"))
		(zero? (system* "go" "test" "golang.org/x/crypto/openpgp/elgamal"))
		(zero? (system* "go" "test" "golang.org/x/crypto/openpgp/packet"))
		(zero? (system* "go" "test" "golang.org/x/crypto/openpgp/s2k"))
		(zero? (system* "go" "test" "golang.org/x/crypto/otr"))
		(zero? (system* "go" "test" "golang.org/x/crypto/pbkdf2"))
		(zero? (system* "go" "test" "golang.org/x/crypto/pkcs12"))
		(zero? (system* "go" "test" "golang.org/x/crypto/pkcs12/internal/rc2"))
		(zero? (system* "go" "test" "golang.org/x/crypto/poly1305"))
		(zero? (system* "go" "test" "golang.org/x/crypto/ripemd160"))
		(zero? (system* "go" "test" "golang.org/x/crypto/salsa20"))
		(zero? (system* "go" "test" "golang.org/x/crypto/salsa20/salsa"))
		(zero? (system* "go" "test" "golang.org/x/crypto/scrypt"))
		(zero? (system* "go" "test" "golang.org/x/crypto/sha3"))
		;; Missing package golang.org/x/sys/unix
		;; (zero? (system* "go" "test" "golang.org/x/crypto/ssh"))
		(zero? (system* "go" "test" "golang.org/x/crypto/ssh/agent"))
		(zero? (system* "go" "test" "golang.org/x/crypto/ssh/knownhosts"))
		;; Missing package golang.org/x/sys/unix
		;; (zero? (system* "go" "test" "golang.org/x/crypto/ssh/terminal"))
		(zero? (system* "go" "test" "golang.org/x/crypto/ssh/test"))
		(zero? (system* "go" "test" "golang.org/x/crypto/tea"))
		(zero? (system* "go" "test" "golang.org/x/crypto/twofish"))
		(zero? (system* "go" "test" "golang.org/x/crypto/xtea"))
		(zero? (system* "go" "test" "golang.org/x/crypto/xts")))))
       (replace 'install
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (assoc-ref outputs "out"))
			  (gopath (string-append (getcwd) "/../gopath")))
		      (with-directory-excursion
		       gopath
		       (copy-recursively "." out)))))
       (delete 'reset-gzip-timestamps))))
     (home-page "https://github.com/golang/crypto")
     (synopsis "Go supplementary cryptography libraries")
     (description "Go supplementary cryptography libraries")
     (license license:bsd-3))))

(define-public go-base58
  (package
   (name "go-base58")
   (version "1.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/jbenet/go-base58/archive/"
			 version
			 ".tar.gz"))
     (sha256
      (base32
       "0j9mki26vssngyby1fgg4nibf5ja4pzvl2pv7cqrqy6g5lzfi81i"))))
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
		    "/../gopath/src/github.com/jbenet/go-base58"))
	  (copy-recursively
	   (getcwd)
	   (string-append
	    (getcwd)
	    "/../gopath/src/github.com/jbenet/go-base58"))))
       (replace 'build
		(lambda* (#:key outputs #:allow-other-keys)
		  (let* ((cwd (getcwd))
			 (gopath (string-append (getcwd) "/../gopath")))
		    (setenv "GOPATH" gopath)
		    (zero? (system* "go" "install" "github.com/jbenet/go-base58")))))
       (replace 'check
		(lambda* _
		  (zero? (system* "go" "test" "github.com/jbenet/go-base58"))))
       (replace 'install
		(lambda* (#:key outputs #:allow-other-keys)
		  (let ((out (assoc-ref outputs "out"))
			(gopath (string-append (getcwd) "/../gopath")))
		    (with-directory-excursion
		     gopath
		     (copy-recursively "." out))))))))
   (home-page "https://github.com/jbenet/go-base58")
   (synopsis "Simple base58 codec")
   (description "Extracted from @uref{https://github.com/conformal/btcutil} to
provide a package that:
@itemize @bullet
@item defaults to base58-check (btc)
@item allows using different alphabets.
@end itemize")
   (license license:isc)))

(define-public go-hashland-minimal
  (let ((commit "e13accbe55f7fa03c73c74ace4cca4c425e47260")
	(revision "1"))
    (package
     (name "go-hashland")
     (version (string-append "0.0.0-" revision "." (string-take commit 7)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/tildeleb/hashland.git")
	     (commit commit)))
       (sha256
	(base32
	 "1ygfp7kjnm964w2bv61bsyxpw5y6vy6k4pra1lhd5r3nhlxgs31g"))))
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
		    "/../gopath/src/leb.io/hashland"))
	  (copy-recursively
	   (getcwd)
	   (string-append
	    (getcwd)
	    "/../gopath/src/leb.io/hashland"))))
       (replace 'build
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((cwd (getcwd))
		    (gopath
		     (string-append
		      (getcwd)
		      "/../gopath")))
	       (setenv "GOPATH" gopath)
	       (zero? (system* "go" "install" "leb.io/hashland")))))
       (replace 'check
	 (lambda* _
	   (zero? (system* "go" "test" "leb.io/hashland"))))
       (replace 'install
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (assoc-ref outputs "out"))
			  (gopath (string-append (getcwd) "/../gopath")))
		      (with-directory-excursion
		       gopath
		       (copy-recursively "." out))))))))
     (home-page "https://github.com/tildeleb/hashland")
     (synopsis "Go versions of murmur, cityhash, jenkins, spooky, sbox, and more")
     (description "Hashland is a collection of hash functions and functionality
to test them.")
     (license license:expat))))

(define-public go-keccakpg
  (package
   (inherit go-hashland-minimal)
   (name "go-keccakpg")
   (arguments
    (substitute-keyword-arguments
     (package-arguments go-hashland-minimal)
     ((#:phases phases)
      `(modify-phases
	,phases
	(replace 'build
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((cwd (getcwd))
		    (gopath
		     (string-append
		      (getcwd)
		      "/../gopath")))
	       (setenv "GOPATH" gopath)
	       (zero? (system* "go" "install" "leb.io/hashland/keccakpg")))))
	(replace 'check
	 (lambda* _
	   (zero? (system* "go" "test" "leb.io/hashland/keccakpg"))))))))))

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
