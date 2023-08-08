#!/usr/bin/env -S sbcl --script

(load #p"~/.local/lib/quicklisp/setup.lisp")
(ql:quickload :kaptchapelo)

(kaptchapelo:start-server
  ;; Change captcha-directory’s path to your captchas
  :captcha-directory #p"~/.local/lib/quicklisp/local-projects/kaptchapelo/captcha/"
  :address "0.0.0.0"
  :port 5001
  :background nil)
