;;; Copyright 2023, Jaidyn Ann <jadedctrl@posteo.at>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see <https://www.gnu.org/licenses/>.

(defpackage #:kaptchapelo
  (:use #:cl)
  (:export :start-server))

(in-package #:kaptchapelo)


(defun byte-array-to-hex-string (simple-array)
  "Given an array of bytes (integers), return an equivalent string in hex."
  (string-downcase
   (reduce #'str:concat
           (loop for byte across simple-array
                 collect (format nil "~2,'0X" byte)))))


(defun random-file (directory &key (file-ext ""))
  (alexandria:random-elt
   (directory (str:concat (format nil "~A" directory)
                          "/*"
                          (if (not (str:emptyp file-ext))
                              (str:concat "." file-ext)
                              "")))))


(defun new-captcha-json (captcha-image-uri captcha-text-file)
  "Return the kocaptcha-formed JSON to be returned for a new captcha request."
  (yason:with-output-to-string* ()
    (yason:encode-plist
     (list "md5" (byte-array-to-hex-string captcha-text-file)
           "url" captcha-image-uri
           "token" "This_isnt_actually_used_lol"))))


(defun new-captcha-response (captcha-dir)
  "Create an HTTP response for use with Clack with a new captcha."
  (let* ((captcha-txt-file (random-file captcha-dir :file-ext "txt"))
         (captcha-img-file (str:concat (pathname-name captcha-txt-file) ".png"))
         (captcha-md5-str (byte-array-to-hex-string
                           (md5:md5sum-file captcha-txt-file))))
    (list 201 '(:content-type "application/json")
          (list (new-captcha-json (str:concat "/captcha/" captcha-img-file)
                                  captcha-md5-str)))))


(defun image-response (request-uri captcha-dir)
  (let ((image-path (str:replace-first "/captcha/" (format nil "~A" captcha-dir) request-uri)))
    (list 201 '(:content-type "image/png") (pathname image-path))))


(defun index-response ()
  '(201 (:content-type "text/plain") ("You’ve installed Kaptĉapelo! Good work, guy!")))

(defun 404-response ()
  '(404 (:content-type "text/plain") ("No such page.")))


(defun server (env captcha-dir)
  (let* ((uri (quri:uri (getf env :request-uri)))
         (uri-path (quri:uri-path uri))
         (params (quri:uri-query-params uri)))
    (format *error-output* "~A" uri-path)
    (cond ((string= uri-path "/new")
           (new-captcha-response captcha-dir))
          ((or (string= uri-path "/")
               (string= uri-path "/index.html"))
           (index-response))
          ((str:starts-with? "/captcha/" uri-path)
;;                (str:ends-with? ".png" uri-path))
           (image-response uri-path captcha-dir))
          ('t
           (404-response)))))


(defun start-server (&key (address "0.0.0.0") (port 5001) (captcha-directory #p"captcha/"))
  (clack:clackup
   (lambda (env)
     (funcall #'server env captcha-directory))
   :address address
   :port port))
