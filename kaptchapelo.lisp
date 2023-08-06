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


(defun new-captcha-json ()
  "Return the kocaptcha-formed JSON to be returned for a new captcha request."
  (yason:with-output-to-string* ()
    (yason:encode-plist
     '("md5" "e" "token" "This_isnt_actually_used_lol" "url" "/bird"))))


(defun new-captcha-response ()
  "Create an HTTP response for use with Clack with a new captcha."
  (list 201 '(:content-type "application/json")
        (list (new-captcha-json))))


(defun index-response ()
  '(201 (:content-type "text/plain") ("You’ve installed Kaptĉapelo! Good work, guy!")))


(defun 404-response ()
  '(404 (:content-type "text/plain") ("No such page.")))


(defun server (env)
  (let* ((uri (quri:uri (getf env :request-uri)))
         (params (quri:uri-query-params uri)))

    (format 't (quri:uri-path uri))
    (cond ((string= (quri:uri-path uri) "/new")
           (new-captcha-response))
          ((or (string= (quri:uri-path uri) "/")
               (string= (quri:uri-path uri) "/index.html"))
           (index-response))
          ('t
           (404-response))
        ;; At any other path, give control back over to the user’s server
        (or (and clack-app (funcall clack-app env))))))



(defun start-server ()
  (clack:clackup
   (lambda (env)
     (funcall #'server env))))
