;;; inv.el --- easy interaction with invidious instances -*- lexical-binding: t -*-

;; Author: kpm <kpm@krzysckh.org>
;; Created: 28 Aug 2024
;; Keywords: network, invidious, youtube
;; URL: https://github.com/krzysckh/inv.el
;;
;; Copyright (C) 2024 kpm <kpm@krzysckh.org>
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; This file is not part of GNU Emacs.

;;; Code:

(require 'request)
(require 'dash)
(require 'json)
(require 'url)

(defvar inv/instances '("inv.vern.cc"))
(defvar inv/request-timeout 3 "timeout for a `inv/json-request' (in seconds)")
(defvar inv/thumbnail-quality 'sddefault "the thumbnail returned by `inv/fetch-thumbnail-url': one of maxres, sddefault, high, medium, default, start, middle, end.")
(defvar inv/search-buffer-name "*Invidious search results*")
(defvar inv/chanlist-buffer-name "*Invidious channel search results*")
(defvar inv/chan-buffer-name "*Invidious channel videos*")
(defvar inv/viddesc-buffer-name "*Invidious video description*")

(defvar inv/display-additional-data nil "a function that may insert additional data per line for `inv//display-data'. gets passed an assoc list of values it may use")

(defvar inv//thumb-cache nil)

(defun inv//json-read-l (&rest r)
  (let ((json-array-type 'list))
    (apply #'json-read r)))

(defun inv//shuf (seq)
  (let ((n (length seq)))
    (prog1 seq
      (dotimes (i n)
        (cl-rotatef (elt seq i) (elt seq (+ i (cl-random (- n i)))))))))

(defun inv/videop (url)
  "check if `url' is a video, if so return the id, elsewise nil"
  (if (string-match "watch\\?v=\\(.*\\)" (or url ""))
      (match-string 1 url)
    nil))

(defun inv/id-at-point ()
  (let ((url (thing-at-point 'url)))
    (inv/videop url)))

(defun inv/clear-thumbnail-cache ()
  (interactive)
  (setq inv//thumb-cache nil))

(defun inv/get-instances (cb)
  "call `cb' with a list of invidious instances."
  (request "https://api.invidious.io/instances.json"
    :parser #'inv//json-read-l
    :complete (cl-function (lambda (&key data &allow-other-keys)
                             (funcall cb (mapcar #'car data)))))
  t)

(defun inv/get-clearnet-instances (cb)
  "call `cb' with a list of clearnet invidious instances."
  (inv/get-instances (lambda (l) (funcall cb (--filter (not (string-match "onion\\|i2p" it)) l)))))

(defun inv/json-request (to cb &rest opts)
  "invoke a request `to' to an invidious instance from `inv/instances'. if it fails, try another. call `cb' with the result."
  (letrec ((f (lambda (l)
                (message "inv/json-request: trying %s..." (car l))
                (if (null l)
                    (funcall cb 'oops)
                  (let* ((url (concat "https://" (car l) to)))
                    (request url
                      :timeout inv/request-timeout
                      :data opts
                      :parser #'inv//json-read-l
                      :error (cl-function
                              (lambda (&key data &allow-other-keys)
                                (funcall f (cdr l))))
                      :success (cl-function
                                 (lambda (&key data &allow-other-keys)
                                   (if (null (assoc 'error data))
                                       (funcall cb data)
                                     (funcall f (cdr l)))))))))))
    (let ((l (inv//shuf (-copy inv/instances))))
      (funcall f l)))
  t)

(defun inv/load-instances ()
  "download and invidious instances and set `inv/instances' to the result."
  (interactive)
  (inv/get-clearnet-instances
   #'(lambda (l)
       (setq inv/instances l)
       (message "set inv/instances to %s" l))))

(defun inv/search (q cb)
  "search for `q' on one of `inv/instances'. call `cb' with the
result. if called interactivaly, `cb' defaults to
`inv/display-search-results'"
  (interactive
   (list (read-string "Enter query: ") #'inv/display-search-results))
  (inv/json-request (concat "/api/v1/search?q=" (url-encode-url q)) cb))

(defun inv/search-channel (q cb)
  "search for channel `q' on one of `inv/instances'. call `cb with the
result. if called interactively, `cb' defaults to
`inv/display-channels'."
  (interactive
   (list (read-string "Enter query (chan): ") #'inv/display-channels))
  (inv/json-request (concat "/api/v1/search?type=channel&q=" (url-encode-url q)) cb))

(defun inv/channel (id cb)
  "get videos of channel with id `id', pass them to `cb', when called
interactively, `cb' defaults to `inv/display-channel'"
  (interactive
   (list (read-string "Enter chan id: ") #'inv/display-channel))
  (inv/json-request (concat "/api/v1/channels/" (url-encode-url id) "/videos") cb))

(defun inv/fetch-video-data (id cb)
  (inv/json-request (concat "/api/v1/videos/" id) cb))

(defun inv/parse-thumbnails (exp)
  (--map
   (let ((name (cdr (assoc "quality" it #'string=))))
     (append (list (intern name)) (--filter (not (string= (car it) "quality")) it)))
   (cdr (assoc 'videoThumbnails exp))))

(defun inv/fetch-thumbnail-urls (id cb)
  (inv/fetch-video-data
   id
   (lambda (data)
     (funcall cb (inv/parse-thumbnails data)))))

(defun inv/urlp (s)
  (string-match "^http" s))

(defun inv/fetch-thumbnail-url (id cb)
  (if (inv/urlp id)
      (funcall cb id)
    (inv/fetch-thumbnail-urls id (lambda (l) (funcall cb (cdr (assoc 'url (cdr (assoc inv/thumbnail-quality l)))))))))

(defun inv/thumbnail-to-image (id-or-url cb)
  "Call `cb' with the image thumbnail associated with the video of id `id-or-url'. This funcion caches images in `inv//thumb-cache'. If url provided, use it instead of fetching it by video id."
  (let ((c (assoc id-or-url inv//thumb-cache #'string=)))
    (if c
        (funcall cb (cdr c))
      (let ((i (create-image nil 'jpeg t)))
        (setq inv//thumb-cache (append inv//thumb-cache (list (cons id-or-url i))))
        (funcall cb i)
        (inv/fetch-thumbnail-url
         id-or-url
         (lambda (url)
           (request url
             :encoding 'binary
             :error (cl-function
                     (lambda (&key data &allow-other-keys)
                       (message "Couldn't load a thumbnail for %s" id-or-url)))
             :complete (cl-function
                        (lambda (&key data &allow-other-keys)
                          (when-let ((data (string-as-unibyte data)))
                            (image--set-property i :data data)
                            (image-refresh i)))))))))))

(defun inv/popup-thumbnail (id)
  (let ((buf (get-buffer-create "*Thumbnail preview*")))
    (inv/thumbnail-to-image
     id
     (lambda (image)
       (with-current-buffer buf
         (erase-buffer)
         (insert-image image)
         (setq-local cursor-type nil))
       (save-window-excursion
         (pop-to-buffer buf '(display-buffer--maybe-at-bottom))
         (setq char (read-char-choice "(press q to close)" '(?q)))
         (if (= char ?q)
             (quit-window t)))))))

(defun inv/display-search-results (data)
  (inv//display-data data inv/search-buffer-name))

(defun inv/display-channel (data)
  (inv//display-data (cdr (assoc 'videos data)) inv/chan-buffer-name))

;; TODO: maybe this should be combined with inv/display-data
(defun inv/display-channels (data)
  (let ((buf (get-buffer-create inv/chanlist-buffer-name)))
    (letrec ((f (lambda (l)
                  (cond
                   ((null l)
                    (switch-to-buffer buf)
                    (goto-char (point-min)))
                   (t
                    (let* ((r (car l))
                           (type (cdr (assoc 'type r))))
                      (cond
                       ((string= type "channel")
                        (let ((id (cdr (assoc 'authorId r)))
                              (author (cdr (assoc 'author r)))
                              (author-url (cdr (assoc 'authorUrl r))))
                          (with-current-buffer buf
                            (insert-button
                             "Open"
                             'face 'button
                             'follow-link t
                             'action (lambda (_) (inv/channel id #'inv/display-channel)))
                            ;; (insert "  ")
                            ;; (insert-image img)
                            (insert " ")
                            ;; (put-text-property 0 (length title) 'face 'warning title)
                            (insert author)
                            (insert " ")
                            (insert-button
                             "Copy URL"
                             'face 'button
                             'follow-link t
                             'action (lambda (_) (kill-new author-url)))
                            (insert "\n")
                            (funcall f (cdr l)))))
                       (t
                        (funcall f (cdr l))))))))))
      (with-current-buffer buf
        (erase-buffer))
      (funcall f data))))

(defun inv//display-data (data bufn)
  (let ((buf (get-buffer-create bufn)))
    (letrec ((f (lambda (l)
                  (cond
                   ((null l)
                    (switch-to-buffer bufn)
                    (goto-char (point-min)))
                   (t
                    (let* ((r (car l))
                           (type (cdr (assoc 'type r))))
                      (cond
                       ((string= type "video")
                        (let ((title (cdr (assoc 'title r)))
                              (id (cdr (assoc 'videoId r)))
                              (author (cdr (assoc 'author r)))
                              (author-id (cdr (assoc 'authorId r)))
                              (thumbnail-url (cdr (assoc 'url (cdr (assoc 'default (inv/parse-thumbnails r))))))
                              (views (cdr (assoc 'viewCountText r))))
                          (inv/thumbnail-to-image
                           thumbnail-url
                           (lambda (img)
                             (with-current-buffer buf
                               (insert-button
                                "Watch"
                                'face 'button
                                'follow-link t
                                'action (lambda (_) (browse-url (concat "https://youtube.com/watch?v=" id))))
                               (insert " ")
                               (insert-image img)
                               (insert " ")
                               (put-text-property 0 (length title) 'face 'warning title)
                               (insert title)
                               (insert " ")
                               (insert-button
                                author
                                'face 'button
                                'follow-link t
                                'action (lambda (_) (inv/channel author-id #'inv/display-channel)))
                               (insert " ")
                               (insert-button
                                "URL"
                                'face 'button
                                'follow-link t
                                'action (lambda (_) (kill-new (concat "https://youtube.com/watch?v=" id))))
                               (insert " ")
                               (insert-button
                                "desc"
                                'face 'button
                                'follow-link t
                                'action (lambda (_)
                                          (inv/fetch-video-data
                                           id
                                           #'(lambda (data)
                                               (let ((buf (get-buffer-create inv/viddesc-buffer-name))
                                                     (desc (cdr (assoc 'description data))))
                                                 (with-current-buffer buf
                                                   (erase-buffer)
                                                   (insert desc)
                                                   (switch-to-buffer buf)))))))
                               (when inv/display-additional-data
                                 (insert " ")
                                 (funcall inv/display-additional-data r))
                               (insert "\n")
                               (funcall f (cdr l)))))))
                       (t
                        (funcall f (cdr l))))))))))
      (with-current-buffer buf
        (erase-buffer))
      (funcall f data))))

;; does not call an invidious instance, but a nice thing to have
(defun inv/dislikes (id cb)
  "ask the return youtube dislike api for stats on video with id `id', pass that data to `cb'"
  (request (concat "https://returnyoutubedislikeapi.com/votes?videoId=" id)
    :parser #'inv//json-read-l
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall cb data))))
  t)

(provide 'inv)
