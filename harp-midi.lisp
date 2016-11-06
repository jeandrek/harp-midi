;;;; -*- eval: (put 'donotes 'lisp-indent-function 1); -*-

;;;; Harp-MIDI -- Convert MIDI files to a format for easy playing on a harmonica.

;;;; Copyright (C) 2016 Jeandre Kruger

;;;; This file is part of Harp-MIDI.

;;;; Harp-MIDI is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.

;;;; Harp-MIDI is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with Harp-MIDI.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :harp-midi
  (:use :common-lisp :midi)
  (:export :harp-midi))

(in-package :harp-midi)

;;;; User interface

(defun harp-midi (arguments)
  (load-configuration-file)
  (let ((command (car arguments))
        (filenames (cdr arguments)))
    (cond ((null arguments)
           (format t "Usage: ~a FILE...~%" command))
          (t
           (load-configuration-file)
           (dolist (filename filenames)
             (cond ((probe-file filename)
                    (format t "~a:~%" filename)
                    (print-harp (read-midi-file filename)))
                   (t
                    (format t "File does not exist: ~a~%" filename)
                    (return))))))))

(defun load-configuration-file ()
  (if (probe-file "~/.harp-midi.lisp")
      (load "~/.harp-midi.lisp")))

;;;; Printing harmonica tabs

(defmacro donotes ((key track) &body body)
  (let ((track-name (gensym))
        (index-name (gensym))
        (message-name (gensym)))
    `(do* ((,track-name ,track (cdr ,track-name))
           (,index-name 0 (+ ,index-name 1))
           (,message-name (car ,track-name) (car ,track-name)))
          ((null ,track-name))
       ;; For some reasons there are always two NOTE ON messages for each note
       ;; and no NOTE OFF messages :-(
       ;; TODO: Find out why and try to fix it.
       (if (and (oddp ,index-name) (typep ,message-name 'note-on-message))
           (let ((,key (message-key ,message-name)))
             ,@body)))))

(defun print-harp (midi-file)
  (let* ((track (car (midifile-tracks midi-file)))
         (time-signature (find-if #'time-signature-message-p track)))
    (format t "~d |" (message-numerator time-signature))
    (donotes (key track)
      (format t "~2d" (key->hole key)))
    (format t "~%~d |" (expt 2 (message-denominator time-signature)))
    (donotes (key track)
      (cond ((blowp key) (princ " ↑"))
            ((drawp key) (princ " ↓"))))
    (format t "~%~%")))

(defun time-signature-message-p (message)
  (typep message 'time-signature-message))

;;;; Converting MIDI keys to holes and directions

(defun key->hole (key)
  (nth (mod key 12)
       '(4 nil 4 nil 5 5 nil 6 nil 6 nil 7)))

(defun blowp (key)
  (nth (mod key 12)
       '(t nil nil nil t nil nil t nil nil nil nil)))

(defun drawp (key) (not (blowp key)))
