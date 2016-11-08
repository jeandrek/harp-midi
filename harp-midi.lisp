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
  (:export :harp-midi :*track*))

(in-package :harp-midi)

;;;; User interface

(defun harp-midi (arguments)
  (cond ((null (cdr arguments))
         (format t "Usage: ~a [option or filename ...]~%" (car arguments)))
        (t
         (load-configuration-file)
         (do ((arguments (cdr arguments) (cdr arguments)))
             ((null arguments))
           (let ((argument (car arguments)))
             (cond ((command-line-option-p argument)
                    (setf (symbol-value (command-line-option->symbol argument))
                          (read-from-string (cadr arguments)))
                    (setf arguments (cdr arguments)))
                   ((probe-file argument)
                    (format t "~a:~%" argument)
                    (print-harp (read-midi-file argument)))
                   (t
                    (format t "File does not exist: ~a~%" argument)
                    (return))))))))

(defun load-configuration-file ()
  (let ((pathname (merge-pathnames (user-homedir-pathname) ".harp-midi.lisp")))
    (if (probe-file pathname)
        (load pathname))))

(defun command-line-option-p (argument)
  (and (>= (length argument) 2)
       (string= (subseq argument 0 2) "--")))

(defun command-line-option->symbol (argument)
  (intern (concatenate 'string "*" (string-upcase (subseq argument 2)) "*") 'harp-midi))

;;;; Configurable options

(defparameter *track* 0)

;;;; Printing output for harmonica

(defparameter *the-midifile* nil)

(defmacro donotes ((note-name track-exp) &body body)
  (let ((track-name (gensym))
        (index-name (gensym))
        (message-name (gensym)))
    `(do* ((,track-name ,track-exp (cdr ,track-name))
           (,index-name 0 (+ ,index-name 1))
           (,message-name (car ,track-name) (car ,track-name)))
          ((null ,track-name))
       (when (and (evenp ,index-name) (typep ,message-name 'note-on-message))
         (let ((,note-name
                (make-note
                 (message-key ,message-name)
                 (round (- (message-time (cadr ,track-name)) (message-time ,message-name))
                        (midifile-division *the-midifile*)))))
           ,@body)))))

(defun print-harp (*the-midifile*)
  (let ((track (nth *track* (midifile-tracks *the-midifile*))))
    (format t " Time signature: ~d~% " (time-signature track))
    (donotes (note track)
      (let ((hole (note->hole note)))
        (if hole
            (format t "~2d" hole)
            (return-from print-harp))))
    (format t "~% ")
    (donotes (note track)
      (cond ((blowp note) (princ " ↑"))
            ((drawp note) (princ " ↓"))))
    (format t "~% ")
    (donotes (note track)
      (format t "~2d" (duration note)))
    (format t "~%~%")))

(defun time-signature (track)
  (labels ((time-signature-message-p (message)
             (typep message 'time-signature-message)))
    (let ((message (find-if #'time-signature-message-p track)))
      (/ (message-numerator message) (expt 2 (message-denominator message))))))

;;;; Notes

(defun make-note (pitch duration) (cons pitch duration))
(defun pitch (note) (car note))
(defun duration (note) (cdr note))

;;;; Converting MIDI notes to holes and directions

(defun note->hole (note)
  (let ((pitch (pitch note)))
    (cond ((< pitch 48) (format t "~%Note too low: ~d~%" pitch))
          ((> pitch 84) (format t "~%Note too high: ~d~%" pitch))
          (t (aref #(1 nil 1 nil 2 2 nil 3 nil nil nil 3
                     4 nil 4 nil 5 5 nil 6 nil 6 nil 7
                     7 nil 8 nil 8 9 nil 9 nil 10 nil nil
                     10)
                   (- pitch 48))))))

(defun blowp (note)
  (aref #(t nil nil nil t nil nil t nil nil nil nil)
        (mod (pitch note) 12)))

(defun drawp (note) (not (blowp note)))
