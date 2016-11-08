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
         (process-arguments (cdr arguments)))))

(defun load-configuration-file ()
  (let ((pathname (merge-pathnames (user-homedir-pathname) ".harp-midi.lisp")))
    (if (probe-file pathname)
        (load pathname))))

(defun process-arguments (arguments)
  (do* ((arguments arguments (cdr arguments))
        (argument (car arguments) (car arguments)))
       ((null arguments))
    (cond ((command-line-option-p argument)
           (setf (symbol-value (command-line-option->symbol argument))
                 (read-from-string (cadr arguments)))
           (setf arguments (cdr arguments)))
          (t
           (format t "~a:~%" argument)
           (catch 'stop
             (if (probe-file argument)
                 (print-harp (read-midi-file argument))
                 (signal-error "File does not exist: ~a" argument)))))))

(defun command-line-option-p (argument)
  (and (>= (length argument) 2)
       (string= (subseq argument 0 2) "--")))

(defun command-line-option->symbol (argument)
  (intern (concatenate 'string "*" (string-upcase (subseq argument 2)) "*") 'harp-midi))

;;;; Configurable options

(defparameter *track* 0)

;;;; Printing output for harmonica

(defparameter *the-midifile* nil)

(defun print-harp (*the-midifile*)
  (let* ((track (nth *track* (midifile-tracks *the-midifile*)))
         (notes (notes track)))
    (print-time-signature track)
    (dolist (note notes)
      (format t "~2d" (note->hole note)))
    (format t "~% ")
    (dolist (note notes)
      (cond ((blowp note) (princ " ↑"))
            ((drawp note) (princ " ↓"))))
    (format t "~% ")
    (dolist (note notes)
      (format t "~2d" (duration note)))
    (terpri)))

(defun print-time-signature (track)
  (let ((time-signature (time-signature track)))
    (if time-signature
        (format t " Time signature: ~d/~d~% " (car time-signature) (cdr time-signature)))))

(defun time-signature (track)
  (let ((message (find-if #'time-signature-message-p track)))
    (if message
        (cons (message-numerator message) (expt 2 (message-denominator message))))))

(defun notes (track)
  (do ((track track (cdr track))
       (message (car track) (car track))
       (result '()))
       ((null track) (nreverse result))
    (when (note-on-message-p message)
      ;; Find matching NOTE-OFF-MESSAGE.
      (do ((message-2 (car track) (car track)))
          ((note-off-message-p message-2)
           (setf result (cons (messages->note message message-2) result)))
        (if (note-on-message-p message-2)
            (signal-error "Notes playing concurrently: ~d and ~d"
                          (message-key message) (message-key message-2))
            (setf track (cdr track)))))))

(defun messages->note (message-1 message-2)
  (make-note (message-key message-1)
             (round (- (message-time message-2) (message-time message-1))
                    (midifile-division *the-midifile*))))

;;;; Message predicates

(defun note-on-message-p (message)
  (and (typep message 'note-on-message)
       (> (message-velocity message) 0)))

(defun note-off-message-p (message)
  (or (typep message 'note-off-message)
      (and (typep message 'note-on-message)
           (= (message-velocity message) 0))))

(defun time-signature-message-p (message) (typep message 'time-signature-message))

;;;; Notes

(defun make-note (pitch duration) (cons pitch duration))
(defun pitch (note) (car note))
(defun duration (note) (cdr note))

;;;; Converting MIDI notes to holes and directions

(defun note->hole (note)
  (let ((pitch (pitch note)))
    (cond ((< pitch 48) (signal-error "Note too low: ~d" pitch))
          ((> pitch 84) (signal-error "Note too high: ~d" pitch))
          (t (aref #(1 nil 1 nil 2 2 nil 3 nil nil nil 3
                     4 nil 4 nil 5 5 nil 6 nil 6 nil 7
                     7 nil 8 nil 8 9 nil 9 nil 10 nil nil
                     10)
                   (- pitch 48))))))

(defun blowp (note)
  (aref #(t nil nil nil t nil nil t nil nil nil nil)
        (mod (pitch note) 12)))

(defun drawp (note) (not (blowp note)))

;;;; Signalling errors

(defun signal-error (control-string &rest args)
  (princ " ")
  (apply #'format t control-string args)
  (terpri)
  (throw 'stop nil))
