;;;; System definition for Harp-MIDI.

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

(defsystem harp-midi
  :description "Convert MIDI files to a format for easy playing on a harmonica."
  :version "0.2.1"
  :author "Jeandre Kruger"
  :license "GNU GPLv3"
  :depends-on (midi)
  :components ((:file "harp-midi")))
