# Harp-MIDI

Harp-MIDI is a program to convert [Standard MIDI Files](https://en.wikipedia.org/wiki/MIDI#Standard_MIDI_files)
into a format for easy playing on a harmonica.

This format shows, for each note, the number of the hole you need to use and whether you need to blow in it or draw
out of it, and the time signature.

## Compilation

To compile Harp-MIDI you'll need an implementation of [Common Lisp](https://common-lisp.net/) that works with UTF-8.
You will also need the cool [Common Lisp MIDI Library](http://www.doc.gold.ac.uk/isms/lisp/midi/).

The recommended way to compile Harp-MIDI and the Lisp libraries it depends on is to use [Quicklisp](https://www.quicklisp.org/)
and [Buildapp](http://www.xach.com/lisp/buildapp/). Clone this repository, start the Lisp read-eval-print-loop in this directory
and type the following:

```
(push #p"./" asdf:*central-registry*)
(ql:quickload "harp-midi")
```

Then enter the following into your shell:

```
buildapp --asdf-tree ~/quicklisp --asdf-tree . --load-system harp-midi --entry harp-midi:harp-midi --output harp-midi
```

Now you should have a binary `harp-midi` in your current directory.

## Configuration

Upon startup Harp-MIDI loads the Lisp file at `~/.harp-midi.lisp`, if it exists. Here you (will be able to &ndash;
currently there is nothing to configure) put configuration options for Harp-MIDI.

## Current Limitations

* Output format is not configurable.
* Only works for 10-hole diatonic harmonicas with the usual C key &ndash; I think these are the most common type.
* No semitones.
* Only works for sequential tunes: no tunes with more than one note playing at a time.

## Copyright

Harp-MIDI is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License
as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

Harp-MIDI is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Harp-MIDI.  If not, see http://www.gnu.org/licenses/.
