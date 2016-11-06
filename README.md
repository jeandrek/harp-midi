# Harp-MIDI

Harp-MIDI is a program to convert [Standard MIDI Files](https://en.wikipedia.org/wiki/MIDI#Standard_MIDI_files)
into a format for easy playing on a harmonica.

This format shows, for each note, the number of the hole you need to use and whether you need to blow in it or draw
out of it, and the time signature.

## Installation

To use Harp-MIDI you'll need an implementation of [Common Lisp](https://common-lisp.net/) that works with UTF-8 and
[ASDF](https://common-lisp.net/project/asdf/). You will also need the cool [Common Lisp MIDI Library]
(http://www.doc.gold.ac.uk/isms/lisp/midi/).

You can find a list of Common Lisp implementations [here](https://common-lisp.net/downloads/). The recommended way to
install Harp-MIDI and its dependencies is to use [Quicklisp](https://www.quicklisp.org/). Clone this repository, start
the Lisp read-eval-print-loop in this directory and run:

```lisp
(ql:quickload "harp-midi")
```

## Configuration

Upon startup Harp-MIDI loads the Lisp file at `~/.harp-midi.lisp`, if it exists. Here you (will be able to &ndash;
currently there is nothing to configure) put configuration options for Harp-MIDI.

## Current Limitations

* Output format is not configurable.
* Only works in one octave.
* Only works for harmonicas with the usual C key &ndash; I think&hellip.
