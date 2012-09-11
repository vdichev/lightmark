LightMark
=========

LightMark (short for Lightweight Markup) is a reStructuredText parser, written in Scala.

Currently there's only a reStructuredText to HTML conversion.

The following elements are supported:

* sections
* paragraphs
* bullet lists
* emphasis, strong, inline literals
* escapes of inline markup
* block quotes
* literal blocks

Usage
-----

From command-line::

	sbt 'run [source [destination]]'

When destination is omitted, standard output is assumed. When source is omitted, standard input is assumed.

From Scala::

	new reStructuredTextParser().parse(stringContents)

TODO
----

* comments
* substitutions
* enumerated lists
* definition lists
* field lists
* tables
* hyperlinks
* footnotes
* citations
* directives
