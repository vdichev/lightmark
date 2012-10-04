LightMark
=========

LightMark (short for Lightweight Markup) is a reStructuredText parser, written in Scala.

Currently there's only a reStructuredText to HTML conversion.

The following elements are supported:

* sections
* paragraphs
* bullet lists
* definition lists
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

Design
------

Lightmark is a collection of functions, known as parser combinators, which combine to define the grammar of reStructuredText in a fairly readable DSL. Simple combinators are used to define higher-level ones. When raw text is parsed by a combinator, the output is the resulting syntax tree of reStructuredText elements. The text is preprocessed initially (e.g. trailing spaces are stripped) to make it more regular and avoid code complication due to redundant corner case checking.

There are different approaches to parsing. For instance, the Python state engine for parsing reStructuredText is using regular expressions and invoking nested instances of the state engine in order to be able to parse embedded markup. It's questionable whether this provides significant performance gains, while Scala's parser combinators DSL significantly improves the readability and clarity of intent.

reStructuredText is a fairly complicated markup language to parse. Different text elements can be parsed differently depending on context (for instance, heading level depends on the order of heading underline symbols used). Using Scala is very convenient in this case, since we can express the statefulness of the parser using state encapsulated in the object, while retaining the expressiveness of the functional solution using parser combinators.

TODO
----

* comments
* substitutions
* enumerated lists
* field lists
* tables
* hyperlinks
* footnotes
* citations
* directives
