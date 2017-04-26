SVSDOC is a quick-and-dirty documentation system for Common Lisp.
Converts Common Lisp documentation strings to HTML.
Minimal dependencies: Just closer-mop.
It's very simple: It only generates HTML output, and it sends all the
documentation for a package to a single HTML page. It's much simpler
than most of the other documentation systems for Common Lisp but that
also makes it much easier to maintain and easier to port.

EXAMPLE
(with-open-file (s "ccl:svsdoc.html" :direction :output :if-exists :supersede)
  (svsdoc:print-package-docs :svsdoc s :external t :internal t :variables t :functions t :macros t :classes t :generic-functions t))

OTHER DOCUMENTATION SYSTEMS FOR COMMON LISP
(ql:quickload "atdoc")
www.lichteblau.com/atdoc/doc/
Not recommended. Loads, but doesn't run, in CCL because it's
not conditionalized for CCL, and depends on a WHOLE BUNCH
of other stuff. Very hard to call. See the example of calling it in 
gendocs.lisp, in the trivial-garbage system. cl-geonames also uses
it for documentation:
https://github.com/nlamirault/cl-geonames

(ql::quickload "cl-api")
Generates HTML output, simple, easy to call. Has to be modified
to make it work in CCL. This is easy to do, but it doesn't seem
to have a public Git or other repo so I can't do a pull request for
my changes.