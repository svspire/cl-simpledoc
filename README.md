CL-SIMPLEDOC is a quick-and-dirty documentation system for Common Lisp, which
converts Common Lisp documentation strings to HTML.

It has minimal dependencies: Just `closer-mop`.
It's intended to be generic Common Lisp, but it's been tested only in CCL.

CL-SIMPLEDOC is very simple: It only generates HTML output, and it sends all the
documentation for a package to a single HTML page. It's much simpler
than most of the other documentation systems for Common Lisp but that
also makes it easier to maintain and easier to port.

Since any Common Lisp automatic documentation system should be able to document itself, we've done that with [cl-simpledoc.html](http://htmlpreview.github.com/?https://github.com/svspire/cl-simpledoc/blob/master/cl-simpledoc.html), which is included in this repository for your convenience.

## Example

Here's how we generated [cl-simpledoc.html](http://htmlpreview.github.com/?https://github.com/svspire/cl-simpledoc/blob/master/cl-simpledoc.html), which is in this repository.

```lisp
(ql:quickload :cl-simpledoc) ; we're not in quicklisp as yet. This assumes you cloned this repo to your local disk.
(with-open-file (s "ccl:cl-simpledoc.html" :direction :output :if-exists :supersede)
  (cl-simpledoc:print-package-docs :cl-simpledoc s :external t :internal t :variables t :functions t :macros t :classes t :generic-functions t))
  ```
  
## Other documentation systems for common lisp

### ATDOC
www.lichteblau.com/atdoc/doc/

```lisp
(ql:quickload "atdoc")
```

Not recommended for CCL. Appears to be designed for SBCL.
Loads, but doesn't run, in CCL because it's not conditionalized for CCL, and depends on a WHOLE BUNCH
of other stuff. Very hard to call. See the example of calling it in 
gendocs.lisp, in the trivial-garbage system. `cl-geonames` also uses
it for documentation:
https://github.com/nlamirault/cl-geonames

### CL-API

```lisp
(ql::quickload "cl-api")
```

Generates HTML output. Simple, easy to call. Has to be modified
to make it work in CCL. This is easy to do, but I haven't done it.
https://github.com/kiuma/CL-API
