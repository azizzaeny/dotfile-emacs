# Snippet

### What is Yasnippet
> "yasnippet is a way of writting snippet"

a file defining a snippet contains a the template to be expanded.     
the meta configuration directives look like `# property : value` if doesnt found the file itself considered as snippet templates      

example:   

```
# contributor : azizzaeny
# name: ...
# --
_${init}_
```

**Snippet Properties**  

``` 
# key        : abbrev, type expand snippet beffor hitting yas-expand
# name       : one-line displayed on the menu, just descriptive. and distinguishable from others
# condition  : snippet conditions if has condition.
# group      : grouping menu-bar
# expand-env : initiating a value config from yas.
# binding    : direct binding ex: C-c C-c C-m
# uuid       : unique, never mind this one.
```

**Syntax**    

```
` is to emed emacs-lisp code
example: 

for ($1;$2;$3) {
`yas-selected-text`$0
}

Timestamp: `(current-time-string)`

$0, $1, $2 is a tab stop $N, is place holder
bracket is default value
\begin{${1:enumerate}}
$0
\end{$1} 

```

**Organizing Snippet**   

```
(setq yas-snippet-dirs '("~"))

.yas-parents is a way to describing parent mode!

yas auto maticly arrange snippet based on mode 
```

Inspired from various example
- github-url [andreacrotti/yasnippet-snippet](https://github.com/AndreaCrotti/yasnippet-snippets/)


## Snippets Configuration 
Basic or Common snippet configuration per language or major mode specific.
 
#### clojure mode

def   
```clojure file=./snippets/clojure-mode/def 
# name: def
# key : def
# --
(def $0)
```
fn 
```clojure file=./snippets/clojure-mode/fn 
# name: fn
# key: fn
# --
(fn [$1]
  $0)$>
```

opts arguments

```clojure file=./snippets/clojure-mode/opts
# key: opts
# name: opts
# --
{:keys [$1]$>
 :or {$2}$>
 :as $3}$>
```
ns 

```clojure file=./snippets/clojure-mode/ns
# name: ns
# key: ns
# --
(ns `(cl-flet ((try-src-prefix
		(path src-pfx)
		(let ((parts (split-string path src-pfx)))
		  (if (= 2 (length parts))
		      (cl-second parts)
		    nil))))
       (let* ((p (buffer-file-name))
	      (p2 (cl-first
		   (cl-remove-if-not '(lambda (x) x)
				     (mapcar
				      '(lambda (pfx)
					 (try-src-prefix p pfx))
				      '("/src/cljs/" "/src/clj/" "/src/" "/test/")))))
	      (p3 (file-name-sans-extension p2))
	      (p4 (mapconcat '(lambda (x) x)
			     (split-string p3 "/")
			     ".")))
	 (replace-regexp-in-string "_" "-" p4)))`)
```

require

```clojure file=./snippets/clojure-mode/require
# name: require
# key: require
# expand-env: ((yas-triggers-in-field nil))
# --
(:require [$1 :as $2])$>
```

#### css-mode 

bg 

```css file=./snippets/css-mode/bg
# name: background-color: ...
# --
background-color: #${1:fff};
```

#### html-mode

html 
```html file=./snippets/html-mode/html
# name: <html>...</html>
# --
<html>
$0
</html>
```


doctype 
```html file=./snippets/html-mode/doctype
# name: Doctype HTML 5
# group : meta
# --
<!DOCTYPE html>
```

link stylesheet 
```html file=./snippets/html-mode/link.style
# name: link stylesheet
# --
<link rel="${1:stylesheet}" href="${2:url}" type="${3:text/css}" media="${4:screen}" />
```

script js src 
```html file=./snippets/html-mode/script.js-src
# name: script js src
# --
<script type="text/javascript" src="$1"></script>
```

script js inline 
```html file=./snippets/html-mode/script.js 
# name: script js inner
# --
<script type="text/javascript">
$0
</script>
```

#### emacs-lisp-mode 

defun 
```elisp file=./snippets/emacs-lisp-mode/defun
# name: defun
# key: def
# --
(defun ${1:fun} (${2:args})
  "${3:docstring}"
  ${4:(interactive${5: "${6:P}"})}
  $0)
```


#### sh-mode

args 
```sh file=./snippets/sh-mode/args
# name:args
# key: args
# --
if [ $# -lt ${1:2} ]
   then $0
fi
```


#### go-mode

import 
```go file=./snippets/go-mode/import
# name: import
# key: imp
# --
import "$1"
$0
```


#### js-mode

class 
```js file=./snippets/js-mode/class
# name: class
# key: class
# --
class ${1:Class}${2: extends ${3:ParentClass}} {
  ${4:constructor(${5:arg}) {
    ${6:super(arg);}
    $7
  }}

  $0
}
```

function 
```js file=./snippets/js-mode/function
# -*- mode: snippet; require-final-newline: nil -*-
# name: function
# key: f
# --
function ${1:name}(${2:arg}) {
    $0
}
```

const 
```js file=./snippets/js-mode/const
# name: const declaration
# key: const
# --
const ${1:name} = ${2:initial};
```

let 
```js file=./snippets/js-mode/let
# -*- mode: snippet; require-final-newline: nil -*-
# name: let declaration
# key: let
# --
let ${1:name} = ${2:initial};
```

for loop 
```js file=./snippets/js-mode/for
# -*- mode: snippet; require-final-newline: nil -*-
# name: for
# --
for (var ${1:i} = ${2:0}; $1 < ${3:collection}.length; $1++) {
  $0
}
```

for loop 
```js file=./snippets/js-mode/fof
# name: forOf
# key: fof
# --

for(let ${1:item} of ${2:object}) {
  ${0}
}
```
#### js2-mode 

using parent js-mode

```text file=./snippets/js2-mode/.yas-parents
js-mode
```


#### python-mode 

function

```python file=./snippets/python-mode/f 
# -*- mode: snippet -*-
# name: function
# key: f
# group: definitions
# --
def ${1:fun}(${2:args}):
    $0
```
