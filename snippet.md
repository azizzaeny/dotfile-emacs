# Snippet

### What is Yasnippet
> "yasnippet is a way of writting snippet"

file defining a snippet contains the template to be expanded.     
the meta configuration directives look like `# property : value` if it does not found in the prop value then the file itself considered as snippet templates      

example:   

```txt
# contributor : azizzaeny
# name: ...
# --
_${init}_
```
**Snippet Properties**  

```txt 
# key        : abbrev, type expand snippet beffor hitting yas-expand
# name       : one-line displayed on the menu, just descriptive. and distinguishable from others
# condition  : snippet conditions if has condition.
# group      : grouping menu-bar
# expand-env : initiating a value config from yas.
# binding    : direct binding ex: C-c C-c C-m
# uuid       : unique, never mind this one.
```

**Syntax**    

```txt
use `` to eval emacs-lisp code
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

```txt
(setq yas-snippet-dirs '("/path/to/dir/snippets"))
.yas-parents is a way to describing parent mode!
yas automaticly arrange snippet based on major or minor mode that currently active

```

Inspired from various example
- github-url [andreacrotti/yasnippet-snippet](https://github.com/AndreaCrotti/yasnippet-snippets/)


## Snippets Configuration 
Basic or Common snippet configuration per language or major mode specific.

**js2-mode**

```text file=~/.emacs.d/snippets/js2-mode/.yas-parents 

js-mode
```

**js-mode**
1. for of

```js file=~/.emacs.d/snippets/js-mode/fof 

# name: forOf
# key: fof
# --

for(let ${1:item} of ${2:object}) {
	${0}
}
```

2. for loop

```js file=~/.emacs.d/snippets/js-mode/for 

# -*- mode: snippet; require-final-newline: nil -*-
# name: for
# --
for (var ${1:i} = ${2:0}; $1 < ${3:collection}.length; $1++) {
	$0
}
```

3. let 

```js file=~/.emacs.d/snippets/js-mode/let 

# -*- mode: snippet; require-final-newline: nil -*-
# name: let declaration
# key: let
# --
let ${1:name} = ${2:initial};
```

4. const 

```js file=~/.emacs.d/snippets/js-mode/const 

# name: const declaration
# key: const
# --
const ${1:name} = ${2:initial};
```

5. function

```js file=~/.emacs.d/snippets/js-mode/function 

# -*- mode: snippet; require-final-newline: nil -*-
# name: function
# key: f
# --
function ${1:name}(${2:arg}) {
    $0
}
```
6. class

```js file=~/.emacs.d/snippets/js-mode/class  

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
**go-mode**

1. import 

```go file=~/.emacs.d/snippets/go-mode/import 

# name: import
# key: imp
# --
import "$1"
$0
```

**emacs-lisp-mode**

1. defun

```elisp file=~/.emacs.d/snippets/emacs-lisp-mode/defun 

# name: defun
# key: def
# --
(defun ${1:fun} (${2:args})
  "${3:docstring}"
  ${4:(interactive${5: "${6:P}"})}
  $0)
```

**html-mode**

1.js-inline 

```html file=~/.emacs.d/snippets/html-mode/script.js 

# name: script js inner
# --
<script type="text/javascript">
  $0
</script>
```

2. js-src 

```html file=~/.emacs.d/snippets/html-mode/script.js-src 

# name: script js src
# --
<script type="text/javascript" src="$1"></script>
```

3. html 

```html file=~/.emacs.d/snippets/html-mode/html 

# name: html
# --
<html>
  $0
</html>
```

4. link stylesheet

```html file=~/.emacs.d/snippets/html-mode/link 

# name: link stylesheet
# --
<link rel="${1:stylesheet}" href="${2:url}" type="${3:text/css}" media="${4:screen}" />
```


**css-mode** 

1. bg 

```txt file=~/.emacs.d/snippets/css-mode/bg 

# name: background-color
# --
background-color: #${1:fff};
```

**clj-mode**

1. require 

```clojure file=~/.emacs.d/snippets/clojure-mode/require 

# name : require
# key  : require
# expand-env : ((yas-triggers-in-field nil)))
# --
(:require [$1 :as $2]) $>
```

2. key opts 

```clojure file=~/.emacs.d/snippets/clojure-mode/opts 

# key: opts
# name: opts
# --
{:keys [$1]$>
 :or {$2}$>
 :as $3}$>
```

3. fn 

```clojure file=~/.emacs.d/snippets/clojure-mode/fn 

# name: fn
# key: fn
# --
(fn [$1]
  $0)$>
```

4. def

```clojure file=~/.emacs.d/snippets/clojure-mode/def 

# name: def
# key : def
# --
(def $0)
```
