# javasgrips
This is an esoteric meme programming language, inspired by a colleague of mine.
Only disciples can understand.

### Prompt
Actually only the interpreter within a repl is available. The prompt is `AS> `.
If you know, you know.

### Types
The language actually has two types of values: inetegers and strings
```
AS> "hello world!"
"hello world!"
AS> -123
-123
```

### Variable declarations
You can declare a new variable like this:
```
AS> catorcino foo è 42
```
In this case, `foo` is the variable name and `42` the assigned value.

### Commands
Some interesting commands:

#### Print
You can pretty print values:
```
AS> stampa etichetta 126
126
```
Well, it doesn't do nothing special with numbers, but look at what it does with
strings:
```
AS> stampa etichetta "IU AR BIUTIFUL"
"IU AR BIUTIFUL, ti è piaciuto?"
```

#### Stack pretty printing
You can display with `penetra` command:
```
AS> catorcino X è 6
AS> catorcino _pippo è "pluto"
AS> catorcino bar è 555
AS> penetra
bar    -> 555
_pippo -> "pluto"
X      -> 6
```

### Errors
Some special message for you, if you are unable to write a program in javasgrips.

#### Syntax error
```
AS> "ciao
*** Questa è brutta come frase
```

#### Runtime error
```
AS> catorcino a è -88
AS> catorcino b è "hi"
AS> catorcino a è 7
*** Ma sei fulminato?
```
