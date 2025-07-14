This project is a simple MiniLisp interpreter implemented in Wolfram Mathematica. It supports basic Lisp-like functionality, including function definitions, conditionals, list operations, and basic evaluation. The interpreter includes a minimal REPL interface.

## 🔧 How to Run

1. Open the `MiniLisp.nb` or `Dev.nb` notebook in **Wolfram Mathematica**.

2. Make sure the `MiniLisp.wl` file (your implementation) is saved in the same folder.

3. Run the following code to load the interpreter:

   ```mathematica
   SetDirectory[NotebookDirectory[]]
   Remove["MiniLisp`*"]
   Needs["MiniLisp`"]
   Get["MiniLisp`"]
   rep[]
   ```

4. A **REPL (Read-Eval-Print Loop)** prompt will open:

   ```
   Mini‑Lisp (type quit / exit)
   mini>
   ```

You can now enter MiniLisp commands interactively!

---

## 📜 Supported Commands & Examples

Each command can be tested directly in the REPL. Below each example is the expected output.

---

### 🔹 `quote`

Returns the unevaluated expression.

**Examples and Outputs:**

```lisp
(quote a)              => a
(quote (1 2 3))        => (1 2 3)
(quote (a b c))        => (a b c)
(quote ((1 . 2) . 3))  => ((1 . 2) . 3)
(quote (quote x))      => (quote x)
```

---

### 🔹 `label`

Defines a named function.

**Examples and Outputs:**

```lisp
(label add (lambda (x y) (cons x y)))   => add
(label id (lambda (x) x))               => id
(label tru (lambda (x y) x))            => tru
(label fals (lambda (x y) y))           => fals
```

**Tests (invoking the defined functions):**

```lisp
(add 1 2)        => (1 . 2)
(id 5)           => 5
(tru 1 2)        => 1
(fals 1 2)       => 2
```

---

### 🔹 `lambda`

Defines an anonymous function.

**Examples and Outputs:**

```lisp
((lambda (x) x) 5)                               => 5
((lambda (x y) x) 1 2)                           => 1
((lambda (x) (cons x x)) a)                      => (a . a)
((lambda (x) (car x)) (quote (1 2)))             => 1
((lambda (x y) (cdr y)) a (quote (x y z)))       => (y z)
```

---

### 🔹 `cond`

Conditional logic.

**Examples and Outputs:**

```lisp
(cond ((eq a a) 1) ((eq a b) 2))                 => 1
(cond ((null (quote ())) 5) (t 10))              => 5
(cond ((eq 1 2) no) ((eq 2 2) yes))               => yes
(cond ((atom 1) true) (t false))                 => true
(cond ((null (quote a)) no) ((atom a) yes))      => yes
```

---

### 🔹 `car`

Returns the first element.

**Examples and Outputs:**

```lisp
(car (cons a b))                             => a
(car (quote (x y z)))                        => x
(car (cons 1 2))                             => 1
(car (quote ((a . b) c)))                    => (a . b)
(car (cons (cons 1 2) 3))                    => (1 . 2)
```

---

### 🔹 `cdr`

Returns the second element.

**Examples and Outputs:**

```lisp
(cdr (quote (x y z)))                        => (y z)
(cdr (cons 1 2))                             => 2
(cdr (quote ((a . b) c)))                    => (c)
(cdr (cons 1 (cons 2 3)))                    => (2 . 3)
```

---

### 🔹 `atom`

Checks if the input is an atom.

**Examples and Outputs:**

```lisp
(atom a)                                     => true
(atom (quote (a b)))                         => false
(atom (cons 1 2))                            => false
(atom 5)                                     => true
(atom (quote ()))                            => true
```

---

### 🔹 `null`

Checks if the input is the empty list.

**Examples and Outputs:**

```lisp
(null (quote ()))                            => true
(null (quote a))                            => false
(null (cdr (quote (x))))                    => true
(null (cdr (quote (x y))))                  => false
(null (quote (1 2)))                        => false
```

---

### 🔹 `eq`

Checks equality between two atoms.

**Examples and Outputs:**

```lisp
(eq a a)                                     => true
(eq a b)                                     => false
(eq 1 1)                                     => true
(eq 1 2)                                     => false
(eq (quote x) (quote x))                     => true
```

---

### 🔹 `list`

Evaluates and constructs a list.

**Examples and Outputs:**

```lisp
(list a b c)                                 => (a b c)
(list 1 2 3)                                 => (1 2 3)
(list (car (quote (x y))) (cdr (quote (x y)))) => (x (y))
(list (atom a) (null (quote ())))            => (true true)
(list (quote 1) (quote 2) (quote 3))          => (1 2 3)
```

---

### 🔹 `assoc`

Searches an association list.

**Examples and Outputs:**

```lisp
(assoc a (quote ((pair a 1) (pair b 2))))   => (pair a 1)
(assoc b (quote ((pair a 1) (pair b 2))))   => (pair b 2)
(assoc x (quote ((pair a 1) (pair b 2))))   => ()
(assoc 1 (quote ((pair 1 x) (pair 2 y))))   => (pair 1 x)
(assoc (quote a) (quote ((pair a apple) (pair b banana)))) => (pair a apple)
```

---

### 🔹 `pairup`

Zips two lists into a list of pairs.

**Examples and Outputs:**

```lisp
(pairup (quote (a b)) (quote (1 2)))        => ((a . 1) (b . 2))
(pairup (quote ()) (quote (1 2)))            => ()
(pairup (quote (x y z)) (quote (1 2 3)))     => ((x . 1) (y . 2) (z . 3))
(pairup (quote (x)) (quote (y)))             => ((x . y))
(pairup (quote (a b c)) (quote (x y z)))     => ((a . x) (b . y) (c . z))
```

---

### 🔹 `ffappend`

Concatenates two lists.

**Examples and Outputs:**

```lisp
(ffappend (quote (1 2)) (quote (3 4)))      => (1 2 3 4)
(ffappend (quote ()) (quote (a b)))          => (a b)
(ffappend (quote (a)) (quote (b c)))         => (a b c)
(ffappend (quote (x y)) (quote ()))          => (x y)
(ffappend (quote ()) (quote ()))             => ()
```

---


## 🛑 Exiting the Interpreter

Type:

```
quit
```

or

```
exit
```

---

## 💬 Notes

* Any syntax errors or unsupported operations will raise a clear error message.
* Use `'` as a shorthand for `quote`, e.g., `'a` is equivalent to `(quote a)`.
