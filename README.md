# clojure-lexer
clojure-lexer is simple Lexer basedon regular expression.

## Usage


```clojure
(defrecord Ident [name])
(defrecord EQ [literal])
(defrecord NUM [value])

(def regex-cb-map 
{"[a-zA-Z_]+": (fn [lexeme] (Ident. lexeme)) #create Id Token
 "=": (fn [lexeme] (EQ. lexeme)) # create EQ Token
 “\\d\\.\\d”: (fn [lexeme] (NUM. (number lexeme))) # create Number Token
})

(def lexer (create-lexer "pi = 3.14" regex-cb-map))

(token lexer)
```

output is a lazy-seq
```clojure
({:name pi} {:literal "="} {:value 3.14})
```


