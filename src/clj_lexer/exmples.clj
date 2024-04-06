(ns clj-lexer.exmples
  (:require [clj-lexer.core :as lex]))


(def yaml-version-directive-pattern "YAML\\s+\\d\\.\\d")

(def rules
  {yaml-version-directive-pattern (fn [lexeme] lexeme)})

(def lexer (lex/create-lexer "YAML 1.1" rules))



(lex/tokens lexer)
