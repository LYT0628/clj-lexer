(ns clj-lexer.core
  (:import [java.util.regex Matcher Pattern ])
  (:require [clojure.string :as str ]))


(defprotocol MatchableSeq
  "A String Seq that contains several mattchable tokens."
  (forward [this] "Move pointer go forward a character.") 
  (match [this pattern] "Return a token if matched.Otherwise return nil.")
  (next-match [this] "Inform the Seq a token was matched to match next token.") )

 
(defrecord LexBuffer [content lexeme-begin forward])

(extend-type LexBuffer
  MatchableSeq
  (forward [this]
    (if (>= (count (:content this)) (:forward this) )
      (LexBuffer. (:content this)
                    (:lexeme-begin this)
                    (+ (:forward this) 1))))
  (match [this pattern]
    (let [m (.matcher (Pattern/compile pattern)
                      (subs (:content this)
                            (:lexeme-begin this)
                            (:forward this)))]
      (if (.find m)
        (.group m))))
  (next-match [this]
    (if (>=  (count (:content this)) (+ (:forward this) 1) )
      (LexBuffer. (:content this)
                  (+ (:forward this) 1) 
                  (+ (:forward this) 1)))))




(defn any-match? [buf patterns]
  "Return true if the given pattern list has regular expression that can match current  token in buf."
  (some (fn [pattern]
          (some? (match buf pattern)))
        patterns))

(defn first-match [buf patterns]
  "Return the first matched regular expression if any."
  (some (fn [pattern]  (when-let [result (match buf pattern)]
                         {:regex pattern :lexeme result}) ) patterns))



(defprotocol ILexer
  "A Lexer can scan a String Sequence."
  (tokens [this] "Return a lazy list of matched tokens."))


(defrecord Lexer [input buffer regex-callback-map])

(extend-type Lexer
  ILexer
  (tokens [this]
   (let [buf  (:buffer this)
         patterns (keys (:regex-callback-map this))]
     (if-not (nil? buf)
       (if (any-match? buf
                       patterns)
         (let [cb (get (:regex-callback-map this)
                       (:regex (first-match buf patterns)))]
           
           (lazy-seq (cons (cb (:lexeme (first-match buf patterns)))
                           (tokens (Lexer. (:input this)
                                          (next-match buf)
                                          (:regex-callback-map this))))))
                  (lazy-seq
          (tokens (Lexer. (:input this)
                         (forward buf)
                         (:regex-callback-map this)))))
     ))))



(defn create-lexer [input regex-callback-map]
  (Lexer. input (LexBuffer. input 0 0) regex-callback-map ) )

