(ns clj-lexer.core
  (:import [java.util.regex Matcher Pattern ])
  (:require [clojure.string :as str ]))

;;; Util

(defn longest-string [str-list]
  (reduce (fn [max-str str]
            (if (> (count str) (count max-str))
              str
              max-str))
          nil str-list))

;;; Protocol


(defprotocol MatchableSeq
  "A String Seq that contains several mattchable tokens."
  (result [this] "Return a nre MatchableSeq if matched.Otherwise return nil.")
  (pattern [this] "Return the pattern that matches the token.")
  (match [this pattern] "Inform the Seq a token was matched to match next token.")
  (rest-seq [this] "Return a rest string that still are not matched."))

 
(defprotocol ILexer
  "A Lexer can scan a String Sequence."
  (tokens [this] "Return a lazy list of matched tokens."))


;;; Buffer

(defrecord LexBuffer [content lexeme-begin lexeme regex])

(extend-type LexBuffer
  MatchableSeq
  (match [this regex]
    (let [m (.matcher (Pattern/compile regex)
                      (subs (:content this)
                            (:lexeme-begin this)))]
      (if (.lookingAt m)
        (LexBuffer. (:content this)
                    (+ (.end m) (:lexeme-begin this)) 
                    (.group m)
                    regex)
        (LexBuffer. (:content this)
                    (:lexeme-begin this)
                    nil
                    nil))))
  (result [this] (:lexeme this))
  (pattern [this] (:regex this))
  (rest-seq [this]
    (subs (:content this)
          (:lexeme-begin this))))



;;; Lexer

(defn longest-match [buf patterns]
  "Return the longest matched regular expression if any."
  (->> patterns
       (map #(match buf %))
       (filter #(some? (result %)))
       (longest-string)))

(defrecord Lexer [input buffer regex-callback-map])

(extend-type Lexer
  ILexer
  (tokens [this]
   (when-let [buf (:buffer this)]
     (let [cb-map (:regex-callback-map this)
           patterns (keys cb-map)]
       (if-let [cb (get cb-map (pattern buf))]
         (lazy-seq (cons (cb (result buf))
                         (tokens (Lexer. (:input this)
                                         (longest-match buf patterns)
                                         cb-map))))
         ;; first match
         (lazy-seq (tokens (Lexer. (:input this)
                                   (longest-match buf patterns)
                                   cb-map))))))))


  (defn create-lexer [input regex-callback-map]
    (Lexer. input
            (LexBuffer. input 0 nil nil)
            regex-callback-map ))

