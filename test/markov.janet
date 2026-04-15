(import ../markov)

# train chain from text
(def chain (markov/train "the cat sat on the mat the cat sat"))

# chain should map bigrams to possible next words
(assert (deep= (sort (keys chain))
               (sort @["the cat" "cat sat" "sat on" "on the" "the mat" "mat the"]))
        "chain keys are bigrams")

(assert (deep= (chain "the cat") @["sat" "sat"])
        "the cat -> sat (appears twice)")

(assert (deep= (chain "cat sat") @["on"])
        "cat sat -> on")

(assert (deep= (chain "on the") @["mat"])
        "on the -> mat")

(assert (deep= (chain "the mat") @["the"])
        "the mat -> the")

(assert (deep= (chain "mat the") @["cat"])
        "mat the -> cat")

# reply uses input to seed from matching bigrams
(def result (markov/reply chain "the cat"))
(assert (string? result) "reply returns a string")
(assert (string/has-prefix? "the cat" result) "reply starts from matching bigram")

# reply with no matching bigrams still produces output
(def fallback (markov/reply chain "xyz abc"))
(assert (string? fallback) "reply with no match returns a string")
(assert (> (length fallback) 0) "reply with no match returns non-empty string")

# reply respects max-words
(def short (markov/reply chain "the cat" :max-words 4))
(assert (<= (length (string/split " " short)) 4) "max-words limits output")

# train with empty string
(def empty-chain (markov/train ""))
(assert (deep= empty-chain @{}) "empty input produces empty chain")

# train with fewer than 3 words
(def short-chain (markov/train "hello world"))
(assert (deep= short-chain @{}) "two words produces empty chain")

# incremental training
(def inc-chain (markov/train "the cat sat"))
(markov/train "the cat ran" inc-chain)
(assert (deep= (sort (inc-chain "the cat")) @["ran" "sat"])
        "incremental train merges next words")

(print "All tests passed.")
