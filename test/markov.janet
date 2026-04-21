(import ../markov)

# ---------------------------------------------------------------------------
# tokenization
# ---------------------------------------------------------------------------

(defn tokenize [text]
  # exercise tokenization through train by inspecting the resulting keys
  (keys ((markov/train text (markov/new-chain :order 1)) :transitions)))

(assert (deep= (markov/train "" ) @{:transitions @{} :order 3 :starts @[]})
        "empty input produces empty chain")

# punctuation splits into its own token
(let [chain (markov/train "Hello, world." (markov/new-chain :order 1))]
  (assert ((chain :transitions) "Hello") "Hello is a key")
  (assert ((chain :transitions) ",")     "comma is a key")
  (assert ((chain :transitions) "world") "world is a key"))

# contractions stay intact
(let [chain (markov/train "don't stop, can't stop." (markov/new-chain :order 1))]
  (assert ((chain :transitions) "don't") "don't is one token")
  (assert ((chain :transitions) "can't") "can't is one token"))

# hyphenated words stay intact
(let [chain (markov/train "self-aware systems work." (markov/new-chain :order 1))]
  (assert ((chain :transitions) "self-aware") "self-aware is one token"))

# UTF-8 characters pass through untouched.
# Validated against a real Gutenberg text that contains curly quotes and em-dashes.
(when (os/stat "/tmp/moby-dick.txt")
  (let [chain1 (markov/train (slurp "/tmp/moby-dick.txt") (markov/new-chain :order 1))
        curly-apos (string/from-bytes 0xe2 0x80 0x99)
        queequeg-key (string "Queequeg" curly-apos "s")]
    (assert ((chain1 :transitions) queequeg-key)
            "curly apostrophe stays intact: possessive is one token")
    (assert (not ((chain1 :transitions) (string/from-bytes 0xe2)))
            "lone UTF-8 lead byte is not a chain key")))

# ---------------------------------------------------------------------------
# sentence boundary tracking
# ---------------------------------------------------------------------------

(let [chain (markov/train "The cat sat. A dog ran.")]
  # "The" starts the first sentence, "A" starts the second
  (assert (some |(string/has-prefix? "The cat" $) (chain :starts))
          "first sentence start is tracked")
  (assert (some |(string/has-prefix? "A dog" $) (chain :starts))
          "post-period sentence start is tracked"))

# ---------------------------------------------------------------------------
# chain structure
# ---------------------------------------------------------------------------

(def text "the cat sat on the mat the cat sat")

(def chain (markov/train text))
(assert (= (chain :order) 3) "default order is 3")

(assert (deep= (sort (keys (chain :transitions)))
               (sort @["the cat sat" "cat sat on" "sat on the" "on the mat" "the mat the" "mat the cat"]))
        "chain keys are trigrams")

(assert (deep= ((chain :transitions) "the cat sat") @["on"]) "the cat sat -> on")
(assert (deep= ((chain :transitions) "cat sat on") @["the"]) "cat sat on -> the")
(assert (deep= ((chain :transitions) "sat on the") @["mat"]) "sat on the -> mat")
(assert (deep= ((chain :transitions) "on the mat") @["the"]) "on the mat -> the")
(assert (deep= ((chain :transitions) "the mat the") @["cat"]) "the mat the -> cat")
(assert (deep= ((chain :transitions) "mat the cat") @["sat"]) "mat the cat -> sat")

# explicit order 2 (bigrams)
(def chain2 (markov/train text (markov/new-chain :order 2)))
(assert (= (chain2 :order) 2) "explicit order 2")
(assert (deep= ((chain2 :transitions) "the cat") @["sat" "sat"]) "order-2: the cat -> sat twice")

# fewer than 4 words with order-3 produces empty transitions
(assert (empty? ((markov/train "hello world how") :transitions))
        "three words with order-3 produces empty transitions")

# incremental training merges next words
(def inc-chain (markov/train "the cat sat by the fire"))
(markov/train "the cat sat on the mat" inc-chain)
(assert (deep= (sort ((inc-chain :transitions) "the cat sat")) @["by" "on"])
        "incremental train merges next words")

# ---------------------------------------------------------------------------
# reply
# ---------------------------------------------------------------------------

# reply uses input to seed from matching trigrams
(def result (markov/reply chain "the cat sat"))
(assert (string? result) "reply returns a string")
(assert (string/has-prefix? "the cat sat" result) "reply starts from matching trigram")

# input match is preferred over sentence starts
(def long-text
  (string "First sentence starts here. "
          "the cat sat on a big blue mat. "
          "Another sentence begins now."))
(def lchain (markov/train long-text))
(def lresult (markov/reply lchain "the cat sat"))
(assert (string/has-prefix? "the cat sat" lresult)
        "reply honours input match over sentence-start fallback")

# reply with no matching trigrams still produces output
(def fallback (markov/reply chain "xyz abc def"))
(assert (string? fallback) "reply with no match returns a string")
(assert (> (length fallback) 0) "reply with no match returns non-empty string")

# reply respects max-words
(def short (markov/reply chain "the cat sat" :max-words 4))
(assert (<= (length (string/split " " short)) 4) "max-words limits output")

# punctuation attaches correctly in output (no space before , . ; : ! ?)
(def pchain (markov/train "She smiled, and left. He waited."))
(def presult (markov/reply pchain "She smiled"))
(assert (not (string/find " ," presult)) "no space before comma in output")
(assert (not (string/find " ." presult)) "no space before period in output")

(print "All tests passed.")
