(import ../markov)
(import sqlite3 :as sql)

(defn- fresh [&named order]
  (let [conn (sql/open ":memory:")]
    (if order
      (markov/new-chain conn :order order)
      (markov/new-chain conn))))

(defn- query-grams [conn]
  (seq [row :in (sql/eval conn "SELECT DISTINCT gram FROM markov_transitions")]
    (row :gram)))

(defn- query-nexts [conn gram]
  (seq [row :in (sql/eval conn "SELECT next FROM markov_transitions WHERE gram = :g" {:g gram})]
    (row :next)))

(defn- query-starts [conn]
  (seq [row :in (sql/eval conn "SELECT DISTINCT gram FROM markov_starts")]
    (row :gram)))

# ---------------------------------------------------------------------------
# tokenization
# ---------------------------------------------------------------------------

# punctuation splits into its own token
(let [chain (fresh :order 1)]
  (markov/train "Hello, world." chain)
  (assert (some (fn [x] (= "Hello" x)) (query-grams (chain :conn))) "Hello is a gram")
  (assert (some (fn [x] (= "," x)) (query-grams (chain :conn))) "comma is a gram")
  (assert (some (fn [x] (= "world" x)) (query-grams (chain :conn))) "world is a gram"))

# contractions stay intact
(let [chain (fresh :order 1)]
  (markov/train "don't stop, can't stop." chain)
  (assert (some (fn [x] (= "don't" x)) (query-grams (chain :conn))) "don't is one token")
  (assert (some (fn [x] (= "can't" x)) (query-grams (chain :conn))) "can't is one token"))

# hyphenated words stay intact
(let [chain (fresh :order 1)]
  (markov/train "self-aware systems work." chain)
  (assert (some (fn [x] (= "self-aware" x)) (query-grams (chain :conn))) "self-aware is one token"))

# URLs are kept as atomic tokens, not split at punctuation
(let [chain (fresh :order 1)]
  (markov/train "check out https://example.com/path?q=1 cool" chain)
  (assert (some (fn [x] (= "https://example.com/path?q=1" x)) (query-grams (chain :conn))) "URL is a single gram")
  (assert (not (some (fn [x] (= "example" x)) (query-grams (chain :conn)))) "URL is not split into fragments")
  (assert (some (fn [x] (= "check" x)) (query-grams (chain :conn))) "words around a URL are still tokenized"))
(let [chain (fresh :order 1)]
  (markov/train "visit http://example.com or https://foo.bar/baz today" chain)
  (assert (some (fn [x] (= "http://example.com" x)) (query-grams (chain :conn))) "http URL is atomic")
  (assert (some (fn [x] (= "https://foo.bar/baz" x)) (query-grams (chain :conn))) "https URL is atomic"))

# UTF-8 characters pass through untouched.
# Validated against a real Gutenberg text that contains curly quotes and em-dashes.
(when (os/stat "/tmp/moby-dick.txt")
  (let [chain (fresh :order 1)
        curly-apos (string/from-bytes 0xe2 0x80 0x99)
        queequeg-key (string "Queequeg" curly-apos "s")]
    (markov/train (slurp "/tmp/moby-dick.txt") chain)
    (assert (some (fn [x] (= queequeg-key x)) (query-grams (chain :conn)))
            "curly apostrophe stays intact: possessive is one token")
    (assert (not (some (fn [x] (= (string/from-bytes 0xe2) x)) (query-grams (chain :conn))))
            "lone UTF-8 lead byte is not a chain key")))

# ---------------------------------------------------------------------------
# sentence boundary tracking
# ---------------------------------------------------------------------------

(let [chain (fresh)]
  (markov/train "The cat sat. A dog ran." chain)
  (assert (some (fn [x] (string/has-prefix? "The cat" x)) (query-starts (chain :conn)))
          "first sentence start is tracked")
  (assert (some (fn [x] (string/has-prefix? "A dog" x)) (query-starts (chain :conn)))
          "post-period sentence start is tracked"))

# ---------------------------------------------------------------------------
# chain structure
# ---------------------------------------------------------------------------

(def text "the cat sat on the mat the cat sat")

(let [chain (fresh)]
  (markov/train text chain)
  (assert (= (chain :order) 3) "default order is 3")
  (assert (deep= (sort (array ;(query-grams (chain :conn))))
                 (sort @["the cat sat" "cat sat on" "sat on the" "on the mat" "the mat the" "mat the cat"]))
          "chain keys are trigrams")
  (assert (deep= (sort (array ;(query-nexts (chain :conn) "the cat sat"))) @["on"])  "the cat sat -> on")
  (assert (deep= (sort (array ;(query-nexts (chain :conn) "cat sat on")))  @["the"]) "cat sat on -> the")
  (assert (deep= (sort (array ;(query-nexts (chain :conn) "sat on the")))  @["mat"]) "sat on the -> mat")
  (assert (deep= (sort (array ;(query-nexts (chain :conn) "on the mat")))  @["the"]) "on the mat -> the")
  (assert (deep= (sort (array ;(query-nexts (chain :conn) "the mat the"))) @["cat"]) "the mat the -> cat")
  (assert (deep= (sort (array ;(query-nexts (chain :conn) "mat the cat"))) @["sat"]) "mat the cat -> sat"))

# explicit order 2
(let [chain (fresh :order 2)]
  (markov/train text chain)
  (assert (= (chain :order) 2) "explicit order 2")
  (assert (deep= (sort (array ;(query-nexts (chain :conn) "the cat"))) @["sat" "sat"])
          "order-2: the cat -> sat twice"))

# fewer than 4 words with order-3 produces no transitions
(let [chain (fresh)]
  (markov/train "hello world how" chain)
  (assert (empty? (query-grams (chain :conn))) "three words with order-3 produces no transitions"))

# incremental training merges next words
(let [chain (fresh)]
  (markov/train "the cat sat by the fire" chain)
  (markov/train "the cat sat on the mat" chain)
  (assert (deep= (sort (array ;(query-nexts (chain :conn) "the cat sat"))) @["by" "on"])
          "incremental train merges next words"))

# train-many inserts the same transitions as sequential trains
(let [chain (fresh)]
  (markov/train-many ["the cat sat on the mat" "the cat sat by the fire"] chain)
  (assert (deep= (sort (array ;(query-nexts (chain :conn) "the cat sat"))) @["by" "on"])
          "train-many inserts all transitions"))

# ---------------------------------------------------------------------------
# reply
# ---------------------------------------------------------------------------

# untrained chain returns empty string
(let [chain (fresh)]
  (assert (= "" (markov/reply chain "the cat sat")) "untrained chain returns empty string"))

# reply uses input to seed from matching trigrams
(let [chain (fresh)]
  (markov/train text chain)
  (let [result (markov/reply chain "the cat sat")]
    (assert (string? result) "reply returns a string")
    (assert (string/has-prefix? "the cat sat" result) "reply starts from matching trigram")))

# input match is preferred over sentence starts
(let [chain (fresh)]
  (markov/train (string "First sentence starts here. "
                        "the cat sat on a big blue mat. "
                        "Another sentence begins now.") chain)
  (assert (string/has-prefix? "the cat sat" (markov/reply chain "the cat sat"))
          "reply honours input match over sentence-start fallback"))

# reply with no matching trigrams still produces output
(let [chain (fresh)]
  (markov/train text chain)
  (let [fallback (markov/reply chain "xyz abc def")]
    (assert (string? fallback) "reply with no match returns a string")
    (assert (> (length fallback) 0) "reply with no match returns non-empty string")))

# reply respects max-words
(let [chain (fresh)]
  (markov/train text chain)
  (let [short (markov/reply chain "the cat sat" :max-words 4)]
    (assert (<= (length (string/split " " short)) 4) "max-words limits output")))

# punctuation attaches correctly in output (no space before , . ; : ! ?)
(let [chain (fresh :order 1)]
  (markov/train "She smiled, and left. He waited." chain)
  (let [result (markov/reply chain "She smiled")]
    (assert (not (string/find " ," result)) "no space before comma in output")
    (assert (not (string/find " ." result)) "no space before period in output")))

(print "All tests passed.")
