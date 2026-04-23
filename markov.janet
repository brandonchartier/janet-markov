(import sqlite3 :as sql)

(def- token-peg
  ~{:break (+ :s (set ".!?,;:"))
    :word-char (if-not :break 1)
    :word (some :word-char)
    :punct (set ".!?,;:")
    :url (sequence (choice "https://" "http://") (some (if-not :s 1)))
    :main (any (+ :s (capture :url) (capture :word) (capture :punct)))})

(defn- tokenize [text]
  (or (peg/match token-peg text) @[]))

(defn- sentence-end? [tok]
  (or (= tok ".") (= tok "!") (= tok "?")))

(defn- ngrams [ws n]
  (seq [i :range [0 (- (length ws) (dec n))]]
    (array/slice ws i (+ i n))))

(defn- ngram-key [gram]
  (string/join gram " "))

(def- no-space-before (peg/compile ~(set ",;:.!?")))

(defn- join-tokens [tokens]
  (reduce (fn [acc tok]
            (if (or (empty? acc) (peg/match no-space-before tok))
              (string acc tok)
              (string acc " " tok)))
          ""
          tokens))

(defn- pick [rng items]
  (get items (math/rng-int rng (length items))))

(defn- init-db [conn]
  (sql/eval conn "CREATE TABLE IF NOT EXISTS markov_transitions (gram TEXT NOT NULL, next TEXT NOT NULL)")
  (sql/eval conn "CREATE INDEX IF NOT EXISTS idx_markov_gram ON markov_transitions(gram)")
  (sql/eval conn "CREATE TABLE IF NOT EXISTS markov_starts (gram TEXT NOT NULL)"))

(defn- transitions-exist? [conn gram]
  (not (empty? (sql/eval conn
                         "SELECT 1 FROM markov_transitions WHERE gram = :g LIMIT 1"
                         {:g gram}))))

(defn- sentence-start? [conn gram]
  (not (empty? (sql/eval conn
                         "SELECT 1 FROM markov_starts WHERE gram = :g LIMIT 1"
                         {:g gram}))))

(defn- random-next [conn gram]
  (let [r (sql/eval conn
                    "SELECT next FROM markov_transitions WHERE gram = :g ORDER BY RANDOM() LIMIT 1"
                    {:g gram})]
    (when (not (empty? r)) ((r 0) :next))))

(defn- random-sentence-start [conn]
  (let [r (sql/eval conn
                    ``SELECT ms.gram FROM markov_starts ms
               WHERE EXISTS (SELECT 1 FROM markov_transitions WHERE gram = ms.gram)
               ORDER BY RANDOM() LIMIT 1``)]
    (when (not (empty? r)) ((r 0) :gram))))

(defn- random-gram [conn]
  (let [r (sql/eval conn "SELECT gram FROM markov_transitions ORDER BY RANDOM() LIMIT 1")]
    (when (not (empty? r)) ((r 0) :gram))))

(defn- insert-transition [conn gram next-tok start?]
  (sql/eval conn "INSERT INTO markov_transitions VALUES (:gram, :next)"
            {:gram gram :next next-tok})
  (when start?
    (sql/eval conn "INSERT INTO markov_starts VALUES (:gram)" {:gram gram})))

(defn- train-text [conn n text]
  (let [ws (tokenize text)]
    (each [i gram] (pairs (ngrams ws n))
      (when-let [next-tok (get ws (+ i n))]
        (insert-transition conn (ngram-key gram) next-tok
                           (or (zero? i) (sentence-end? (get ws (dec i)))))))))

(defn- best-start [conn order rng input]
  (let [input-ngrams (map ngram-key (ngrams (tokenize input) order))
        with-trans (filter (fn [g] (transitions-exist? conn g)) input-ngrams)
        at-sentence (filter (fn [g] (sentence-start? conn g)) with-trans)]
    (cond
      (not (empty? at-sentence)) (pick rng at-sentence)
      (not (empty? with-trans)) (pick rng with-trans)
      (random-sentence-start conn)
      (random-gram conn))))

(defn- generate-step [conn order stop-after max-words words]
  (let [key (string/join (array/slice words (- (length words) order)) " ")
        tok (random-next conn key)
        words+ (if tok (array ;words tok) words)]
    (cond
      (>= (length words) max-words) words
      (nil? tok) words
      (and (> (length words+) stop-after) (sentence-end? tok)) words+
      (generate-step conn order stop-after max-words words+))))

(defn- generate [conn order start max-words]
  (join-tokens
    (generate-step conn order
                   (max 10 (math/floor (* max-words 0.4)))
                   max-words
                   (array ;(string/split " " start)))))

(defn new-chain [conn &named order]
  (default order 3)
  (init-db conn)
  {:order order :conn conn})

(defn trained? [chain]
  (not (empty? (sql/eval (chain :conn) "SELECT 1 FROM markov_transitions LIMIT 1"))))

(defn train [text chain]
  (let [conn (chain :conn)]
    (sql/eval conn "BEGIN")
    (train-text conn (chain :order) text)
    (sql/eval conn "COMMIT"))
  chain)

(defn train-many [texts chain]
  (let [conn (chain :conn)]
    (sql/eval conn "BEGIN")
    (each text texts (train-text conn (chain :order) text))
    (sql/eval conn "COMMIT"))
  chain)

(defn reply [chain input &named max-words rng]
  (default max-words 50)
  (default rng (math/rng (os/time)))
  (if-let [start (best-start (chain :conn) (chain :order) rng input)]
    (generate (chain :conn) (chain :order) start max-words)
    ""))
