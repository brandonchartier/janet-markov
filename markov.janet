(def- token-peg
  ~{:break (+ :s (set ".!?,;:"))
    :word-char (if-not :break 1)
    :word (some :word-char)
    :punct (set ".!?,;:")
    :main (any (+ :s (capture :word) (capture :punct)))})

(defn- tokenize [text]
  (or (peg/match token-peg text) @[]))

(defn- sentence-end? [tok]
  (or (= tok ".") (= tok "!") (= tok "?")))

(defn- ngrams [ws n]
  (seq [i :range [0 (- (length ws) (dec n))]]
    (array/slice ws i (+ i n))))

(defn- ngram-key [gram]
  (string/join gram " "))

(defn- add-transition [transitions key word]
  (unless (transitions key)
    (put transitions key @[]))
  (array/push (transitions key) word))

(defn- pick [rng items]
  (get items (math/rng-int rng (length items))))

(def- no-space-before (peg/compile ~(set ",;:.!?")))

(defn- join-tokens [tokens]
  (def buf @"")
  (each tok tokens
    (unless (or (empty? buf) (peg/match no-space-before tok))
      (buffer/push buf " "))
    (buffer/push buf tok))
  (string buf))

(defn- walk [transitions starts order rng start max-words]
  (let [ws (array ;(string/split " " start))
        stop-after (max 10 (math/floor (* max-words 0.4)))]
    (var key start)
    (while (and (< (length ws) max-words) (transitions key))
      (let [tok (pick rng (transitions key))]
        (array/push ws tok)
        (set key (string/join (array/slice ws (- (length ws) order)) " "))
        (when (and (> (length ws) stop-after) (sentence-end? tok))
          (break))))
    (join-tokens ws)))

(defn- find-start [transitions starts order rng input]
  (let [ws (tokenize input)
        input-ngrams (map ngram-key (ngrams ws order))
        matches (filter |(transitions $) input-ngrams)
        starts-set (tabseq [s :in starts] s true)
        sentence-starts (distinct (filter |(transitions $) starts))
        best (filter |(starts-set $) matches)]
    (cond
      (not (empty? best)) (pick rng best)
      (not (empty? matches)) (pick rng matches)
      (not (empty? sentence-starts)) (pick rng sentence-starts)
      (pick rng (keys transitions)))))

(defn new-chain [&named order]
  (default order 3)
  @{:transitions @{} :order order :starts @[]})

(defn train [text &opt chain]
  (default chain (new-chain))
  (let [{:transitions transitions :order n :starts starts} chain
        ws (tokenize text)]
    (each [i gram] (pairs (ngrams ws n))
      (when-let [next-word (get ws (+ i n))]
        (add-transition transitions (ngram-key gram) next-word)
        (when (or (zero? i) (sentence-end? (get ws (dec i))))
          (array/push starts (ngram-key gram))))))
  chain)

(defn reply [chain input &named max-words rng]
  (default max-words 50)
  (default rng (math/rng (os/time)))
  (let [{:transitions transitions :order order :starts starts} chain]
    (if (empty? transitions)
      ""
      (let [start (find-start transitions starts order rng input)]
        (walk transitions starts order rng start max-words)))))
