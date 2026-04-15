(defn- words [text]
  (string/split " " (string/trim text)))

(defn- bigrams [words]
  (seq [i :range [0 (dec (length words))]]
    [(words i) (words (inc i))]))

(defn- bigram-key [pair]
  (string (pair 0) " " (pair 1)))

(defn- add-transition [chain key word]
  (unless (chain key)
    (put chain key @[]))
  (array/push (chain key) word)
  chain)

(defn- pick [rng items]
  (get items (math/rng-int rng (length items))))

(defn- walk [chain rng start max-words]
  (let [words (array ;(string/split " " start))]
    (var key start)
    (while (and (< (length words) max-words)
                (chain key))
      (let [word (pick rng (chain key))]
        (array/push words word)
        (set key (bigram-key [(get words (- (length words) 2))
                              (get words (- (length words) 1))]))))
    (string/join words " ")))

(defn- find-start [chain rng input]
  (let [input-bigrams (map bigram-key (bigrams (words input)))
        matches (filter |(chain $) input-bigrams)]
    (if (empty? matches)
      (pick rng (keys chain))
      (pick rng matches))))

(defn train [text &opt chain]
  (default chain @{})
  (let [ws (words text)]
    (each [i [a b]] (pairs (bigrams ws))
      (when-let [next-word (get ws (+ i 2))]
        (add-transition chain (bigram-key [a b]) next-word))))
  chain)

(defn reply [chain input &named max-words]
  (default max-words 50)
  (if (empty? chain)
    ""
    (let [rng (math/rng (os/time))
          start (find-start chain rng input)]
      (walk chain rng start max-words))))
