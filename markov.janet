(defn train [text &opt chain]
  (default chain @{})
  (let [words (string/split " " (string/trim text))]
    (when (>= (length words) 3)
      (for i 0 (- (length words) 2)
        (let [key (string (words i) " " (words (+ i 1)))
              next-word (get words (+ i 2))]
          (unless (nil? next-word)
            (unless (chain key)
              (put chain key @[]))
            (array/push (chain key) next-word))))))
  chain)

(defn- pick [rng items]
  (get items (math/rng-int rng (length items))))

(defn reply [chain input &named max-words]
  (default max-words 50)
  (if (empty? chain)
    ""
    (let [rng (math/rng (os/time))
          input-words (string/split " " (string/trim input))
          bigrams (seq [i :range [0 (- (length input-words) 1)]]
                    (string (input-words i) " " (input-words (+ i 1))))
          matches (filter |(chain $) bigrams)
          start (if (empty? matches)
                  (pick rng (keys chain))
                  (pick rng matches))
          words (string/split " " start)]
      (var key start)
      (while (and (< (length words) max-words)
                  (chain key))
        (let [next-word (pick rng (chain key))]
          (array/push words next-word)
          (set key (string (get words (- (length words) 2))
                           " "
                           (get words (- (length words) 1))))))
      (string/join words " "))))
