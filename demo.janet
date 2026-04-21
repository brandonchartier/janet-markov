(import ./markov)

(def prompts
  ["she could not"
   "it was not"
   "he had been"
   "there was no"
   "I did not"])

(defn train-file [path order]
  (def text (slurp path))
  (def t (os/clock))
  (def chain (markov/train text (markov/new-chain :order order)))
  (printf "  order-%d: %d keys in %.2fs\n" order (length (chain :transitions)) (- (os/clock) t))
  chain)

(defn section [title]
  (print)
  (print (string/repeat "─" 60))
  (print title)
  (print (string/repeat "─" 60)))

(defn demo [path]
  (def label (last (string/split "/" path)))
  (section (string label " — training"))
  (def chain2 (train-file path 2))
  (def chain3 (train-file path 3))
  (each prompt prompts
    (section (string label " — \"" prompt "\""))
    (print "  order-2: " (markov/reply chain2 prompt :max-words 80))
    (print)
    (print "  order-3: " (markov/reply chain3 prompt :max-words 80))))

(demo "/tmp/pride-and-prejudice.txt")
(demo "/tmp/moby-dick.txt")
