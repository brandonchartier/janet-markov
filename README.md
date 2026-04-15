# Janet Markov

A Markov chain text generator for Janet.

`jpm install https://github.com/brandonchartier/janet-markov`

---

```janet
(import markov)

# train on text
(def chain (markov/train "the cat sat on the mat the cat sat on the rug"))

# train incrementally
(markov/train "the dog sat on the porch" chain)

# generate a reply seeded by input
(markov/reply chain "the cat")
```

## License

GPL-3.0
