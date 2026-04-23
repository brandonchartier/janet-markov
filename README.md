# Janet Markov

A Markov chain text generator for Janet, backed by SQLite.

`jpm install https://github.com/brandonchartier/janet-markov`

---

```janet
(import sqlite3 :as sql)
(import markov)

(def conn (sql/open "chain.db"))
(def chain (markov/new-chain conn))

# train on text
(markov/train "the cat sat on the mat the cat sat on the rug" chain)

# train incrementally
(markov/train "the dog sat on the porch" chain)

# train on many texts at once
(markov/train-many ["one sentence." "another sentence."] chain)

# generate a reply seeded by input
(markov/reply chain "the cat")

# close when done
(sql/close conn)
```

## API

### `(new-chain conn &named order)`

Creates a chain struct backed by an open SQLite connection. `order` controls the
ngram size (default `3`). The caller is responsible for opening and closing `conn`.

### `(trained? chain)`

Returns true if the chain has any transitions. Useful for skipping an expensive
initial training pass on restart when the DB already has data.

### `(train text chain)`

Trains the chain on `text`. Can be called repeatedly to train incrementally.
Returns `chain`.

### `(train-many texts chain)`

Trains on a sequence of texts in a single transaction. Returns `chain`.

### `(reply chain input &named max-words rng)`

Generates a reply seeded by `input`. Defaults to 50 words max. Pass an `rng`
(from `math/rng`) for deterministic output.

## Notes

State is stored in the SQLite database, so it persists across restarts. On
restart, check `trained?` before calling `train` or `train-many` to avoid
re-inserting duplicate transitions.

## License

GPL-3.0
