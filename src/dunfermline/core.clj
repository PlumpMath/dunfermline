(ns dunfermline.core)

(defn char-range
  [& bounds]
  (let [go #(map char (range (int %1) (inc (int %2))))]
    (loop [in bounds out '()]
      (if (empty? in)
        (vec out)
        (recur (drop 2 in) (concat (go (first in) (second in)) out))))))

(def default-identifier-chars (concat '[\- \_] (char-range \a \z \A \Z \0 \9)))
(def default-whitespace-chars
  [\space \tab \formfeed \return \newline \backspace])

(defn char-parser
  ([target return-val]
   (fn [input]
     (if (= (.charAt input 0) target)
       [return-val (.substring input 1)]
       [nil input])))
  ([target] (char-parser target target)))

(defn word-parser
  ([target return-val separator?]
   (let [len (.length target)]
     (fn [input]
       (if (and (.startsWith input target) (separator? (.charAt input len)))
         [return-val (.substring input len)]
         [nil input]))))
    ([target return-val]
     (word-parser target return-val #(not (some #{%} default-identifier-chars))))
    ([target] (word-parser target target)))

(defn keyword-parser
  ([target return-val]
   (word-parser (.substring (str target) 1) return-val))
  ([target] (keyword-parser target target)))

(defn- take-some [char-class coll]
  (take-while #(some #{%} char-class) (seq coll)))

(defn whitespace-parser
  ([return-val whitespace-seq]
   (fn [input]
     (let [c (count (take-some input))]
       (if (pos? c)
         [return-val (.substring input c)]
         [nil input]))))
  ([return-val] (whitespace-parser return-val default-whitespace-chars))
  ([] (whitespace-parser :whitespace)))

(defn identifier-parser
  ([returner initial-seq subsequent-seq]
   (fn [input]
     (if-let [the-first (some #{(.charAt input 0)} initial-seq)]
       (let [the-rest (take-some subsequent-seq input)]
         [(returner (apply str (cons the-first the-rest)))
          (.substring input (inc (count the-rest)))])
       [nil input])))
  ([returner initial-seq]
   (identifier-parser returner initial-seq default-identifier-chars))
  ([returner]
   (identifier-parser returner (char-range \a \z \A \Z)))
  ([] (identifier-parser identity)))

