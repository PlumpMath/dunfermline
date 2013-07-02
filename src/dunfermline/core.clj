(ns dunfermline.core)

(defn char-range
  "Build a vec of chars as specified by bounds.
  example: (char-range \\a \\e \\A \\E)
  => [\\a \\b \\c \\d \\e \\A \\B \\C \\D \\E]"
  [& bounds]
  (let [go #(map char (range (int %1) (inc (int %2))))]
    (loop [in bounds out []]
      (if (seq in)
        (recur (drop 2 in) (into out (go (first in) (second in))))
        out))))

(def default-identifier-class
  "[A-Za-z0-9_-]"
  (into [\- \_] (char-range \a \z \A \Z \0 \9)))

(def default-whitespace-class
  "[\\space \\tab \\formfeed \\return \\newline \\backspace]"
  [\space \tab \formfeed \return \newline \backspace])

(defn char-parser
  "Create a parser for a specific char. return-val defaults to the target char."
  ([target return-val]
   (fn [input]
     (if (= (.charAt input 0) target)
       [return-val (.substring input 1)]
       [nil input])))
  ([target] (char-parser target target)))

(defn word-parser
  "Create parser for specific string. return-val defaults to the target string.
  separator? should be a function that returns true for a non word char.
  Defaults to checking for non-membership in default-identifier-class."
  ([target return-val separator?]
   (let [len (.length target)]
     (fn [input]
       (if (and (.startsWith input target) (separator? (.charAt input len)))
         [return-val (.substring input len)]
         [nil input]))))
    ([target return-val]
     (word-parser target return-val #(not (some #{%} default-identifier-class))))
    ([target] (word-parser target target)))

(defn keyword-parser
  "Create parser for keyword, less the initial ':'. return-val defaults to the
  target keyword."
  ([target return-val]
   (word-parser (.substring (str target) 1) return-val))
  ([target] (keyword-parser target target)))

(defn take-some
  "Take from coll as long as elements are members of char-class"
  [char-class coll] (take-while #(some #{%} char-class) (seq coll)))

(defn char-class-parser
  "Create parser sequence of members of char-class."
  [char-class]
  (fn [input]
     (let [c (count (take-some char-class input))]
       (if (pos? c)
         [(.substring input 0 c) (.substring input c)]
         [nil input]))))

(defn whitespace-parser
  "Create parser for whitespace. return-val defaults to :whitespace and
  whitespace-class defaults to default-whitespace-class."
  ([return-val whitespace-class]
   (fn [input]
     (let [stripped ((char-class-parser whitespace-class) input)]
       (if (first stripped)
         [return-val (second stripped)]
         [nil input]))))
  ([return-val] (whitespace-parser return-val default-whitespace-class))
  ([] (whitespace-parser :whitespace)))

(defn identifier-parser
  "Create parser for identifiers. Parsed string is passed to returner. First
  character must belong to initial-class and following characters to
  subsequent-class. return defaults to identity, initial-class to [A-Za-z], and
  subsequent-class to default-identifier-class."
  ([returner initial-class subsequent-class]
   (fn [input]
     (if (some #{(.charAt input 0)} initial-class)
       (let [c (inc (count (take-some subsequent-class (.substring input 1))))]
         [(returner (.substring input 0 c)) (.substring input c)])
       [nil input])))
  ([returner initial-class]
   (identifier-parser returner initial-class default-identifier-class))
  ([returner] (identifier-parser returner (char-range \a \z \A \Z)))
  ([] (identifier-parser identity)))

(defn integer-parser
  "Create parser for integers."
  []
  (fn [input]
    (let [digits (char-range \0 \9)
          answer ((identifier-parser identity (concat [\+ \-] digits) digits) input)]
      (if-let [value (first answer)]
        [(Integer/parseInt value) (second answer)]
        [nil input]))))

(defn check-parse
  "Apply parser to input, returning result if succesful, nil on failure."
  [parser input]
  (let [result (parser input)]
    (if (nil? (first result)) nil result)))

(defn or-parser
  "Create parser that tries parsers until successful."
  [parsers]
  (fn [input]
    (if-let [result (some #(check-parse % input) parsers)]
      result
      [nil input])))

(defn and-parser
  "Create parser that only succeeds if all parsers succeed in order."
  [parsers]
  (fn [input]
    (loop [ps parsers results [] in input]
      (if (seq ps)
        (if-let [result (check-parse (first ps) in)]
          (recur (rest ps) (conj results (first result)) (second result))
          [nil input])
        [results in]))))
