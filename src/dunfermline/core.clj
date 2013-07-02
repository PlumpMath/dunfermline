(ns dunfermline.core)

(defn char-range
  [& bounds]
  (let [go #(map char (range (int %1) (inc (int %2))))]
    (loop [in bounds out '()]
      (if (empty? in)
        (vec out)
        (recur (drop 2 in) (concat (go (first in) (second in)) out))))))

(def default-identifier-class (concat '[\- \_] (char-range \a \z \A \Z \0 \9)))
(def default-whitespace-class
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
     (word-parser target return-val #(not (some #{%} default-identifier-class))))
    ([target] (word-parser target target)))

(defn keyword-parser
  ([target return-val]
   (word-parser (.substring (str target) 1) return-val))
  ([target] (keyword-parser target target)))

(defn- take-some [char-class coll]
  (take-while #(some #{%} char-class) (seq coll)))

(defn char-class-parser
  [char-class]
  (fn [input]
     (let [c (count (take-some char-class input))]
       (if (pos? c)
         [(.substring input 0 c) (.substring input c)]
         [nil input]))))

(defn whitespace-parser
  ([return-val whitespace-class]
   (fn [input]
     (let [stripped ((char-class-parser whitespace-class) input)]
       (if (first stripped)
         [return-val (second stripped)]
         [nil input]))))
  ([return-val] (whitespace-parser return-val default-whitespace-class))
  ([] (whitespace-parser :whitespace)))

(defn identifier-parser
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

(defn integer-parser []
  (fn [input]
    (let [digits (char-range \0 \9)
          answer ((identifier-parser identity (concat [\+ \-] digits) digits) input)]
      (if-let [value (first answer)]
        [(. Integer (parseInt value)) (second answer)]
        [nil input]))))

;;TODO or-parse = (some #(not (nil? (apply first incoming))))
