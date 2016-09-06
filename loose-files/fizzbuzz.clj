;; If you're so inclined, running this file as a script with
;; Leiningen should be pretty easy. Install the lein-exec plugin,
;; then just run cat fizzbuzz.clj | lein exec

(defn fizz-buzz
  "Fizz buzz in idiomatic Clojure"
  [n]
  (->> (range 1 n)
       (map #(cond (= 0 (rem % 15)) "FizzBuzz"
                   (= 0 (rem % 5))  "Buzz"
                   (= 0 (rem % 3))  "Fizz"
                   :otherwise %))))

(defn horrible-fizz-buzz
  "Fizz buzz in unidiomatic Clojure."
  [n]
  (loop [i 1]
    (if (<= i n)
      (do
        (if (and (= (rem i 3) 0) (= (rem i 5) 0))
          (println "FizzBuzz")
          (if (= (rem i 3) 0)
            (println "Fizz")
            (if (= (rem i 5) 0)
              (println "Buzz")
              (println i))))
        (recur (inc i))))))

(defn -main
  [n & _]
  (dorun (map println (fizz-buzz n))))

(-main 100)
