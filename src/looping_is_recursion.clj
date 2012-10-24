(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [a n]
                 (if (empty? a)
                   n
                   (recur (rest a) (first a))))]
    (helper a-seq nil)))
        

(defn seq= [seq1 seq2]
  (if (= (count seq1) (count seq2))
    (loop [s1 seq1
           s2 seq2
           t true]
      (if (or (empty? s1) (not t))
        t
        (recur (rest s1) (rest s2) (= (first s1) (first s2)))))
    false))

(defn find-first-index [pred a-seq]
  (let [c (count (take-while
                  (complement pred)
                  a-seq))]
    (if (= c (count a-seq))
      nil
      c)))

(defn avg [a-seq]
  "(if (empty? a-seq) nil (/ (sum a-seq) (count a-seq)))"
  (loop [s1 a-seq
         nominator 0
         denominator 0]
    (if (empty? s1)
      (if (zero? denominator)
        nil
        (/ nominator denominator))
      (recur (rest s1) (+ (first s1) nominator) (inc denominator)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [s #{}
           a a-seq]
      (if (empty? a)
        s
        (recur (toggle s (first a)) (rest a))))))

(defn fast-fibo [n]
  (loop [a 0
         b 1
         k n]
    (if (zero? k)
      a
      (recur b (+ a b) (dec k)))))

(defn cut-at-repetition [a-seq]
  (let [s a-seq]
    (loop [s1 (rest s)
           s2 [(first s)]
           m  #{}]
      (if (empty? s1)
        (reverse s2)
        (if (contains? m (first s1))
          (reverse (rest s2))
          (recur (rest s1) (cons (first s1) s2) (conj m (first s1))))))))

