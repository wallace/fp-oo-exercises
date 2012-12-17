(def make
     (fn [type & args]
       (apply type args)))

(def send-to
     (fn [object message & args]
       (apply (message (:__methods__ object)) object args)))

(def Point
     (fn [x y]
       {:x x,
        :y y
        :__class_symbol__ 'Point
        :__methods__ {
           :x :x
           :y :y
           :class :__class_symbol__
           :shift (fn [this xinc yinc]
                    (make Point (+ (send-to this :x) xinc)
                                (+ (send-to this :y) yinc)))
           :add (fn [this other]
                  (send-to this :shift (send-to other :x)
                                       (send-to other :y)))}}))
                      

(def my-point (make Point 1 2))
(prn (send-to my-point :x))
(prn (send-to my-point :y))
(prn (send-to my-point :shift -1 -100))
(prn (send-to my-point :add (make Point -1 -100)))

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
	step
	(fn step [v-seqs]
	  (let [increment
		(fn [v-seqs]
		  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
		    (if (= i -1) nil
			(if-let [rst (next (v-seqs i))]
			  (assoc v-seqs i rst)
			  (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
	    (when v-seqs
	       (cons (map first v-seqs)
		     (lazy-seq (step (increment v-seqs)))))))]
    (when (every? first seqs)
      (lazy-seq (step v-original-seqs)))))

(cartesian-product (range -1 2) (range -1 2))

(def neighborhood
  (fn [coll]
    (list ((first coll) (last coll)))
  )
)

(def jiterate 
  (fn [coll]
    
  )
)
