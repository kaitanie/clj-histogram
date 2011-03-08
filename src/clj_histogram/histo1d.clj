(ns clj-histogram.histo1d)

(defn bin-search [coll el]
  (let [v (vec coll)]
    (loop [li (int 0)
	   ri (int (count v))]
      (if (= (get v 0) el)
	0
	(let [p (int (/ (- ri li) 2))]
	  (if (> p 0)
	    (let [next-p (int (+ li p))
		  ce (get v next-p)]
	      (cond
	       (= ce el) next-p
	       (> ce el) (recur li next-p)
	       (< ce el) (recur next-p ri)))))))))

(defn in-bin? [bin x]
  (let [low (:xmin bin)
	high (:xmax bin)]
    (and (>= x low) (< x high))))

(defn bin-value-to-index [bin-coll x]
  (let [v (vec bin-coll)]
    (loop [lower (int 0)
	   upper (int (count v))]
      (if (in-bin? (get v 0) x)
	0
	(let [pos (int (/ (- upper lower) 2))]
	  (if (> pos 0)
	    (let [next-pos (int (+ lower pos))
		  center (get v next-pos)]
	      (cond
	       (in-bin? center x) next-pos
	       (< x (:xmin center)) (recur lower next-pos)
	       (>= x (:xmax center)) (recur next-pos upper)))))))))

(defn make-bin [xmin xmax]
  {:xmin xmin :xmax xmax})

(defn make-linear-binning [xmin nbins xmax]
  (let [width (/ (- xmax xmin) nbins)
	bin-fn (fn [n] (make-bin (+ xmin (* n width))
				 (+ xmin (* (+ n 1) width))))
	bin-ids (range 0 nbins)]
    (map bin-fn bin-ids)))

;;(defstruct histogram :name :binning :contents)

(defn make-histogram
  ([name binning x-coll]
     (make-histogram name binning x-coll (constantly 1.0)))
  ([name binning x-coll weight-fn]
     (let [nbins (count binning)
	   bin-grouper (fn [x] (bin-value-to-index binning x))
	   bin-content-groups (group-by bin-grouper x-coll)]
       (loop [ind 0
	      h []]
	 (if (< ind nbins)
	   (let [bin (nth binning ind)
		 content (get bin-content-groups ind)
		 weighted-content (map #(weight-fn bin %) content)
		 bin-content (apply + weighted-content)
		 new-bin (conj bin {:content bin-content})]
	     (recur (+ ind 1) (conj h new-bin)))
	   {:name name :histo-type :histo-1d :data h})))))

(defn rndms [n]
  (let [g (java.util.Random.)
	gfn #(.nextGaussian g)]
    (repeatedly n gfn)))