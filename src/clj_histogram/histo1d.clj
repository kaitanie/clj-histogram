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

(defn make-bin
  "Create 1-dimensional bin"
  [xmin xmax]
  (if (> xmax xmin)
    {:xmin xmin :xmax xmax}
    {:error "xmax > xmin!"}))

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

(defn histo-cumulative-integral [direction histo]
  (let [[direction-fn new-name] (cond (= direction :left->right) [identity (str (:name histo) "-cumulative-left->right")]
                                      (= direction :right->left) [reverse  (str (:name histo) "-cumulative-right->left")])
        bins                    (direction-fn (:data histo))
        sum-fn                  (fn [processed-bins new-bin]
                                  (let [latest-value   (if (empty? processed-bins)
                                                         0.0
                                                         (:content (last processed-bins)))
                                        new-content    (+ latest-value (:content new-bin))
                                        new-bin-summed (conj new-bin {:content new-content})]
                                    (conj processed-bins new-bin-summed)))
        new-bins                (direction-fn (reduce sum-fn [] bins))]
    (conj histo {:data new-bins :name new-name})))

(defn histo1d-sum
  "Sum over all bin contents in the histogram."
  [histo]
  (let [bin-contents (->> (:data histo)
                          (map :content)
                          )]
    (reduce + bin-contents)))

(defn histo1d-divide
  "Divides each histogram bin with a constant"
  [histo divisor]
  (let [bins (:data histo)
        new-bins (map (fn [bin] (let [new-content (/ (:content bin) divisor)]
                                 (conj bin {:content new-content})))
                      bins)]
    (conj histo {:data new-bins})))

(defn histo1d-add [h1 h2]
  (let [data1 (:data h1)
        data2 (:data h2)
        equal-xmins (= (map :xmin data1) (map :xmin data2))
        equal-xmaxes (= (map :xmax data1) (map :xmax data2))]
    (if (and equal-xmins equal-xmaxes)
      (let [newname (str (:name h1) "+" (:name h2))
            xmins1 (map :xmin data1)
            xmins2 (map :xmin data2)
            xmaxs1 (map :xmax data1)
            xmaxs2 (map :xmax data2)
            contents1 (map :content data1)
            contents2 (map :content data2)
            sum-contents (map (fn [[x y]] (+ x y))
                              (partition 2
                                         (interleave contents1
                                                     contents2)))]
        {:name newname
         :histo-type :histo-1d
         :data (map (fn [[xmin xmax content]]
                      {:xmin xmin :xmax xmax :content content})
                    (partition 3 (interleave xmins1 xmaxs1 sum-contents)))})
      {:error-message "Not equal binnings. Can't sum!"})))

(defn rndms [n]
  (let [g (java.util.Random.)
        gfn #(.nextGaussian g)]
    (repeatedly n gfn)))
