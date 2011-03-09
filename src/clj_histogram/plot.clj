(ns clj-histogram.plot
  (use clj-histogram.histo1d)
  (use [incanter core io charts stats]))

(defmulti to-xy-step-points (fn [h] (:histo-type h)))

(defmethod to-xy-step-points nil [h]
  (println (str "No to-xy-step-points implementation for this datatype."))
  nil)

(defmethod to-xy-step-points :histo-1d [h]
  (let [name (:name h)
	data (:data h)
	xmaxs (map :xmax data)
	xmins (map :xmin data)
	ys (map :content data)
	xpoints (interleave xmins xmaxs)
	ypoints (interleave ys ys)
	points {:x xpoints :y ypoints :name name}]
    points))

(defn plot-histo [h]
  (let [p (to-xy-step-points h)
	x (:x p)
	y (:y p)]
    (view (xy-plot x y))))

(defn plot [histo-coll opt-map]
  (let [title (or (:title opt-map) (:name (first histo-coll)))
	xlabel (or (:xlabel opt-map) "x")
	ylabel (or (:ylabel opt-map) "y")
	legend (or (:legend opt-map) false)
	xys (map to-xy-step-points histo-coll)
	first-xys (first xys)
	first-x (:x first-xys)
	first-y (:y first-xys)
	first-name (:name first-xys)
	remaining-xys (rest xys)
	plot (xy-plot first-x first-y
		      :title title
		      :legend legend
		      :series-label first-name
		      :x-label xlabel
		      :y-label ylabel)]
    (doseq [p remaining-xys]
      (let [x (:x p)
	    y (:y p)
	    name (:name p)]
	(add-lines plot x y
		   :series-label name)))
    plot))
	