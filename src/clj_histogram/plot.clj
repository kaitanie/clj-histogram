(ns clj-histogram.plot
  (:use clj-histogram.histo1d)
  (:use [incanter core io charts stats pdf])
  (:import [org.jfree.chart.axis LogarithmicAxis]))

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

(defn plain-plotting-style!
  "Set the graphics settings to bare-bones black text/lines on
  white background style."
  [chart]
  (let [p (.getPlot chart)
	y-axis (.getRangeAxis p)
	x-axis (.getDomainAxis p)
	white java.awt.Color/WHITE
	black java.awt.Color/BLACK
	axis-settings (fn [axis] (doto axis
				   (.setAxisLineVisible true)
				   (.setAxisLinePaint black)
				   (.setLabelPaint black)
				   (.setTickMarkPaint black)
				   (.setTickLabelPaint black)))]
    (do
      (doto chart
	(.setBackgroundPaint white))
      (doto p
	(.setDomainGridlinesVisible false)
	(.setRangeGridlinesVisible false)
	(.setBackgroundPaint white))
      (axis-settings x-axis)
      (axis-settings y-axis))
    chart))
	
(defn log-y-axis!
  [chart]
  (let [plot (.getPlot chart)
	old-axis (.getRangeAxis plot)
	label (.getLabel old-axis)
	label-font (.getLabelFont old-axis)
	log-y-axis (LogarithmicAxis. label)]
;;	 (.setStrictValuesFlag false)
    (doto log-y-axis
      (.setLabelFont label-font)
      (.setAllowNegativesFlag true))
    (.setRangeAxis plot log-y-axis)
    chart))

(defn log-x-axis!
  [chart]
  (let [plot (.getPlot chart)
	old-axis (.getDomainAxis plot)
	label (.getLabel old-axis)
	label-font (.getLabelFont old-axis)
	log-x-axis (LogarithmicAxis. label)]
;;	 (.setStrictValuesFlag false)
    (doto log-x-axis
      (.setLabelFont label-font))
    (.setDomainAxis plot log-x-axis)
    chart))

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

(defn example-plot []
  (let [b (make-linear-binning -10.0 100 10.0)
	h1 (make-histogram "h1" b (rndms 100000))
	h2 (make-histogram "h2" b (rndms 100000))
	pdf-file "./example-plot.pdf"
	chart (log-y-axis! (plain-plotting-style! (plot [h1 h2] {:title "Gaussians"
						:legend true
						:ylabel "Counts"})))]
    (do
      (view chart)
      (save-pdf chart pdf-file)
      (println (str "Generated plot saved as " pdf-file)))))
