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
;;       (.setStrictValuesFlag false)
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
;;       (.setStrictValuesFlag false)
    (doto log-x-axis
      (.setLabelFont label-font))
    (.setDomainAxis plot log-x-axis)
    chart))

(defn plot [histo-coll opt-map]
  (let [title (or (:title opt-map) (:name (first histo-coll)))
        xlabel (or (:xlabel opt-map) "X axis")
        ylabel (or (:ylabel opt-map) "Y axis")
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
        chart (-> (plot [h1 h2] {:title "Gaussians"
                                 :legend true
                                 :ylabel "Counts"})
                  plain-plotting-style!
                  )]
    (do
      (view chart)
      (save-pdf chart pdf-file)
      (println (str "Generated plot saved as " pdf-file)))))

(defn example-cumulative-integral []
  (let [b (make-linear-binning -5.0 100 5.0)
        h1 (make-histogram "Gaussian" b (rndms 10000))
        h2 (histo-cumulative-integral :right->left h1)
        h3 (histo-cumulative-integral :left->right h1)]
    (view (-> (plot [h1 h2 h3] {:title "Gaussian integrated from left to right and right to left"
                                :legend true})
              log-y-axis!
              plain-plotting-style!))))

(defn data-lines->seq [file]
  (let [d (slurp file)
        dd  (.split d "\n")
        n (map #(Double/parseDouble %) dd)]
    n))

(defn ex1 []
  (let [n (data-lines->seq "../neutrons/NeutronFramework/he3tube/data_He3Tube_mini_cylinder_source.dat")
        binning (make-linear-binning 0.0 500 30000.0)
        h1 (make-histogram "h2" binning n)
        p (-> (plot [h1] {:title "Energy deposits in Helium" :xlabel "Energy (eV)" :ylabel "Counts"})
              log-y-axis!
              plain-plotting-style!)]
    (do
      (view p)
      (save p "he3_mini_cylinder.png" :width 900 :height 700))))

(defn ex2 []
  (let [d1 (data-lines->seq "../neutrons/NeutronFramework/he3tube/data_He3Tube_mini_cylinder_source.dat")
        d2 (data-lines->seq "../neutrons/NeutronFramework/he3tube/data_He3Tube_cylinder_source.dat")
        binning (make-linear-binning 0.0 500 30000.0)
        h1 (make-histogram "Mini cylinder" binning d1)
        h2 (make-histogram "Cylinder" binning d2)
        p (-> (plot [h1 h2] {:title "Neutron" :xlabel "Energy" :ylabel "Counts" :legend true})
              log-y-axis!
              plain-plotting-style!)]
    (do
;;      (set-x-label p "Energy")
;;      (set-y-label p "Counts")
      (view p)
      (save p "hcomp.png" :width 900 :height 700))))
