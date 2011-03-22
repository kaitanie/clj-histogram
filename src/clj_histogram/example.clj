(ns clj-histogram.example
  (:use clj-histogram.plot)
  (:use clj-histogram.histo1d)
  (:use [incanter core io stats charts pdf])
  (:import (java.io BufferedReader FileReader)))

(defn spl [s]
  (.split s " "))

(defn read-data [#^String file]
  (let [frdr (FileReader. file)
	brdr (BufferedReader. frdr)
	d (line-seq brdr)
	parse-fn (#^Double fn [#^String entry] (double (Double/parseDouble entry)))
	line->numbers (fn [#^String line] (map parse-fn (into [] (.split line " "))))]
    (map line->numbers d)))

(defn read-data-recur [file]
  (let [frdr (FileReader. file)
	brdr (BufferedReader. frdr)
	d (line-seq brdr)
	splitter-fn (fn [#^String line] (.split line " "))
	parse-fn (#^Double fn [#^String entry] (double (Double/parseDouble entry)))]
    (loop [remaining d
	   rows []]
      (let [current-row (splitter-fn (first (take 1 remaining)))
	    current-remaining (drop 1 remaining)
	    new-rows (conj rows (map parse-fn (drop 1 current-row)))]
	(cond (empty? current-remaining) new-rows
	      true (recur current-remaining new-rows))))))

(defn read-incl++-data [file]
  (let [frdr (FileReader. file)
	brdr (BufferedReader. frdr)
	d (line-seq brdr)
	splitter-fn (fn [#^String line] (.split line " "))
	parse-fn (#^Double fn [#^String entry] (double (Double/parseDouble entry)))
	is-particle-fn (#^Boolean fn [entries] (= \p (first entries)))]
    (map splitter-fn (filter is-particle-fn d))))
    (filter is-particle-fn (map splitter-fn d))))

(defn data-row-a [row]
  (nth row 0))

(defn data-row-z [row]
  (nth row 1))

(defn data-row-energy [row]
  (nth row 3))

(defn data-row-theta [row]
  (nth row 7))

(defn data-row-phi [row]
  (nth row 8))

(defn proton? [row]
  (let [z (data-row-z row)
	a (data-row-a row)]
    (and (== z 1.0) (== a 1.0))))

(defn neutron? [row]
  (let [z (data-row-z row)
	a (data-row-a row)]
    (and (== z 0.0) (== a 1.0))))

(defn deg-to-rad [x]
  (* x (/ Math/PI 180.0)))

(defn rad-to-deg [x]
  (* x (/ 180.0 Math/PI)))

(defn make-double-diff-xs-normalization-fn [reaction-xs events angle delta-angle]
  (let [angle-rad (deg-to-rad angle)
	delta-angle-rad (deg-to-rad delta-angle)
	min-angle (- angle-rad delta-angle-rad)
	max-angle (+ angle-rad delta-angle-rad)
	angle-term (- (Math/cos min-angle) (Math/cos max-angle))]
  (fn [bin value]
    (let [width (- (:xmax bin) (:xmin bin))]
      (/ (* 1.0 (/ reaction-xs (* 2.0 events Math/PI angle-term))) width)))))

;; (defn double-differential-histo-fn
;;   [reaction-xs events binning angle delta-angle particle-filter-fn data-name data]
;;   (let [normalization-fn (make-double-diff-xs-normalization-fn reaction-xs
;; 							       events
;; 							       angle delta-angle)
;; 	name (str data-name " angle " angle)
;; 	selected-data data-selector data
;; 	histo (make-histogram name binning d)]

(defn make-angle-selector [angle angle-acceptance]
  (let [angle-min (- angle angle-acceptance)
	angle-max (+ angle angle-acceptance)]
    (fn [row]
      (let [angle (data-row-theta row)]
	(and (< angle angle-max) (>= angle angle-min))))))

;; C + C @ 135 MeV/nucleon
;; ### factor  = 5.561434e-03### factora = 2.867820e-01### factorb = 9.009522e-02
;; cross(mb)= 9.009522e+02
(defn cc135 []
  (let [events 100000
	cross-section 9.009522e02 ;; From Geant4
	angle-acceptance 2.0 ;; for ddxs: angle +/- angle-acceptance
	incl-abla (read-incl++-data "/home/mael/src/thintarget/c_c_135/incl.out")
	incl-abla-neutrons (filter neutron? incl-abla)
	incl-g4deex (read-incl++-data "/home/mael/src/thintarget/c_c_135/incl-g4deex.out")
	incl-g4deex-neutrons (filter neutron? incl-g4deex)
	binning (make-linear-binning 0.0 100 600.0)
	normalization-fn (partial make-double-diff-xs-normalization-fn
				  cross-section
				  events)
	angle-15-selector (make-angle-selector 15.0 angle-acceptance)
	angle-15-normalizer-fn (normalization-fn 15.0 angle-acceptance)
	incl-neutrons-angle-10 (make-histogram "INCL (inv) 10 deg"
					       binning 
					       (pmap data-row-energy
						    (filter angle-15-selector incl-abla-neutrons)))
;;					       angle-15-normalizer-fn)
	incl-g4deex-neutrons-angle-10 (make-histogram "INCL (direct) 10 deg"
						       binning 
						       (pmap data-row-energy
							    (filter angle-15-selector incl-g4deex-neutrons)))
;;						       angle-15-normalizer-fn)
	chart (plain-plotting-style! (plot [incl-neutrons-angle-10
					    incl-g4deex-neutrons-angle-10]
					   {:legend true}))]
    (view chart)))

(defn compare-variable [style-fn selector-fn name1 data1 name2 data2]
  (let [data-1 (selector-fn data1)
	data-2 (selector-fn data2)
	xmin (min (apply min data-1) (apply min data-2))
	xmax (max (apply max data-1) (apply max data-2))
	b (make-linear-binning xmin 100 xmax)
	h1 (make-histogram name1 b data-1)
	h2 (make-histogram name2 b data-2)
	chart (style-fn (plot [h1 h2]
			       {:title "Var"
				:legend true}))]
    (view chart)
    chart))

(defn energy-plot [data model-name]
  (let [neutrons (filter neutron? data)
	n-energies (map data-row-energy neutrons)
	protons (filter proton? data)
	p-energies (map data-row-energy protons)
	b (make-linear-binning 0.0 200 1300.0)
	h1 (make-histogram (str model-name " neutron energies") b n-energies)
	h2 (make-histogram (str model-name " proton energies") b p-energies)
	chart (plain-plotting-style! (log-y-axis! (plot [h1 h2]
							{:title "p (1200 MeV) + C"
							 :xlabel "Energy (MeV)"
							 :ylabel "Counts"
							 :legend true})))]
    (view chart)
    chart))

(defn energy-comparison-plot [incl bic buggy]
  (let [b (make-linear-binning 0.0 300 1200.0)
	buggy-protons (filter proton? buggy)
	buggy-incl-p (map data-row-energy buggy-protons)
	buggy-neutrons (filter neutron? buggy)
	buggy-incl-n (map data-row-energy buggy-neutrons)
	incl-neutrons (filter neutron? incl)
	incl-protons (filter proton? incl)
	incl-n-energies (map data-row-energy incl-neutrons)
	incl-p-energies (map data-row-energy incl-protons)
	bic-neutrons (filter neutron? bic)
	bic-protons (filter proton? bic)
	bic-n-energies (map data-row-energy bic-neutrons)
	bic-p-energies (map data-row-energy bic-protons)
	incl-proton-histo (make-histogram "INCL p (inverse kin)" b incl-p-energies)
	incl-neutron-histo (make-histogram "INCL n (inverse kin)" b incl-n-energies)
	bic-proton-histo (make-histogram "BIC p " b bic-p-energies)
	bic-neutron-histo (make-histogram "BIC n" b bic-n-energies)
	buggy-incl-protons (make-histogram "INCL p (direct kin)" b buggy-incl-p)
	buggy-incl-neutrons (make-histogram "INCL n (direct kin)" b buggy-incl-n)
	chart (plain-plotting-style! (log-y-axis! (plot [incl-proton-histo
							 bic-proton-histo
							 incl-neutron-histo
							 bic-neutron-histo
							 buggy-incl-protons
							 buggy-incl-neutrons]
							{:title "C + C at 290 MeV/nucleon"
							 :xlabel "Energy (MeV)"
							 :ylabel "Counts"
							 :legend true})))]
    (view chart)
    chart))

(defn comparison-plot [data-row-selector-fn incl bic buggy]
  (let [b (make-linear-binning 0.0 300 1200.0)
	buggy-protons (filter proton? buggy)
	buggy-incl-p (map data-row-selector-fn buggy-protons)
	buggy-neutrons (filter neutron? buggy)
	buggy-incl-n (map data-row-selector-fn buggy-neutrons)
	incl-neutrons (filter neutron? incl)
	incl-protons (filter proton? incl)
	incl-n-energies (map data-row-selector-fn incl-neutrons)
	incl-p-energies (map data-row-selector-fn incl-protons)
	bic-neutrons (filter neutron? bic)
	bic-protons (filter proton? bic)
	bic-n-energies (map data-row-selector-fn bic-neutrons)
	bic-p-energies (map data-row-selector-fn bic-protons)
	incl-proton-histo (make-histogram "INCL p (inverse kin)" b incl-p-energies)
	incl-neutron-histo (make-histogram "INCL n (inverse kin)" b incl-n-energies)
	bic-proton-histo (make-histogram "BIC p " b bic-p-energies)
	bic-neutron-histo (make-histogram "BIC n" b bic-n-energies)
	buggy-incl-protons (make-histogram "INCL p (direct kin)" b buggy-incl-p)
	buggy-incl-neutrons (make-histogram "INCL n (direct kin)" b buggy-incl-n)
	chart (plain-plotting-style! (log-y-axis! (plot [incl-proton-histo
							 bic-proton-histo
							 incl-neutron-histo
							 bic-neutron-histo
							 buggy-incl-protons
							 buggy-incl-neutrons]
							{:title "C + C at 290 MeV/nucleon"
							 :xlabel "Energy (MeV)"
							 :ylabel "Counts"
							 :legend true})))]
    (view chart)
    chart))
