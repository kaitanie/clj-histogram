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
      (/ (* value (/ reaction-xs (* 2.0 events Math/PI angle-term))) width)))))

(defn double-differential-histo-fn
  [reaction-xs events binning angle delta-angle particle-filter-fn data-name data]
  (let [normalization-fn (make-double-diff-xs-normalization-fn reaction-xs
							       events
							       angle delta-angle)
	name (str data-name " angle " angle)
	selected-data data-selector data
	histo (make-histogram name binning d)]

(defn energy-plot [data]
  (let [neutrons (filter neutron? data)
	n-energies (map data-row-energy neutrons)
	protons (filter proton? data)
	p-energies (map data-row-energy protons)
	b (make-linear-binning 0.0 300 2000.0)
	h1 (make-histogram "neutron energies" b n-energies)
	h2 (make-histogram "proton energies" b p-energies)]
    (view (plain-plotting-style! (log-y-axis! (plot [h1 h2]
						    {:title "Energy spectra"
						     :xlabel "Energy (MeV)"
						     :ylabel "Counts"
						     :legend true}))))))

(defn energy-comparison-plot [incl bic buggy]
  (let [b (make-linear-binning 0.0 300 3000.0)
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
	incl-proton-histo (make-histogram "INCL proton energies" b incl-p-energies)
	incl-neutron-histo (make-histogram "INCL neutron energies" b incl-n-energies)
	bic-proton-histo (make-histogram "BIC proton energies" b bic-p-energies)
	bic-neutron-histo (make-histogram "BIC neutron energies" b bic-n-energies)
	buggy-incl-protons (make-histogram "Buggy INCL protons" b buggy-incl-p)
	buggy-incl-neutrons (make-histogram "Buggy INCL neutrons" b buggy-incl-n)]
    (view (plain-plotting-style! (log-y-axis! (plot [incl-proton-histo
						     bic-proton-histo
						     incl-neutron-histo
						     bic-neutron-histo
						     buggy-incl-protons
						     buggy-incl-neutrons]
						    {:title "C + C at 290 MeV/nucleon"
						     :xlabel "Energy (MeV)"
						     :ylabel "Counts"
						     :legend true}))))))