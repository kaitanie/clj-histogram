(ns clj-histogram.histo1d-test
  (:use [clj-histogram.histo1d])
  (:use [clojure.test])
  (:use [midje.sweet]))

(def binning1 '({:xmin 0.0, :xmax 0.1} {:xmin 0.1, :xmax 0.2} {:xmin 0.2, :xmax 0.30000000000000004} {:xmin 0.30000000000000004, :xmax 0.4} {:xmin 0.4, :xmax 0.5} {:xmin 0.5, :xmax 0.6000000000000001} {:xmin 0.6000000000000001, :xmax 0.7000000000000001} {:xmin 0.7000000000000001, :xmax 0.8} {:xmin 0.8, :xmax 0.9} {:xmin 0.9, :xmax 1.0}))

(fact "Create one bin with xmin = 0.0 and xmax = 1.0" (make-bin 0.0 1.0) => {:xmin 0.0 :xmax 1.0})
(fact "Error when trying to create one bin with xmin = 1.0 and xmax = 0.0" (make-bin 1.0 0.0) => {:error "xmax > xmin!"})

(fact "Linear binning from 0.0 to 1.0 with 10 bins" (make-linear-binning 0.0 10 1.0) => binning1)
