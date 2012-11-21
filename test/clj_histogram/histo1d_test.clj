(ns clj-histogram.histo1d-test
  (:use [clj-histogram.histo1d])
  (:use [clojure.test])
  (:use [midje.sweet]))

(def binning1 '({:xmin 0.0, :xmax 0.1} {:xmin 0.1, :xmax 0.2} {:xmin 0.2, :xmax 0.30000000000000004} {:xmin 0.30000000000000004, :xmax 0.4} {:xmin 0.4, :xmax 0.5} {:xmin 0.5, :xmax 0.6000000000000001} {:xmin 0.6000000000000001, :xmax 0.7000000000000001} {:xmin 0.7000000000000001, :xmax 0.8} {:xmin 0.8, :xmax 0.9} {:xmin 0.9, :xmax 1.0}))
(def test-histo-1 {:name "test histo 1", :histo-type :histo-1d, :data [{:content 0, :xmin 0.0, :xmax 0.1} {:content 2.0, :xmin 0.1, :xmax 0.2} {:content 0, :xmin 0.2, :xmax 0.30000000000000004} {:content 0, :xmin 0.30000000000000004, :xmax 0.4} {:content 0, :xmin 0.4, :xmax 0.5} {:content 1.0, :xmin 0.5, :xmax 0.6000000000000001} {:content 0, :xmin 0.6000000000000001, :xmax 0.7000000000000001} {:content 0, :xmin 0.7000000000000001, :xmax 0.8} {:content 0, :xmin 0.8, :xmax 0.9} {:content 1.0, :xmin 0.9, :xmax 1.0}]})
(def test-histo-2 {:name "test histo 2", :histo-type :histo-1d, :data [{:content 0, :xmin 0.0, :xmax 0.1} {:content 1.0, :xmin 0.1, :xmax 0.2} {:content 0, :xmin 0.2, :xmax 0.30000000000000004} {:content 0, :xmin 0.30000000000000004, :xmax 0.4} {:content 0, :xmin 0.4, :xmax 0.5} {:content 0.5, :xmin 0.5, :xmax 0.6000000000000001} {:content 0, :xmin 0.6000000000000001, :xmax 0.7000000000000001} {:content 0, :xmin 0.7000000000000001, :xmax 0.8} {:content 0, :xmin 0.8, :xmax 0.9} {:content 0.5, :xmin 0.9, :xmax 1.0}]})

(def small-binning1 (make-linear-binning 0.0 10 1.0))
(def small-dataset1 [0.1 0.3 0.5 0.75 0.1])
(def simple-h1 (make-histogram "simple-h1" small-binning1 small-dataset1))
(def normalization-factor (double (count small-dataset1))) ;; All weights are 1.0

(fact "Create one bin with xmin = 0.0 and xmax = 1.0" (make-bin 0.0 1.0) => {:xmin 0.0 :xmax 1.0})
(fact "Error when trying to create one bin with xmin = 1.0 and xmax = 0.0" (make-bin 1.0 0.0) => {:error "xmax > xmin!"})

(fact "Linear binning from 0.0 to 1.0 with 10 bins" (make-linear-binning 0.0 10 1.0) => binning1)

(fact "Fill a simple histogram" (make-histogram "test histo 1" binning1 [0.1 0.15 0.9 0.6]) => test-histo-1)
(fact "Fill a simple histogram with weights" (make-histogram "test histo 2" binning1 [0.1 0.15 0.9 0.6] (fn [bin x] 0.5)) => test-histo-2)

(fact "Normalization factor for simple histo1d" (histo1d-sum simple-h1) => normalization-factor)
(fact "Normalization using simple factor" (histo1d-sum (histo1d-divide simple-h1 normalization-factor)) => 1.0)
