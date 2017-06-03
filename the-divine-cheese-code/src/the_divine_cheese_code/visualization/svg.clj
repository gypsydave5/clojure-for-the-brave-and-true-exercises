(ns the-divine-cheese-code.visualization.svg
  (:refer-clojure :exclude [min max]))

(defn latlng->point
  "Convert lat/lng map to comma-separated string"
  [latlng]
  (str (:lng latlng) "," (:lat latlng)))

(defn points
  [locations]
  (clojure.string/join " " (map latlng->point locations)))

(defn comparator-over-maps
  [comparison-fn ks maps]
  (zipmap ks
          (map #(apply comparison-fn (map % maps))
               ks)))

(def min (partial comparator-over-maps clojure.core/min [:lat :lng]))
(def max (partial comparator-over-maps clojure.core/max [:lat :lng]))

(defn translate-to-00
  [locations]
  (let [mincoords (min locations)]
    (map #(merge-with - % mincoords) locations)))

(defn scale
  [width height locations]
  (let [maxcoords (max locations)
        ratio {:lat (/ height (:lat maxcoords))
               :lng (/ width (:lng maxcoords))}]
    (map #(merge-with * % ratio) locations)))

(defn line [points]
  "generates an svg polyline node from a series of points"
  [points]
  (str "<polyline points=\"" points "\" />"))

(defn transform
  "generates a series of scaled and 0'd coordinates from a list of coordinates"
  [width height locations]
  (->> locations
       translate-to-00
       (scale width height)))

(defn xml
  "basic svg template for coordinate mapping"
  [width height locations]
  (str "<svg height=\"" height "\" width=\"" width "\">"
       "<g transform=\"translate(0," height ") \">"
       "<g transform=\"scale(1,-1)\">"
       (-> (transform width height locations)
           points
           line)))
