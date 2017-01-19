;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit alt+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns dry-surf
  (:require [gorilla-plot.core :as plot]))

(def max-bet 1.0e6)

(defn init [cash]
  {:cash cash
   :color :black
   :bet 1
   :wins 0
   :losses 0})

(defn opposite [c]
  (first (clojure.set/difference #{:red :black} #{c})))

(defn new-bet [state x]
  (assoc state :bet (min max-bet (:cash state) x)))

(defn win [state]
  (let [{:keys [cash color wins bet]} state]
      (new-bet
        (merge state
        {:wins (+ wins 1)
         :cash (+ cash bet)
         :color (opposite color)})
        1)))

(defn lose [state]
  (let [{:keys [cash color losses bet]} state]
    (new-bet
      (merge
        state
        {:losses (+ losses 1)
         :cash (- cash bet)
       	 :color color})
      (* 2 bet))))

(defn turn-wheel []
  (let [x (rand-int 37)]
    (if (= 0 x)
      :green
      (if (= 0 (mod x 2))
        :red :black))))

(defn with-counter [old new]
  (assoc new :counter (+ 1 (if-let [y (:counter old)] y 0))))

(defn play [x]
  (with-counter x
    (if (< 0 (:cash x))
      (if (= (:color x) (turn-wheel))
        (win x)
        (lose x))
      x)))

;; @@

;; @@
(def init-cash 100)
(def n 20000)

(defn play-seq [n init-cash] (take n (iterate play (init init-cash))))

(defn sample-seq []
  (play-seq n init-cash))

(def xtitle "Number of bets played (time)")

(defn plot-seq [sq c rng]
  (plot/list-plot (map
                    (fn [state]
                       [(:counter state) (:cash state)])
                    sq)
                  :joined true
                  :color c
                  :x-title xtitle
                  :y-title "Cash"
                  :plot-range rng))

(defn plot-bet [sq c rng]
  (plot/list-plot (map 
                    (fn [state]
                      [(:counter state) (:bet state)])
                    sq)
                  :joined true
                  :color c
                  :x-title xtitle
                  :y-title "Size of bet"
                  :plot-range rng))
                  
(def samples (take 3 (repeatedly #(sample-seq))))
(def colors [:red :green :blue])

(defn end-time [x]
  (ffirst
    (filter
      (fn [p]
        (let [[i state] p]
          (= 0 (:cash state))))
      (map (fn [a b] [a b])
           (range (count x))
           x))))
	
;; @@

;; @@
(defn max-cash [x]
  (apply max (map :cash x)))


(def y-limit (apply max (map max-cash samples)))

(map end-time samples)

(def x-limit (* 1.4 (apply max (map end-time samples))))

(def rng [[0 x-limit] [0 y-limit]])


(apply plot/compose
       (map (fn [data color]
              (plot-seq data color rng))
            samples colors))

(apply plot/compose
       (map (fn [data color]
              (plot-bet data color rng))
            samples colors))
;; @@

;; @@

;; @@
