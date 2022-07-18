(ns swarm.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:gen-class
   :main true))

(import 'javax.swing.JFrame)
(import 'javax.swing.JPanel)
(import 'javax.swing.JButton)
(import 'javax.swing.JComboBox)

(def frame (JFrame. "Swarm controller"))
(def panel (JPanel.))
(def buttonFollow (JButton. "Follow"))
(def buttonEscape (JButton. "Escape"))
(def ruleCombo (JComboBox. (java.util.Vector. ["Rule1" "Rule2"])))
(def swarm (atom { :map {}}))
(def switch (atom { :value 0}))
(def ^:dynamic radian 10)
(def ^:dynamic room 20)
(def ^:dynamic amount 15)

(defn expt [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
        (recur (* x acc) (dec n)))))

(defn calc-distance [x1 y1 x2 y2]
  (Math/sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))

(defn calc-angle [x1 y1 x2 y2]
 (/ (* (Math/atan2 (- y2 y1) (- x2 x1)) 180) Math/PI))

(defn collision-detect [x1 y1 x2 y2 radian]
  (if (< (int (calc-distance x1 y1 x2 y2)) (* radian 2))
    (calc-angle x1 y1 x2 y2) false))

(defn check-collision [m x y]
  (map (fn [columns]
         (map (fn [rows]
                (let [angle (collision-detect x y (:x rows) (:y rows) radian)]
                (if (and angle (not (= (:x rows) x)) (not (= (:y rows) y)))
                  {
                   :x (:x rows)
                   :y (:y rows)
                   :c (rand-int 100)
                   :x-move (* -4 (Math/sin angle))
                   :y-move (* -4 (Math/cos angle))
                   }       
                  {
                   :x (:x rows)
                   :y (:y rows)
                   :c (:c rows)
                   :x-move (:x-move rows)
                   :y-move (:y-move rows)
                   }
                  )))
              columns))
       m))

(defn check-collision-escape [m x y]
  (map (fn [columns]
         (map (fn [rows]
                (let [angle (collision-detect x y (:x rows) (:y rows) radian)
                      distance (calc-distance (:x rows) (:y rows) x y)
                      mouse-distance (calc-distance (:x rows) (:y rows)
                                              (q/mouse-x) (q/mouse-y))
                      mouse-angle (calc-angle
                                   (:x rows) (:y rows) (q/mouse-x) (q/mouse-y))]
                (if (and angle (not (= (:x rows) x)) (not (= (:y rows) y)))
                  {
                   :x (:x rows)
                   :y (:y rows)
                   :c (rand-int 100)
                   :x-move (* -4 (Math/sin angle))
                   :y-move (* -4 (Math/cos angle))
                   }
                  (if (and (not angle)
                           (not (= (:x rows) x)) (not (= (:y rows) y))
                           (< mouse-distance (* 20 radian))
                           (< distance (* radian 10)))
                   {
                   :x (:x rows)
                   :y (:y rows)
                   :c 150
                   :x-move (* 4 (Math/cos mouse-angle))
                   :y-move (* 4 (Math/sin mouse-angle))
                   }         
                  {
                   :x (:x rows)
                   :y (:y rows)
                   :c (:c rows)
                   :x-move (:x-move rows)
                   :y-move (:y-move rows)
                   }
                  ))))
              columns))
       m))

(defn move-swarm [m]
    (map (fn [columns]
         (map (fn [rows]
                  {
                   :x (+ (:x rows) (:x-move rows))
                   :y (+ (:y rows) (:y-move rows))
                   :c (:c rows)
                   :x-move (:x-move rows)
                   :y-move (:y-move rows)
                   })
              columns))
           m))

(defn rule-set-x [m x y]
  (let [new-map (check-collision-escape m
                                        (:x (nth (nth m y) x))
                                        (:y (nth (nth m y) x)))]
    (if (= x (- (count (first new-map)) 1))
      new-map
      (recur new-map (+ x 1) y))))

(defn rule-set [m y]
  (let [new-map (rule-set-x m 0 y)]
    (if (= y (- (count new-map) 1))
         new-map
         (recur new-map (+ y 1)))))

(defn check-follower [m x y]
  (map (fn [columns]
         (map (fn [rows]
                (let [detect (collision-detect x y (:x rows) (:y rows) radian)
                      angle (calc-angle x y (:x rows) (:y rows))
                      distance (calc-distance (:x rows) (:y rows) x y)
                      mouse-distance (calc-distance (:x rows) (:y rows)
                                              (q/mouse-x) (q/mouse-y))
                      mouse-angle (calc-angle
                                   (:x rows) (:y rows) (q/mouse-x) (q/mouse-y))]

                  ;; collision
                  (if (and detect (not (= (:x rows) x)) (not (= (:y rows) y)))
                  {
                   :x (:x rows)
                   :y (:y rows)
                   :c (rand-int 100)
                   :x-move (* -4 (Math/sin detect))
                   :y-move (* -4 (Math/cos detect))
                   }
                  ;; attract each other
                  (if (and (not detect)
                           (not (= (:x rows) x)) (not (= (:y rows) y))
                           (> distance (* radian 10))
                           (> mouse-distance (* radian 20)))
                  {
                   :x (:x rows)
                   :y (:y rows)
                   :c 0
                   :x-move (* 4 (Math/cos angle))
                   :y-move (* 4 (Math/sin angle))
                   }
                  ;; follow mouse
                  (if (and (not detect)
                           (not (= (:x rows) x)) (not (= (:y rows) y))
                           (< mouse-distance (* 20 radian))
                           (< distance (* radian 10)))
                   {
                   :x (:x rows)
                   :y (:y rows)
                   :c 150
                   :x-move (* 4 (Math/cos mouse-angle))
                   :y-move (* 4 (Math/sin mouse-angle))
                   }               
                   ;; nothing
                   {
                   :x (:x rows)
                   :y (:y rows)
                   :c (:c rows)
                   :x-move (:x-move rows)
                   :y-move (:y-move rows)
                   }                    
                  )))))
                  columns))
              m))
       
(defn rule-set-follower-x [m x y]
  (let [new-map (check-follower m
                                 (:x (nth (nth m y) x))
                                 (:y (nth (nth m y) x)))]
    (if (= x (- (count (first new-map)) 1))
      new-map
      (recur new-map (+ x 1) y))))

(defn rule-set-follower [m y]
  (let [new-map (rule-set-follower-x m 0 y)]
    (if (= y (- (count new-map) 1))
         new-map
         (recur new-map (+ y 1)))))

(defn reset-swarm-random []
  (vec (map (fn [y] 
              (vec (map (fn [x]
                          {:x (* x (rand-int (* room 2))) 
                           :y (* y (rand-int (* room 2))) 
                           :c 0; (+ (* amount y) x)
                           :x-move 0
                           :y-move 0
                           :angle 0})
                        (vec (range amount))))) (range amount))))


(defn reset-swarm-regular []
  (vec (map (fn [y] 
              (vec (map (fn [x]
                          {:x (* x room)
                           :y (* y room)
                           :c 0; (+ (* amount y) x)
                           :x-move 0
                           :y-move 0
                           :angle 0})
                        (vec (range amount))))) (range amount))))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  
  ;; add gui
  ;; window
  (.setBounds frame 0 0 200 70)

  (.setVisible frame true)
  ;; panel
  (.add frame panel)
  (.setContentPane frame panel)
  ;; buttons
  (.add panel buttonFollow)
  (.revalidate buttonFollow)
     ; dropdown
;  (.add move-swarmCombo panel)
;  (.revalidate ruleCombo)
  (.addActionListener
   buttonFollow
   (reify java.awt.event.ActionListener
     (actionPerformed [this e]
       (.setText buttonFollow "Follow!")
       (.setText buttonEscape "Escape")
       (swap! switch assoc :value 2))))  
  (.add panel buttonEscape)
  (.revalidate buttonEscape)
  (.addActionListener
   buttonEscape
   (reify java.awt.event.ActionListener
     (actionPerformed [this e]
       (swap! switch assoc :value 1)
       (.setText buttonFollow "Follow")
       (.setText buttonEscape "Escape!"))))
  
  ; setup function returns initial state. It contains
  {:coord (reset-swarm-regular)})

(defn update-state [state]
  {:coord (if (= (:value @switch) 1)
              (rule-set
               (move-swarm
                (check-collision
                 (:coord state) (q/mouse-x) (q/mouse-y))) 0)
              (if (= (:value @switch) 2)
                (rule-set-follower
                 (move-swarm
                  (check-collision
                   (:coord state) (q/mouse-x) (q/mouse-y))) 0)
                (move-swarm
                 (check-collision
                  (:coord state) (q/mouse-x) (q/mouse-y)))))})

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  (doseq [y (range (count (:coord state)))]
    (doseq [x (range (count (nth (:coord state) y)))]
      (q/fill (:c (nth (nth (:coord state) y) x))
                         255 255)
      (q/ellipse (:x (nth (nth (:coord state) y) x))
                 (:y (nth (nth (:coord state) y) x))
                 (* radian 2) (* radian 2))))
  (q/fill 0 0 0)
  (q/ellipse (q/mouse-x) (q/mouse-y) (* radian 2) (* radian 2)))

(defn -main [& args]
  (q/defsketch swarm
    :title "You spin my circle right round"
    :size [800 800]
                                        ; setup function called only once, during sketch initialization.
    :setup setup
                                        ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
                                        ; This sketch uses functional-mode middleware.
                                        ; Check quil wiki for more info about middlewares and particularly
                                        ; fun-mode.
    :middleware [m/fun-mode]))
