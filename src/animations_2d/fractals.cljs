(ns animations-2d.fractals
  (:require
   ;;[thi.ng.geom.core :as g]
   ;;[thi.ng.geom.core.vector :as v :refer [vec2 vec3]]
   ;;[thi.ng.geom.core.matrix :as mat :refer [M32 M44]]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.webgl.animator :refer [animate]]
   [thi.ng.domus.core :as dom]
   [shodan.inspection :as shodan]))


(def pi (.-PI js/Math))
(def cos #(.cos js/Math %))
(def sin #(.sin js/Math %))
(def sqrt #(.sqrt js/Math %))
(def sign #(if (>= % 0) 1 -1))
(def abs #(if (>= % 0) % (- %)))

(def positive-infinity (.-POSITIVE_INFINITY js/Number))
(def negative-infinity (.-NEGATIVE_INFINITY js/Number))
(def epsilon (.-EPSILON js/Number))

;; would have to be a macro :is-from should be the newly defined var, not f
;; (defn add-functional-meta [f]
;;   (fn [& args]
;;     (with-meta (apply f args)
;;       {:is-from f
;;        :args args
;;        })))
;;(def my-partial (add-functional-meta partial))


(defn merged-juxt[fs]
  (with-meta (comp (partial reduce into []) (apply juxt fs))
    {:is-from merged-juxt
     :args fs}))

(defn my-comp [& fs]
  (with-meta (apply comp fs)
    {:is-from my-comp
     :args fs}))
(defn my-partial [& args]
  (with-meta (apply partial args)
    {:is-from my-partial
     :args args}))

(defn my-mapv [& args]
  (with-meta (apply mapv args)
    {:is-from my-mapv}))

(defn rotate [a [x y]]
  (with-meta [(- (* (cos a) x) (* (sin a) y))
              (+ (* (sin a) x) (* (cos a) y))]
    {:is-from rotate}))

(defn ^{:is add} add [[x0 y0] [x1 y1]]
  (with-meta [(+ x0 x1) (+ y0 y1)]
    {:is-from add}))

(defn minus [[x0 y0] [x1 y1]]
  (with-meta [(- x0 x1) (- y0 y1)]
    {:is-from minus }))

(defn rotate-around [[center a] p]
  (with-meta (add center (rotate a (minus p center)))
    {:is-from rotate-around }))

(defn cross-product [[x0 y0] [x1 y1]]
  (+ (* x0 x1) (* y0 y1)))

(def norm-sq #(cross-product % %))

(defn scalar-multiply [k [x y]]
  (with-meta [(* k x) (* k y)]
    {:is-from scalar-multiply
     :args [k [x y]]}))

(defn eltwise_op [op [x0 y0] [x1 y1]]
  [(op x0 x1) (op y0 y1)])

(defn barycenter [ps]
  (scalar-multiply (/ 1 (count ps)) (reduce add [0. 0.] ps)))

(defn regular-polygon [n]
  (take n (iterate (partial rotate (/ (* 2 pi) n)) [1. 0.])))

(defn fractal-step [[step-f step-elts] current-elts]
  (into step-elts (step-f current-elts)))

(defn fractal [[init-elts step-params] details]
  (nth (iterate (partial fractal-step step-params) init-elts) details))

(defn close-polygon [ps] (conj (into [] ps) (first ps)))

(defn sierpinski-params [n]
  (let[step-elt (close-polygon (regular-polygon n))
       make-transform #(my-partial mapv
                                   (my-partial mapv
                                               (my-comp (my-partial add %)
                                                        (my-partial scalar-multiply (/ 1 (dec n))))))]
  (condp = n
    3 [[]
       [(merged-juxt (for [i [0 1 2]] (make-transform (rotate (+ pi (* i 2 (/ pi 3))) [1. 0.]))))
        [step-elt]]]
    4 [[]
       [(merged-juxt (let [d [-1 0 1]]
                       (for [dx d dy d :when (not= 0 dx dy)]
                         (make-transform (scalar-multiply (sqrt 2.) [dx dy])))))
        [(map (partial rotate (/ pi 4)) step-elt)]]])))

(defn tree-params [angles]
  (let[branch [0 1]
       ratio (/ (+ 1 (sqrt 5.)) 2.)]
    [[]
     [(merged-juxt (for [a angles]
                     (my-partial mapv
                                 (my-partial mapv
                                             (my-comp (my-partial add branch)
                                                      (my-partial scalar-multiply (/ 1 ratio))
                                                      (my-partial rotate a))))))
      [[[0. 0] branch]]]]))

(def koch-params [[[[-0.5 0][0.5 0]]]
                  [(merged-juxt (for [[v a] [[[(/ -1 3) 0] 0]
                                             [[(/ 1 3) 0] 0]
                                             [(rotate (/ pi 3) [(/ 1 6) 0]) (/ pi -3)]
                                             [(rotate (/ pi -3) [(/ -1 6) 0]) (/ pi 3)]]]
                                  (my-partial mapv
                                              (my-partial mapv
                                                          (my-comp (my-partial add v)
                                                                   (my-partial rotate a)
                                                                   (my-partial scalar-multiply (/ 1 3)))))))
                   []]])

(defn sequence-steps [n step-factor]
  (let [p (* n step-factor)]
    (map #(-> (- p %) (min 1) (max 0)) (range n))))

(defn is-from [v]
  (get (meta v) :is-from :default))

(defn get-args [v]
  (:args (meta v)))

(defmulti stepify (fn [s v] (is-from v)))

(defmethod stepify :default [s v]
  v)

(defmethod stepify my-partial [s p]
  (let [args (get-args p)
        arg0 (first args)]
    (condp = arg0
      add (my-partial add (scalar-multiply s (second args)))
      rotate (my-partial rotate (* (second args) s))
      scalar-multiply (my-partial scalar-multiply (js/Math.pow (second args) s))
      mapv (my-partial mapv (stepify s (second args)))
      :default (apply p (map (partial stepify s))))))

(defmethod stepify my-comp [s c]
  (let [args (get-args c)]
    (apply my-comp (map stepify
                        (reverse (sequence-steps (count args) s))
                        args))))

(defmethod stepify merged-juxt [s c]
  (let [args (get-args c)]
    (merged-juxt (map stepify
                      (sequence-steps (count args) s)
                      args))))


(defmethod stepify :default [s v]
  v)

(defn params-step [s [init-scene [step-fs step-scene]]]
  [init-scene [(stepify s step-fs) step-scene]])

(defn fractal-with-steps [params details]
  (let [[init-scene step-params] params
        int-d (int details)
        int-fractal (nth (iterate (partial fractal-step step-params) init-scene) int-d)
        fractional-d (- details int-d)]
    (if (<= fractional-d epsilon)
      int-fractal
      (fractal-step (second (params-step fractional-d params)) int-fractal))))


(defn bounding-box [ps]
  (reduce (fn [[[x-min y-min][x-max y-max]] [x y]]
            [ [(min x-min x) (min y-min y)] [(max x-max x) (max y-max y)]])
          [[positive-infinity positive-infinity] [negative-infinity negative-infinity]]
          ps))

(defn merge-bb [[[x-min y-min][x-max y-max]] [[X-min Y-min][X-max Y-max]]]
  [[(min x-min X-min) (min y-min Y-min)] [(max x-max X-max) (max y-max Y-max)]])

(defn adjust-bounding-box [preserve-ratio target-box scene]
  (let [scene-bb (reduce merge-bb (map bounding-box scene))
        center-f (fn[box] (let[center (barycenter box)][center (apply minus (mapv #(minus % center) box))]))
        [target-c target-centered] (center-f target-box)
        [scene-c scene-centered] (center-f scene-bb)
        scaling (partial eltwise_op *
                         (let[tmp (eltwise_op / target-centered scene-centered)]
                           (if preserve-ratio
                             (scalar-multiply (apply min (map abs tmp)) (map sign tmp))
                             tmp)))]
    (fn[v] (-> v (minus scene-c) scaling (add target-c)))))

(defn display-fractal [id params details]
 (let [root (dom/by-id id)
        display-params {:width 512 :height 512}
        color "#000"
        draw #(svg/line-strip % )
       display #(->> root
                     (dom/clear!)
                     (dom/create-dom!
                      (let [[w h] ((juxt :width :height) display-params)
                            f (adjust-bounding-box true [[0 h][w 0]] %)]
                        (svg/svg display-params (svg/group {:stroke color} (map (comp draw (partial mapv f)) %))))
                      root))
        scene (fractal-with-steps params details)]
   (display scene)))

(defn -main
  []
  (animate
     (fn[[t frame]]
       (let [max-detail 5
             frames-per-detail 50
             m-f (* max-detail frames-per-detail)
             i (mod frame (* 2 m-f))
             j (if (> i m-f) (- (* 2 m-f) i) i)
             detail (/ j frames-per-detail)]
         (doseq [[params id] [[(sierpinski-params 3) "sierpinski-triangle"]
                              [(sierpinski-params 4) "sierpinski-square"]
                              [koch-params "koch"]
                              [(tree-params [(/ pi 6) (/ pi -3)]) "tree"]]]
                 (display-fractal id params detail))
        true))))

(-main)
