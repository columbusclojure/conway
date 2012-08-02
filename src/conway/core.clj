(ns conway.core
  (:require [clj-native.direct :refer [defclib loadlib]])
  (:require [clojure.pprint :refer [pprint]]))

(defclib ncurses
  (:libname "ncurses")
  (:functions
   (initscr [] int)
   (printw [constchar*] int)
   (mvprintw [int int constchar*] int)
   (mvaddch [int int int] int)
   (refresh [] int)
   (endwin [] int)
   (curs-set curs_set [int] int)))

(loadlib ncurses)

(def invisible 0)
(def visible 1)

(def initial-grid
  [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0]
   [0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0]
   [0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0]
   [0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0]
   [0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0]
   [0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0]
   [0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   ])

(defn stayin-alive? [my-state live-neighbor-count]
  (cond (and (= 2 live-neighbor-count) (= 1 my-state)) 1
        (= 3 live-neighbor-count) 1
        :else 0))

(defn safe-subvec [v start end]
  (let [max (count v)]
    (subvec (vec v)
            (if (< start 0) 0 start)
            (if (> end max) max end))))

(defn live-neighbor-count [[x y] shape]
  (let [offset (nth (nth shape y) x)
        rows (safe-subvec shape (dec y) (+ 2 y))
        neighborhood (map  #(safe-subvec % (dec x) (+ 2 x)) rows)]
    (- (reduce + (flatten neighborhood)) offset)))

(defn conway [shape]
  (map-indexed
   (fn [y row]
     (map-indexed
      (fn [x cell]
        (stayin-alive? cell (live-neighbor-count  [x y] shape)))
      row))
   shape))

(defn format-row [row]
  (apply str (map #(if (= 0 %)
                     "  "
                     "# ")
                  row)))

(defn display [grid]
  (doall (map-indexed
          (fn [i row]
            (mvprintw i 0 (format-row row))
            ;; doing refresh here fixes display bug on linux. not on mac...
            (refresh)
            )
          grid)))

(defn run-game [grid]
  (display grid)
  ;; should be able to do refresh here after painting all lines
  ;; (refresh)
  (Thread/sleep 1000)
  (recur (conway grid)))

(defn -main
  "it's a hard knock game of life"
  [& args]

  ;; clean up after ourselves
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. #(do
                                (curs-set visible)
                                (endwin))))
  (initscr)
  (curs-set invisible)
  (run-game initial-grid)
  0)
