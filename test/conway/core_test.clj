(ns conway.core-test
  (:use clojure.test
        conway.core))

(deftest test-constant-shape
  (let [shape
        [[0 0 0 0]
         [0 1 1 0]
         [0 1 1 0]
         [0 0 0 0]]]
    (is (= shape (conway shape)))))

(deftest test-stayin-alive?
  (testing "currently alive"
           (is (= 0 (stayin-alive? 1 1)))
           (is (= 1 (stayin-alive? 1 2)))
           (is (= 1 (stayin-alive? 1 3)))
           (is (= 0 (stayin-alive? 1 4))))
  (testing "currently dead"
           (is (= 0 (stayin-alive? 0 1)))
           (is (= 0 (stayin-alive? 0 2)))
           (is (= 1 (stayin-alive? 0 3)))
           (is (= 0 (stayin-alive? 0 4)))))

(deftest test-live-neighbor-count
  (let [shape
        [[0 0 0 0]
         [0 1 1 0]
         [0 1 1 0]
         [0 0 0 0]]]
    (is (= 3 (live-neighbor-count [1 1] shape)))
    (is (= 1 (live-neighbor-count [0 0] shape)))
    (is (= 2 (live-neighbor-count [3 1] shape)))
    (is (= 1 (live-neighbor-count [3 3] shape)))
    (is (= 1 (live-neighbor-count [0 3] shape)))))

 (deftest test-blinker
   (let [blinker-horz
         [[0 0 0 0 0]
          [0 0 0 0 0]
          [0 1 1 1 0]
          [0 0 0 0 0]
          [0 0 0 0 0]]
         blinker-vert
         [[0 0 0 0 0]
          [0 0 1 0 0]
          [0 0 1 0 0]
          [0 0 1 0 0]
          [0 0 0 0 0]]]
     (is (= blinker-horz (conway blinker-vert)))
     (is (= blinker-vert (conway blinker-horz)))))