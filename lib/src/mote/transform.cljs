(ns mote.transform
  (:require
   ["gl-matrix" :as m]))

(defrecord Transform [position rotation scale cached-matrix cached-rotation cached-scale])

(defn transform
  []
  (let [matrix (.create m/mat4)]
    (aset matrix 3 0)
    (aset matrix 7 0)
    (aset matrix 11 0)
    (aset matrix 15 1)
    (->Transform
     [0.0 0.0 0.0]
     [0.0 0.0 0.0 0.0]
     [1.0 1.0 1.0]
     matrix
     (atom nil)
     (atom nil))))

(defn transform-position
  [^Transform transform position]
  (->Transform
   position
   (.-rotation transform)
   (.-scale transform)
   (.-cached-matrix transform)
   (.-cached-rotation transform)
   (.-cached-scale transform)))

(defn- transform->matrix-set-rotation-scale
  [matrix ^Transform transform]
  ;; Especially in a 2d game, rotation & scale rarely change, so caching it has an actual benefit.
  (let [rotation (.-rotation transform)
        cached-rotation (.-cached-rotation transform)
        scale (.-scale transform)
        cached-scale (.-cached-scale transform)]
    (when (or (not (identical? rotation @cached-rotation))
              (not (identical? scale @cached-scale)))
    (reset! cached-rotation rotation)
    (reset! cached-scale scale)
    (let [[x y z w] rotation
          x2 (+ x x)
          y2 (+ y y)
          z2 (+ z z)
          xx (* x x2)
          xy (* x y2)
          xz (* x z2)
          yy (* y y2)
          yz (* y z2)
          zz (* z z2)
          wx (* w x2)
          wy (* w y2)
          wz (* w z2)
          [sx sy sz] scale]
      (aset matrix 0 (* (- 1 (+ yy zz)) sx))
      (aset matrix 1 (* (+ xy wz) sx))
      (aset matrix 2 (* (- xz wy) sx))
      (aset matrix 4 (* (- xy wz) sy))
      (aset matrix 5 (* (- 1 (+ xx zz)) sy))
      (aset matrix 6 (* (+ yz wx) sy))
      (aset matrix 8 (* (+ xz wy) sz))
      (aset matrix 9 (* (- yz wx) sz))
      (aset matrix 10 (* (- 1 (+ xx yy)) sz))))))

(defn- transform->matrix-set-position
  [matrix ^Transform transform]
  ;; We could also check for changes here like rotation and scale, but setting
  ;; position is so cheap, and moving is so common, I doubt it would ever be
  ;; worth it.
  (let [[px py pz] (.-position transform)]
    (aset matrix 12 px)
    (aset matrix 13 py)
    (aset matrix 14 pz)))

(defn transform->matrix
  [^Transform transform]
  (let [matrix (.-cached-matrix transform)]
    (transform->matrix-set-rotation-scale matrix transform)
    (transform->matrix-set-position matrix transform)
    matrix))

(defn transform->inverted-matrix
  [^Transform transform]
  (let [matrix (transform->matrix transform)
        inverted-matrix (.create m/mat4)]
    (.invert m/mat4 inverted-matrix matrix)
    inverted-matrix))
