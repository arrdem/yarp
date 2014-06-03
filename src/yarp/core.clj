(ns yarp.core
  {:doc "Yarp is a quaternary logic system with four values. yar is
         true, nop is false, nar is a garbage value that results in
         exceptions and wut is an indeterminate value that propigates
         silently."})

(defn bool?
  "Arguably the only useful thing in here. Tests to see if x is a
  boolean value. Returns true only if the result is a boolean."
  [x]
  (or (= true x)
      (= false x)))


(def yar  :yar)
(def nop  :nop)
(def nar  :nar)
(def wut  :wut)

(defn narp)

(defn narp?
  [x]
  (= nar x))


(defn wut?
  [x]
  {:pre [(not (narp? x))]}
  (= x wut))


(defn yarp?
  ([x]
     {:pre [(not (narp? x))]}
     (if (wut? x)
       x
       (= x yar)))

  ([x y]
     (and (yarp? x)
          (yarp? y)))

  ([x y & m0arp]
     (reduce yarp?
             (yarp x y)
             m0arp)))


(defn nop?
  ([x]
     {:pre [(not (narp? x))]}
     (if (wut? x)
       x
       (= x nop)))

  ([x y]
     (and (nop? x)
          (nop? y)))

  ([x y & m0arp]
     (reduce nop?
             (nop? x y)
             m0arp)))


(defn xorp
  ([x])
  ([x y])
  ([x y & m0arp]))


(def arp
  (fn  [yarp & yarps]
    (and (yarp? yarp)
         (apply arp yarps))))


(assert (= (yarp (narp (yarp (arp (A B)))
                       (yarp (orp (narp A)
                                  (narp B)))

(assert (= (yarp? (xorp A)) (narp (xnorp A)))
