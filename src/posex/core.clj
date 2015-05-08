(ns posex.core)

(defn price-for-items [items item-price subtotal]
  (let [ [single-price number-for-discount discount-price] item-price
        count (count items)]

    (if (>= count number-for-discount)
      (price-for-items
        (take  (- count number-for-discount) items)
        item-price
        (+ subtotal discount-price))
      (+ (* count single-price) subtotal))))

(defn add-distinct-item [distinct-item-list]
  (if (empty? distinct-item-list)
    [1]
    (conj distinct-item-list 1)))

(defn add-item [purchased-item-map item]
  (let [item-keyword (keyword item)]
    (into purchased-item-map
          {item-keyword (add-distinct-item (item-keyword purchased-item-map))})))

(def price-map1 {
                :A [2 4 7]
                :B [12 1 12]
                :C [1.25 6 6]
                :D [0.15 1 0.15]}
)

(def purchase1 ["A" "B" "C" "D" "A" "B" "A" "A"])
(def purchase2 ["C" "C" "C" "C" "C" "C" "C" ])
(def purchase3 ["A" "B" "C" "D" ])

(defn make-purchased-item-map [items]
  (reduce add-item {} items))

(defn sum-item-prices [subtotal next-item-map price-map]
  (+ subtotal
     (price-for-items (second next-item-map) ((first next-item-map) price-map) 0)))

(defn sum-item-prices-maker [price-map]
  (fn [subtotal next-item-map]
    (sum-item-prices subtotal next-item-map price-map)))

(defn total-price [purchase price-map]
  (reduce (sum-item-prices-maker price-map)  0 (make-purchased-item-map purchase)))

(defn -main
  [& args]

  (println "purchases: " purchase1 )
  (println "total: " (total-price purchase1 price-map1))
  (println)
  (println "purchases: " purchase2 )
  (println "total: " (total-price purchase2 price-map1))
  (println)
  (println "purchases: " purchase3 )
  (println "total: " (total-price purchase3 price-map1))
  (println)
  )

