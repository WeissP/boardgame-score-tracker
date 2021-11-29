(ns main
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [cljs.core.async :refer (chan put! <! go go-loop timeout)]))

(enable-console-print!)
(def bg-color {:player "bg-blue-300"})
(def names (r/atom  ["Bai" "Xiao" "Jintian" "Yunfan"]))
(def games ["general"])


(defn zip [& colls]
  (map #(into [] %) (partition (count colls) (apply interleave colls))))

(defn new-player
  [name]
  {:name name :init-score 0 :score 0 :log [] :delta 0})

(defonce state  (r/atom (zipmap games (repeat  (map new-player @names)))))
(def current-game (atom (first games)))

(defn update!
  [f]
  (swap! state #(update % @current-game (fn [players] (map f players)))))

(defn matched?
  [name player]
  (= (:name player) name))

(defn current-players
  []
  (get @state @current-game))

(defn find-player!
  [name]
  (first (filter #(matched? name %) current-players)))

(defn modify-delta
  [name mod]
  (update!
   (fn [player]
     (if (= (:name player) name)
       (update player :delta mod)
       player))))

(defn change-score
  [player]
  (let [delta (:delta player)]
    (-> player
        (update :score + delta)
        (update :log conj delta)
        (update :delta (constantly 0)))))

(defn log!
  []
  (update! change-score))

(defn score-table-component
  [players]
  [:div.container.flex.justify-center.mx-auto {:id "score-table"}
   [:div.flex.flex-col
    [:div.w-full
     [:div.border-b.border-gray-200.shadow
      (conj [:table.table-auto (into [:tr [:th.table-head ""]]  (zip (repeat :th.table-head) @names))]
            [:tbody.bg-white
             (map-indexed
              (fn [index single-round]
                (into [:tr.whitespace-nowrap [:td.table-body (str "Round " (inc index))]] (map #(conj [:td.table-body.text-center %]) single-round)))
              (apply zip (map :log players)))
             (into [:tr.whitespace-nowrap [:td.table-body [:b "Score"]]] (map (fn [player]  [:td.table-body.text-center [:b (:score player)]]) players))])]]]])


(defn pos-component
  []
  [:div.grid.grid-cols-3.grid-rows-3.gap-4
   (zip [:div.col-start-2 :div.col-start-1.row-start-2 :div.col-start-3.row-start-2 :div.col-start-2.row-start-3]
        (repeat {:class "bg-blue-300"})
        @names
        (map (fn [player] [:span.float-right.pr-2 (:score player)]) (current-players)))])

(def range-min (r/atom -5))

(defn log-score-component
  [players & {:keys [min-fun max-fun] :or {min-fun #(- % 20) max-fun #(+ % 20)}}]
  (let [increase (fn [player] (modify-delta (:name player) inc))
        decrease (fn [player] (modify-delta (:name player) dec))
        reset (fn [player new-value] (modify-delta (:name player) (constantly new-value)))]
    (loop [rest players
           res [:div.grid.grid-cols-6.gap-4.custom-number-input]]
      (if (empty? rest)
        res
        (let [player (first rest)
              delta (:delta player)
              name [:span.px-5.pt-5.col-start-1.col-span-1 [:b (str (:name player) ": ")]]
              number [:div.col-start-2.col-span-2 {:class "flex flex-row h-10 relative bg-transparent mt-3"}
                      [:button {:class "bg-gray-200 text-gray-800 hover:text-gray-900 hover:bg-gray-400 h-full w-44 rounded-l cursor-pointer outline-none"
                                :on-click #(decrease player)} [:span {:class "m-auto text-2xl font-thin"} "-"]]
                      [:input {:type "number" :value delta
                               :class "outline-none focus:outline-none text-center w-full bg-gray-200 font-semibold text-md hover:text-black focus:text-black  md:text-basecursor-default flex items-center text-gray-700"
                               :on-change (fn [e]
                                            (let [new-value (js/parseInt (.. e -target -value))]
                                              (println "changed")
                                              (reset player new-value)))}]
                      [:button
                       {:class "bg-gray-200 text-gray-800 hover:text-gray-900 hover:bg-gray-400 h-full w-44 rounded-r cursor-pointer outline-none"
                        :on-click #(increase player)}
                       [:span {:class "m-auto text-2xl font-thin"} "+"]]]
              range [:div.col-start-4.col-span-3
                     {:class "flex flex-row h-10 relative bg-transparent mt-3"}
                     [:input.mx-3.w-full {:type "range" :value delta :min @range-min :max (max-fun delta)
                                          :on-change (fn [e]
                                                       (let [new-value (js/parseInt (.. e -target -value))]
                                                         (reset player new-value)))
                                          :on-touch-end (fn [e] (let [new-value (js/parseInt (.. e -target -value))]
                                                                  (when (< (- new-value  @range-min) 1)
                                                                    (swap! range-min min-fun))))}]]]
          (recur (drop 1 rest) (into res [name number range])))))))

(defn main-component []
  [:div
   [pos-component]
   [:br]
   [score-table-component (current-players)]
   [:br]
   [log-score-component (current-players)]
   [:br]
   [:button
    {:class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
     :on-click log!} "submit"]
   ;; [:ul
   ;;  (zip (repeat (count @names) :li) @names)]
   ])
;; (log-score-component (current-players))
(defn mount []
  (r/render-component [main-component] (.getElementById js/document "app")))

(defn reload! []
  (mount)
  (print "Hello reload!"))

(defn main! []
  (mount)
  (print "Hello Main"))


