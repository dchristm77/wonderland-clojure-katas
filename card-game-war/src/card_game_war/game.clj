(ns card-game-war.game
  (:require [clojure.zip :as z]))

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

;; In hindsight it would have been easier to use "clojure.lang.PersistentQueue" for
;; each players pile instead of vectors, but I've had enough for now.
(defn init-dealt-cards
  "Sets up the game creating a vector of vectors, each representing one player"
  [num-players]
  (loop [dealt-cards []
         players-left num-players]
    (if (> players-left 0)
      (recur (conj dealt-cards []) (dec players-left))
      dealt-cards)))

;; Uses a zipper mostly because I wanted to know more about them. In hindsite
;; using a filter on the card deck for each player would probably be more efficient.
(defn deal-cards
  "deals all cards to given game setup (players) until cards are depleated dealing from left to right"
  [players cards]
  (loop [player (-> (z/vector-zip players) (z/down))
         remaining-cards (seq cards)]
    (if (empty? remaining-cards)
      (z/node (z/up player))
      (let [new-players (z/append-child player (first remaining-cards))]
        (recur (if (z/right new-players)
                 (z/right new-players)
                 (z/leftmost new-players))
               (rest remaining-cards))))))

(defn play-round
  "Returns true if player 1 wins, false otherwise"
  [player1-card player2-card]
  (let [[p1-suite p1-rank] player1-card
        [p2-suite p2-rank] player2-card]
    (cond
      (> (.indexOf ranks p1-rank) (.indexOf ranks p2-rank)) true
      (< (.indexOf ranks p1-rank) (.indexOf ranks p2-rank)) false
      (> (.indexOf suits p1-suite) (.indexOf suits p2-suite)) true
      (< (.indexOf suits p1-suite) (.indexOf suits p2-suite)) false)))

(defn add-cards
  "adds the two cards to the bottom of the players pile"
  [pile card1 card2]
  (conj (vec pile) card1 card2))

(defn play-game [player1-cards player2-cards]
  (loop [p1-cards player1-cards
         p2-cards player2-cards]
    (let [p1-card (first p1-cards)
          p2-card (first p2-cards)]
      (if (and p1-card p2-card)
        (if (play-round p1-card p2-card)
          (recur (add-cards (rest p1-cards) p1-card p2-card) (rest p2-cards))
          (recur (rest p1-cards) (add-cards (rest p2-cards) p2-card p1-card)))
        (if p1-card "Player 1 wins" "Player 2 wins")))))

;; computer plays game with a "shuffled deck" results depend on the determinacy of "shuffle"
(apply play-game (deal-cards (init-dealt-cards 2) (shuffle cards)))