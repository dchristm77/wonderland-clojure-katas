(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= true (play-round [:diamond 10] [:spade 3]))))
  (testing "queens are higher rank than jacks"
    (is (= false (play-round [:spade :jack] [:spade :queen]))))
  (testing "kings are higher rank than queens"
    (is (= true (play-round [:spade :king] [:heart :queen]))))
  (testing "aces are higher rank than kings"
    (is (= false (play-round [:club :king] [:diamond :ace]))))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= true (play-round [:club :king] [:spade :king]))))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= false (play-round [:club 3] [:diamond 3]))))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= true (play-round [:heart 9] [:diamond 9])))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (is (= "Player 1 wins" (play-game [[:club 3]] [[:spade 3]])))))

