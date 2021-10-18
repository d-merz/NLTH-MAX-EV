library(holdem)
library(tictoc)
#169 non-equivalent starting hands


cards = c('2.s','2.o','3.s','3.o','4.s','4.o','5.s','5.o','6.s','6.o',
          '7.s','7.o','8.s','8.o','9.s','9.o','10.s','10.o','11.s','11.o','12.s','12.o','13.s','13.o')
pockets= c(2,3,4,5,6,7,8,9,10,11,12,13,14)

hands = list()

for (i in 13:1){
  x = i*2-1
  vec = integer(x)
  if (length(vec) == 1){
    names(vec) = c(pockets[i])
  }else{
    names(vec) = c(pockets[i],cards[(length(vec)-1):1])
  }
  hands = append(hands,list(vec))
}
names(hands) = c("A","K",'Q','J',10,9,8,7,6,5,4,3,2)

#returns winning hands as the count of hands that win
#played hands are how many times each hand gets played
winning_hands = hands
played_hands = hands
hand_count = function(sims,numpl){
  
  for (i in 1:sims){
    hands = winner_and_losers(numpl)
    cards_pos = c()
    for (cards in hands){
      card1_pos = 14- cards[1] +1
      poss = length(unlist(played_hands[card1_pos]))
      
      if (cards[1] == cards[2]){
        card2_pos = 1
      }else if (cards[3] == cards[4]){
          suit = 4
          card2_pos = poss-(2*cards[2] - suit)
          
      }else {
          suit = 3
          card2_pos = poss-(2*cards[2] - suit)
        }
      hand= c(card1_pos,card2_pos)
      cards_pos = c(cards_pos,list(hand))
    }
    lead_winning_card = cards_pos[[1]][1]
    second_winning_card = cards_pos[[1]][2]
    winning_hands[[lead_winning_card]][second_winning_card] = 
      winning_hands[[lead_winning_card]][second_winning_card] + 1
    for (i in 1:numpl){
      lead_card = cards_pos[[i]][1]
      second_card = cards_pos[[i]][2]
      played_hands[[lead_card]][second_card] = played_hands[[lead_card]][second_card] + 1
    }
    
  }
  
  return(list("won_hands" = winning_hands,"played_hands" = played_hands))
}

tic()
x = hand_count(1000,4)
toc()




#percentage of time a hand wins at showdown vs gets played for how many players

hand_odds = function(sims,numpl){
  odds = list()
  counts = hand_count(sims,numpl)
  
  for (i in 1:13){
    percent_win = counts$won_hands[[i]]/counts$played_hands[[i]]
    
    odds = append(odds,list(percent_win))
  }
  names(odds) = c(14,13,12,11,10,9,8,7,6,5,4,3,2)
  return(odds)
}

tic()
z= hand_odds(1000,3)
toc()





