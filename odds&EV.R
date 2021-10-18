#the idea is to calculate how often a hand wins based on your cards and the board cards and numpl
library(holdem)
library(tictoc)


#odds of a hand winning based on the board
any_hand_board_odds = function(sims,numpl,hand,board,rang){
  winners = 0
  
  for (x in 1:sims){
    alt_hand = alt_deal_range(numpl,hand,board,rang)

    best_hand_value = 0
    for (i in 1:numpl){
      hand_value = handeval(c(alt_hand$brdnum1,alt_hand$plnum[i, ]), c(alt_hand$brdsuit1, alt_hand$plsuit1[i,]))

      if (hand_value > best_hand_value){
        best_hand = list("winner" = c('card' = alt_hand$plnum[i, ], 'suit' = alt_hand$plsuit1[i,]))
        best_hand_value = hand_value
        num = i
      }
    }
    
    if (num == 1){
      winners = winners +1
    }
  }
  
  return(winners/sims*100)
}

tic()
any_hand_board_odds(1000,2,list(c(11,11),c(1,4)),list(c(13,12,5),c(3,1,2)),.13)
toc()

#Returns both equity and the probability of winning. 
#If the difference (final value returned) is positive
#then it is a winning play to call.
pot_odds = function(sims,numpl,hand,board,rang,bet_size,pot_size){
  equity = any_hand_board_odds(sims,numpl,hand,board,rang)
  ratio = 100*bet_size/(bet_size+pot_size)
  print(equity)
  print(ratio)
  return(equity-ratio)
}

tic()
pot_odds(1000,3,list(c(11,11),c(1,4)),list(c(14,5,8),c(1,2,3)),.1,150,600)
toc()

#EV based on the # of players, your hand, what is on the board,
#your opponents range, the bet size and the pot size.
EV = function(sims,numpl,hand,board,rang,bet_size,pot_size){
  winning_odds = any_hand_board_odds(sims,numpl,hand,board,rang)/100
  losing_odds = 1 - winning_odds
  EV = winning_odds * pot_size - losing_odds*bet_size
  return(EV)
}
EV(1000,3,list(c(11,11),c(1,4)),list(c(14,5,8),c(1,2,3)),.1,150,600)



