library(holdem)
library(tictoc)
#returns the winners and loser of a hand with numpl of players.
winner_and_losers = function(numpl){
  hand = deal1(numpl)
  best_hand_value = 0
  for (i in 1:numpl){
    hand_value = handeval(c(hand$brdnum1,hand$plnum[i, ]), c(hand$brdsuit1, hand$plsuit1[i,]))
    if (hand_value > best_hand_value){
      best_hand = list("winner" = c('card' = hand$plnum[i, ], 'suit' = hand$plsuit1[i,]))
      best_hand_value = hand_value
      num = i
    }
  }
  losing_hands = c()
  for (x in 1:numpl){
    if (x != num){
      loser = list("loser" = c("card" = hand$plnum[x, ], "suit" = hand$plsuit1[x,]))
      losing_hands = append(losing_hands,loser)
    }
  }
  
  return (c(best_hand,losing_hands))
}

winner_and_losers(6)
#hand is a list of 2 vectors, 1 is cards, 1 is suits
#board is the same

#deals a random hand based on inputted hand and board.

alt_deal = function(numpl,hand,board){
  ## numpl is the number of players at the table
  
  nums = c()
  for (i in 1:2){
    num = 13*(hand[[2]][i]-1)+hand[[1]][i]-1
    nums = append(nums,num)
  }
  for (i in 1:length(unlist(board[1]))){
    num = 13*(board[[2]][i]-1)+board[[1]][i]-1
    nums = append(nums,num)
  }
  numcards = 2*numpl+5
  cards = 1:52
  cards = cards[-nums]
  crds1 = sample(x = cards, size = numcards, replace = FALSE)
  crds2 = switch2(crds1)
  num1 = crds2$num
  num1[1] = hand[[1]][1]
  num1[1+numpl]= hand[[1]][2]
  z = 0
  off = 5 - length(unlist(board[1]))
  if (off != 5){
  for (i in (numcards-4):(numcards-off)){
    z = z +1
    num1[i] = board[[1]][z]
  }}
  suit1 = crds2$st 
  suit1[1] = hand[[2]][1]
  suit1[1+numpl] = hand[[2]][2]
  z = 0
  if (off != 5){
  for (i in (numcards-4):(numcards-off)){
    z= z+1
    suit1[i] = board[[2]][z]
  }}
  brdnum1 = num1[(numcards-4):numcards]
  brdsuit1 = suit1[(numcards-4):numcards]
  plnum1 = matrix(num1[1:(2*numpl)],ncol=2)
  plsuit1 = matrix(suit1[1:(2*numpl)],ncol=2)
  
  ## order them
  for(i in c(1:numpl)){
    if(plnum1[i,1]<plnum1[i,2]){
      a = plnum1[i,1]
      plnum1[i,1] = plnum1[i,2]
      plnum1[i,2] = a
      a = plsuit1[i,1]
      plsuit1[i,1] = plsuit1[i,2]
      plsuit1[i,2] = a
    }
  }
  b9 = list(plnum1=plnum1, plsuit1=plsuit1,
            brdnum1=brdnum1, brdsuit1=brdsuit1)
  b9
}

alt_deal(3,list(c(13,12),c(1,4)),list(c(13,12,4),c(3,2,4)))



