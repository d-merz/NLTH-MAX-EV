library(holdem)
#deals a hand based on your cards, any board cards, and the supposed range of your opponents

alt_deal_range = function(numpl,hand,board,rang){
  ## numpl is the number of players at the table
  opponents_hands = hands_to_every_nums(numpl,rang)
  #nums are the nums associated to cards that are guaranteed to be in the hand
  nums = c()
  for (i in 1:2){
    num = 13*(hand[[2]][i]-1)+hand[[1]][i]-1
    nums = append(nums,num)
  }
  for (i in 1:length(unlist(board[1]))){
    num = 13*(board[[2]][i]-1)+board[[1]][i]-1
    nums = append(nums,num)
  }
  while(any(unlist(opponents_hands)%in%nums)){
      opponents_hands = hands_to_every_nums(numpl,rang)

  }
  nums = append(nums,unlist(opponents_hands))
  numcards = 2*numpl+5
  cards = 1:52
  cards = cards[-nums]
  crds1 = sample(x = cards, size = numcards, replace = FALSE)
  op_hand = c()
  for(i in 1:2){
    for (val in 1:(numpl-1)){
      op_hand = append(op_hand,opponents_hands[[val]][i])
    }
  }
  op_hand = append(op_hand,1,numpl-1)

  crds1[2:(numpl*2)] = op_hand
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

alt_deal_range(2,list(c(14,14),c(1,4)),list(c(),c()),1)

