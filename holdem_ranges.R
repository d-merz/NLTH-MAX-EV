library(tictoc)
#this program converts the list of probabilities of hand winning to a range
tic()
two_player_odds = hand_odds(1000,2)
toc()
tic()
three_player_odds = hand_odds(1000,3)
toc()
four_player_odds = hand_odds(10000,4)
five_player_odds = hand_odds(1000,5)
tic()
six_player_odds = hand_odds(1000,6)
toc()
seven_player_odds = hand_odds(1000,7)
eight_player_odds = hand_odds(1000,8)
nine_player_odds = hand_odds(1000,9)
ten_player_odds = hand_odds(1000,10)
#more sims the better!

#Creates a range of cards for the # of players and inputted range. 
holdem_ranges = function(numpl,range){
  if (numpl ==2){
    odds = two_player_odds
  }else if (numpl ==3){
    odds = three_player_odds
  }else if (numpl ==4){
    odds = four_player_odds
  }else if (numpl ==5){
    odds = five_player_odds
  }else if (numpl ==6){
    odds = six_player_odds
  }else if (numpl ==7){
    odds = seven_player_odds
  }else if (numpl ==8){
    odds = eight_player_odds
  }else if (numpl ==9){
    odds = nine_player_odds
  }else if (numpl ==10){
    odds = ten_player_odds
  }else {
    return(print("enter between 2-10"))
  }
  if (range == 1){
    return(sort(unlist(odds),decreasing=TRUE))
  }else{
    total_cards = round(range*169, digits = 0)
    vec_of_odds = unlist(odds)
    vec_of_odds = sort(vec_of_odds, decreasing = TRUE)
    cards_in_range = vec_of_odds[1:total_cards]
  }
  return(cards_in_range)
}
holdem_ranges(2,.05)


#4 combos of XY suited, 12 combos of XY unsuited, 6 combos of XX
#converting a range to a list of set of numbers 1:52
#selecting random hands in the range
hands_to_every_nums = function(numpl,range){
  opponent_nums = list()
  hands_in_range = names(holdem_ranges(6,1))
  opponent_hands = hands_in_range
  for (i in 1:(169*range)){
    the_cards = c(strsplit(opponent_hands[i], ".", fixed = TRUE)[[1]][1],
                  strsplit(opponent_hands[i], ".", fixed = TRUE)[[1]][2:3])
    nums = c()
    if (the_cards[1] == the_cards[2]){
      pocket = as.numeric(the_cards[1])
      nums = list(c(pocket-1,pocket+12),
                  c(pocket+25,pocket+38),
                  c(pocket-1,pocket+38),
                  c(pocket+25,pocket+12),
                  c(pocket-1,pocket+25),
                  c(pocket+12,pocket+38))
      opponent_num = nums
      opponent_nums = append(opponent_nums,opponent_num)
    }else if (the_cards[3] == "s"){
      card1 = as.numeric(the_cards[1])
      card2 = as.numeric(the_cards[2])
      suited_hands = list(c(card1-1,card2-1),c(card1+12,card2+12),c(card1+25,card2+25),c(card1+38,card2+38))
      opponent_num = suited_hands
      opponent_nums = append(opponent_nums,opponent_num)
    }else{
      card1 = as.numeric(the_cards[1])
      card2 = as.numeric(the_cards[2])
      offsuit_hands = list(c(card1-1 ,card2+12),c(card1-1 ,card2+25),c(card1-1 ,card2+38),
                           c(card1+12,card2-1 ),c(card1+12,card2+25),c(card1+12,card2+38),
                           c(card1+25,card2-1 ),c(card1+25,card2+12),c(card1+25,card2+38),
                           c(card1+38,card2-1 ),c(card1+38,card2+25),c(card1+38,card2+12))
      opponent_num = offsuit_hands
      opponent_nums = append(opponent_nums,opponent_num)
    }
  }

  hands = list()
  for (x in 1:(numpl-1)){
    hand = unlist(sample(opponent_nums,1))
    nums_out = c()
    for(i in 1:length(opponent_nums)){
      if (is.na(any(match(hand,opponent_nums[[i]])))){
        next
      }else{
        nums_out = append(nums_out,i)
      }
    }
    opponent_nums[nums_out] = NULL
    hands = append(hands,list(hand))
  }
  
  return(hands)
  
}
x = hands_to_every_nums(6,.1)


#returns the associated values for hands for the number of players within that range
range_to_nums = function(numpl,range){
  opponent_nums = list()
  hands_in_range = names(holdem_ranges(numpl,range))
  opponent_hands = sample(hands_in_range,(numpl-1))
  
  for (i in 1:(numpl-1)){
    the_cards = c(strsplit(opponent_hands[i], ".", fixed = TRUE)[[1]][1],
                  strsplit(opponent_hands[i], ".", fixed = TRUE)[[1]][2:3])
    nums = c()
    if (the_cards[1] == the_cards[2]){
      pocket = as.numeric(the_cards[1])
      nums = c(pocket-1,pocket+12,pocket+25,pocket+38)
      opponent_num = sort(sample(nums,2, replace = FALSE),decreasing = TRUE)
      
      while(is.element(opponent_num[1],unlist(opponent_nums))|is.element(opponent_num[2],unlist(opponent_nums))){
        opponent_hand = sample(hands_in_range,1)
        the_cards = c(strsplit(opponent_hand, ".", fixed = TRUE)[[1]][1],
                      strsplit(opponent_hand, ".", fixed = TRUE)[[1]][2:3])
        if (the_cards[1] == the_cards[2]){
          pocket = as.numeric(the_cards[1])
          nums = c(pocket-1,pocket+12,pocket+25,pocket+38)
          opponent_num = sample(nums,2, replace = FALSE)
        }
      }
      opponent_nums = append(opponent_nums,list(opponent_num))
    }else if (the_cards[3] == "s"){
      card1 = as.numeric(the_cards[1])
      card2 = as.numeric(the_cards[2])
      suited_hands = list(c(card1-1,card2-1),c(card1+12,card2+12),c(card1+25,card2+25),c(card1+38,card2+38))
      opponent_num = sample(suited_hands,1)
      while(any(unlist(opponent_num)%in%unlist(opponent_nums)) == TRUE){
        opponent_hand = sample(hands_in_range,1)
        the_cards = c(strsplit(opponent_hand, ".", fixed = TRUE)[[1]][1],
                      strsplit(opponent_hand, ".", fixed = TRUE)[[1]][2:3])
        if (the_cards[1] == the_cards[2]){
          next
        }else if (the_cards[3] == "s"){
          card1 = as.numeric(the_cards[1])
          card2 = as.numeric(the_cards[2])
          suited_hands = list(c(card1-1,card2-1),c(card1+12,card2+12),c(card1+25,card2+25),c(card1+38,card2+38))
          opponent_num = sample(suited_hands,1)
          
        }
      }
      opponent_nums = append(opponent_nums,opponent_num)
    }else{
      card1 = as.numeric(the_cards[1])
      card2 = as.numeric(the_cards[2])
      offsuit_hands = list(c(card1-1 ,card2+12),c(card1-1 ,card2+25),c(card1-1 ,card2+38),
                           c(card1+12,card2-1 ),c(card1+12,card2+25),c(card1+12,card2+38),
                           c(card1+25,card2-1 ),c(card1+25,card2+12),c(card1+25,card2+38),
                           c(card1+38,card2-1 ),c(card1+38,card2+25),c(card1+38,card2+25))
      opponent_num = sample(offsuit_hands,1)
      while(any(unlist(opponent_num)%in%unlist(opponent_nums)) == TRUE){
        opponent_hand = sample(hands_in_range,1)
        the_cards = c(strsplit(opponent_hand, ".", fixed = TRUE)[[1]][1],
                      strsplit(opponent_hand, ".", fixed = TRUE)[[1]][2:3])
        if (the_cards[1] == the_cards[2]){
          next
        }else if (the_cards[3]=="o"){
          card1 = as.numeric(the_cards[1])
          card2 = as.numeric(the_cards[2])
          offsuit_hands = list(c(card1-1 ,card2+12),c(card1-1 ,card2+25),c(card1-1 ,card2+38),
                               c(card1+12,card2-1 ),c(card1+12,card2+25),c(card1+12,card2+38),
                               c(card1+25,card2-1 ),c(card1+25,card2+12),c(card1+25,card2+38),
                               c(card1+38,card2-1 ),c(card1+38,card2+25),c(card1+38,card2+25))
          opponent_num = sample(offsuit_hands,1)
          
        }
        
      }
      opponent_nums = append(opponent_nums,opponent_num)
    }
  }
  
  return(opponent_nums)
  
}
range_to_nums(6,.1)

