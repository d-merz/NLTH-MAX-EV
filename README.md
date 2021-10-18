# NLTH-MAX-EV
Maximize expected value in Texas Hold'em by inputting key variables into this program. 

"besthandholdem" is used to determine the winning hand after a round of Texas Hold'em. This is used to compile a list of hands ranked from best to worst in terms of how often they win with a variable number of players.

"holdem_rank_of_hands" determines what percent of time a certain hand wins in a round of poker. The more times this is run the better the results.

"holdem_ranges" turns the percent of times a hand wins into a list of the best hands from good to bad according to the number of players at the table. From here, the program takes a subset of this list of the appropriate range. Ex. A range of .2 takes the best 20% of hands from the list. Finally, random hands are drawn from this subset to be used in the final application of this program. 

"alt_deal_range" is the second to last step. Users input the number of players at a poker table, their hand, the board and their opponents range. From here, a random poker hand is dealt out according to all these variables. Through this players can see how certain hands stack up to opponent ranges.

"odds&EV" is the final step. Users simply input the number of Monte Carlo simulations, the number of players at a poker table, their hand, the board and their opponents range, and in some cases bet sizes and pot sizes. The program will then run "alt_deal_range" a number of times, keeping track of how often you win compared to your opponents. The program will report back the liklihood of you having the best hand at showdown, the pot odds of playing a certain hand, and the expected value of playing a certain hand. 

It is important for the user to run each part of the program in this order. Because functions are tied to functions from previous parts unless they have been run the program will not work. 
