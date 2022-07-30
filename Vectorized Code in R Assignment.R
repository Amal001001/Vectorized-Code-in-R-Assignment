#Vectorized Code in R Assignment

#1st Task

#set working directory
setwd("C:/Users/math1/Downloads/Bootcamps/Data_Science/tarck 02/week 07/day 02")

#read the data (.csv)
df <- read.csv("1618705129__deck.csv")

#check the dataframe
head(df)

#create players vars
number_of_players <- 2  #change it as you like

#1.Create a function to shuffle the deck
shuffle <- function(data){

  return(data[sample(1:nrow(data)), ])
}

#testing shuffle function and assign shuffled_deck to use it later
shuffled_deck <- shuffle(df)

#2.Create a deal function (5 cards for each player)
deal <- function(number_of_players, player_number){
  
  if(number_of_players * 5 > nrow(shuffled_deck)){
    print("There are not enough cards for everyone!")
    
  }else{
   
    deal_5cards <- shuffled_deck[(((player_number*5)-4): (player_number*5)), ]
    
    return(deal_5cards)
  }
  
}

#testing deal function
player1 <- deal(number_of_players, 1)

player2 <- deal(number_of_players, 2)

#let's test if the cards out of number
player11 <- deal(11, 1)
#####################################################################################

#2nd Task

#Part 1

#create a function that can deal 5, 7, or 10 cards for each player.
deal_with_chosen_number_of_cards <- function(number_of_players, no_of_deal_cards, player_number){
  
  if(number_of_players * no_of_deal_cards > nrow(shuffled_deck)){
    print("There are not enough cards for everyone!")
    
  }else{
    
    deal_cards <- shuffled_deck[(((player_number*no_of_deal_cards)-(no_of_deal_cards-1)): (player_number*no_of_deal_cards)), ]
    return(deal_cards)
  }
  
}

#testing deal_with_chosen_number_of_cards function with 7 cards deal
player1 <- deal_with_chosen_number_of_cards(number_of_players, 7, 1)

player2 <- deal_with_chosen_number_of_cards(number_of_players, 7, 2)

#let's test if the cards out of number
player8 <- deal_with_chosen_number_of_cards(8, 7, 1)
#////////////////////////////////////////////////////////////////////

#Part 2

#Create a 2-player game that calculates the value of each hand using the "Value" column
#and create an if-else statement that says if player 1 or player 2 has more points.

#holder for players number in this game
players <- 2L

#assign shuffled_deck to shuffle the cards, so I can create many games and check the if-else statement below many times
shuffled_deck <- shuffle(df)

#players holders
player_1 <- deal_with_chosen_number_of_cards(players, 10, 1)
player_2 <- deal_with_chosen_number_of_cards(players, 10, 2)

#players points   
player1_points <- sum(player_1$value)
player2_points <- sum(player_2$value)

#if-else statement to check who won the game
if(player1_points > player2_points) {
  
  print(paste("Player 1 has more points than Player 2.", "Player 1:", player1_points, "points,","Player 2:",player2_points, "points"))
  
}else if(player1_points < player2_points){
  
  print(paste("Player 2 has more points than Player 1.", "Player 1:", player1_points, "points,","Player 2:",player2_points, "points"))
  
}else{
  
  print(paste("It's a tie!", "Player 1:", player1_points, "points,","Player 2:",player2_points, "points"))
}
