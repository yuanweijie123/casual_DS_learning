library(R6)

# gameboard and decks -----------------------------------------------------

gameboard <- data.frame(
  space = 1:40, 
  title = c(
    "Go", "Mediterranean Avenue", "Community Chest", "Baltic Avenue",
    "Income Tax", "Reading Railroad", "Oriental Avenue", "Chance",
    "Vermont Avenue", "Connecticut Avenue", "Jail", "St. Charles Place",
    "Electric Company", "States Avenue", "Virginia Avenue",
    "Pennsylvania Railroad", "St. James Place", "Community Chest",
    "Tennessee Avenue", "New York Avenue", "Free Parking",
    "Kentucky Avenue", "Chance", "Indiana Avenue", "Illinois Avenue",
    "B & O Railroad", "Atlantic Avenue", "Ventnor Avenue", "Water Works",
    "Marvin Gardens", "Go to jail", "Pacific Avenue",
    "North Carolina Avenue", "Community Chest", "Pennsylvania Avenue",
    "Short Line Railroad", "Chance", "Park Place", "Luxury Tax",
    "Boardwalk"), stringsAsFactors = FALSE)
chancedeck <- data.frame(
  index = 1:15, 
  card = c(
    "Advance to Go", "Advance to Illinois Ave.",
    "Advance to St. Charles Place", "Advance token to nearest Utility",
    "Advance token to the nearest Railroad",
    "Take a ride on the Reading Railroad",
    "Take a walk on the Boardwalk", "Go to Jail", "Go Back 3 Spaces",
    "Bank pays you dividend of $50", "Get out of Jail Free",
    "Make general repairs on all your property", "Pay poor tax of $15",
    "You have been elected Chairman of the Board", 
    "Your building loan matures"), stringsAsFactors = FALSE)
communitydeck <- data.frame(
  index = 1:16, 
  card = c(
    "Advance to Go", "Go to Jail",
    "Bank error in your favor. Collect $200", "Doctor's fees Pay $50",
    "From sale of stock you get $45", "Get Out of Jail Free",
    "Grand Opera Night Opening", "Xmas Fund matures", "Income tax refund",
    "Life insurance matures. Collect $100", "Pay hospital fees of $100",
    "Pay school tax of $150", "Receive for services $25",
    "You are assessed for street repairs",
    "You have won second prize in a beauty contest",
    "You inherit $100"), stringsAsFactors = FALSE)

# RandomDice class --------------------------------------------------------

RandomDice <- R6Class(
  classname = "RandomDice",
  public = list(
    verbose = NA,
    initialize = function(verbose = FALSE){
      stopifnot(is.logical(verbose))
      self$verbose = verbose
    },
    roll = function() {
      outcome <- sample(1:6, size = 2, replace = TRUE)
      if(self$verbose){
        cat("Dice Rolled:", outcome[1], outcome[2], "\n")
      }
      outcome
    }
  )
)

# Preset Dice -------------------------------------------------------------

PresetDice <- R6Class(
  classname = "PresetDice",
  public = list(
    verbose = NA,
    preset_rolls = double(0),
    position = 1,
    initialize = function(rolls, verbose = FALSE){
      stopifnot(is.logical(verbose))
      stopifnot(is.numeric(rolls))
      self$preset_rolls = rolls
      self$verbose = verbose
    },
    roll = function(){
      if(self$position > length(self$preset_rolls)){
        stop("You have run out of predetermined dice outcomes.")
      }
      outcome <- c(self$preset_rolls[self$position], 
                   self$preset_rolls[self$position + 1])
      self$position <- self$position + 2
      if(self$verbose){
        cat("Dice Rolled:", outcome[1], outcome[2], "\n")
      }
      outcome
    }
  )
)


# Chance and Community Decks ----------------------------------------------

# This R6 class object shuffles the card deck when initialized.
# It has one method $draw(), which will draw a card from the deck.
# If all the cards have been drawn (position = deck length), then it will
# shuffle the cards again.
# The verbose option cats the card that is drawn on to the screen.
CardDeck <- R6Class(
  classname = "CardDeck",
  public = list(
    verbose = NA,
    deck_order = double(0), 
    deck = data.frame(),
    position = 1,
    initialize = function(deck, verbose = FALSE){
      stopifnot(is.data.frame(deck),
                is.numeric(deck[[1]]),
                is.character(deck[[2]]))
      self$deck_order <- sample(length(deck[[1]]))
      self$verbose <- verbose
      self$deck <- deck
    },
    draw = function(){
      if(self$position > length(self$deck_order)){
        # if we run out of cards, shuffle deck
        # and reset the position to 1
        if(self$verbose){
          cat("Shuffling deck.\n")
        }
        self$deck_order <- sample(length(self$deck[[1]]))
        self$position <- 1
      }
      outcome <- c(self$deck_order[self$position]) # outcome is the value at position
      self$position <- self$position + 1 # advance the position by 1
      if(self$verbose){
        cat("Card:", self$deck[outcome, 2], "\n")
      }
      outcome # return the outcome
    }
  )
)


# R6 Class SpaceTracker ---------------------------------------------------
# Do not change this code

SpaceTracker <- R6Class(
  classname = "SpaceTracker",
  public = list(
    counts = rep(0, 40),
    verbose = TRUE,
    tally = function(x){
      self$counts[x] <- self$counts[x] + 1
      if(self$verbose){
        cat("Added tally to ", x, ": ", gameboard$title[x], ".\n", sep = "")
      }
    },
    initialize = function(verbose){
      self$verbose <- verbose
    }
  )
)

# R6 Class Player ---------------------------------------------------------
## You'll need to expand on this

Player <- R6Class(
  classname = "Player",
  public = list(
    pos = 1,
    verbose = TRUE,
    inJail = FALSE,
    jailRound = 0,
    move_fwd = function(n){
      self$pos <- self$pos + n
      if(self$pos > 40){
        self$pos <- self$pos - 40
      }
      if(self$verbose){
        cat("Player is now at:", self$pos, "\n")
      }
      if(self$pos == 31){
        self.send_to_jail()
      }
    },
    move_bwd = function(n){
      self$pos <- self$pos - n
      if(self$pos < 0){
        self$pos <- self$pos + 40
      }
      if(self$verbose){
        cat("Player is now at:", self$pos, "\n")
      }
      if(self$pos == 31){
        self$send_in_jail()
      }
    },
    advance_pos = function(p){
      self$pos <- p
      if(self$verbose){
        cat("Player moves to ", self$pos, ": ", gameboard$title[self$pos], "\n", sep = "")
      }
    },
    send_in_jail = function(){
      self$pos <- 11
      self$inJail <- TRUE
      self$jailRound <- 1
    },
    initialize = function(verbose = FALSE, pos = 1) {
      self$verbose <- verbose
      self$pos <- pos
    }
  )
)


# VERY BASIC turn taking example ------------------------------------------
# You will need to expand this
# You can write helper function if you want

find_nearest_utility <- function(cur_pos){
  electric <- which(gameboard$title == "Electric Company")
  water <- which(gameboard$title == "Water Works")
  if(cur_pos <= electric){
    return(electric)
  } else if (cur_pos <= water){
    return(water)
  } else {
    return(electric)
  }
}

find_nearest_railroad <- function(cur_pos){
  rr <- which(gameboard$title == "Reading Railroad")
  pr <- which(gameboard$title == "Pennsylvania Railroad")
  bor <- which(gameboard$title == "B & O Railroad")
  sr <- which(gameboard$title == "Short Line Railroad")
  if(cur_pos <= rr) {
    return(rr)
  } else if(cur_pos <= pr) {
    return(pr)
  } else if (cur_pos <= bor) {
    return(bor)
  } else if (cur_pos <= sr) {
    return(sr)
  } else {
    return(rr)
  }
}

take_card <- function(player, spacetracker, chance, community) {
  if(gameboard$title[player$pos] == "Chance"){
    if (player$verbose) {
      cat("Draw a Chance card. \n")
    }
    chance_card <- chance$deck[chance$draw(), 2]
    if(chance_card == "Advance to Go"){
      player$advance_pos(which(gameboard$title == "Go"))
      spacetracker$tally(player$pos)
    }else if(chance_card == "Advance to Illinois Ave."){
      player$advance_pos(which(gameboard$title == "Illinois Avenue"))
      spacetracker$tally(player$pos)
    }else if(chance_card == "Advance to St. Charles Place"){
      player$advance_pos(which(gameboard$title == "St. Charles Place"))
      spacetracker$tally(player$pos)
    }else if(chance_card == "Advance token to nearest Utility"){
      nearest_util <- find_nearest_utility(player$pos)
      player$advance_pos(nearest_util)
      spacetracker$tally(player$pos)
    }else if(chance_card == "Advance token to the nearest Railroad"){
      nearest_railroad <- find_nearest_railroad(player$pos)
      player$advance_pos(nearest_railroad)
      spacetracker$tally(player$pos)
    }else if(chance_card == "Take a ride on the Reading Railroad") {
      player$advance_pos(which(gameboard$title == "Reading Railroad"))
      spacetracker$tally(player$pos)
    }else if(chance_card == "Take a walk on the Boardwalk") {
      player$advance_pos(which(gameboard$title == "Boardwalk"))
      spacetracker$tally(player$pos)
    }else if(chance_card == "Go to Jail") {
      player$send_in_jail()
      spacetracker$tally(player$pos)
    }else if(chance_card == "Go Back 3 Spaces"){
      player$move_bwd(3)
      spacetracker$tally(player$pos)
    }else{
      if(player$verbose){
        cat("Player ends on Chance. No movement. \n")
      }
    }
  } else if(gameboard$title[player$pos] == "Community Chest") {
    if (player$verbose) {
      cat("Draw a Community Chest card. \n")
    }
    community_card <- community$deck[community$draw(), 2]
    if(community_card == "Advance to Go"){
      player$advance_pos(which(gameboard$title == "Go"))
      spacetracker$tally(player$pos)
    }else if(community_card == "Go to Jail") {
      player$send_in_jail()
      spacetracker$tally(player$pos)
    }else{
      if(player$verbose){
        cat("Player ends on Community Chest. No movement. \n")
      }
    }
  }
}


take_turn <- function(player, spacetracker){
  dice_rolls <- dice$roll()
  player$move_fwd(sum(dice_rolls))
  spacetracker$tally(player$pos)
  take_card(player, spacetracker, chance, community)
  
  if(dice_rolls[1] == dice_rolls[2]){
    if(player$verbose){
      cat("\nPlayer rolled doubles, so they take another turn. \n")
    }
    dice_rolls_second <- dice$roll()
    player$move_fwd(sum(dice_rolls_second))
    spacetracker$tally(player$pos)
    take_card(player, spacetracker, chance, community)
    
    if(dice_rolls_second[1]==dice_rolls_second[2]){
      if(player$verbose){
        cat("\nPlayer rolled doubles, so they take another turn. \n")
      }
      dice_rolls_third <- dice$roll()
      player$move_fwd(sum(dice_rolls_third))
      spacetracker$tally(player$pos)
      take_card(player, spacetracker, chance, community)
      
      if (dice_rolls_third[1] == dice_rolls_third[2]){
        player$send_in_jail()
        spacetracker$tally(player$pos)
      } else {
        player$move_fwd(sum(dice_rolls_third))
        spacetracker$tally(player$pos)
        take_card(player, spacetracker, chance, community)
      }
    }
  }
}


