## SIMULATION ##

# Create pokerdeck
rank <- rep(c(1:13),4)  # Card ranks
suit <- rep(c("C","D","S","H"),each = 13)  # Card suits
pokerdeck <- data.frame(rank,suit)


# Exercise 1: Probability full house of two players -----

DrawHands <- function(deck,player=6,nDraws=5){
  # Deal hand to each player
  cards <- sample(deck, player * nDraws)
  hands <- data.frame(matrix(cards, nrow=player))
  return(hands)
}


CheckFH <- function(hand){
  # Check if one player has a full house
  x <- as.integer(hand)
  if(max(tabulate(x)) == 3)
    if(length(unique(x)) == 2) return(1)
      else return(0)
  else return(0)
}


ProbFH <- function(deck,nPL,nDrws,nSim){
  # Compute probability of any two players having a full house in one game
  #
  # Args:
  #  deck: Card deck
  #  nPL: Number of players
  #  nDrws: Number of cards drawn
  #  nSim: Number of simulations
  #
  # Returns:
  #  Number of simulations, number of occurences of two players having a full
  # house, probability of two players having a full house
  #
  # Initial value count full house
  count.fullhouse <- 0
  #
  #Open loop
  for (i in 1:nSim){
    # Deal hand to each player
    hands <- DrawHands(deck, player = nPL, nDraws = nDrws)
    # Check if any two players have a full house in one game
    countFH <- sum(apply(hands, 1, FUN = CheckFH))
     if (countFH == 2) count.fullhouse <- count.fullhouse + 1
  } # Close loop
  #
  # Calculation probability of two players having a full house
  p.fullhouse<-count.fullhouse/nSim
  #
  # Output
  out<-list(nSim,count.fullhouse,p.fullhouse)
  names(out)<-c("nSim","count.fullhouse","p.fullhouse")
  return(out)
}

# Simulation of 100/1'000/10'000/100'000 games
ProbFH(deck = pokerdeck$rank,nPL = 6,nDrws = 5, nSim = 100)

ProbFH(deck = pokerdeck$rank,nPL = 6,nDrws = 5, nSim = 1000)

ProbFH(deck = pokerdeck$rank,nPL = 6,nDrws = 5, nSim = 10000)

ProbFH(deck = pokerdeck$rank,nPL = 6,nDrws = 5, nSim = 100000)



# Exercise 2: Probability royal flush player 1 -----

ProbRF<-function (deck,nSim,nDrws) {
  # Compute probability that player 1 has a royal flush
  #
  # Args:
  #  deck: Card deck
  #  nSim: Number of simulations
  #  nDraws: Number of cards drawn
  #
  # Returns:
  #  Number of simulations, number of occurences of a royal flush, probability
  # of a royal flush
  #
  # Initial value count royal flush
  count.royalflush<-0
  #
  # Open loop
  for(i in 1:nSim) {
    # Determine card numbers for this hand
    select<-sample(nrow(deck),nDrws)
    # Select rows from the card deck for this hand
    hand<-deck[select,]
    # Check for royal flush and increment counter if royal flush occurs
    if (sum(hand[,1])==55)
      if (length(unique(hand[,2]))==1) count.royalflush<-count.royalflush+1
  } # Close loop
  #
  # Calculation probability royal flush
  p.royalflush<-count.royalflush/nSim
  #
  # Output
  out<-list(nSim,count.royalflush,p.royalflush)
  names(out)<-c("nSim","count.royalflush","p.royalflush")
  return(out)
}

# Simulation of 1'000/10'000/100'000/1'000'000 games
ProbRF(deck = pokerdeck,nSim = 1000,nDrws = 5)

ProbRF(deck = pokerdeck,nSim = 10000,nDrws = 5)

ProbRF(deck = pokerdeck,nSim = 100000,nDrws = 5)

ProbRF(deck = pokerdeck,nSim = 1000000,nDrws = 5)
