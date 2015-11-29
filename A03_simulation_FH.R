rank <- rep(c(1:13),4)
suit <- rep(c("C","D","S","H"),each = 13)
pokerdeck <- data.frame(rank,suit)

DrawHands <- function(deck,player=6,draws=5){
  cards <- sample(deck, player * draws)
  hands <- data.frame(matrix(cards, nrow=player))
  return(hands)
}

checkFH <- function(hand){
  x <- as.integer(hand)
  if(length(unique(x)) == 2 & max(tabulate(x)) == 3){
    return(1)
  }
  else{
    return(0)
  }
}

probFH <- function(deck,nPL,nSim){
  count <- 0
  for (i in 1:nSim){
    hands <- DrawHands(deck, player = nPL)
    countFH <- sum(apply(hands, 1, FUN = checkFH))
     if (countFH == 2){
      count <- count + 1
     }
  }
  prob <- count/nSim
  out <- list(nSim, count, prob )
  return(out)
}

probFH(pokerdeck$rank,nPL = 6,nSim = 100000)


