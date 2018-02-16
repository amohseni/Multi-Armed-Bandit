# UNCONCEIVED ALTERNATIVES
# MULTI-ARMED BANDIT MODEL
# by Aydin Mohseni
# aydinmohseni@gmail.com


### GLOBAL VARIABLES
# Duration of simulation,
# in terms if number of scientific hypothesis tests decided
Duration <- 100

# Theories
TheoriesNum <- 3
TheoriesStrengthOfPriors <- 10
TheoriesPriors <- rep(TheoriesStrengthOfPriors, times = TheoriesNum)
# Matrix of hyperparameters r_1,r_2,...,r_t for Dir(r_1,r_2,...,r_n) prior over theories

# Hypotheses
HypothesesNum <- TheoriesNum # Number of hypotheses
HypothesesStrengthOfPriors <-
  TheoriesStrengthOfPriors # Strenght of prior beliefs
HypothesesPriors <-
  matrix(
    data = c(
      HypothesesStrengthOfPriors,
      HypothesesStrengthOfPriors * HypothesesNum
    ),
    nrow = HypothesesNum,
    ncol = 2,
    byrow = TRUE
  ) # Matrix of hyperparameters p,q for Beta(p,q) priors over hypotheses
# Ratio for discounting value of next round payoffs
FutureDiscountRatio <- b <- 0.99
# Relative advantage of optimal hypothesis
Clarity <- 0.1

# Random variables corresponding to each hypothesis
Atrue <- function() {
  # Bernoulli r.v. for the optimal hypothesis
  return(rbinom(1, 1, 0.5 + Clarity))
}
Aother <-
  function () {
    # Bernoulli r.v. for the other suboptimal hypotheses
    return(rbinom(1, 1, 0.5))
  }


### FUNCTIONS
# Gittins index function (Whittle's approximation)
GittinsIndex <- function(Hypothesis) {
  p <- HypothesesBeliefMatrix[Hypothesis, 1]
  q <- HypothesesBeliefMatrix[Hypothesis, 2]
  n <- p + q
  mu <- p / n
  c <- log(b ^ -1)
  index <-
    mu + mu * (1 - mu) / (sqrt(2 * c + n ^ (-1) * mu * (1 - mu)) + mu - 1 / 2)
  return(index)
}

# Best response function (choice of hypothesis to test) given current beliefs
BestResponse <- function() {
  GittinsVector <- sapply(seq(from = 1, to = n), GittinsIndex)
  # Choose the experiment with the highest Gittins index
  BRset <- which(GittinsVector == max(GittinsVector))
  BR <- sample(BRset, 1) # In case of a tie, randomize to tie-break
  return(BR)
}

# Conduct experiment function
Experiment <- function(HypothesisToTest) {
  if (HypothesisToTest == 1) {
    result <-
      Atrue() # If the optimal hypothesis is chosen, drawn from the corresponding R.V.
  } else {
    result <-
      Aother() # If another hypothesis is chosen, drawn from the other R.V.
  }
  return(result)
}


### LEARNING PROCESS

# Vector of Dir(r_1,r_2,...,r_n) beliefs about each theory j=1,2,...,n at a given time t
TheoriesBeliefVector <- TheoriesPriors
# Matrix in which to store the history of beliefs about each theory
TheoriesBeliefMatrix <-
  matrix(
    data = NA,
    nrow = Duration + 1,
    ncol = length(TheoriesBeliefVector),
    byrow = TRUE
  )
# Populate the first row of the theories belief matrix with the prior beliefs
TheoriesBeliefMatrix[1,] <- TheoriesBeliefVector

for (round in 1:Duration) {
  # Matrix of Beta(p_i,q_i) beliefs about each hypothesis i=1,2,...,n at a given time t
  HypothesesBeliefMatrix <- HypothesesPriors
  
  # Initiate relevant variables
  HistoryHypothesis <- c() # Vector of hypotheses tested
  HistoryResult <- c() # Vector of experimental results obtained
  Converged <-
    FALSE # Status of learning, stops on having converged to a hypothesis
  t <- 0 # Time counter
  
  # Continue experimenting with hypotheses until having converged
  while (Converged == FALSE) {
    # Increment time
    t <- t + 1
    
    ### HYPOTHESIS TEST
    # Choose a hypothesis to test
    Hypothesis <- BestResponse()
    # Conduct an experiment of that hypothesis
    Result <- Experiment(Hypothesis)
    # Update beliefs after an experiment
    if (Result == 1) {
      HypothesesBeliefMatrix[Hypothesis, 1] <-
        HypothesesBeliefMatrix[Hypothesis, 1] + 1 # If the result was a success, then update the p parameter of the Beta(p,q) posterior of the hypothesis tested
    } else {
      HypothesesBeliefMatrix[Hypothesis, 2] <-
        HypothesesBeliefMatrix[Hypothesis, 2] + 1 # If the result was a failure, then update the q parameter of the Beta(p,q) posterior of the hypothesis tested
    }
    
    # Print matrix of current beliefs
    print(HypothesesBeliefMatrix)
    
    # Store hypothesis and results in the history
    HistoryHypothesis <-
      append(HistoryHypothesis, Hypothesis, after = length(HistoryHypothesis))
    HistoryResult <-
      append(HistoryResult, Result, after = length(HistoryResult))
    
    # Determine whether learning has converged
    if (t > 30) {
      # If the process has repeated enought times, we takes this to indicate convergence
      RecentHistory <- HistoryHypothesis[(t - 20):t]
      if (length(unique(RecentHistory)) == 1 | t == 1000) {
        Converged <- TRUE
        
        # If we have CONVERGED, then compute final probabilies
        for (i in 1:n) {
          rowTotal <- sum(HypothesesBeliefMatrix[i, ])
          HypothesesBeliefMatrix[i, ] <-
            HypothesesBeliefMatrix[i, ] / rowTotal
        }
        FinalHypothesis <- which.max(HypothesesBeliefMatrix[, 1])
        
        # UPDATE BELIEF over the higher-level THEORIES
        TheoriesBeliefVector[FinalHypothesis] <- TheoriesBeliefVector[FinalHypothesis] + 1
        # Given that learning has converged to hypothesis H_k,
        # update the belief over theories Dir(p_2, ..., p_n)
        # by incrementing the value of the coressponding theory parameter p_k.
        
        # Save the current belief over the higher-level theories in the belief matrix
        TheoriesBeliefMatrix[round + 1, ] <- TheoriesBeliefVector
        
        # Print out the HISTORY of LEARNING
        # consisting of hypotheses tested and results obtained
        print("History of hypotheses tested:")
        print(HistoryHypothesis)
        print("History of results obtained:")
        print(HistoryResult)
        
        # Print out the FINAL HYPOTHESIS, and POSTERIOR BELIEFS
        print("Posterior probabilites on hypotheses:")
        print(HypothesesBeliefMatrix)
        print(paste(
          "Learning has converged to: HYPOTHESIS ",
          FinalHypothesis,
          sep = ""
        ))
      }
    }
    
  }
  
  # Compuste the final belief vector.
  TheoriesFinalBeliefMatrix <-
    matrix(
      data = NA,
      nrow = Duration + 1,
      ncol = TheoriesNum,
      byrow = TRUE
    )
  for (i in 1:(Duration + 1)) {
    TheoriesFinalBeliefMatrix[i,] <-
      TheoriesBeliefMatrix[i,] / sum(TheoriesBeliefMatrix[i,])
  }
  
  # Print the evolution of the belief in the theories
  print("History of posterior proabilities on theories:")
  print(TheoriesFinalBeliefMatrix)
  
  # Determine the final theory
  FinalBeliefVector <- TheoriesFinalBeliefMatrix[nrow(TheoriesFinalBeliefMatrix),]
  FinalBelief <- which.max(FinalBeliefVector)
  
  # Print the final theory
  print(paste("Learning has converged to: THEORY ",
              FinalBelief,
              sep = ""))
}

### EOD