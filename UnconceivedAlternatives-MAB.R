# UNCONCEIVED ALTERNATIVES
# MULTI-ARMED BANDIT MODEL
# by Aydin Mohseni
# aydinmohseni@gmail.com


### GLOBAL VARIABLES
# Hypotheses
HypothesesNum <- n <- 3 # Number of hypotheses
StrengthOfPriors <- s <- 10 # Strenght of prior beliefs
HypothesesPrior <-
  matrix(
    data = c(s, s * n),
    nrow = n,
    ncol = 2,
    byrow = TRUE
  ) # Matrix of hyperparameters p,q for Beta(p,q) priors
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

# Matrix of Beta(p_i,q_i) beliefs about each hypothesis i=1,2,...,n at a given time t
BeliefMatrix <- HypothesesPrior


### FUNCTIONS
# Gittins index function (Whittle's approximation)
GittinsIndex <- function(Hypothesis) {
  p <- BeliefMatrix[Hypothesis, 1]
  q <- BeliefMatrix[Hypothesis, 2]
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
    BeliefMatrix[Hypothesis, 1] <-
      BeliefMatrix[Hypothesis, 1] + 1 # If the result was a success, then update the p parameter of the Beta(p,q) posterior of the hypothesis tested
  } else {
    BeliefMatrix[Hypothesis, 2] <-
      BeliefMatrix[Hypothesis, 2] + 1 # If the result was a failure, then update the q parameter of the Beta(p,q) posterior of the hypothesis tested
  }
  
  # Print matrix of current beliefs
  print(BeliefMatrix)
  
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
      # If we have converged, then compute final probabilies
      for (i in 1:n) {
        rowTotal <- sum(BeliefMatrix[i, ])
        BeliefMatrix[i, ] <- BeliefMatrix[i, ] / rowTotal
      }
      FinalHypothesis <- which.max(BeliefMatrix[, 1])
      # Print out the final history of learning,
      # consisting of hypotheses test and results obtained
      print("History of hypotheses tested:")
      print(HistoryHypothesis)
      print("History of results obtained:")
      print(HistoryResult)
      # Print out the final hypothesis, and posterior beliefs
      print(paste("Learning has converged to: HYPOTHESIS ",
                  FinalHypothesis,
                  sep = ""))
      print("Posterior probabilites:")
      print(BeliefMatrix)
    }
  }
  
}

### EOD