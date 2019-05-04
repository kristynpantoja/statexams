generate_probability_rules = function(special = NULL){
  if(special == 2){
    tenths = seq(0.1, 0.8, 0.1)
    prob_a = sample(tenths, 1)
    prob_a_un_b = sample(seq(prob_a, 0.9, 0.1))
    prob_b = prob_a_un_b - prob_a
    prob_a_int_b = 0
    return(c(prob_a, prob_b, prob_a_int_b, prob_a_un_b))
  }
  tenths = seq(0.2, 0.9, 0.1)
  prob_a = sample(tenths, 1)
  prob_b = sample(tenths, 1)
   if(special == 1){
    prob_a_int_b = prob_a * prob_b
  }
  else{
    check = 0
    while(check < 1.5){ #makes sure rounding doesn't give a bad value and events are dependent
      check = 0
    prob_a_int_b = round(runif(1, max(0.01, prob_a + prob_b - 1), min(prob_a, prob_b)), digits = 2)
    if(prob_a_int_b != prob_a * prob_b){
      check = check + 1
      }
    if(prob_a_int_b >= max(0.01, prob_a + prob_b - 1) & prob_a_int_b < min(prob_a, prob_b)){
      check = check + 1
      }
    }
  }
  prob_a_un_b = prob_a + prob_b - prob_a_int_b
  return(c(prob_a, prob_b, prob_a_int_b, prob_a_un_b))
}


#' Title Generate probability question
#'
#' @param type Determines the type of probability question.  Valid inputs: 1, 2, 3
#'
#' @return Returns a probability question
#' @export
#'
#' @examples makeQA_eventprob(type = 1)
#' [1] "Events A and B are independent.The probability of A is 0.9.  The probability of B is 0.3.  What is the probability that neither A nor B will occur?"
#'[2] "0.0699999999999998"
#'[3] "0.6"
#'[4] "0.27"
#'[5] "1.2"
#'[6] "0.93"
makeQA_eventprob = function(type = NULL){
  #type = 1 gives independence.  2 gives no assumptions.  3 gives mutually exclusive
  if(is.null(type)){
  type = sample(1:2,1)
  }
  if(type == 1){
    checkques = 0
    type.2 = sample(1:3, 1)
    if(type.2 == 1){
      while(checkques == 0){
        probs = generate_probability_rules(1)
        question = paste("Events A and B are independent.The probability of A is ", probs[1],
                   ".  The probability of B is ", probs[2], ".  What is the probability of A intersection B?", sep = "")
        ans1 = probs[3]
        ans2 = abs(probs[1] - probs[2])
        ans3 = probs[4]
        ans4 = probs[1] + probs[2]
        ans5 = 1 - probs[4]
        answers = c(ans1, ans2, ans3, ans4, ans5)
        if(length(unique(answers)) == 5){
          checkques = 1
        }
      }
      return(c(question, answers))
    }
    if(type.2 ==2){
      while(checkques == 0){
        probs = generate_probability_rules(1)
        question = paste("Events A and B are independent.The probability of A is ", probs[1],
                       ".  The probability of B is ", probs[2], ".  What is the probability of at least A or B occuring?", sep = "")
        ans1 = probs[4]
        ans2 = abs(probs[1] - probs[2])
        ans3 = probs[3]
        ans4 = probs[1] + probs[2]
        ans5 = 1 - probs[4]
        answers = c(ans1, ans2, ans3, ans4, ans5)
        if(length(unique(answers)) == 5){
          checkques = 1
        }
      }
      return(c(question, answers))
    }
    if(type.2 ==3){
      while(checkques == 0){
        probs = generate_probability_rules(1)
        question = paste("Events A and B are independent.The probability of A is ", probs[1],
                         ".  The probability of B is ", probs[2], ".  What is the probability that neither A nor B will occur?", sep = "")
        ans1 = 1 - probs[4]
        ans2 = abs(probs[1] - probs[2])
        ans3 = probs[3]
        ans4 = probs[1] + probs[2]
        ans5 = probs[4]
        answers = c(ans1, ans2, ans3, ans4, ans5)
        if(length(unique(answers)) == 5){
          checkques = 1
        }
      }
      return(c(question, answers))
    }
  }
  if(type == 2){
    checkques = 0
    while(checkques == 0){
      probs = generate_probability_rules(0)
      question = paste("The probability of A is ", probs[1],
            ".  The probability of B is ", probs[2],
            ".  The probability of A and B both occuring is ", probs[3],
            ".  What is the probability of A or B occurring?", sep = "")
      ans1 = probs[4]
      ans2 = abs(probs[1] - probs[2])
      ans3 = probs[3]
      ans4 = probs[1] + probs[2]
      ans5 = 1 - probs[4]
      answers = c(ans1, ans2, ans3, ans4, ans5)
      if(length(unique(answers)) == 5){
        checkques = 1
      }
    }
    return(c(question, answers))
  }
  if(type == 3){
    checkques = 0
    while(checkques == 0){
      probs = generate_probability_rules(2)
      question = paste("The probability of A is ", probs[1],
                       ".  The probability of B is ", probs[2],
                       ".  A and B are mutually exclusive.  What is the probability of either A or B occurring?", sep = "")
      ans1 = probs[1] + probs[2]
      ans2 = abs(probs[1] - probs[2])
      ans3 = probs[3]
      ans4 = probs[4]
      ans5 = 1 - probs[4]
      answers = c(ans1, ans2, ans3, ans4, ans5)
      if(length(unique(answers)) == 5){
        checkques = 1
      }
    }
    return(c(question, answers))
  }
}


makeQA_ExpectedValue = function(prob = NULL, values = NULL){
  if(is.null(prob) & is.null(values)){
    n = sample(3:5, 1)
  }
  else if(is.null(prob) | is.null(values)){
    n1 = length(prob)
    n2 = length(values)
    n = n1 + n2
  }
  else{
    if(length(prob) != length(values)){
      stop("prob and values lengths do not agree")
    }
    n = length(prob)
  }
  if(!is.null(prob) & sum(prob) != 1){
    stop("prob must sum to 1 or be null")
  }
  if(!is.null(prob) & (sum(prob < 0) != 0)){
    stop("prob must be positive")
  }
  if(is.null(values)){
    values = sample(1:20, n)
    values = sort(values)
  }
  if(is.null(prob)){
    checkques = 0
    while(checkques == 0){
      prob = rep(0, n)
      sticks = sample((25:90)/100, n-1 , replace = T)
      remainprob = 100
      for(i in 1:(n-1)){
        prob[i] = floor(remainprob * sticks[i])
        remainprob = remainprob - prob[i]
      }
      prob[n] = remainprob
      prob = prob/100
      question = ""
      for(i in 1:n){
      question = paste(question, "f(", values[i], ") = ", prob[i], ".  ", sep = "")
      }
      question = paste(question, "What is the expected value of X?", sep = "")
      ans1 = round(sum(values * prob), 3)
      ans2 = round(sum(values), 3)
      ans3 = round(sum(values + prob), 3)
      ans4 = round(prod(values + prob), 3)
      ans5 = round(mean(values))
      answers = c(ans1, ans2, ans3, ans4, ans5)
      if(length(unique(answers)) == 5){
        checkques = 1
        }
    }
    return(c(question,answers))
    }
  else{
    question = ""
    for(i in 1:n){
      question = paste(question, "f(", values[i], ") = ", prob[i], ".  ", sep = "")
    }
    question = paste(question, "What is the expected value of X?", sep = "")
    ans1 = round(sum(values * prob), 3) #right
    ans2 = round(sum(values), 3) #only summing the values
    ans3 = round(sum(values + prob), 3) #adding values and prob
    ans4 = round(prod(values + prob), 3) #product of (values + prob)
    ans5 = round(mean(values)) #ignores probability and treats like sample mean
    answers = c(ans1, ans2, ans3, ans4, ans5)
    return(c(question,answers))
  }
}




makeQA_HypothesisTest = function(type = NULL, level = 0.05, Xbar = NULL, sigma = NULL, n = NULL, mu_0 = NULL){
  if(level >0.5){
    stop("Level must be no greater than 0.5")
  }
  if(is.null(sigma)){
    sigma = sample(seq(0, 5, 0.1), 1)
  }
  if(is.null(n)){
    n = sample(30:100, 1)
  }
  if(is.null(type)){
    type = sample(0:2, 1)
  }
  if(is.null(mu_0)){
    mu_0 = round(sample(seq(-10, 10, 0.1),1),2)
  }
  if(type == 0){ #1 sided, H_a <
    if(is.null(Xbar)){
     reject = sample(c(0,1), 1)
       error = runif(1, -.2, .2)
      Xbar = round(mu_0 - (qt(1 - level, n-1) + error + 0.5*reject)*sigma/n, 3)
      }
    else{
        reject = Xbar <= mu_0 - qt(1 - level, n-1)*sigma/n
      }

    question = paste("H_0: mu = ", mu_0,"; H_a = mu < ", mu_0,".  You take ", n," samples and find that the sample mean is ", Xbar,
                   ".  The sample standard deviation is ", sigma, ". Do you reject the null hypothesis at the ", level, " level?", sep = "")
    yesno = c("no", "yes")
    ans1 = yesno[reject + 1]
    ans2 = yesno[-(reject + 1)]
    answers = c(ans1, ans2)
    return(c(question, answers))
  }
  if(type == 1){ #1 sided, H_a >
    if(is.null(Xbar)){
      reject = sample(c(0,1), 1)
      error = runif(1, -.2, .2)
      Xbar = round(mu_0 + (qt(1 - level, n-1) + error + 0.5*reject)*sigma/n, 3)
    }
    else{
      reject = Xbar >= mu_0 - qt(1 - level, n-1)*sigma/n
    }

    question = paste("H_0: mu = ", mu_0,"; H_a = mu > ", mu_0,".  You take ", n," samples and find that the sample mean is ", Xbar,
                     ".  The sample standard deviation is ", sigma, ". Do you reject the null hypothesis at the ", level, " level?", sep = "")
    yesno = c("no", "yes")
    ans1 = yesno[reject + 1]
    ans2 = yesno[-(reject + 1)]
    answers = c(ans1, ans2)
    return(c(question, answers))
  }
  if(type ==2){ #2 sided
    if(is.null(Xbar)){
      direction = sample(c(-1,1), 1)
      reject = sample(c(0,1), 1)
      error = runif(1, -.2, .2)
      Xbar = round(mu_0 + direction*(qt(1 - 0.5*level, n-1) + error + 0.5*reject)*sigma/n, 3)
    }
    else{
      reject = Xbar >= mu_0 - qt(1 - 0.5*level, n-1)*sigma/n | Xbar <= mu_0 - qt(1 - 0.5*level, n-1)*sigma/n
    }
    question = paste("H_0: mu = ", mu_0,"; H_a = mu is not equal to ", mu_0,".  You take ", n," samples and find that the sample mean is ", Xbar,
                     ".  The sample standard deviation is ", sigma, ". Do you reject the null hypothesis at the ", level, " level?", sep = "")
    yesno = c("no", "yes")
    ans1 = yesno[reject + 1]
    ans2 = yesno[-(reject + 1)]
    answers = c(ans1, ans2)
    return(c(question, answers))
  }
}

makeQA_ConditionalProbability = function(proportion = NULL, condprob = NULL, type = NULL){
  if(is.null(proportion) && is.null(condprob)){
    proportion = rep(0, 3)
    proportion[1] = sample(seq(0.1, 0.7, 0.1), 1)
    proportion[2] = sample(seq(0.1, 1 - proportion[1] - 0.2, 0.1), 1)
    proportion[3] = 1 - proportion[1] - proportion[2]
  }
  if(is.null(proportion)){
    n = length(condprob)
    proportion = rep(0, n)
    proportion[1] = sample(seq(0.1, 0.4, 0.1), 1)
    probsum = proportion[1]
    for(i in 2:{n-1}){
      proportion[i] = sample(seq(0.1, 1 - probsum - 0.1*{3-i}, 0.1), 1)
      probsum = probsum + proportion[i]
    }
    proportion[n] = 1 - probsum
  }
  if(is.null(condprob)){
    condprob = round(runif(length(proportion), 0.2, 0.95), 2)
  }
  if(sum(condprob > 1) !=0 || sum(condprob < 0) !=0){
    stop("Conditional probability outside valid domain.")
  }
  if(length(proportion) != 3 || length(condprob) != 3){
    stop("proportion or conditional probability vector must be null or 3")
  }
  if(is.null(type)){
    type = sample(1:2, 1)
  }
  if(type == 1){
    question = paste("A company buys resistors from vendor companies A, B, and C.  ",
                     100*proportion[1], "% come from company A  ",
                     100*proportion[2], "% come from comapny B, and ",
                     100*proportion[3], "% come from company C.  If a resistor comes from company A, there is a ",
                     100*condprob[1], "% chance that it is within tolerance.  If a resistor comes from company B, there is a ",
                     100*condprob[2], "% chance that it is within tolerance.  If a resistor comes from company C, there is a ",
                     100*condprob[3], "% chance that it is within tolerance.  If a resistor is randomly sampled, what is the probability it is within tolerance?", sep = "")

    ans1 = round(sum(proportion*condprob), 3)
    ans2 = round(mean(condprob), 3)
    ans3 = round(mean(proportion), 3)
    ans4 = sum(condprob)
    ans5 = round(mean(proportion)*mean(condprob), 3)
    answers = c(ans1, ans2, ans3, ans4, ans5)
    return(c(question, answers))
  }
  if(type == 2){
    question = paste("A company buys resistors from vendor companies A, B, and C.  ",
                     100*proportion[1], "% come from company A  ",
                     100*proportion[2], "% come from comapny B, and ",
                     100*proportion[3], "% come from company C.  If a resistor comes from company A, there is a ",
                     100*condprob[1], "% chance that it is within tolerance.  If a resistor comes from company B, there is a ",
                     100*condprob[2], "% chance that it is within tolerance.  If a resistor comes from company C, there is a ",
                     100*condprob[3], "% chance that it is within tolerance.  If a randomly selected resistor is within tolerance, what is the probability it came from company A?", sep = "")

    ans1 = round(proportion[1]*condprob[1] / sum(proportion*condprob), 3)
    ans2 = condprob[1]
    ans3 = sum(proportion*condprob)
    ans4 = round(mean(condprob), 3)
    ans5 = sum(condprob)
    answers = c(ans1, ans2, ans3, ans4, ans5)
    return(c(question, answers))
  }
}
