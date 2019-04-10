generate_probability_rules = function(independent){
  tenths = seq(0.2, 0.9, 0.1)
  prob_a = sample(tenths, 1)
  prob_b = sample(tenths, 1)
  if(independent == 1){
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


makeQuestion_eventprob = function(type){
  if(is.null(type)){
  type = sample(1,1)
  }
  if(type == 1){
    checkques = 0
    type.2 = sample(1:3, 1)
    if(type.2 == 1){
      while(checkques == 0){
        probs = generate_probability_rules(1)
        question = paste("Events A and B are independent.The probability of A is", probs[1],
                   ".  The probability of B is ", probs[2], ".  What is the probability of A intersection B?")
        ans1 = probs[3]
        ans2 = abs(probs[1] - probs[2])
        ans3 = probs[4]
        ans4 = min(probs[1] + probs[2], 1)
        ans5 = 1 - probs[4]
        answers = c(ans1, ans2, ans3, ans4, ans5)
        if(length(unique(answers)) == 5){
          checkques = 1
        }
      }
      return(list(question, answers))
    }
    if(type.2 ==2){
      while(checkques == 0){
        probs = generate_probability_rules(1)
        question = paste("Events A and B are independent.The probability of A is", probs[1],
                       ".  The probability of B is ", probs[2], ".  What is the probability of at least A or B occuring?")
        ans1 = probs[4]
        ans2 = abs(probs[1] - probs[2])
        ans3 = probs[3]
        ans4 = min(probs[1] + probs[2], 1)
        ans5 = 1 - probs[4]
        answers = c(ans1, ans2, ans3, ans4, ans5)
        if(length(unique(answers)) == 5){
          checkques = 1
        }
      }
      return(list(question, answers))
    }
    if(type.2 ==3){
      while(checkques == 0){
        probs = generate_probability_rules(1)
        question = paste("Events A and B are independent.The probability of A is", probs[1],
                         ".  The probability of B is ", probs[2], ".  What is the probability that neither A nor B will occur?")
        ans1 = 1 - probs[4]
        ans2 = abs(probs[1] - probs[2])
        ans3 = probs[3]
        ans4 = min(probs[1] + probs[2], 1)
        ans5 = probs[4]
        answers = c(ans1, ans2, ans3, ans4, ans5)
        if(length(unique(answers)) == 5){
          checkques = 1
        }
      }
      return(list(question, answers))
    }
  }
}

test = makeQuestion_eventprob(1)
test
