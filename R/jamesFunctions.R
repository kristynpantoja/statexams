generate_probability_rules = function(independent){
  tenths = seq(0.2, 0.9, 0.1)
  prob_a = sample(tenths, 1)
  prob_b = sample(tenths, 1)
  if(independent == 1){
    prob_a_int_b = prob_a * prob_b
  }
  else{
    check = 0
    while(check < 1.5){
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

generate_probability_rules(0)

