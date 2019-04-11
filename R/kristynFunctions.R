
# --- Functions for Importing/Exporting to/from a .txt File --- #

import_question = function(file){
  # 1. read in the file. it has 2 parts: the name of the file, and the text inside the file.
  readin_question = readtext::readtext(file, dvsep = "\t")
  # 2. get the long string of question and answers components, i.e. the text inside the file.
  contents_as_string = readin_question[1, 2]
  # 3. split the components into question and corresponding answers:
  #     output is it's a list (with one element)
  question_contents_as_list = strsplit(contents_as_string, split = "\\tQ|\\tA")
  # 4. grab the single element in the list, which is the vector
  #     whose elements are the question followed by the answers
  question_contents = question_contents_as_list[[1]]
  # [TO DO] at some point, get rid of \n's
  return(question_contents)
}

# [TO DO] write import_questions() function
import_questions = function(file){
  # make a list of questions
  list_of_questions_contents = list()
  # something goes here, using import_question()
  return(list_of_questions_contents)
}


# export a string as contents of a txt file called filename (should have .txt extension)
# note: it interprets special characters (e.g. "\n"), unless you put an extra backslash in front.
export_txt = function(contents, filename){
  file_connection  = file(filename)
  writeLines(contents, file_connection)
  close(file_connection)
  return("exported txt file")
}


# --- Functions for Creating Questions for Normal Distribution Calculations --- #

makeQuestion_normal = function(variable = "X", mean = 0, sd = 1, interval, tail = NULL){
  # Cleaning up arguments and error-catching
  if(sd <= 0) stop("standard deviation should be a positive number")
  if(length(interval) != 1 & length(interval) != 2) stop("interval must be a vector of length 1 or 2")
  if(length(interval) == 2) if(interval[1] >= interval[2]) stop("for interval = (a, b), b must be greater than a")
  if(length(interval) == 1 & is.null(tail)) stop("Need to specify type of tail probability")
  if(!is.null(tail)){
    if(tail == "l" | tail == "left") tail = 0
    else if(tail == "r" | tail == "right") tail = 1
    else stop("Need to specify a valid type of tail probability: left or right")
  }
  if(is.null(tail)) tail = 2

  component1 = " is normally distributed with mean "
  component2 = " and standard deviation "
  component3 = ". What is the probability that "
  context_component1 = paste(variable, component1, mean, component2, sd, component3, sep = "")

  context_component2 = ""
  # Question for Left-tail probabilities
  if(length(interval) == 1 & tail == 0){
    component4 = " is less than "
    context_component2 = paste(variable, base_component4, interval, "?", sep = "")
  }

  # Question for Right-tail probabilities
  if(length(interval) == 1 & tail == 1){
    component4 = " is greater than "
    context_component2 = paste(variable, base_component4, interval, "?", sep = "")
  }

  # Question for interval probabilities
  if(length(interval) == 2){
    component4 = " is between "
    component5 = " and "
    context_component2 = paste(variable, base_component4, interval[1],
                               base_component5, interval[2], "?", sep = "")
  }

  question = paste(context_component1, context_component2, sep = "")
  return(question)
}

makeAnswers_normal = function(variable = "X", mean = 0, sd = 1, interval, tail = NULL){
  # Cleaning up arguments and error-catching
  if(sd <= 0) stop("standard deviation should be a positive number")
  if(length(interval) != 1 & length(interval) != 2) stop("interval must be a vector of length 1 or 2")
  if(length(interval) == 2) if(interval[1] >= interval[2]) stop("for interval = (a, b), b must be greater than a")
  if(length(interval) == 1 & is.null(tail)) stop("Need to specify type of tail probability")
  if(!is.null(tail)){
    if(tail == "l" | tail == "left") tail = 0
    else if(tail == "r" | tail == "right") tail = 1
    else stop("Need to specify a valid type of tail probability: left or right")
  }
  if(is.null(tail)) tail = 2

  # Create answer choices
  answers = rep(NA, 4)
  labeled_answers = c("correct_answer" = NA, "incorrect_answer1" = NA, "incorrect_answer2" = NA, "incorrect_answer3" = NA)

  if(length(interval) == 1){
    # left tail probability
    answers[1] = pnorm(q = interval[1], mean, sd)
    # right tail probability
    answers[2] = pnorm(q = interval[1], mean, sd, lower.tail = FALSE)
    # z-score
    answers[3] = (interval[1] - mean) / sd
    # negative z-score
    answers[4] = - answers[3]
  }
  if(length(interval) == 2){
    # correct answer: P(X < b) - P(X < a)
    labeled_answers[1] = pnorm(q = interval[2], mean, sd) - pnorm(q = interval[1], mean, sd)
    # incorrect answers:
    # positive difference between z-scores
    labeled_answers[2] = abs((interval[1] - mean) / sd - (interval[2] - mean) / sd)
    # P(X < a) - P(X > b)
    labeled_answers[3] = pnorm(q = interval[1], mean, sd) - pnorm(q = interval[2], mean, sd, lower.tail = FALSE)
    # sum of tail probabilities
    labeled_answers[4] = pnorm(q = interval[1], mean, sd) + pnorm(q = interval[2], mean, sd, lower.tail = FALSE)
  }

  # Output list where 1st element is correct answer

  # For Left-tail probabilities
  if(length(interval) == 1 & tail == 0){
    labeled_answers[1] = answers[1]
    leftover_answers = answers[-1]
    for(i in 1:(length(leftover_answers))){
      labeled_answers[i + 1] = answers[i]
    }
  }

  # For Right-tail probabilities
  if(length(interval) == 1 & tail == 1){
    labeled_answers[1] = answers[2]
    leftover_answers = answers[-2]
    for(i in 1:(length(leftover_answers))){
      labeled_answers[i + 1] = answers[i]
    }
  }

  return(labeled_answers)
}

makeQA_normal = function(variable = "X", mean = 0, sd = 1, interval, tail = NULL){
  question = makeQuestion_normal(variable = "X", mean = 0, sd = 1, interval, tail = NULL)
  answers = makeAnswers_normal(variable = "X", mean = 0, sd = 1, interval, tail = NULL)
  return(list(question, answers))
}



# --- Functions for Creating Questions for Confidence Intervals for Proportions --- #

makeQuestion_CIprop = function(n, numPositive, C = 0.95, population = NULL, individuals = "individuals",
                               question = NULL, answer = NULL){
  # error checking
  # check if n, numPositive, population (if given) are all integers
  if(n %% 1 != 0) stop("n is not an integer")
  if(numPositive %% 1 != 0) stop("n is not an integer")

  # the set-up
  component1 = "In a random sample of "
  # individuals
  component2 = paste(" ", individuals, sep = "")
  # out of a population
  component3 = ""
  if(is.null(population)) component3 = ", "
  else component3 = paste(" from a population of size ", population, ", ", sep = "")
  context_component1 = paste(component1, n, component2, component3, sep = "")

  # answer to the question
  context_component2 = ""
  component10 = ""
  # question == NULL, answer == NULL : numPositive " of them resulted in a success."
  if(is.null(question) & is.null(answer)){
    context_component2 = paste(numPositive, " of them resulted in a success.", sep = "")
    component10 = " that resulted in a success?"
  }
  # question == NULL, answer != NULL : numPositive " responded " answer "to a question."
  if(is.null(question) & !is.null(answer)){
    context_component2 = paste(numPositive, " responded: \'", answer, "\' to a question.", sep = "")
    component10 = paste(" who responded \'", answer, "\'?", sep = "")
  }
  # question != NULL, answer == NULL : numPositive " responded favorably to the question, " question
  if(!is.null(question) & is.null(answer)){
    context_component2 = paste(numPositive, " responded favorably to the question: \'", question, "\'", sep = "")
    component10 = " who responded favorably?"
  }
  # question != NULL, answer != NULL : numPositive " responded " answer " to the question, " question
  if(!is.null(question) & !is.null(answer)){
    context_component2 = paste(numPositive, " responded: \'", answer, "\' to the question: \'", question,"\'", sep = "")
    component10 = paste(" who responded \'", answer, "\'?", sep = "")
  }

  # confidence interval question
  context_component3 = paste(" What is the ", C*100, "% confidence interval for the proportion of ",
                             individuals, component10, sep = "")
  question = paste(context_component1, context_component2, context_component3, sep = "")
  return(question)
}


makeAnswers_CIprop = function(n , numPositive, C = 0.95, population = 100, individuals = "individuals",
                              question = NULL, answer = "no"){
  labeled_answers = list("correct_answer" = NA, "incorrect_answer1" = NA, "incorrect_answer2" = NA, "incorrect_answer3" = NA)
  # correct values
  phat = numPositive / n
  se = sqrt(phat * (1 - phat) / n)
  z_crit = qnorm(C + (1 - C) / 2)
  # incorrect values
  incorrect_se = sqrt(phat * (1 - phat)) / n
  incorrect_se2 = phat * (1 - phat) / n
  incorrect_z_crit = qnorm(C) # forget to add the tail

  # correct answer
  correct_MoE = c(-z_crit * se, z_crit * se)
  labeled_answers[[1]] = phat + correct_MoE

  # incorrect answer: incorrect standard error
  incorrect_MoE1 = c(-z_crit * incorrect_se, z_crit * incorrect_se)
  labeled_answers[[2]] = phat + incorrect_MoE1

  # incorrect answer: incorrect critical z value
  incorrect_MoE2 = c(-incorrect_z_crit * se, incorrect_z_crit * se)
  labeled_answers[[3]] = phat + incorrect_MoE2

  # incorrect answer: other incorrect standard error
  incorrect_MoE3 = c(-z_crit * incorrect_se2, z_crit * incorrect_se2)
  labeled_answers[[4]] = phat + incorrect_MoE3

  return(labeled_answers)
}

makeQA_CIprop = function(n , numPositive, C = 0.95, population = 100, individuals = "individuals",
                         question = NULL, answer = "no"){
  question = makeQuestion_CIprop(n , numPositive, C = 0.95, population = 100, individuals = "individuals",
                                 question = NULL, answer = "no")
  answers = makeAnswers_CIprop(n , numPositive, C = 0.95, population = 100, individuals = "individuals",
                               question = NULL, answer = "no")
  return(list(question, answers))
}
