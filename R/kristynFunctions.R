import_question_raw = function(file){
  # 1. read in the file. it has 2 parts: the name of the file, and the text inside the file.
  readin_question = readtext::readtext(file, dvsep = "\t")
  # 2. get the long string of question and answers components, i.e. the text inside the file.
  contents_as_string = readin_question[1, 2]
  # 3. split the components into question and corresponding answers:
  #     output is it's a list (with one element)
  question_contents_as_list = strsplit(contents_as_string, split = "\\t\\*Q\\*|\\t\\*A\\*") # first it's a list
  # 4. grab the single element in the list, which is the vector
  #     whose elements are the question followed by the answers
  question_contents = question_contents_as_list[[1]]
  return(question_contents)
}


process_question_contents = function(question_contents){
  if(length(question_contents) < 3) stop("Not enough answer choices.")
  # get rid of empty elements
  delete = c()
  for(i in 1:length(question_contents)){
    if(nchar(question_contents[i]) == 0){
      delete = c(delete, i)
    }
  }
  if(length(delete) > 0) question_contents = question_contents[-delete]

  # put together the question and answers (indicating which is the correct answer)

  # first non-empty element is the question
  question = question_contents[1]

  # find the correct answer, and put it last
  answers_vector = question_contents[-1]
  is_correct_answer = grepl('\\%ans$', answers_vector)
  incorrect_answers_indices = which(is_correct_answer == FALSE)
  correct_answer_index = which(is_correct_answer == TRUE)
  answers = answers_vector[incorrect_answers_indices]
  answers = c(answers_vector[correct_answer_index], answers)
  # name the elements
  answers_names = c("correct_answer")
  for(i in 1:length(incorrect_answers_indices)){
    another_answer_name = paste("incorrect_answer", i, sep = "")
    answers_names = c(answers_names, another_answer_name)
  }
  names(answers) = answers_names

  question_and_answers = c(question, answers)
  return(question_and_answers)
}


# a wrapper for importing a question and corresponding answers so that it's ready to be rearranged with other QAs
#' Import Your Own Question
#'
#' @param file a .txt file that has questions (preceded by a tab and a *Q*) and answer choices (preceded by a tab and a *A*, with correct answer choice ending in \%ans)
#'
#' @return a vector with first element as question, second element as correct answer, and other elements as other answer choices.
#' @export
#'
#' @examples # for a .txt file, call:
#' # import_question(file)
import_question = function(file){
  question_contents = import_question_raw(file)
  QA = process_question_contents(question_contents)
  return(QA)
}


# after using rearrange function, get output ready for latex
# output is two strings: 1 is actual test, the other is test with solutions
enumerate_QAs_for_latex = function(rearranged_QAs){
  QAs = rearranged_QAs[[1]] # questions (first element) and answers (rest of elements)
  QAs_lengths = rearranged_QAs[[4]] # number of elements in questions and answers
  numQuestions = length(QAs)

  test = "\n\\begin{enumerate}"
  solutions = "\n\\begin{enumerate}"
  for(i in 1:numQuestions){
    # get question
    question_and_answers = QAs[[i]]
    question = question_and_answers[1]
    test = paste(test, "\n\t\\item ", question, sep = "")
    # get answers
    answers = "\n\t\\begin{enumerate}"
    for(j in 2:QAs_lengths[i]){
      answers = paste(answers, "\n\t\t\\item ", question_and_answers[j], sep = "")
    }
    answers = paste(answers, "\n\t\\end{enumerate}")
    test = paste(test, answers, sep = "")
    solutions = paste(solutions, "\n\t\\item ", rearranged_QAs[[2]][i], " : ", rearranged_QAs[[3]][i], sep = "")
  }
  test = paste(test, "\n\\end{enumerate}\n", sep = "")
  solutions = paste(solutions, "\n\\end{enumerate}\n", sep = "")
  return(c(test, solutions))
}


# a function that takes as input a (long) string of all questions and answers
# and adds the latex begin and end parts
export_for_latex = function(contents){ # a string of all the q/a's
  document_begin = "\\documentclass[11pt]{article} \n\\usepackage[margin=1in]{geometry} \n\\usepackage{graphicx} \n\\usepackage{amsthm, amsmath, amssymb} \n\\usepackage{setspace}\\onehalfspacing \n\\begin{document}\n\n"
  document_end = "\n\n\\end{document}"
  latex_ready_txt = paste(c(document_begin, contents, document_end), collapse = " \n")
  return(latex_ready_txt)
}



# export a string as contents of a txt file called filename (should have .txt extension)
# note: it interprets special characters (e.g. "\n"), unless you put an extra backslash in front.
export_txt = function(contents, filename){
  file_connection  = file(filename)
  writeLines(contents, file_connection)
  close(file_connection)
  return("exported txt file")
}

# wrapper to export list of QAs as a latex file, after rearranging them
#' Export Test (and Solutions!)
#'
#' @param list_of_QAs a list of character vectors
#' @param testfile name of test file (if none given, will be named "test.txt")
#' @param solutionsfile name of test file (if none given, will be named "solutions.txt")
#'
#' @return produces the two latex files (as .txt files)
#' @export
#'
#' @examples QA1 = makeQA_CIproportion()
#' QA2 = makeQA_Normal()
#' QA3 = makeQA_EventProbability()
#' QA4 = makeQA_ExpectedValue()
#' QA5 = makeQA_HypothesisTest()
#' QA6 = makeQA_ConditionalProbability()
#' QAs = list(QA1, QA2, QA3, QA4, QA5, QA6)
#' export_test(QAs)
export_test = function(list_of_QAs, testfile = NULL, solutionsfile = NULL){
  list_of_QAs = rearrange(list_of_QAs) # turn into rearrange object
  result = enumerate_QAs_for_latex(list_of_QAs)
  test_latex = export_for_latex(result[1])
  solutions_latex = export_for_latex(result[2])
  if(is.null(testfile)) testfile = "test.txt"
  if(is.null(solutionsfile)) solutionsfile = "solutions.txt"
  export_txt(test_latex, testfile)
  export_txt(solutions_latex, solutionsfile)
  return("exported test and solutions files")
}



# --- Functions for Creating Questions for Normal Distribution Calculations --- #

makeQuestion_Normal = function(variable, mean, sd, interval, tail = NULL){
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

  sd = round(sd, 3)
  mean = round(mean, 3)
  interval = round(interval, 3)

  component1 = " is normally distributed with mean "
  component2 = " and standard deviation "
  component3 = ". What is the probability that "
  context_component1 = paste(variable, component1, mean, component2, sd, component3, sep = "")

  context_component2 = ""
  # Question for Left-tail probabilities
  if(length(interval) == 1 & tail == 0){
    component4 = " is less than "
    context_component2 = paste(variable, component4, interval, "?", sep = "")
  }

  # Question for Right-tail probabilities
  if(length(interval) == 1 & tail == 1){
    component4 = " is greater than "
    context_component2 = paste(variable, component4, interval, "?", sep = "")
  }

  # Question for interval probabilities
  if(length(interval) == 2){
    component4 = " is between "
    component5 = " and "
    context_component2 = paste(variable, component4, interval[1],
                               component5, interval[2], "?", sep = "")
  }

  question = paste(context_component1, context_component2, sep = "")
  return(question)
}

makeAnswers_Normal = function(variable, mean, sd, interval, tail = NULL){
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
    answers[1] = round(pnorm(q = interval[1], mean, sd), 3)
    # right tail probability
    answers[2] = round(pnorm(q = interval[1], mean, sd, lower.tail = FALSE), 3)
    # z-score
    answers[3] = round((interval[1] - mean) / sd, 3)
    # negative z-score
    answers[4] = round(- answers[3], 3)
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
      labeled_answers[i + 1] = leftover_answers[i]
    }
  }

  # For Right-tail probabilities
  if(length(interval) == 1 & tail == 1){
    labeled_answers[1] = answers[2]
    leftover_answers = answers[-2]
    for(i in 1:(length(leftover_answers))){
      labeled_answers[i + 1] = leftover_answers[i]
    }
  }

  return(labeled_answers)
}


#' Make a Normal Probability Question
#'
#' @param variable name of variable (default name is "X")
#' @param mean mean of Normal distribution (default is 0)
#' @param sd standard deviation of Normal distribution (default is 1)
#' @param interval interval or tail probability. Interval if a vector with 2 elements; tail if scalar.
#' @param tail If `interval` is scalar value, must specify which tail probability: "left" or "right"
#'
#' @return a vector with first element as question, second element as correct answer, and other elements as other answer choices.
#' @export
#'
#' @examples makeQA_Normal()
makeQA_Normal = function(variable = "X", mean = NULL, sd = NULL, interval = NULL, tail = NULL){
  if(is.null(sd)){
    sd = sample(seq(0.1, 5, 0.1), 1)
  }
  if(is.null(mean)){
    mean = round(sample(seq(-10, 10, 0.1),1),2)
  }
  if(is.null(interval)){
    sign = sample(c(-1, 1), 1)
    which.tail = sample(c(0, 1), 1)
    interval = mean + sign * sample(seq(0.1, 2.5, 0.1), 1) * sd
    if(which.tail){
      tail = "left"
    } else{
      tail = "right"
    }
  }
  question = makeQuestion_Normal(variable, mean, sd, interval, tail)
  q_answers = makeAnswers_Normal(variable, mean, sd, interval, tail)
  return(c(question, as.character(q_answers)))
}






# --- Functions for Creating Questions for Confidence Intervals for Proportions --- #

makeQuestion_CIproportion = function(n = NULL, numPositive = NULL, C = 0.95, population = NULL, individuals = "individuals",
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
  if(is.null(population)){
    component3 = ", "
  }
  else {
    if(typeof(population) != "character") stop("population parameter must be a string")
    component3 = paste(" from a population of size ", population, ", ", sep = "")
  }
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
  context_component3 = paste(" What is the ", C*100, "\\% confidence interval for the proportion of ",
                             individuals, component10, sep = "")
  question = paste(context_component1, context_component2, context_component3, sep = "")
  return(question)
}


makeAnswers_CIproportion = function(n = NULL, numPositive = NULL, C = 0.95, population = NULL, individuals = "individuals",
                              question = NULL, answer = NULL){
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
  labeled_answers[[1]] = round(phat + correct_MoE, 3)

  # incorrect answer: incorrect standard error
  incorrect_MoE1 = c(-z_crit * incorrect_se, z_crit * incorrect_se)
  labeled_answers[[2]] = round(phat + incorrect_MoE1, 3)

  # incorrect answer: incorrect critical z value
  incorrect_MoE2 = c(-incorrect_z_crit * se, incorrect_z_crit * se)
  labeled_answers[[3]] = round(phat + incorrect_MoE2, 3)

  # incorrect answer: other incorrect standard error
  incorrect_MoE3 = c(-z_crit * incorrect_se2, z_crit * incorrect_se2)
  labeled_answers[[4]] = round(phat + incorrect_MoE3, 3)

  return(labeled_answers)
}




#' Make a Confidence Interval for Proportion Question
#'
#' @param n sample size
#' @param numPositive number of positive samples (number of individuals who gave positive responses)
#' @param C confidence level (default is 0.95)
#' @param population population size (optional)
#' @param individuals title of individuals (default is "individuals")
#' @param question "survey question to be asked to individuals" (optional)
#' @param answer survey answer choice to be considered a positive response
#'
#' @return a vector with first element as question, second element as correct answer, and other elements as other answer choices.
#' @export
#'
#' @examples makeQA_CIproportion()
makeQA_CIproportion = function(n = NULL, numPositive = NULL, C = 0.95, population = NULL, individuals = "individuals",
                         question = NULL, answer = NULL){
  if(is.null(n) & !is.null(numPositive)) stop("to specify numPositive, must also specify n")
  if(is.null(n)) n = sample(30:100, 1)
  if(is.null(numPositive)) numPositive = sample(1:n, 1)
  question = makeQuestion_CIproportion(n , numPositive, C = 0.95, population, individuals,
                                 question, answer)
  q_answers = makeAnswers_CIproportion(n , numPositive, C = 0.95, population, individuals,
                               question, answer)
  answers = rep(NA, length(q_answers))
  for(i in 1:length(answers)){
    answers[i] = paste("(", q_answers[[i]][1], ", ", q_answers[[i]][2], ")", sep = "")
  }
  return(c(question, answers))
}
