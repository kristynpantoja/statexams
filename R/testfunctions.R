library(readtext)
source("R/statexamsFunctions.R")

#####################################################
# Trying Things Here : Importing & Exporting Things #
#####################################################


## Import the data: sample question.txt

test = readtext("sample question.txt", dvsep = "\t")
dim(test)
test[1, 2]

class(test[1, 2])
print(test)

# this is what the data looks like, after being imported:
# first element of test is the name of the txt file,
# second element of test is the contents of the txt file (i.e. long string of question and answers)
question_contents = strsplit(test[1, 2], "\t") # this is a list

question_contents = question_contents[[1]] # this is a vector (of question, answer, answer, answer)

length(question_contents)




## Import the data: sample question.txt

readfile = readtext("samplequestionlabeledparts.txt", dvsep = "\t")
# dim(test)
# test[1, 1]
# test[1, 2]

class(test[1, 2])
print(test)

# this is what the data looks like, after being imported:
# first element of test is the name of the txt file,
# second element of test is the contents of the txt file (i.e. long string of question and answers)

# --- Testing things --- #
question_contents_list = strsplit("\tQHello, this is a question.\tAAnd this is an answer.", split = "\\tQ|\\tA")

# how to ignore empty-ish elements in the beginning? such as tabs or spaces...

question_contents = question_contents_list[[1]] # add this as an element of the list of questions/answers

question_contents
# --- it works!!! --- #


# --- now for our actual question! --- #

# 1. Read in the File
readfile = readtext("samplequestionlabeledparts.txt", dvsep = "\t")

# 2. Get the string containing Q and A's (one big string)
contents_as_string = readfile[1, 2]

# 3. Split the big string into a vector that contains the Question and each Answer Choice as elements
question_contents_as_list = strsplit(contents_as_string, split = "\\tQ|\\tA") # first it's a list

question_contents = question_contents_as_list[[1]] # now it's a vector

question_contents # look at it. works!




# --- Now want to try some kind of output --- #

a = paste(question_contents, collapse = " ")

# sinking to a pdf does NOT work!
sink("testoutput.txt", append=FALSE, split=FALSE)
test
sink()


# consider: https://stackoverflow.com/questions/14796501/is-it-possible-to-call-external-r-script-from-r-markdown-rmd-in-rstudio/14796502#14796502


######################################################
# Trying Things Here : Normal Distribution Questions #
######################################################

## Normal computation-type questions

## The function:
# normal_distribution_question()
# arguments:
# variable = "X" (the variable, optional)
# mean = 0 (mu, optional)
# sd = 1 (sigma, optional)
# interval = c(a, b) OR a OR b if it's a tail probability
# tail = NULL, but can be "l" / "left" / 0 OR "r" / "right" / 1
#         (only required if interval is a OR b)

makeQuestion_normal = function(variable = "X", mean = 0, sd = 1, interval, tail = NULL){
  # Cleaning up arguments and error-catching
  if(length(interval) == 1 & is.null(tail)) stop("Need to specify type of tail probability")
  if(!is.null(tail)){
    if(tail == "l" | tail == "left") tail = 0
    else if(tail == "r" | tail == "right") tail = 1
    else stop("Need to specify a valid type of tail probability: left or right")
  }
  if(is.null(tail)) tail = 2

  base_component1 = " is normally distributed with mean "
  base_component2 = " and standard deviation "
  base_component3 = ". What is the probability that "
  context_component1 = paste(variable, base_component1, mean, base_component2, sd, base_component3, sep = "")

  context_component2 = ""
  # Question for Left-tail probabilities
  if(length(interval) == 1 & tail == 0){
    base_component4 = " is less than "
    context_component2 = paste(variable, base_component4, interval, "?", sep = "")
  }

  # Question for Right-tail probabilities
  if(length(interval) == 1 & tail == 1){
    base_component4 = " is greater than "
    context_component2 = paste(variable, base_component4, interval, "?", sep = "")
  }

  # Question for interval probabilities
  if(length(interval) == 2){
    base_component4 = " is between "
    base_component5 = " and "
    context_component2 = paste(variable, base_component4, interval[1],
                               base_component5, interval[2], "?", sep = "")
  }

  question = paste(context_component1, context_component2, sep = "")
  return(question)
}

makeQuestion_normal(variable = "X", mean = 0, sd = 1, interval = c(0, 1))

## Making the answers:

makeAnswers_normal = function(variable = "X", mean = 0, sd = 1, interval, tail = NULL){
  # Cleaning up arguments and error-catching
  if(length(interval) == 1 & is.null(tail)) stop("Need to specify type of tail probability")
  if(!is.null(tail)){
    if(tail == "l" | tail == "left") tail = 0
    else if(tail == "r" | tail == "right") tail = 1
    else stop("Need to specify a valid type of tail probability: left or right")
  }
  if(is.null(tail)) tail = 2

  answers = vector("correct_answer" = NA, "incorrect_answer" = NA, "incorrect_answer" = NA, "incorrect_answer" = NA, "incorrect_answer" = NA)

  # Answers for Left-tail probabilities
  if(length(interval) == 1 & tail == 0){
    base_component4 = " is less than "
    context_component2 = paste(variable, base_component4, interval, "?", sep = "")
  }

  # Answers for Right-tail probabilities
  if(length(interval) == 1 & tail == 1){
    base_component4 = " is greater than "
    context_component2 = paste(variable, base_component4, interval, "?", sep = "")
  }

  # Answers for interval probabilities
  if(length(interval) == 2){
    base_component4 = " is between "
    base_component5 = " and "
    context_component2 = paste(variable, base_component4, interval[1],
                               base_component5, interval[2], "?", sep = "")
  }

}












####################################################
# Testing Functions Here
####################################################


#######################
# Output Testing Here #
#######################

source("R/statexamsFunctions.R")
test2 = paste("hi", "\n", "there!", collapse = "", sep = "")
test2
export_txt(test2, "test.txt")

#question1 = call_some_function_from_statexams_here

#question2 = call_some_function_from_statexams_here

#...

#test_components = some_function_that_outputs_test_and_testsolutions_here







