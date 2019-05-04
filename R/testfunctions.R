library(readtext)
source("R/kristynFunctions.R")

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

# this is what the data looks like, after being imported:
# first element of test is the name of the txt file,
# second element of test is the contents of the txt file (i.e. long string of question and answers)
# dim(test)
# readfile[1, 1] # this is the file name
# readfile[1, 2] # this is the file contents
# class(readfile[1, 2])
print(readfile)

# --- Testing things --- #
question_contents_list = strsplit("\t*Q*Hello, this is a question.\t*A*And this is an answer.",
                                  split = "\\t\\*Q\\*|\\t\\*A\\*")
?strsplit
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
question_contents_as_list = strsplit(contents_as_string, split = "\\t\\*Q\\*|\\t\\*A\\*") # first it's a list

question_contents = question_contents_as_list[[1]] # now it's a vector

question_contents # look at it. works!





question_contents = import_question("samplequestionlabeledparts.txt")
process_question_contents = process_question_contents(question_contents)





# --- Now want to try some kind of output --- #

a = paste(question_contents, collapse = " ")

# sinking to a pdf does NOT work!
sink("testoutput.txt", append=FALSE, split=FALSE)
test
sink()


# consider: https://stackoverflow.com/questions/14796501/is-it-possible-to-call-external-r-script-from-r-markdown-rmd-in-rstudio/14796502#14796502


######################################################
# Normal Distribution Questions Testing Here #
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

makeQuestion_normal(variable = "X", mean = 0, sd = 1, interval = c(0, 1))

makeAnswers_normal(variable = "X", mean = 0, sd = 1, interval = c(0, 1))


######################################################
# Confidence Interval Questions Testing Here #
######################################################

makeQuestion_CIprop(n = 10, numPositive = 4, C = 0.97, population = 100, individuals = "individuals",
  question = NULL, answer = "no")

makeAnswers_CIprop(n = 10, numPositive = 4)











##############################
# Read in multiple questions #
##############################

readin_questions = readtext::readtext("samplequestions.txt", dvsep = "\t")
contents_as_string = readin_questions[1, 2]
question_contents_as_list = strsplit(contents_as_string, split = "\\t\\*Q\\*|\\t\\*A\\*") # first it's a list
# 4. grab the single element in the list, which is the vector
#     whose elements are the question followed by the answers
question_contents = question_contents_as_list[[1]]
# [TO DO] at some point, get rid of \n's

############
# https://stackoverflow.com/questions/39428474/split-string-without-losing-character-r







#######################
# Output Testing Here #
#######################

test2 = paste("hi", "\n", "there!", collapse = "", sep = "")
test2
export_txt(test2, "test.txt")

#question1 = call_some_function_from_statexams_here

#question2 = call_some_function_from_statexams_here

#...

#test_components = some_function_that_outputs_test_and_testsolutions_here



















##################################
# Export a LaTeX-ready .txt file #
##################################

readin_questions = readtext::readtext("samplequestions.txt", dvsep = "\t")
contents_as_string = readin_questions[1, 2]
question_contents_as_list = strsplit(contents_as_string, split = "\\t\\*Q\\*|\\t\\*A\\*") # first it's a list
# 4. grab the single element in the list, which is the vector
#     whose elements are the question followed by the answers
question_contents = question_contents_as_list[[1]]
question_contents = paste(question_contents, collapse = " ")

export_txt(question_contents, "test.txt")

export_for_latex_string = function(contents){ # a string of all the q/a's
  document_begin = "\\documentclass[11pt]{article} \n\\usepackage[margin=1in]{geometry} \n\\usepackage{graphicx} \n\\usepackage{amsthm, amsmath, amssymb} \n\\usepackage{setspace}\\onehalfspacing \n\\begin{document}\n\n"
  document_end = "\n\n\\end{document}"
  latex_ready_txt = paste(c(document_begin, contents, document_end), collapse = " \n")
  return(latex_ready_txt)
}

latex = export_for_latex(question_contents)
export_txt(latex, "test.txt")



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
    answers = paste(answers, "\n\t\\end{enumerate}[a]")
    test = paste(test, answers, sep = "")
    solutions = paste(solutions, "\n\t\\item", i, " . ", rearranged_QAs[[2]][i], " : ", rearranged_QAs[[3]][i], sep = "")
  }
  test = paste(test, "\n\\end{enumerate}\n", sep = "")
  solutions = paste(test, "\n\\end{enumerate}\n", sep = "")
  return(c(test, solutions))
}




