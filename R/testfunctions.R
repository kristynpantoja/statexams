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








