# Outputting a LaTeX file

library(readtext)
source("R/kristynFunctions.R")

question_contents = import_question("samplequestionlabeledparts.txt")
process_question_contents = process_question_contents(question_contents)
