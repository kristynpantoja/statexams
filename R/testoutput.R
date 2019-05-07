# Outputting a LaTeX file

library(readtext)
source("R/kristynFunctions.R")
source("R/jamesFunctions.R")
source("R/rearrange.R")

testquestion1 = makeQA_eventprob()
testquestion2 = makeQA_ExpectedValue()
testquestion3 = makeQA_ExpectedValue()

test = list(testquestion1, testquestion2, testquestion3)
testrearrange = rearrange(test)
testrearrange




test = enumerate_QAs_for_latex(testrearrange)
export = export_for_latex(test)
export_txt(export, "test.txt")




#question_contents = import_question("samplequestionlabeledparts.txt")
#process_question_contents = process_question_contents(question_contents)
#rearrange(process_question_contents)
#length(process_question_contents)
