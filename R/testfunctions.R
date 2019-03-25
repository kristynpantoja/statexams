library(readtext)

## Import the data: sample question.txt

test = readtext("sample question.txt", dvsep = "\t")
?readtext
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
a
sink()









