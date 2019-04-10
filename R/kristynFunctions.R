
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
  if(length(interval) != 1 & length(interval) != 2) stop("interval must be a vector of length 1 or 2")
  if(length(interval) == 2) if(interval[1] <= interval[2]) stop("for interval = (a, b), b must be greater than a")
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

# add makeAnswers_normal



# add generate_normal





