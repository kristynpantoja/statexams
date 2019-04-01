
# --- Functions --- #

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
export_txt = function(contents, filename){
  sink(filename, append=FALSE, split=FALSE)
  contents
  sink()
  return()
}





