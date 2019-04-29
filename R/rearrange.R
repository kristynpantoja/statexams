x = list(c(1, "a", "b"), c(2, "a", "b", "c"), c(3, "a", "b", "c", "d", "e"))

rearrange = function(x){


  num_ques = length(x)
  correct = ""
  for(i in 1:num_ques){
    correct[i] = x[[i]][2]
  }
  QA = rep(NA, num_ques)
  for(i in 1:num_ques){
    QA[i] = length(x[[i]]) # number of answers +1 per question
  }

  ind = sample(num_ques)
  reorder_ans = x
  for(i in 1:num_ques){
    num_ans = length(x[[i]]) #technically number of answers + 1
    ind_ans = sample(2:num_ans)
    for(j in 2:num_ans){
      reorder_ans[[i]][j] = x[[i]][ind_ans[j - 1]]

    }
  }
  reorder_x = list(NA)
  reorder_QA = rep(NA, num_ques)
  reorder_correct = ""
  for(i in 1:num_ques){
    reorder_x[i] = reorder_ans[ind[i]]
    reorder_QA[i] = QA[ind[i]]
    reorder_correct[i] = correct[ind[i]]
  }
  letter_correct = rep(0, num_ques)
  for(i in 1:num_ques){
    letter_correct[i] = which(reorder_x[[i]] == reorder_correct[i])
  }

  letters = c("", "a", "b", "c", "d", "e", "f", "g")
  letter_correct_out = ""
  for(i in 1:num_ques){
    letter_correct_out[i] = letters[letter_correct[i]]
  }
  #Output string of question and answer
  #Output number of elements per question(eg 1 question with 3 answers outputs 4)
  return(list(reorder_x, letter_correct_out, reorder_correct, reorder_QA))
}

xre = rearrange(x)

xre_unlist = unlist(xre[1])
xre_unlist

# xre[2]
# recreation = function(x){
#   #totalout = "header stuff and question 1" FIX THIS
#   for(i in 2:length(x)){
#     check = suppressWarnings(as.numeric(substr(x[i], 1, 1)))
#     if(!is.na(check)){
#       #add \n \end{enumerate} before
#       #add \item before
#       # add \begin{enumerate} after
#     }
#     else{
#       #\item before
#       #line break after
#     }
#   }
#   #add \end{enumerate}x2 at end
# }
# as.numeric("$")
