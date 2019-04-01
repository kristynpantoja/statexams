x = list(c(1, "a", "b"), c(2, "a", "b", "c"), c(3, "a", "b", "c", "d", "e"))

rearrange = function(x){
  #put checks here
  num_ques = length(x)
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
  for(i in 1:num_ques){
    reorder_x[i] = reorder_ans[ind[i]]
  }
  return(reorder_x)
}

xre = rearrange(x)

xre_unlist = unlist(xre)
xre_unlist

recreation = function(x){
  #totalout = "header stuff and question 1" FIX THIS
  for(i in 2:length(x)){
    check = suppressWarnings(as.numeric(substr(x[i], 1, 1)))
    if(!is.na(check)){
      #add \n \end{enumerate} before
      #add \item before
      # add \begin{enumerate} after
    }
    else{
      #\item before
      #line break after
    }
  }
  #add \end{enumerate}x2 at end
}
as.numeric("$")
