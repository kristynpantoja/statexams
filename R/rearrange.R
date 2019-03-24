x = list(c(1, "a", "b"), c(2, "a", "b", "c"), c(3, "a", "b", "c", "d", "e"))

rearrange = function(x){
  #put checks here
  num_ques = length(x)
  ind = sample(num_ques)
  reorder_ans = list(NA)
  for(i in 1:num_ques){
    num_ans = length(x[[i]]) #technically number of answers + 1
    ind_ans = sample(2:num_ans)
    reorder_ans[[i]] = x[[i]][1]
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

rearrange(x)
