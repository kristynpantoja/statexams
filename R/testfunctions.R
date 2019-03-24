library(readtext)

test = readtext("sample question.txt", dvsep = "\t")
?readtext
dim(test)
test[1, 2]

class(test[1, 2])
print(test)

a = strsplit(test[1, 2], "\t")

a[[1]]

length(a[[1]])
