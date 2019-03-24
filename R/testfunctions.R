install.packages("readtext")
library(readtext)

test = readtext("sample question.txt", dvsep = "\t")
?readtext
dim(test)
test[1, 2]

class(test[1, 2])
print(test)

strsplit("Hello,\tWorld!", "\t")
strsplit(test[1, 2], "\t")
