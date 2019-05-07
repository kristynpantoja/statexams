# statexams Package

statexams is an R package for creating exams for introductory statistics courses. It allows users to randomly generate specific types of statistics questions (such as normal probability, expectation, confidence interval, and hypothesis test questions, just to name a few) as well as import their own questions, then randomize their order and export the test files and solutions as LaTeX files.

Here are some question-generating functions:

- makeQA_eventprob(type = NULL) - This function generates a question involving probabilities of events.  This question will use intersection, union, and independence of events.  You can select type = 1, 2, or 3 for different types of questions.
- makeQA_ExpectedValue(prob = NULL, values = NULL) - This function generates an expected value question.  The values vector is the values the random variable takes, and the prob vector is the value of the pmf at these values.  You may input both, one, or neither vector.  The function automatically generates the other value.  makeQA_ExpectedValue(values = 1:6) will generate a question corresponding to an unfair 6 sided die.
- makeQA_HypothesisTest(type = NULL, level = 0.05, Xbar = NULL, sigma = NULL, n = NULL, mu_0 = NULL) - This function generates a question about hypothesis testing.  User may provide any or all of the values.  Type 0 generates a left sided test.  Type 1 generates a right sided test, and type 2 generates a two sided test.  If not specified, an Xbar will be generate that is reasonably close to the critical value without being too close.
- makeQA_ConditionalProbability(proportion = NULL, condprob = NULL, type = NULL) - This function will generate either a total probability (type = 1) or a Bayes theorem question (type = 2).  Proportion is a vector of unconditional probabilities (P(A), P(B), P(C)) and condprob is a vector of conditional probabilities.(P(W|A), P(W|B), P(W|C)).  Type 1 asks the student to solve for P(W), while type 2 asks for P(A|W).
- makeQA_Normal(variable = "X", mean = NULL, sd = NULL, interval = NULL, tail = NULL) - Generate a question that requires solving a normal probability. The default variable name is “X.” If no mean and standard deviation are provided, they will be generated. If the user wants to generate a probability question over an interval, then they should specify the interval (a vector) c(a, b) such that P(a < X < b) is the solution. If a tail probability is preferred, the user should provide a scalar and tail argument (tail = “left” or “right”).
- makeQA_CIproportion = function(n = NULL, numPositive = NULL, C = 0.95, population = NULL, individuals = "individuals", question = NULL, answer = NULL) - Generate a question that requires computing a level C confidence interval for a proportion. The sample proportion is given by numPositive / n; if n and numPositive are not given, they will be generated for you. The default level C = 0.95. A population size (a string) may also be provided. In addition, the user may also specify the question being asked in the poll, and the response (answer) of interest.


As previously mentioned, a question can also be imported using:

- import_question(file) - This function inputs a .txt file that looks something like:
	*Q*What is the probability that this package is awesome?
	*A*0
	*A*0.5
	*A*1%ans
As you can see, the question is in the first line of the .txt file that you want to import, and it is preceded by a tab, followed by “*Q*”. The lines that follow are answer choices, each preceded by a tab and followed by a “*A*” with the correct answer being trailed by a “%ans”

After creating a set of questions, the user should put the questions in a list and call export_test() on that list. 

- export_test(list_of_QAs, testfile = NULL, solutionsfile = NULL) - This function will create two .txt files that can be compiled in LaTeX: one file, called “test.txt” (unless a different name is given in the testfile argument), contains the test with all of the questions and answer choices; the other file, called “solutions.txt” (unless a different name is given in the solutionsfile argument), contains the solutions to each of the questions.




Here is a helper function that might be useful:

Rearrange(x) – Proper input is a list of vectors. Each of these vectors will have a question as the first element, the correct answer as a second element, and incorrect answer as the other elements. These vectors can be any length greater than or equal to 2.

Rearrange will output a list.  The first element is a list with all  the questions randomly shuffled and their answers also randomly shuffled.  The second element is the answer key for the new exam in letter form.  The third element is the correct answer as an alphanumeric character.  For instance if the correct answer was 0.47 and it was answer choice C after rearrangement, then the second element contains a C, and the third element contains 0.47.  Rearrange also outputs a fourth element which counts the number of elements corresponding to each question.  This is useful for formatting documents.
