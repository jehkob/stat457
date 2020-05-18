library(jsonlite)
library(tm)

#read the jason file
train <- fromJSON("train.json", flatten = TRUE)
test <- fromJSON("test.json", flatten = TRUE)

#First, create a vocabulary of all words
#Second, create word occurrence matrices: 
  #each column corresponds to a word, and each row is a recipe
  #(i,j)-th entry is the number of occurrence for word j in i-th recipe
#Third, remove terms (i.e. columns) that do not appear very often
ingredients <- Corpus(VectorSource(c(train$ingredients,test$ingredients)))
ingredientsDTM <- DocumentTermMatrix(ingredients, 
                                     control = list(removePunctuation = TRUE,
                                                    stopwords = TRUE))
sparse <- removeSparseTerms(ingredientsDTM, 0.995)

#create training and test data in the form of matrices and vectors
Xtrain = as.matrix(sparse[1:dim(train)[1],])
Ytrain = train$cuisine

Xtest = as.matrix(sparse[(dim(train)[1]+1):(dim(train)[1]+dim(test)[1]), ])
testID = test$id #use for output

#Output the preprocessed data for later analysis
save(file = 'processed_text.RData', 'Xtrain', 'Ytrain', 'Xtest', 'testID')