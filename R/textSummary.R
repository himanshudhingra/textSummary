install.packages("quanteda")

library(quanteda)
library(stringi)

# Function to get the summary
summary<-function(content,summary_length,important_words){
  
#Split by sentence
sentences<-tokenize(content,what="sentence")
n<-lengths(sentences)
sentences_matrix <- matrix(unlist(sentences),nrow = n,byrow = TRUE)

for(i in 1:n){
  if ((nchar(sentences_matrix[i,])) <= 10)
    sentences_matrix[i,]<-"empty"
}

# Function to Caculate the intersection between 2 sentences
intersection_score<- function(sent1,sent2) {

  # split the sentence into words/tokens and remove stop words/remove punct/convert to lower case
  #If not to remove stop words use: tokenize(sent1,what="word")
  s1<-features(dfm(sent1,ignoredFeatures = stopwords("english")))
  s2<-features(dfm(sent2,ignoredFeatures = stopwords("english")))

  # If either of the sets is < 2 or = 0 or no intersecting words, return zero immediately
  # That way we speed the analysis up and avoid division by zero in the next statement
  if (length(s1) < 2 || length(s2) < 2 || length(intersect(s1,s2)) ==0 ) { score<-0 
  } else if (any(important_words %in% s1) || any(important_words %in% s2)) { score<-(n+1)
  } else score<-length(intersect(s1,s2)) / ((length(s1) + length(s2)) / 2) #To normalize the result by the average number of words.
  
  return(score)
}

upper_lower <- function(matrix) {
  matrix[lower.tri(matrix)] <- t(matrix)[lower.tri(matrix)]
  matrix
}

#create empty nxn matrix
values<-matrix(, nrow = n, ncol = n) 

# storing scores of each pair in a 2D matrix
for(i in 1:n) {
    for(j in i:n){
    values[i,j]<-intersection_score(sentences_matrix[i,],sentences_matrix[j,])
  }
}
values<-upper_lower(values)

# forming KEY-SCORE dictionary, 
#key:sentence
#score:sum of all its intersection except itself.
sentence<-"aa"
score<-"1.0000"
dict<-data.frame(sentence,score,stringsAsFactors = FALSE)
for(i in 1:n){
  score<-0
  for(j in 1:n){
    if(values[i,j]==1)      next 
      score<-score+values[i,j]
      dict[i,]<-rbind(sentences_matrix[i,],score)
      }
}

#Sorting score column in highest to lowest order along with sentences.
new_dict<-dict[order(dict[,2],decreasing=TRUE),]


   summary_dict<-new_dict[1:(summary_length+2),]
     summary<-summary_dict[order(rownames(summary_dict)),] 
    
     return(summary[1:(summary_length+2),1])
}

#NOTE1: important_words are character vector of case insensetive words.

#NOTE2: Summary function will return 2 extra sentences than what is 
       #provided as summary length to not to miss key sentence because of important words.

#NOTE3: content can be string entered manually or a csv(comma delim) with all content in the first
        #cell only as x<-paste(read.csv("F:/R/book1.csv", sep=",",header = FALSE,stringsAsFactors=FALSE),collapse=" ")

#NOTE4: Speed of the summary depends on the no. of special characters which is removed by the algorithm
       #and the number of characters in the content. usual speed is 4~5 seconds/1000 characters.
