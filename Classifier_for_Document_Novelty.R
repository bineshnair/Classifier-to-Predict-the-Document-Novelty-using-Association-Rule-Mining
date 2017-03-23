# Importing the libraries
library(NLP)
library(tm) #load text mining library

#Building the Corpus
Dir<-DirSource("### PATH ###") #sets R's working directory wherein my background documents (abstracts) reside
text_corpus<-Corpus(Dir,readerControl = list(reader=readPlain)) #specifies the exact folder where my text file(s) is for analysis with tm.
summary(text_corpus)  #Validing that the corpus is built properly
text_corpus_temp<-Corpus(VectorSource(text_corpus)) #Converting the corpus to a vector type to enable construction of tdm

#List of words with Synonyms
synonyms<-list(
  list(word="treatment",syns=c("treatments","treat","treated","treats")),
  list(word="illness",syns=c("illnesses","ill","illnes")),
  list(word="diagnosis",syns=c("diagnose","diagnosed","diagnoses")),
  list(word="disease",syns=c("diseases","ailment")),
  list(word="individual",syns=c("individuals")),
  list(word="quack",syns=c("quacks")),
  list(word="professional",syns=c("professionals")),
  list(word="competent",syns=c("competently")),
  list(word="long",syns=c("length","longer")),
  list(word="error",syns=c("erros","mistake","mistakes")),
  list(word="long",syns=c("length","longer")),
  list(word="medicine",syns=c("medicines","medication","medications","med","medical","medicals")),
  list(word="drug",syns=c("drugs")),
  list(word="test",syns=c("tested","testing","tests")),
  list(word="trials",syns=c("trial")),
  list(word="study",syns=c("studies")),
  list(word="patient",syns=c("patients")),
  list(word="reasonable",syns=c("reasonably")),
  list(word="clinic",syns=c("clinics")),
  list(word="adult",syns=c("adults")),
  list(word="suffer",syns=c("suffering","suffers","sufferred")),
  list(word="physician",syns=c("physicians")),
  list(word="service",syns=c("services","serve","served","serves")),
  list(word="symptoms",syns=c("symptom")),
  list(word="vaccine",syns=c("vaccines")),
  list(word="immunity",syns=c("immune")),
  list(word="hospital",syns=c("hospitals")),
  list(word="change",syns=c("changes","changed","changing")),
  list(word="train",syns=c("training","trains","trained","trainings")),
  list(word="result",syns=c("results","output")),
  list(word="provide",syns=c("provides","provided","providing")),
  list(word="available",syns=c("availablity")),
  list(word="additional",syns=c("addition","additions")),
  list(word="procedure",syns=c("procedures","proceduring")),
  list(word="license",syns=c("licensed","licensing","liscenced","liscened")),
  list(word="involve",syns=c("involving","involves","involved")),
  list(word="get",syns=c("getting","gets","receive","receiving","receives","received","gettable"))
)

#Function for Replacing Synonyms
replaceSynonyms <- content_transformer(function(x, syn=NULL){ 
  Reduce(function(a,b){
    gsub(paste0("\\b(", paste(b$syns, collapse="|"),")\\b"), b$word, a)}, syn, x)   
})

#Text Cleaning
#Input is a Corpus
textClean<-function(x){
  x<-tm_map(x,content_transformer(removePunctuation))
  x<-tm_map(x,content_transformer(removeNumbers))
  x<-tm_map(x,stripWhitespace)
  x<-tm_map(x,content_transformer(tolower))
  x<-tm_map(x,removeWords,stopwords("english"))
  x<-tm_map(x,replaceSynonyms,synonyms)
  x<-tm_map(x,removeWords,mystopwords)
  x<-Corpus(VectorSource(x))
  return(x)
}

#Constructing Term Document Matrix
tdm<-TermDocumentMatrix(text_corpus_temp,control=list(wordLengths=c(1,Inf)))

#Finding Strongly Associated Terms based on keywords provided by users
associted_terms<-findAssocs(tdm,"medicine",0.35) #medicine is the keyword of interest for the user; 0.35 is the confidence threshold

#Function to identify the strongly associated terms with the domian keywords
#Input is the tdm
#Output is a CSV file with the strongly associated terms. The CSV file is created in the working directory

findAssociatedTerms<-function(x){
  for(i in 1:nrow(domain_keywords)){
    word<-as.character(domain_keywords[i,])
    associated_terms_x<-findAssocs(x,word,0.10)
    write.table(associated_terms_x,"associated_terms1.csv",col.names=FALSE,append=TRUE,sep=",")
  }
}



#Extracting the strongly associated terms
write.csv(associated_terms_medicine,"associated_terms.csv")
write.table(associated_terms_drugs,"associated_terms.csv",append=TRUE,col.names=FALSE,sep=",") #appending the csv file if there exists other keywords of interest

#Note that the duplicates are removed from the extracted csv file in case the file have associated terms from multiple keywords

#Finding top 3 terms in each document
apply(tdm_compact_tfidf, 1, function(x) {
  x2 <- sort(x, TRUE)
  x2[x2 >= x2[3]]
})


# Reading multiple lines from text file into one row in a data frame
filename<-"57110"
input_file4<-readChar(filename,file.info(filename)$size)

#Function to convert the test file to a bag of words
#Input is the test file
#Output is a data frame containing the words in the test document

converttoBagOfWords<-function(x){
  x_corpus<-Corpus(VectorSource(x))
  x_corpus<-textClean(x_corpus)
  tdm_x<-TermDocumentMatrix(x_corpus,control=list(wordLengths=c(1,Inf)))
  words_in_test_file<-data.frame(findFreqTerms(tdm_x,lowfreq=1))
  return(words_in_test_file)
}


#Checking for Domain Similarity using domain specific keywords. It takes as input the test file to be examined for similarity
similarity_detection<-function(x){
  match=0
  for(i in 1:nrow(domain_keywords)){
    word<-domain_keywords[i,]
    if(length(grep(word,x[,1]))>0){
      match=match+1
    }
  }
  if(match>0)
  {
    return(0)
  }
  else
  {
    return(1)	
  }
}
> similarity_detection(freq_terms_test_file) #Function returns '0' if the document is similar; else returns '1'
[1] 0

#Finding Novel Terms
library(sqldf)
#Function to determine the novelty of a test document
#Input is the list(data frame structure) of words composing the test document
#Output is a boolean value (Novel or Redundant)

noveltyCheck<-function(x){
  library(RSQLite)
  library(gsubfn)
  library(sqldf)
  library(tcltk)
  novel_terms<-sqldf('SELECT * FROM x EXCEPT SELECT * FROM associated_terms')
  if(nrow(novel_terms)>(round(0.40*nrow(x))))
  {
    return("Novel")
  }
  else
  {
    return("Redundant")
  }
}

#Fucntion to classify documents based on their novelty

noveltyDocClassifier<-function(file_name,path){
  library(NLP)
  library(tm)
  setwd(path)
  test_file<-file_name
  test_file<-readChar(file_name,file.info(file_name)$size)
  word_list<-converttoBagOfWords(test_file)
  match<-similarity_detection(word_list)
  if(match==0){
    return(noveltyCheck(word_list))
  }
  else{
    return("Redundant")
  }
}

#Accessing files from a directory and classifying them as 'novel' or 'redundant'


noveltyDirClassifier<-function(dir_path){
  library(NLP)
  library(tm)
  filenames<-list.files(dir_path)
  for(i in 1:length(filenames)){
    file_name<-filenames[i]
    tag<-noveltyDocClassifier(filenames[i],dir_path)
    test_result<-c(file_name,tag)
    write.table(test_result,"Test Results Vini.csv",append=TRUE,col.names=FALSE,sep=",")
  }
}

