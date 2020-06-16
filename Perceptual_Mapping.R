#this is the R for the marketing Assignment #2
#loading the packages
library(jsonlite)
library(tidyverse)
library(stringr)
library(tm)
library(stringr)
library(ggplot2)

#setting a working directory linked to the marketing folder
setwd("C:/Users/patri/Desktop/Semester 2/6065")
#loading in our text data from the Json file for Appliances
data <- stream_in(file("Appliances.json.gz"))
metadata <- stream_in(file("meta_Appliances.json.gz"))

#looking at the reviews data and then reading it out into a csv for ease of access,
# rather than reading it in everytime. We only need the reviewText and asin for analysis.
reviews <- data[,c("asin","reviewText")]

#reading the file out into a csv. "app" is short for appliances
write.csv(reviews, "app.csv")

#looking at our metadata and reading it out for ease of access
#we are only taking into account the title, asin, and brand for analysis
metadata <- metadata[,c("title", "asin", "brand")]

write.csv(metadata,"app_meta.csv")

################################ this is where you would read in the files ##############################

#reading the files from the csvs that we just wrote. 
reviews1 <- read.csv("app.csv", stringsAsFactors = FALSE)
meta <- read.csv("app_meta.csv", stringsAsFactors = FALSE)

#whenever you read metadata and reviews in for some reason it creates an observation column, so delete the observation column
meta <- meta[-1]
reviews1 <- reviews1[-1]

#merging the two datasets on the asin, so we can see what review is for what brand
appliances <- merge(meta, reviews1, by = "asin")





###################################################################################


#this is now seperating the previous data into data frames for better analysis
#also filtering out by research done on popular refridgerators 

#identifying the brands within the petsupplies category, arranged in desc order
brands <- appliances %>% 
  group_by(brand) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#we decided to rename some brands that could be coupled into brand names 

appliances$brand <- str_replace_all(appliances$brand, "General Electric", "GE")

appliances$brand <- str_replace_all(appliances$brand, "EveryDrop by Whirlpool", "Whirlpool")

#after researching fridges to use. We found these to be the top 20 and most prevalent in the data set
#we will assign the collection of the characters into a string of values
fridge_names <- c("GE", "Whirlpool", "Bosch", "LG", "Frigidaire", "Samsung", "Kenmore", "Maytag"
                                                ,"KitchenAid","Hitachi","Haier", "Electrolux", "Miele", "Viking", "Panasonic",
                                                "Danby", "Honeywell", "Avanti", "MIDEA", "RCA")


#this is reassigning the fridge names to the brands value for coding purposes. 
brands1 <- fridge_names
df1 <- appliances %>% filter(brand == brands1)

######################### more tidying ##################################

#removing stop words from the reviews to make the functions and loops run faster
#creating a stopwords values for future tidying, code provided by stackoverflow
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')



#removing the english stopwords(the, and, etc) from the reviewtext of the dataframe, might take a while
df1$reviewText <- str_replace_all(df1$reviewText, stopwords_regex, '')
#now stripping away the whitespace to condense the review text sectio
df1$reviewText <- stripWhitespace(df1$reviewText)


###################################################################################################333


#next section we will start building our similarty matrix and the foundational functions for analysis
#######################################################################

#building an empty data frame to populate with the reviews
#the dimensions of the df can be taken from the amount of brands we want to take into account and the # of reviews
# since df1 has 13455 observations(reviews), and we want to look at 20 brands then the df will have 13455 obs and 20 columns

#this data frame will be used for each incident of brand occurence in any review
appdf1 <- data.frame(matrix(ncol = 20, nrow = nrow(df1)))


#this is just putting the 20 brands we filtered out earlier and assigning them the column names
for (x in 1:20) {
  names(appdf1)[x] = brands1[x]
}

#this is a data frame using the same dimensions as the one before
#for use of seeing what review(observation) corresponds to what brand
branddf1 <- appdf1



#############################################################################################################33######

### now we're going to detect if there are happenings of the brand in each review

#for loop iterates over the 20 columns and each review, so if "GE" appears in review 5, 
#then a true will appear for row 5 under "GE" in the df
#ignore case is used because someone might not type the brand using proper capitlization


for (i in 1:20) {
  appdf1[,i] <- str_detect(df1[,c("reviewText")], fixed(names(appdf1[i]), ignore_case=TRUE))
}

#this for loop is iterating over every review in df1 and seeing if its column name matches the brand
#if the brand for review 1 matches the column name(specific brand), then it will throw back a true for that observation

for (i in 1:20) {
  branddf1[,i] <- str_detect(df1[,c("brand")], fixed(names(branddf1[i]), ignore_case=TRUE))
}



#####################################################
#making a matrix for the probablity of a showing in a review based on the sample of each brand
#p_of_ab is the df we will use to calculate the  probability of "a" showing up out of every review
#p_ab is the df we will use to calculate the probability of both "a" and "b" showing up in the same review

#assigning a matrix that is 20 by 20 for a probability matrix
p_of_ab <- data.frame(matrix(ncol = 20, nrow = 20))


#assinging a matrix that is 20 bt 20 for a probability matrix
p_ab <- data.frame(matrix(ncol = 20,nrow = 20))



#assinging the column names of each matrix to the brand names of the refrigerators
for (x in 1:20) {
  names(p_ab)[x] = brands1[x]
  names(p_of_ab)[x] = brands1[x]
}


#this for loop is the function that will create the probability of a brand appearing in a review
#the correct formula is shown below with 
# p(a) = the probability of a brand appearing in a review and 
# p(x) = the probablity of a review being about the brand in the df

#formula: (p(a) + b(a) - p(a&x))/ num of reviews
for(a in 1:20){
  p_of_ab[a] <- ((((length(which(appdf1[,a] == "TRUE"))) + (length(which(branddf1[,a] == "TRUE")))) 
                 - (length(which(appdf1[,a] == "TRUE" & branddf1[,a] == "TRUE"))))/ nrow(appdf1))
  }


#this for loop is the function that will create the probability of both a brand and another brand appearing in a review
#the correct formula is shown below with 
# p(a) = the probability of brand "a" appearing in a review and 
# p(x) = the probablity of a review being about brand "a" in the df 
# p(y) = the probablity of a reivew being about brand "b" in the df
# p(b) = the probablity of brand "b" appearing in a review

#formula: (p(x&b) + p(y&a) + p(a) + p(b) - p(a&x) - p(y&b))/ num of reviews

for (x in 1:20){
  for(y in 1:20){
    p_ab[x,y] <- 
      ((length(which(branddf1[,x] == "TRUE" & length(which(appdf1[,y] == "TRUE"))))
     + length(which(branddf1[,y]== "TRUE" & length(which(appdf1[,x] == "TRUE"))))
     + length(which(appdf1[,x] == "TRUE")) + length(which(appdf1[,y] == "TRUE"))
    - length(which(branddf1[,x] == "TRUE" & length(which(appdf1[,x] == "TRUE"))))
    - length(which(branddf1[,y] == "TRUE" & length(which(appdf1[,y] == "TRUE"))))) / nrow(appdf1))
  
  }
}


############################# (dis)similartiy matrix(s) #############################################


#creating the matrixes to assign the values to, each one consiting of a 20 by 20 grid
#simMatrix is similarity matrix
#dissimMatrix is the dissimilarity matrix
simMatrix <- data.frame(matrix(ncol = 20,nrow = 20))
dissimMatrix <- data.frame(matrix(ncol = 20,nrow = 20))

#repeating the naming process as the ones before
for (x in 1:20) {
  names(simMatrix)[x] = brands1[x]
  names(dissimMatrix)[x] = brands1[x]
}

#for loop for the disimilarity matrix
#dividing by one scales it and makes it better for showing the disimilarties

for(x in 1:20){
  for(y in 1:20){
   data1 <- 1/(p_ab[x,y]/(p_of_ab[x,y] * p_of_ab[x,x]))
   
   dissimMatrix[x,y] <- data1 %>% round(5)
    
  }
}


#for loop for the similarity matrix
for(x in 1:20){
  for(y in 1:20){
    data2 <- (p_ab[x,y]/(p_of_ab[x,y] * p_of_ab[x,x]))
    
    simMatrix[x,y] <- data2 %>% round(5)
    
  }
}

#putting the rownames to be the column names
rownames(simMatrix) <- colnames(simMatrix)

#the code to write out the similarity matrix

write.csv(simMatrix, "similarityMatrix.csv")

########################################################################################

############### Perceptual Map ###########################
#our code
tmp.df1 <- dissimMatrix
dat.df1 <- tmp.df1
rownames(dat.df1) <- colnames(tmp.df1)
dat.mat1 <- data.matrix(dat.df1)
dat.mds1 <- cmdscale(dat.mat1, eig=TRUE, k=2)
#save results in new dataset
dat.mds1 # dat.mds is a list
result = data.frame(dat.mds1$points)
colnames(result) = c("Coordinate1", "Coordinate2")
# plot solution
options(scipen=999)
ggplot(data = result, aes(x = Warranty, y = Features)) +
  annotate(geom = "text", x = result$Coordinate1, y = result$Coordinate2, label =
             row.names(result)) + 
  ggtitle("Perceptual Map for Appliances Sold on Amazon ") +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
