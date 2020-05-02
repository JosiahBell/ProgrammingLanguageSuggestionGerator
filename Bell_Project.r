## Josiah Bell
## Bell_Project.r
## Analyzing data on GitHub devolopers
## about what programming languages they have tagged
## in their user and forked repositories to suggest
## languages they may find useful or interesting.

getwd()
setwd("C:/Users/ctjbe/Documents/College/Classes/Spring-2020/Data Mining I/Project")
getwd()

## I cut the original dataset to only include the first 38 columns to only include the
## tags for programming languages, not any other technologies. [It took me 8 hours to
## get to this point because I kept trying to use the entire dataset when I did not
## need to.]
users<-read.csv("user-languages.csv",header = TRUE, sep =",", stringsAsFactors = TRUE)
names(users)

## [1] "user_id"          "assembly"         "batchfile"        "c"                "c."              
## [6] "c.."              "clojure"          "coffeescript"     "css"              "elixir"          
## [11] "emacs.lisp"       "go"               "haskell"          "html"             "java"            
## [16] "javascript"       "jupyter.notebook" "kotlin"           "lua"              "matlab"          
## [21] "objective.c"      "objective.c.."    "ocaml"            "perl"             "php"             
## [26] "powershell"       "purebasic"        "python"           "rascal"           "ruby"            
## [31] "rust"             "scala"            "shell"            "swift"            "tex"             
## [36] "typescript"       "vim.script"       "vue"

nrow(users) ## 17462
ncol(users) ## 38

## Let's simplify the data: let's set a tag for a user to 1 if they used that tag
## at all, and 0 if they did not. This allows us to simply look at whether or not
## a user has used the tag of a particular langauge or technology in their 
## repositories, rather than how much they used it in ratio to other tags they used.

## Let's use this converted data to count the quantity of different 
## types of tags that each user used in their repositories in a new
## column called tagQuantity.

## We need to create a column for the tagQuantity:
users$tagQuantity <- "?"


## Let's also use the new dataset as we convert it to count how many
## users used each tag and store the totals in a new row at the end.

## To do this, I manually added the 17462nd row for the totals with each column populated
## with a 0, because I was experiencing great trouble in attempting to add a row for it.

## To do all of this at once to save time, I simply
## combine each to the follow nested for loop:
for(i in 1:17461) {
  tagQuant <- 0
  for(j in 2:38) {
    if(users[i,j] > 0)
      users[i,j] <- 1
    tagQuant <- tagQuant + users[i,j]
    users[17462,j] <- users[17462,j] + users[i,j]
  }
  users[i,39] <- tagQuant
}

## Let's also store the total number of different tags used by all of the users:
totVal <- 0
for(j in 2:38) {
  totVal <- totVal + users[17462,j]
}
users[17462, 39] <- totVal

## To get here took me 8 hours of messing around with the dataset to make it work
## Now that I have the dataset with the information I want, I can begin making some
## graphs and models.

## Let's make a bar plot of the amount of users that used each programming language tag.
totalUses <- matrix(nrow = 1, ncol = 37)
for(i in 1:37) {
  totalUses[1,i] <- users[17462,i]
}
rownames(totalUses) <- c("TotalUses")
languages <- names(users[2:38])
languages
colnames(totalUses) <- c(languages)
totalUses
barplot(totalUses, xlab="Language", ylab="Number of Users Tagged", main="Frequency of Languages Tagged on GitHub")

## Now let's make a histogram that displays the number of tags used by a single user on
## the x-axis, and number of users on the y-axis.
userTags <- matrix(nrow = 1, ncol = 17461)
for(i in 1:17461) {
  userTags[1,i] <- as.numeric(users[i, 39])
}

hist(as.numeric(userTags[1,]), xlab="Number of Languages Tagged By A User", ylab="Number of Users", main="Number of Users vs. Number of Languages Tagged By A User")

# --------

## Now let's try to predict whether a user tagged a language
## based on other languages tagged in their repositories.

## First let's try by training a Decision Tree to predict
## if a user has tagged Java based on their other tags.
library(C50)
for(i in 1:17462) {
  for (j in 2:38) {
    if(users[i,j] == 1)
      users[i,j] <- "yes"
    else
      users[i,j] <- "no"
  }
}
for (j in 2:38) {
  users[,j] <- as.factor(users[,j])
}
str(users$java)
users[1,2]
c5tree=C5.0(users[1:17461,c(2:14,16:38)],users[1:17461,15:15])
summary(c5tree)

# Predict users having tagged java on test data
predTest<-read.csv("JavaTest.csv",header = TRUE, sep =",", stringsAsFactors = TRUE)
predTest
predTest$java<-"?"
predTest
pred=predict(c5tree,predTest[1:10,c(2:14,16:38)],type="class")
pred

# Get probability of users having tagged java
pred=predict(c5tree,predTest[1:10,c(2:14,16:38)],type="prob")
pred

# Get prediction accuracy based on training data
predAcc=predict(c5tree,users[1:17461,c(2:14,16:38)],type="class")
predAcc
javaTagged=users$java
# Calculate Decision Tree Error
treeError=mean(predAcc != javaTagged[1:17461])
treeError # [1] 0.3078289

# Finally, let's try predicting with a Naive Bayes model

library(e1071)
nb = naiveBayes(java~.,users[1:17461,c(2:38)])
nb

# Predict users having java tagged on test data
predNB<-predict(nb,predTest[1:10,c(2:14,16:38)],type=c("class"))
predNB

# Get probability of users having java tagged
predNB<-predict(nb,predTest[1:10,c(2:14,16:38)],type=c("raw"))
predNB

# Get prediction accuracy bassed off of training data
predNBAcc<-predict(nb,users[1:17461, c(2:14, 16:38)],type=c("class"))
predNBAcc
javaTagged
# Calcualte Naive Bayes Error
nbError=mean(predNBAcc != javaTagged[1:17461])
nbError # [1] 0.3177367

# Compare the errors:
# Decision Tree Error
treeError # [1] 0.3078289

# Naive Bayes Error:
nbError # [1] 0.3177367

# Neither are entirely accurate, each with a ~30.8-31.8% error.
# This is understandable, considering the fact that anyone 
# can use any set of programming languages they learn.

# --------

# This information could be used to suggest programming languages
# to users based on languages they have already tagged in their
# repositories. But how would we do that?

# Well, we would need to know which ones they have tagged and
# which ones they have not. Then, we would run either a
# Decision Tree or Naive Bayes model prediction on each of the
# languages they have not yet tagged to determine which ones they
# would most likely have tagged based on the others. Then, we would
# recommend the one(s) that relate closest.

# Let's make a simple function for this using decision trees:
suggestLanguage <- function(taggedLanguages, userData) {
  langs <- names(taggedLanguages)
  suggestions = matrix(nrow = 1, ncol = 37)
  for (i in 1:37) {
    suggestions[i] <- 0
  }
  for(i in 1:37) {
    if(i == 1) {
      if(taggedLanguages[i] == "no") {
        c5tree=C5.0(userData[1:17461,c(3:38)],userData[1:17461, 2:2])
        prob=predict(c5tree, taggedLanguages[1:1,c(2:37)],type="prob")
        suggestions[i] <- prob[1,2]
      }
    }
    else if(i == 37) {
      if(taggedLanguages[i] == "no") {
        c5tree=C5.0(userData[1:17461,c(2:37)],userData[1:17461, 38:38])
        prob=predict(c5tree, taggedLanguages[1:1,c(1:36)],type="prob")
        suggestions[i] <- prob [1,2]
      }
    }
    else {
      if(taggedLanguages[i] == "no") {
        # Train the tree for this language
        c5tree=C5.0(userData[1:17461,c(2:i,(i+2):38)],userData[1:17461, (i+1):(i+1)])
        prob=predict(c5tree, taggedLanguages[1:1,c(1:(i-1),(i+1):37)],type="prob")
        suggestions[i] <- prob [1,2]
      }
    }
  }
  suggestion = 0
  relation = 0
  for(i in 1:37) {
    if(suggestions[i] > relation) {
      relation <- suggestions[i]
      suggestion <- i
    }
  }
  finalSuggestion <- matrix(nrow=1, ncol=2)
  finalSuggestion[1,1] <- langs[suggestion]
  finalSuggestion[1,2] <- relation
  return(finalSuggestion)
}

# Let's test on user achalddave
achalddaveLanguages <- users[224,2:38]
achalddaveLanguages
achalddaveSuggested <- suggestLanguage(achalddaveLanguages, users)
achalddaveSuggested

##      [,1] [,2]               
## [1,] "c"  "0.645758152046694"

## This result suggests that achalddave might be interested in the c
## programming language, stating that it relates to the languages 
## they have already tagged in their user and forked repositories
## by 64.576% - which essentially means that it predicted a 64.576%
## probability that achalddave would have already had 'c' tagged 
## in their repositories. It is verified: they have not according to
## the training data.