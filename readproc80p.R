

# gonna go to 80% in pieces....

options(java.parameters = "-Xmx2048m")
library(rJava)

library(tm)
library(readr)

library(SnowballC)
library(stringr)

readtextfile <- function(filename) {
  
  # binary mode is essential, so we also read some weird character(codes)
  fileconn <- file(filename, open="rb") 
  filedata <- scan(file=fileconn, what=character(), sep="\n", fileEncoding="UTF-8")
  
  filedata 
}

# Still not completes satisfactory, some substirution mangle text, do more harm than good...
# so these are the left overs...
cleanuptext <- function(text) {
  # bit of cleaning, needed several runs. Should be easier somehow.
  # part of cleaning will be done use tm.
  
  # Note: is this system dependent?!
  # http://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
  
    
  # perhaps some codes should not be replaced with space.
  text <- gsub(" '"  , " ", text ) # drop single quotes some where in text 
  text <- gsub("' "  , " ", text )
  text <- gsub("^'"  , " ", text ) # at beginning and end of a line...
  text <- gsub("'$"  , " ", text )
  
  text <- gsub("-"  , " ", text ) # drop single dash
  text <- gsub("/"  , " ", text ) # drop single /
  
  text <- gsub("@"   , " ", text )
  text <- gsub("#"   , " ", text )
  
  text <- gsub("+[-]", " ", text ) # drop line ----
  text <- gsub("+[=]", " ", text ) # drop line ====

  text <- gsub("[:;(]([-]|[\\+]|[a-z]|[A-Z])?[)(:)]", " ", text) # new try to drop some smileys
  text <- gsub("[^[:alnum:][:space:]\']", " ", text)  # Drop all non alpha numerics originaaly dropped too many.
  
  # Trying to clean up some special characters...
  text <- gsub("[\u0080-\u0081]+", " ", text )
  text <- gsub("[\u0082\u0091\u0092\u00e2\u00ca]+", "'", text )
  text <- gsub("[\u0084\u0093\u0094]+", "\"", text )
  text <- gsub("[\u0085-\u00bf]+", " ", text )
  text <- gsub("[\u00c0-\u00c6]+", "a", text )
  text <- gsub("[\u00c7-\u00ff]+", " ", text )
  
  # found lots of single characters, like s, probably left overs
  # also lots of aa and aaa and aaaa
  # drop most single characters, should we leave i and a?
  text <- gsub(" [b-hj-z] "," ",text )
  
  # still found some of those ' left!
  text <- gsub(" '"  , " ", text ) # drop single quotes some where in text 
  text <- gsub("' "  , " ", text )
  
  text 
}

# cleanup test single line works fine... 
cleanuptext("most of the Oil fields and platforms were named after pagan '  gods'   ")

# assume in /Data Science - Hopkins/10 Capstone/CapstoneExcercises/final/

# use 4 pieces (80%) for traing, expect data is random enough...
# one pice left for testing (20%)
inpieces<- function(rawfilename) {
  print(paste("reading", rawfilename))
  rawfile     <- readtextfile(rawfilename)
  sample_size <- round(length(rawfile)/ 5, 0) # cur in 10 pieces
  
  for (i in 1:4) {
    print(c("loop ", i, format(Sys.time(), "%a %b %d %X %Y")))
    start = (i-1) * sample_size + 1
    end   = start + sample_size
    train_ind = seq(start, end)
    
    print(train_ind[1])
    train.data <- rawfile[train_ind]
    #print(class(train.data))
    train.data <- cleanuptext(train.data)
    name = paste(rawfilename,".train.",i,".txt", sep="")
    writeLines(train.data, name)
    #breakhere
  }
}


inpieces("en_US/en_US.blogs.txt")
inpieces("en_US/en_US.news.txt")
inpieces("en_US/en_US.twitter.txt")


# erm so on to corpus processing

# Assuming Data Science - Hopkins/10 Capstone/CapstoneExcercises/final"
setwd("en_US")
cname<-getwd()

badwords <- read_lines("../bad-words.txt")

library("RWeka")

onegramTokenizer   <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1)) 
twogramTokenizer   <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
threegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
fourgramTokenizer  <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))


# found that still some conversion were needed. expected alraedy doen in earkier phase.
# note carefull with ocntenttransfomer, without other part of the corpus gets mangled.
# See also https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/
tospace <- content_transformer(function(x, pattern) gsub(pattern," ", x))

docorpustdm <- function(piece) {
  print(paste("reading", piece))
  # expect 4 pieces
  for (i in 1:4) {
    print(c("loop ", i, format(Sys.time(), "%a %b %d %X %Y")))
    nameoutput = paste(piece,".train.",i,".txt", sep="")
    nameinput = paste("^",nameoutput, sep="")
    print(c(nameinput, nameoutput))
    
    docs <- Corpus(DirSource(cname, pattern=nameinput, mode="text"))
    docs <- tm_map(docs, content_transformer(tolower)) # without content_transfomer als is mapped... loosing meta info
    docs <- tm_map(docs, content_transformer(removePunctuation))
    docs <- tm_map(docs, content_transformer(removeNumbers))
    docs <- tm_map(docs, tospace, "' ") # also tried when reading data. investigate later why gsub didnt replace
    docs <- tm_map(docs, tospace, " '")
    
    #docs <- tm_map(docs, removeWords, stopwords("english")) # also removes you ... for instance, too much ifon lost
    docs <- tm_map(docs, removeWords, badwords)
    #docs <- tm_map(docs, content_transformer(stemDocument)) # lost a bittoo much in the translation....
    
    
    
    docs <- tm_map(docs, content_transformer(stripWhitespace))
    docs <- tm_map(docs, content_transformer(PlainTextDocument))
    namsesave= paste("corpus.", nameoutput, sep="")
    save(docs, file=namsesave)
    
    # might enhance weight: # TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
    
    print(c("Start n1", format(Sys.time(), "%a %b %d %X %Y")))
    tdmn1 <- TermDocumentMatrix(docs, control = list(tokenize = onegramTokenizer)) 
    print(c("Start n2", format(Sys.time(), "%a %b %d %X %Y")))
    tdmn2 <- TermDocumentMatrix(docs, control = list(tokenize = twogramTokenizer)) 
    print(c("Start n3", format(Sys.time(), "%a %b %d %X %Y")))
    tdmn3 <- TermDocumentMatrix(docs, control = list(tokenize = threegramTokenizer))
    print(c("Start n4", format(Sys.time(), "%a %b %d %X %Y")))
    tdmn4 <- TermDocumentMatrix(docs, control = list(tokenize = fourgramTokenizer))
    print(c("Done", format(Sys.time(), "%a %b %d %X %Y")))
    
    namsesave= paste("tdmn1.", nameoutput, sep="")
    print(c("saving:" , namsesave))
    save(tdmn1, file=namsesave)
    
    namsesave= paste("tdmn2.", nameoutput, sep="")
    print(c("saving:" , namsesave))
    save(tdmn2, file=namsesave)
    
    namsesave= paste("tdmn3.", nameoutput, sep="")
    print(c("saving:" , namsesave))
    save(tdmn3, file=namsesave)
    
    namsesave= paste("tdmn4.", nameoutput, sep="")
    print(c("saving:" , namsesave))
    save(tdmn4, file=namsesave)
  }
}


docorpustdm("en_US.blogs.txt")
docorpustdm("en_US.news.txt")
docorpustdm("en_US.twitter.txt")

rm(badwords)

# convert list of dtms into one big dtm
# tdmn1.list = dir(pattern="tdmn1.")

# TODO improve logic / loop

# 111111111111111111
# Combine all n1
# ------------------
blogsn1.1 <- local({	
  load("tdmn1.en_US.blogs.txt.train.1.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
blogsn1.2 <- local({	
  load("tdmn1.en_US.blogs.txt.train.2.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
blogsn1.3 <- local({	
  load("tdmn1.en_US.blogs.txt.train.3.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
blogsn1.4 <- local({	
  load("tdmn1.en_US.blogs.txt.train.4.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
})

# ------------------
newsn1.1 <- local({	
  load("tdmn1.en_US.news.txt.train.1.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
newsn1.2 <- local({	
  load("tdmn1.en_US.news.txt.train.2.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
newsn1.3 <- local({	
  load("tdmn1.en_US.news.txt.train.3.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
newsn1.4 <- local({	
  load("tdmn1.en_US.news.txt.train.4.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
})

# ------------------
twitn1.1 <- local({	
  load("tdmn1.en_US.twitter.txt.train.1.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
twitn1.2 <- local({	
  load("tdmn1.en_US.twitter.txt.train.2.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
twitn1.3 <- local({	
  load("tdmn1.en_US.twitter.txt.train.3.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
twitn1.4 <- local({	
  load("tdmn1.en_US.twitter.txt.train.4.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
})


print(c("Combine n1", format(Sys.time(), "%a %b %d %X %Y")))
combinedn1<-c(blogsn1.1, blogsn1.2, blogsn1.3, blogsn1.4, 
           newsn1.1 , newsn1.2 , newsn1.3 , newsn1.4 ,
           twitn1.1 , twitn1.2 , twitn1.3 , twitn1.4 )
print(c("Done", format(Sys.time(), "%a %b %d %X %Y")))

rm(blogsn1.1, blogsn1.2, blogsn1.3, blogsn1.4, 
   newsn1.1 , newsn1.2 , newsn1.3 , newsn1.4 ,
   twitn1.1 , twitn1.2 , twitn1.3 , twitn1.4 )

save(combinedn1,     file="combinedn1")

# remove sparse terms, need some optimisation.
combinedn1.rst <- removeSparseTerms(combinedn1, 0.90) # remove terms more sparse than ...
inspect(combinedn1.rst[1:10,4])
save(combinedn1.rst, file="combinedn1.rst")

# 222222222222222222
# Combine all n2
# ------------------
blogsn2.1 <- local({	
  load("tdmn2.en_US.blogs.txt.train.1.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
blogsn2.2 <- local({	
  load("tdmn2.en_US.blogs.txt.train.2.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
blogsn2.3 <- local({	
  load("tdmn2.en_US.blogs.txt.train.3.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
blogsn2.4 <- local({	
  load("tdmn2.en_US.blogs.txt.train.4.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
})

# ------------------
newsn2.1 <- local({	
  load("tdmn2.en_US.news.txt.train.1.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
newsn2.2 <- local({	
  load("tdmn2.en_US.news.txt.train.2.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
newsn2.3 <- local({	
  load("tdmn2.en_US.news.txt.train.3.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
newsn2.4 <- local({	
  load("tdmn2.en_US.news.txt.train.4.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
})

# ------------------
twitn2.1 <- local({	
  load("tdmn2.en_US.twitter.txt.train.1.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
twitn2.2 <- local({	
  load("tdmn2.en_US.twitter.txt.train.2.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
twitn2.3 <- local({	
  load("tdmn2.en_US.twitter.txt.train.3.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
twitn2.4 <- local({	
  load("tdmn2.en_US.twitter.txt.train.4.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
})


print(c("Combine n2", format(Sys.time(), "%a %b %d %X %Y")))
combinedn2<-c(blogsn2.1, blogsn2.2, blogsn2.3, blogsn2.4, 
              newsn2.1 , newsn2.2 , newsn2.3 , newsn2.4 ,
              twitn2.1 , twitn2.2 , twitn2.3 , twitn2.4 )
print(c("Done", format(Sys.time(), "%a %b %d %X %Y")))
save(combinedn2,     file="combinedn2")

rm(blogsn2.1, blogsn2.2, blogsn2.3, blogsn2.4, 
   newsn2.1 , newsn2.2 , newsn2.3 , newsn2.4 ,
   twitn2.1 , twitn2.2 , twitn2.3 , twitn2.4 )

# remove sparse terms, need some optimisation.
combinedn2.rst <- removeSparseTerms(combinedn2, 0.90) # 90% remove terms more sparse than ...
inspect(combinedn2.rst[1:10,7])

save(combinedn2.rst, file="combinedn2.rst")


# 333333333333333333
# Combine all n3
# ------------------
blogsn3.1 <- local({	
  load("tdmn3.en_US.blogs.txt.train.1.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
blogsn3.2 <- local({	
  load("tdmn3.en_US.blogs.txt.train.2.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
blogsn3.3 <- local({	
  load("tdmn3.en_US.blogs.txt.train.3.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
blogsn3.4 <- local({	
  load("tdmn3.en_US.blogs.txt.train.4.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
})

# ------------------
newsn3.1 <- local({	
  load("tdmn3.en_US.news.txt.train.1.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
newsn3.2 <- local({	
  load("tdmn3.en_US.news.txt.train.2.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
newsn3.3 <- local({	
  load("tdmn3.en_US.news.txt.train.3.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
newsn3.4 <- local({	
  load("tdmn3.en_US.news.txt.train.4.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
})

# ------------------
twitn3.1 <- local({	
  load("tdmn3.en_US.twitter.txt.train.1.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
twitn3.2 <- local({	
  load("tdmn3.en_US.twitter.txt.train.2.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
twitn3.3 <- local({	
  load("tdmn3.en_US.twitter.txt.train.3.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
twitn3.4 <- local({	
  load("tdmn3.en_US.twitter.txt.train.4.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
})


print(c("Combine n3", format(Sys.time(), "%a %b %d %X %Y")))
combinedn3<-c(blogsn3.1, blogsn3.2, blogsn3.3, blogsn3.4, 
              newsn3.1 , newsn3.2 , newsn3.3 , newsn3.4 ,
              twitn3.1 , twitn3.2 , twitn3.3 , twitn3.4 )
print(c("Done", format(Sys.time(), "%a %b %d %X %Y")))
save(combinedn3,     file="combinedn3")

rm(blogsn3.1, blogsn3.2, blogsn3.3, blogsn3.4, 
   newsn3.1 , newsn3.2 , newsn3.3 , newsn3.4 ,
   twitn3.1 , twitn3.2 , twitn3.3 , twitn3.4 )

# remove sparse terms, need some optimisation.
combinedn3.rst <- removeSparseTerms(combinedn3, 0.90) # 90% remove terms more sparse than ...
inspect(combinedn3.rst[1:10,7])

save(combinedn3.rst, file="combinedn3.rst")


# 444444444444444444
# Combine all n4
# ------------------
blogsn4.1 <- local({	
  load("tdmn4.en_US.blogs.txt.train.1.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
blogsn4.2 <- local({	
  load("tdmn4.en_US.blogs.txt.train.2.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
blogsn4.3 <- local({	
  load("tdmn4.en_US.blogs.txt.train.3.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
blogsn4.4 <- local({	
  load("tdmn4.en_US.blogs.txt.train.4.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
})

# ------------------
newsn4.1 <- local({	
  load("tdmn4.en_US.news.txt.train.1.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
newsn4.2 <- local({	
  load("tdmn4.en_US.news.txt.train.2.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
newsn4.3 <- local({	
  load("tdmn4.en_US.news.txt.train.3.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
newsn4.4 <- local({	
  load("tdmn4.en_US.news.txt.train.4.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
})

# ------------------
twitn4.1 <- local({	
  load("tdmn4.en_US.twitter.txt.train.1.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
twitn4.2 <- local({	
  load("tdmn4.en_US.twitter.txt.train.2.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
twitn4.3 <- local({	
  load("tdmn4.en_US.twitter.txt.train.3.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
}) 
twitn4.4 <- local({	
  load("tdmn4.en_US.twitter.txt.train.4.txt") 
  stopifnot(length(ls())==1) 
  environment()[[ls()]] 
})


print(c("Combine n4", format(Sys.time(), "%a %b %d %X %Y")))
combinedn4<-c(blogsn4.1, blogsn4.2, blogsn4.3, blogsn4.4, 
              newsn4.1 , newsn4.2 , newsn4.3 , newsn4.4 ,
              twitn4.1 , twitn4.2 , twitn4.3 , twitn4.4 )
print(c("Done", format(Sys.time(), "%a %b %d %X %Y")))
save(combinedn4,     file="combinedn4")

rm(blogsn4.1, blogsn4.2, blogsn4.3, blogsn4.4, 
   newsn4.1 , newsn4.2 , newsn4.3 , newsn4.4 ,
   twitn4.1 , twitn4.2 , twitn4.3 , twitn4.4  )

# remove sparse terms, need some optimisation.
combinedn4.rst <- removeSparseTerms(combinedn4, 0.90) # 90% remove terms more sparse than ...
inspect(combinedn4.rst[1:10,7])


save(combinedn4.rst, file="combinedn4.rst")


# --- next processing step
# using combined term document matrix slightly pruned


# For prediction we may need some adjsutments
# https://www.cs.cornell.edu/courses/cs4740/2012sp/lectures/smoothing+backoff-1-4pp.pdf
# Smoothing
# - Add-one smoothing 
# - Discounting

# trying discounting
# Good-Turing discounting

# For count < 5
#                N(c+1)
# C* = (c + 1) ------------
#                  Nc

# So we need to sum all ngrams which occur 1,2,3,4 and 5 times
# in  the cleanup set.
# sum over rows (i.e. the 3 documents)

# note: counts will be decimal number 

discount <- function(dfinput){
  # Only for the lower Counts we discount... 
  counts={}
  for (i in 1:25) {
    counts[i] = nrow((subset(dfinput, rowsum == i)))
    #print(i)
  }
  print(counts)
  # correct value
  for (i in 1:24) {
    if (counts[i] > 0) {
      #print(i)
      #print(head(dfinput$rowsum[ dfinput$rowsum == i ]))
      Cnew = (i+1)*counts[i+1]/counts[i]
      #print(Cnew)
      dfinput$rowsum[ dfinput$rowsum == i ] <- Cnew
      #print(head(dfinput$rowsum[ dfinput$rowsum == i ]))
      #print(head(dfinput$rowsum[ dfinput$rowsum == Cnew ]))
    } else {
      print(c("Had2skip i ", i))
    }
  }
  dfinput
}



# TODO: put in helper function.

# -------------------------------------------
# N = 1 = unigram
# sum occurences over all docs (12) -> rowsum
tdmn1.rst.matrix   <- as.matrix(combinedn1.rst)
dftdmn1            <- data.frame(tdmn1.rst.matrix)
totalsn1           <- colSums(dftdmn1)# determine totals before adding another column.

dftdmn1[,"rowsum"] <- rowSums(tdmn1.rst.matrix)
dftdmn1            <- discount(dftdmn1)
dftdmn1[,"p"]      <- dftdmn1$rowsum / sum(totalsn1)

# enhace dataframe
dftdmn1$text = rownames(dftdmn1)
rownames(dftdmn1)<-seq(nrow(dftdmn1))

keepcolumns=c("rowsum", "p", "text")
dftdmn1 <- dftdmn1[keepcolumns]

object.size(dftdmn1)

rm(tdmn1.rst.matrix, totalsn1)
save(dftdmn1,file="dftdmn1")

# ------------------------------------------
# N = 2 == bigram
# tdmn2.rst.matrix   <- as.matrix(combinedn2.rst)
# dftdmn2            <- data.frame(tdmn2.rst.matrix)
# totalsn2           <- colSums(dftdmn2)# determine totals before adding another column.
# 
# dftdmn2[,"rowsum"] <- rowSums(tdmn2.rst.matrix)
# dftdmn2            <- discount(dftdmn2)
# dftdmn2[,"p"]      <- dftdmn2$rowsum / sum(totalsn2)
# 
# # enhace dataframe
# dftdmn2$text = rownames(dftdmn2)
# rownames(dftdmn2)<-seq(nrow(dftdmn2))
# 
# object.size(dftdmn2)
# rm(tdmn2.rst.matrix, totalsn2)
# save(dftdmn2, file="dftdmn2")


# For infused dataset
load("dftdmn2infused")
dftdmn2infused$count<-as.numeric(dftdmn2infused$count)
totalsn2 <- sum(dftdmn2infused$count)
colnames(dftdmn2infused)<-c("text", "rowsum")

dftdmn2infused       <- discount(dftdmn2infused)
dftdmn2infused[,"p"] <- dftdmn2infused$rowsum / sum(totalsn2)

dftdmn2<-dftdmn2infused
save(dftdmn2, file="dftdmn2")


# ------------------------------------------
# N = 3 == trigram
# tdmn3.rst.matrix   <- as.matrix(combinedn3.rst)
# dftdmn3            <- data.frame(tdmn3.rst.matrix)
# totalsn3           <- colSums(dftdmn3)# determine totals before adding another column.
# 
# dftdmn3[,"rowsum"] <- rowSums(tdmn3.rst.matrix)
# dftdmn3            <- discount(dftdmn3)
# dftdmn3[,"p"]      <- dftdmn3$rowsum / sum(totalsn3)
# 
# 
# # enhace dataframe
# dftdmn3$text = rownames(dftdmn3)
# rownames(dftdmn3)<-seq(nrow(dftdmn3))
# 
# object.size(dftdmn3)
# rm(tdmn3.rst.matrix, totalsn3)
# save(dftdmn3, file="dftdmn3")

# For infused dataset
load("dftdmn3infused")
dftdmn3infused$count<-as.numeric(dftdmn3infused$count)
totalsn3 <- sum(dftdmn3infused$count)
colnames(dftdmn3infused)<-c("text", "rowsum")

dftdmn3infused       <- discount(dftdmn3infused)
dftdmn3infused[,"p"] <- dftdmn3infused$rowsum / sum(totalsn3)

dftdmn3<-dftdmn3infused
save(dftdmn3, file="dftdmn3")



# ------------------------------------------
# N = 4 == trigram
# tdmn4.rst.matrix   <- as.matrix(combinedn4.rst)
# dftdmn4            <- data.frame(tdmn4.rst.matrix)
# totalsn4           <- colSums(dftdmn4)# determine totals before adding another column.
# 
# dftdmn4[,"rowsum"] <- rowSums(tdmn4.rst.matrix)
# dftdmn4            <- discount(dftdmn4)
# dftdmn4[,"p"]      <- dftdmn4$rowsum / sum(totalsn4)
# 
# # enhace dataframe
# dftdmn4$text = rownames(dftdmn4)
# rownames(dftdmn4)<-seq(nrow(dftdmn4))
# 
# object.size(dftdmn4)
# rm(tdmn4.rst.matrix, totalsn4)
# save(dftdmn4, file="dftdmn4")
# 

# For infused dataset
load("dftdmn4infused")
dftdmn4infused$count<-as.numeric(dftdmn4infused$count)
totalsn4 <- sum(dftdmn4infused$count)
colnames(dftdmn4infused)<-c("text", "rowsum")

dftdmn4infused       <- discount(dftdmn4infused)
dftdmn4infused[,"p"] <- dftdmn4infused$rowsum / sum(totalsn4)

dftdmn4<-dftdmn4infused
save(dftdmn4, file="dftdmn4")


# ------------------------------
# Try using only relavnt data...
# drop other columns

# probabilities for n=1 unigram. 
pn1 <- data.frame(
  predict = dftdmn1$text,
  p       = dftdmn1$p
)

object.size(pn1)
save(pn1, file="pn1")

# Is there a better way?
splitonspacen2 <- strsplit(as.character(dftdmn2$text)," ")
pn2 <- data.frame(
  word1   = sapply(splitonspacen2, function(x){x[1]}),
  predict = sapply(splitonspacen2, function(x){x[2]}),
  p       = dftdmn2$p
)
# So we have the words separated and added the earlier determined probablity

object.size(pn2)
save(pn2, file="pn2")


# Lets try this for n=3 
splitonspacen3 <- strsplit(as.character(dftdmn3$text)," ")
pn3 <- data.frame(
  word1   = sapply(splitonspacen3, function(x){x[1]}),
  word2   = sapply(splitonspacen3, function(x){x[2]}),
  predict  = sapply(splitonspacen3, function(x){x[3]}),
  p       = dftdmn3$p
)


object.size(pn3)
save(pn3, file="pn3")


# -----

splitonspacen4 <- strsplit(as.character(dftdmn4$text)," ")
pn4 <- data.frame(
  word1   = sapply(splitonspacen4, function(x){x[1]}),
  word2   = sapply(splitonspacen4, function(x){x[2]}),
  word3   = sapply(splitonspacen4, function(x){x[3]}),
  predict = sapply(splitonspacen4, function(x){x[4]}),
  p       = dftdmn4$p
)

object.size(pn4)
save(pn4, file="pn4")

# --- clean up, only need the words and probilities
rm(combinedn1.rst, combinedn2.rst, combinedn3.rst, combinedn4.rst)
rm(dftdmn1, dftdmn2, dftdmn3, dftdmn4)
rm(splitonspacen2, splitonspacen3, splitonspacen4)
rm(discount)


