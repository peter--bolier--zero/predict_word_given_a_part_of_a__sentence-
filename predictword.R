

library(tm)


# gonna go to 80% in pieces....

# Load ngrams pruned with probablities
# Second version
load("pn1.2") # n=1 grams, unigrams
load("pn2.2") # n=2 grams, bigrams
load("pn3.2") # n=3 grams, trigrams
load("pn4.2") # n=4 grams

# helper start reverse
# use same clean up (more or less) as used to built tdm
# tolower
# etc.

# Would it help to convert? Sigh.
# http://stackoverflow.com/questions/19204729/how-to-change-factor-labels-into-string-in-a-data-frame
# for now we dont convert the factor

# Handle input, i.e clean the text a bit. same cleanup as for corpus.
# Dropped stemming since that didn't rellay work.
# Dropped some other cleanup, sinde must text entered will be less corrupted/mangeld then the text used for analysis
# we have max n=4, so max3 words to use to predict the necxt...
# should we move the n=5 grams?



handleinput <- function(text="") {
  
  input.data <- data.frame(n=0, word1= NA, word2=NA, word3=NA) # default NA
  #print(text)
  
  # Preprocesses input, as we did for tetx, simplified for now
  # can we tm_map single line of text?
  text <- gsub("[^[:alnum:][:space:]\']", " ", text) 
  #text <- gsub(" '", " ", text ) # drop single quotes some where in text
  #text <- gsub("' ", " ", text )
  text <- tolower(text)
  text <- stripWhitespace(text)
  #print(text)
  
  # Check number of words in the text
  inputwords <- unlist(strsplit(text, " "))
  
  # Deal with unigram and bigrams
  n <- length(inputwords)
  #print(c("input in words:", inputwords, n))
  
  # use last n words in sentence
  if(n >= 3){
    # n, word1 word2 word3
    input.data <- data.frame(n=3, word1=inputwords[n-2], word2=inputwords[n-1], word3=inputwords[n])
  } else if ( n == 2) {
    # n, word1 word2 word3
    input.data <- data.frame(n=2, word1=NA, word2=inputwords[n-1], word3=inputwords[n])
  } else if (n == 1) {
    # so its just the one....
    # n, word1 word2 word3
    input.data <- data.frame(n=1, word1=NA, word2=NA, word3=inputwords[n])
  } # else already taken care of
  #print(class(input.data)) #,  as.character(input.data$word2)))
  result <- input.data
}


# n4 word1 word2 word3 expect/predict
# n3       word2 word3 expect/predict
# n2             word3 expect/predict
# n1 ... just...       expect

# example: The guy in front of me just bought a pound of bacon, a bouquet, and a case of
#
# subset(pn4, word1 == "a" & word2 == "case" & word3 == "of")
# subset(pn3,                word1 == "case" & word2 == "of")
# subset(pn2,                                  word1 == "of")


library(data.table)

lambda1<-7/10
lambda2<-2/10
lambda3<-1/10

predictnextword<- function (line="") {
  toplist       <- data.frame() # initially, we dont have anything, yet.
  stepstaken    <- data.frame() # initially, we dont have anything, yet.

  # clean, split get last few (max 3) words of sentence.
  if (is.null(line)) {
    line =""
  }
  input.data <- handleinput(line)

  # word1, word2, word3, predict
  if (input.data$n == 3) {
    # Using 4 gram
    # TODO refactor in sub
    shortlist <- subset(pn4, 
                       word1 == as.character(input.data$word1) 
                     & word2 == as.character(input.data$word2) 
                     & word3 == as.character(input.data$word3) )
    # shortlist$step <- "quadgram, 3 words used."

    answers = nrow(shortlist)
    if (answers >  5) {
      answers = 5
    }

    if (answers > 0) {
      shortlist$p <- shortlist$p * lambda1
      toplist   <- rbind(toplist, shortlist[order(shortlist$p, decreasing = TRUE)[1:answers],c("predict","p")])
      # stepstaken<- rbind(stepstaken)
      toplist<-aggregate(p ~ predict, toplist, sum)
    }

    if (nrow(toplist) < 10) {
      # look 4 more
      shortlist <- subset(pn4, 
                          word1 == as.character(input.data$word1) 
                        & word3 == as.character(input.data$word3) )
      # shortlist$step <- "quadgram, 2 words used (1 & 3)."
      
      answers = nrow(shortlist)
      if (answers >  5) {
        answers = 5
      }
      if (answers > 0) {
        shortlist$p <- shortlist$p * lambda1 * 2/3
        toplist <- rbind(toplist, shortlist[order(shortlist$p, decreasing = TRUE)[1:answers],c("predict","p")])
        toplist<-aggregate(p ~ predict, toplist, sum)
      }
    }
    
    if (nrow(toplist) < 10) {
      # look 4 more
      shortlist <- subset(pn4,
                        word1 == as.character(input.data$word1)
                      & word2 == as.character(input.data$word2) )
      # shortlist$step <- "quadgram, 2 words used (1 & 2)."

      answers = nrow(shortlist)
      if (answers >  5) {
        answers = 5
      }
      if (answers > 0) {
        shortlist$p <- shortlist$p * lambda1 * 2/3
        toplist <- rbind(toplist, shortlist[order(shortlist$p, decreasing = TRUE)[1:answers],c("predict","p")])
        toplist<-aggregate(p ~ predict, toplist, sum)
      }
    }

    if (nrow(toplist) < 10) {
      # look 4 more
      shortlist <- subset(pn4,
                           word2 == as.character(input.data$word2)
                         & word3 == as.character(input.data$word3) )
      # shortlist$step <- "quadgram, 2 words used (2 & 3)."

      answers = nrow(shortlist)
      if (answers >  5) {
        answers = 5
      }
      if (answers > 0) {
        shortlist$p <- shortlist$p * lambda1 * 2/3
        toplist <- rbind(toplist, shortlist[order(shortlist$p, decreasing = TRUE)[1:answers],c("predict","p")])
        toplist<-aggregate(p ~ predict, toplist, sum)
      }
    }
  }

  if (nrow(toplist) < 10) {
    # look 4 more
    
    # word1, word2, word3, predict
    if (input.data$n >= 2) {
      # Using with 3 gram
      # TODO refactor in sub
      shortlist <- subset(pn3, 
                          word1 == as.character(input.data$word2) 
                        & word2 == as.character(input.data$word3) )
      # shortlist$step <- "trigram, 2 words used."      

      answers = nrow(shortlist)
      if (answers >  5) {
        answers = 5
      }
      if (answers > 0) {
        shortlist$p <- shortlist$p * lambda2
        toplist <- rbind(toplist, shortlist[order(shortlist$p, decreasing = TRUE)[1:answers],c("predict","p")])
        toplist<-aggregate(p ~ predict, toplist, sum)
      }
      
      if (nrow(toplist) < 10) {
        # look 4 more
        shortlist <- subset(pn3,
                            word1 == as.character(input.data$word2) )
        # shortlist$step <- "trigram, 1 word used (1)."

        answers = nrow(shortlist)
        if (answers >  5) {
          answers = 5
        }
        if (answers > 0) {
          shortlist$p <- shortlist$p * lambda2 * 1/2
          toplist <- rbind(toplist, shortlist[order(shortlist$p, decreasing = TRUE)[1:answers],c("predict","p")])
          toplist<-aggregate(p ~ predict, toplist, sum)
        }
      }

      if (nrow(toplist) < 10) {
        # look 4 more
        shortlist <- subset(pn3,
                             word2 == as.character(input.data$word3) )
        # shortlist$step <- "trigram, 1 word used (2)."

        answers = nrow(shortlist)
        if (answers >  5) {
          answers = 5
        }
        if (answers > 0) {
          shortlist$p <- shortlist$p * lambda2 * 1/2
          toplist <- rbind(toplist, shortlist[order(shortlist$p, decreasing = TRUE)[1:answers],c("predict","p")])
          toplist<-aggregate(p ~ predict, toplist, sum)
        }
      }
    }
  }
  
  if (nrow(toplist) < 10) {
    # look 4 more
    
    # word1, word2, word3, predict
    if (input.data$n >= 1) {
      # Using with 2 gram
      # TODO refactor in sub
      shortlist <- subset(pn2, 
                          word1 == as.character(input.data$word3) )
      # shortlist$step <- "bigram, 1 word used."      
      
      answers = nrow(shortlist)
      if (answers >  5) {
        answers = 5
      }
      if (answers > 0) {
        shortlist$p <- shortlist$p * lambda3
        toplist <- rbind(toplist, shortlist[order(shortlist$p, decreasing = TRUE)[1:answers],c("predict","p")])
        toplist<-aggregate(p ~ predict, toplist, sum)
      }
    }
  }
  
  if (nrow(toplist) < 10) {
    # look 4 more
    
    shortlist <- pn1
    # shortlist$step <- "unigram, no words used."      
    
    answers = nrow(shortlist)
    if (answers >  5) {
      answers = 5
    }
    if (answers > 0) {
      shortlist$p <- shortlist$p * lambda3
      toplist <- rbind(toplist, shortlist[order(shortlist$p, decreasing = TRUE)[1:answers],c("predict","p")])
      toplist<-aggregate(p ~ predict, toplist, sum)
    }
  }
  
  # list(toplist[order(toplist$p, decreasing = TRUE),], stepstaken)
  toplist[order(toplist$p, decreasing = TRUE),]
}


# -------------------------------------------------------
# Quiz 1 NLP

doquizzes <- function() {
  system.time(aha <- predictnextword("The guy in front of me just bought a pound of bacon, a bouquet, and a case of"))
  # expect: beer
  print(aha)
  
  aha<-predictnextword("You're the reason why I smile everyday. Can you follow me please? It would mean the")
  # expect: world
  print(aha)
  
  aha <-predictnextword("Hey sunshine, can you follow me and make me the")
  # expect: happiest
  print(aha)
  
  aha<-predictnextword("Very early observations on the Bills game: Offense still struggling but the")
  # expect: defense
  print(aha)
  
  aha<-predictnextword("Go on a romantic date at the")
  # expect: beach
  print(aha)
  
  aha<-predictnextword("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
  # expect: way
  print(aha)
  
  aha<-predictnextword("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")
  # expect: time
  print(aha)
  
  aha<-predictnextword("Love that film and haven't seen it in quite some")
  # expect: time SHORTEND version
  print(aha)
  
  aha<-predictnextword("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
  # expect: fingers
  print(aha)
  
  aha<-predictnextword("Be grateful for the good times and keep the faith during the")
  # expect: bad
  print(aha)
  
  aha<-predictnextword("If this isn't the cutest thing you've ever seen, then you must be")
  # expect: asleep
  print(aha)
  
  # -------------------------------------------------------
  # Quiz 2 NLP
  aha<-predictnextword("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd")
  # expect die
  print(aha)
  
  aha<-predictnextword("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his")
  # expect marital
  print(aha)
  
  aha<-predictnextword("I'd give anything to see arctic monkeys this")
  # expect weekend
  print(aha)
  
  aha<-predictnextword("Talking to your mom has the same effect as a hug and helps reduce your")
  # expect stress
  print(aha)
  
  aha<-predictnextword("When you were in Holland you were like 1 inch away from me but you hadn't time to take a")
  # expect picture
  print(aha)
  
  aha<-predictnextword("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the")
  # expect matter
  print(aha)
  
  aha<-predictnextword("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each")
  # expect hand
  print(aha)
  
  aha<-predictnextword("Every inch of you is perfect from the bottom to the")
  # expect top
  print(aha)
  
  aha<-predictnextword("I'm thankful my childhood was filled with imagination and bruises from playing")
  # expect outside
  print(aha)
  
  aha<-predictnextword("I like how the same people are in almost all of Adam Sandler's")
  # expect movies
  print(aha)
}
