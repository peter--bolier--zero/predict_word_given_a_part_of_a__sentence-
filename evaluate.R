# evaluate.R

# Evaluate the built language 

# https://www.youtube.com/watch?v=OHyVNCvnsTo
# Best way is to actually test the model as if it were used for the appliaction, in our case for word prediction.
# and determine the accuracy
# Not practical

# so using intrinsic value, such as perplexity even though this is not a very good parameter to evaluate a model on, 
# for pilots and ' simple' excercises / projects it good enough to be used as an indicator

# for all senstences
#   assign P 
#   sum P
# normalised its the perplexity...

# https://www.cs.cornell.edu/courses/cs4740/2012sp/lectures/smoothing+backoff-1-4pp.pdf
#
# Perplexity
# For a test set W = w1 w2 .. wN,
# PP (W) = P (w1 w2 .. wN) ^ -1/N
# 
# PP(W) = N root of 1/P(W1W2...Wn)
# 
# 
# Chain rule , 1/P(w1w1..wn) is product 1/P(wi | w1...wi-1)
# for bigramns we only have van preceding word.

# min PP is max P(robablity)


# The higher the conditional probability of the word
# sequence, the lower the perplexity.
# Must be computed with models that have no knowledge of the test set. 

# So we held out parts of the corpus document. Is this reaaly a good approach or should we have a completely separate document? 
# how independent is the test set?

# https://lagunita.stanford.edu/c4x/Engineering/CS-224N/asset/slp4.pdf


# Need to detemine all the probablities given an unseen testset. 
# 
# checking https://www.quora.com/How-does-perplexity-function-in-natural-language-processing
# and https://www.youtube.com/watch?v=OHyVNCvnsTo
# Empirical tests showed it performed just good enough to get through most of the questions of the quizzes 
# when using a top 10 list  for suggestions.


# So lets only use 10% as is suggested by doc mentioned above
# PP(W) for 2grams = N root ( product 1,N * 1 / P(Wi|Wi-1)  )
# PP(W) for 3grams = N root ( product 1,N * 1 / P(Wi|Wi-1*Wi-2)  )
# PP(W) for 4grams = N root ( product 1,N * 1 / P(Wi|Wi-1*Wi-2*Wi-3)  ) OR PP(W)=P(W1, W2, W3) ^ (-1/N)


# rework formula for actual calculation?
# http://stats.stackexchange.com/questions/129352/how-to-find-the-perplexity-of-a-corpus
# \begin{align}
# \Perplexity(C) &= \sqrt[N]{\frac{1}{\prod_{i=1}^{m} p(s_{i})}}  \\
# &= 2^{\log_{2}{[\prod_{i=1}^{m} p(s_{i})]}^{-N}}  \\
# &= 2^{-\frac{1}{N}\log_{2}{[\prod_{i=1}^{m} p(s_{i})]}}  \\ 
# &= 2^{-\frac{1}{N}\sum_{i=1}^{m}\log_{2}{p(s_i)}}
# \end{align}
pp <- function(p) {
  return(exp(-sum(log(p)) / length(p)))
}

# Pragmatic index
# go trough test text and use ngram / model to predict word, use position in resulting top 10 as an indicator.
# Sum all position, lowest sum is best performing.
# Best possible score (highly unlikely) would be N*1 == N.
#
# We could test for model and test for each ngram, this would give some insight about the strength of a particualr ngram 
# and might help for further tuning
#
# still takes a lot of time when using the test sets...
#




library(stringr)

# load the the left out data of the corpora.



# load prediction model
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

# Use the last 10%, the other 80% is used for training, 10% unsued for now
last10p<- function(rawfilename) {
  print(paste("reading", rawfilename))
  rawfile     <- readtextfile(rawfilename)
  sample_size <- round(length(rawfile)/ 10, 0) # cut in 10 pieces
  
  for (i in 9) {
    print(c("loop ", i, format(Sys.time(), "%a %b %d %X %Y")))
    start = (i-1) * sample_size + 1
    end   = start + sample_size
    test_ind = seq(start, end)
    
    print(test_ind[1])
    test.data <- rawfile[test_ind]

    test.data <- cleanuptext(test.data)
    name = paste(rawfilename,".test.",i,".txt", sep="")
    writeLines(test.data, name)
    #breakhere
  }
}

last10p("en_US/en_US.blogs.txt")
last10p("en_US/en_US.news.txt")
last10p("en_US/en_US.twitter.txt")

# use files to test, open them and read line by line

readtextlines <- function(filename) {
  probs   <- {}

  getwd() # logging as a check
  
  # binary mode is essential, so we also read some weird character(codes)
  fileconn <- file(filename, open="rb") 
  fileconn # logging as a check
  
  # So we want to know how many lines there are...
  # Go over file until end is reached...
  nroflines  <- 0
  while (length( lineoffile <- readLines(fileconn, n = 1, warn = FALSE)) > 0) {
    nroflines  <- nroflines + 1
    linelenght <- nchar(lineoffile)

    #print(lineoffile)
    if (nroflines %% 25 == 0) {
      print(paste(" processed", nroflines, probs, sep=","))
    }
    
    # Get last word of sentence, give list with vector in it.
    # words <- strsplit(lineoffile, " ")[[1]]
    # print(words)
    # numberofwords <- length(words)
    # if (numberofwords > 4) {
    #   
    #   predictnextword
    #   print(paste())
    # }

    parts <- str_match(line, "^(.*\\s)(\\w*)$")
    lookup <- predictnextword(parts[,2])
    
    # print(parts[,2])
    #print(lookup)
    probs[nroflines] <- log(lookup[1,2])
    #print[parts[,3]]
        
    # count love lines
    # if (attr(regexpr("love", lineoffile),"match.length") > 0) {
    #   linesoflove <- linesoflove + 1
    # }
    print(probs)
    breakhere

  } 
  
  #nroflines
  
  # we're done
  close(fileconn)
  
  # list (numberoflines=nroflines, maxlinelength=maxlenline, lineofmaxlinelength=maxnoline, 
  #       loves = linesoflove, hates = linesofhate, lck = linechesskickbox)
  list(nroflines, probs, exactly)
}


results <- readtextlines("en_US/en_US.blogs.txt.test.5.txt")



readtextlines("en_US/en_US.news.txt.test.5.txt")

readtextlines("en_US/en_US.twitter.txt.test.5.txt")

# PP score

# PS score

# test using quizzes

# input, output, expected word
evaluatesentence <- function (sentence, expectedword) {
  wordnotfound  <- 1 # initially we cant predict right 
  
  predictedwords <- predictnextword(sentence)
  
  row <- predictedwords[predictedwords$predict == expectedword,]
  rownumber <- as.integer(row.names(row))
  if (length(rownumber) > 0) {
    wordnotfound <- 0
  } else {
    rownumber    <- 0 # prevent using empty numeric...
  }
  
  return (list(wordnotfound, rownumber))
}

evaluatequizzes <- function() {
  score  <- 0
  misses <- 0
  tests  <- 0
  
  # TODO rewrite in list/loop
  result <- evaluatesentence("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", "beer" )
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("You're the reason why I smile everyday. Can you follow me please? It would mean the", "world")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("Hey sunshine, can you follow me and make me the", "happiest")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("Very early observations on the Bills game: Offense still struggling but the", "defense")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("Go on a romantic date at the", "beach")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", "way")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some","time")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little","fingers")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("Be grateful for the good times and keep the faith during the","bad")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("If this isn't the cutests thing you've ever seen, then you must be","asleep")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  # -------------------------------------------------------
  # Quiz 2 NLP
  result <- evaluatesentence("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd","die")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his","marital")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("I'd give anything to see arctic monkeys this", "weekend")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("Talking to your mom has the same effect as a hug and helps reduce your", "stress")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("When you were in Holland you were like 1 inch away from me but you hadn't time to take a","picture")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the", "matter")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each", "hand")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("Every inch of you is perfect from the bottom to the","top")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("I'm thankful my childhood was filled with imagination and bruises from playing","outside")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]
  
  result <- evaluatesentence("I like how the same people are in almost all of Adam Sandler's", "movies")
  tests  <- tests + 1
  score  <- score  + result[[2]] # add rownumber as a score, lower rownumber is better
  misses <- misses + result[[1]]

  return(list(tests,score,misses))  
}



quizscore <- evaluatequizzes()
print(paste("Date,", Sys.time()," Tests# ", quizscore[[1]], ", score ", quizscore[[2]], ", not predicted at all ", quizscore[[3]]))

# [1] "Date, 2017-03-16 17:24:38  Tests#  20 , score  83 , not predicted at all  10"

predictnextword("I always order pizza with cheese and")


