Word prediction

Given a sentence 'predict' the next word. 

The corpus used was donated by swiftkey for the Coursera data science course. The text consisted of new, blogs and tweets in several languages. For this project only the English version was used.

Cleaning took several iterations and is vital for the wordprediction. For example, first I used stemming which decreased the dataset somewhat but lead to too much information loss and bad predictions. Another topic is the dataset's format, i.e. the characterset. After cleaning the resulating ngram sets were enhanced by adding datagrams from another source:
Davies, Mark. (2011) N-grams data from the Corpus of Contemporary American English (COCA). Downloaded from http://www.ngrams.info on March 4, 2017. 

The prediction algorithm is fairly simple:

1 - use 3 words of the 4-grams (quadgrams) to find the word to predict
2 - if we don't have enough words found use a partial match of the 4-gram
3 - if we still don't have enough, we use 2 words of the 3-gram (trigram) to find the word to predict.
4 - if we still don't have enough words found, we use a partial match
5 - Use the 2-gram (bigram) and finally,
6 - use the unigrams.

Still to do: better clean up, simplify algorithm, or look for a smarter algorithm with a better prediction.

Shiny app, deployed at: https://peterbolier.shinyapps.io/predict_word_given_a_part_of_a__sentence/
Presentation at: http://rpubs.com/peterbolier/basicwordprediction
