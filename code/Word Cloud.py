#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Dec  5 00:09:59 2017
"""

import pandas as pd
import pprint, pickle
import numpy as np
import sklearn.utils

input_data = pd.read_csv('desc.csv')

import re
import unicodedata
import string
import nltk
from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer
from nltk.tokenize import RegexpTokenizer
import html.parser 

tokenizer = RegexpTokenizer(r'\w+')
punctuation = list(string.punctuation)
htmlParser = html.parser.HTMLParser()
username_pattern = re.compile("@\S+")
url_pattern = re.compile("http\S+")
word_pattern = re.compile("wo[m]+")

corpus = []
for i in range(len(input_data)):
    tweet = re.sub('[^a-zA-Z]', ' ', input_data['word'][i])
    parsedTweet = htmlParser.unescape(tweet)
    tweet_v1 = re.sub(url_pattern, "", parsedTweet)
    tweet_v2=re.sub(username_pattern, "", tweet_v1)
    review= tokenizer.tokenize(tweet_v2)
    review = [word for word in review if not word in set(stopwords.words('english') + punctuation + ['rt', 'https','via', 'co'])]
    review = [str(x) for x in review]
    review = ' '.join(review)
    corpus.append(review)
    

import operator 
from collections import Counter
from collections import defaultdict
  
all_words = []
sorted_freq_word = defaultdict(lambda : defaultdict(int))

for i in range(len(corpus)):
    tweet = corpus[i]
    tweet = tweet.split()
    for w in tweet:
        all_words.append(w)
        
all_words_freq = nltk.FreqDist(all_words)
sorted_freq_word = all_words_freq.most_common()
sorted_freq_word_DF = pd.DataFrame(sorted_freq_word)
sorted_freq_word_DF.to_csv('Desc_1_Term.csv')
print(sorted_freq_word[:20])

#all_words_pd = pd.DataFrame(all_words_freq)

#Co- Occurance Checking for location
import operator 
from collections import Counter
from collections import defaultdict
com = defaultdict(lambda : defaultdict(int))
count_all = Counter()
for i in range(len(corpus)):
    for word in corpus[i]:
        tweet = corpus[i]
        tweet = tweet.split()
        # Create a list with all the terms
        terms_all = [term for term in tweet]
        # Build co-occurrence matrix
        for i in range(len(terms_all)-1):            
            for j in range(i+1, len(terms_all)):
                w1, w2 = sorted([terms_all[i], terms_all[j]])                
                if w1 != w2:
                    com[w1][w2] += 1
# Creating 20 most common used words                    
com_max = []
# For each term, look for the most common co-occurrent terms
for t1 in com:
    t1_max_terms = sorted(com[t1].items(), key=operator.itemgetter(1), reverse=True)[:5]
    for t2, t2_count in t1_max_terms:
        com_max.append(((t1, t2), t2_count))
# Get the most frequent co-occurrences
terms_max = sorted(com_max, key=operator.itemgetter(1), reverse=True)
print(terms_max[:20])
