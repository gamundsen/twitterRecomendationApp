#create a function to remove unwanted charecters from the 
# tweet in preperation for sentiment analysis
removeBadChar <- function (pText) {
    
    pText = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", pText)
    pText = gsub("@\\w+", "", pText)
    pText = gsub("[[:punct:]]", "", pText)
    pText = gsub("[[:digit:]]", "", pText)
    pText = gsub("http\\w+", "", pText)
    pText = gsub("[ \t]{2,}", "", pText)
    pText = gsub("^\\s+|\\s+$", "", pText)
    pText = gsub("amp", "", pText)
    pText = gsub("\n", " ", pText)
    pText = gsub("\xed\xa0\xbd","",pText)
    pText = gsub("\xed\xb2\x9b","",pText)
    pText

}

getSentiment <- function(text) {
    p=NA
    getPol<-function(text){p<<-polarity(text)[[1]]$polarity}
    try(getPol(text),silent = T)
    return(p)    
}
#movieNames=c("Fury","Guardians of the Galaxy","Interstellar","Into The Woods","The Imitatioin Game","Gone Girl")
movieNames=c("Guardians of the Galaxy","Into The Woods","Interstellar")


tweetList=list()
tweetdf=data.frame()
#downlaod the tweets for each of the movies
for(i in 1:length(movieNames)) {
    tweets<-searchTwitter(movieNames[i],lang="en",n=5000)
    # get text
    tweet_txt = sapply(tweets, function(x) x$getText())
    tweet_usrname = sapply(tweets, function(x) x$getScreenName())
    
    # clean text
    tweet_clean = removeBadChar(tweet_txt)
    tweet_num = length(tweet_clean)
    tweet_sentiment = numeric()
    #get the sentiment for each tweet
    for( j in 1:tweet_num) {
        tweet_sentiment[j]<-getSentiment(tweet_clean[j])  
    }
    mnVec<-rep(movieNames[i],length(tweet_clean))

    #create a dataframe of tweet info
    tempdf<-data.frame(mnVec,tweet_usrname,tweet_sentiment)
    #remove tweets with 0 or na sentiment
    tempdf<-tempdf[!is.na(tempdf$tweet_sentiment),]
    tempdf<-tempdf[tempdf$tweet_sentiment != 0,]
    tweetList[[movieNames[i]]]<-tempdf
}
meanS=numeric()
for(i in 1:length(tweetList)){
    meanS<-c(meanS,sum(tweetList[[i]]$tweet_sentiment))
}
barplot(meanS,names.arg=c("n=1837\nGuardians of the \nGalaxy ","n=1725\n Into the Woods\n","n=2310\nIntersteller\n"),col=3:5,xlab="Movies",ylab="sum of sentiments",main="Movie Preferences",ylim=c(0,500))

#find people who liked the guardians of the galaxy and also tweeted about the other movies
skips1_2i=numeric()
skips1_2j=numeric()
skips1_3i=numeric()
skips1_3j=numeric()
sumSent12=0
sumSent13=0
for(i in 1:nrow(tweetList[[1]])){
    if(tweetList[[1]]$tweet_sentiment[i]<=0) next
    for(j in 1:nrow(tweetList[[2]])){
        if(i %in% skips1_2i) break
        if(j %in% skips1_2j) next
        if(as.character(tweetList[[1]]$tweet_usrname[i]) == as.character(tweetList[[2]]$tweet_usrname[j])){
            print("into the woods")
            print(tweetList[[1]]$tweet_sentiment[i])
            print(tweetList[[2]]$tweet_sentiment[j])
            sumSent12 = sumSent12 + tweetList[[2]]$tweet_sentiment[j]
            skips1_2i=c(skips1_2i,i)
            skips1_2j=c(skips1_2i,j)
        }
        
    }
    for(j in 1:nrow(tweetList[[3]])){
        if(i %in% skips1_3i) break
        if(j %in% skips1_3j) next
        if(as.character(tweetList[[1]]$tweet_usrname[i]) == as.character(tweetList[[3]]$tweet_usrname[j])){
            print("intersteller")
            print(tweetList[[1]]$tweet_sentiment[i])
            print(tweetList[[3]]$tweet_sentiment[j])
            sumSent13 = sumSent13 + tweetList[[3]]$tweet_sentiment[j]
            skips1_3i=c(skips1_3i,i)
            skips1_3j=c(skips1_3i,j)
        }
        
    }
}
barplot(c(sumSent12,sumSent13),names.arg=c(" n=18 \nInto the Woods\n","n=6\n Intersteller\n "),col=4:5,xlab="Movies",ylab="sum of sentiments",main="Movie Preferences\n People Who Liked Gardians of the Galaxy",ylim=c(0,5))
