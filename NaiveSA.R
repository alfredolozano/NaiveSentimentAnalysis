## Autorización para minería de twitter

library(twitteR)

consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


library(RCurl) 

# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))


twitCred$handshake()
registerTwitterOAuth(twitCred)



#### Usando el análisis
tweets <- c()
tweets <- c(tweets,searchTwitter('#AsambleaConstituyente',n=10000))


### Sentiment Scoring

twit <- c()
for(t in tweets)
  twit <- c(twit,t$getText())

positive <- scan("sentiment/pspa.txt",encoding = 'UTF-8', what='character',comment.char=';')
negative <- scan("sentiment/nspa.txt",encoding = 'UTF-8', what='character',comment.char=';')

require(plyr)
require(stringr)

sentences <- c()
score <- c()

for(t in twit){
  result = tryCatch({
  sentence = tolower(t)
  low_sentences <- c(sentence,t)
  sentences = c(sentences, t)
  word_list = str_split(sentence, ' ')
  words = unlist(word_list)
  pos_matches = match(words, positive)
  neg_matches = match(words, negative)
  
  pos_matches = sum(!is.na(pos_matches))
  neg_matches = sum(!is.na(neg_matches))
  score = c(score, pos_matches - neg_matches)
  
  }, error = function(e) {
    print(t)
  })
}
  
scores_df = data.frame(score=score, text=sentences)

### Just mining

tweet_DF <- do.call("rbind", lapply(tweets, as.data.frame))
tweet_DF <- left_join(tweet_DF,scores_df,by='text')
tweet_DF <- distinct(tweet_DF,id,.keep_all = T)
tweet_DF <- tweet_DF[!is.na(tweet_DF$score),]
tweet_DF$positive <- tweet_DF$score >0

# hist(scores_df$score,main='Score de sentimientos #AsambleaConstituyente',xlab = 'Score por tweet',ylab='Frecuencia')
ggplot(tweet_DF,aes(x=tweet_DF$score,fill =positive)) + geom_histogram() + theme_bw() + xlab('Score por tweet') +
  ylab('Frecuencia') + ggtitle('Score de sentimientos #AsambleaConstituyente')


### partidos
partidos <- data.frame(partido = c('pri','pan','prd','pt','verde','panal','mc','en_soc','indep','morena'), 
                       frecuencia =c(0,0,0,0,0,0,0,0,0,0))
for(t in sentences){
  partidos$frecuencia[1] = partidos$frecuencia[1]+ grepl('pri',t) + grepl('PRI',t)
  partidos$frecuencia[2] = partidos$frecuencia[2]+ grepl('pan',t) + grepl('PAN',t)
  partidos$frecuencia[3] = partidos$frecuencia[3]+ grepl('prd',t) + grepl('PRD',t)
  partidos$frecuencia[4] = partidos$frecuencia[4]+ grepl('pt',t) + grepl('PT',t)
  partidos$frecuencia[5] = partidos$frecuencia[5]+ grepl('verde',t) + grepl('Verde',t)+ grepl('PVEM',t)
  partidos$frecuencia[6] = partidos$frecuencia[6]+ grepl('Nueva Alianza',t) + grepl('PANAL',t) + grepl('panal',t)
  partidos$frecuencia[7] = partidos$frecuencia[7]+ grepl('mc',t) + grepl('movimiento ciudadano',t) + 
    grepl('Movimiento Ciudadano',t)
  partidos$frecuencia[8] = partidos$frecuencia[8]+ grepl('encuentro social',t) + grepl('Encuentro Social',t)+
    grepl('Encuentro Social',t)
  partidos$frecuencia[9] = partidos$frecuencia[9]+ grepl('indep',t) + grepl('Indep',t)
  partidos$frecuencia[10] = partidos$frecuencia[10]+ grepl('morena',t) + grepl('Morena',t)
}

tweet_DF$created <- as.Date(tweet_DF$created)
tweet_DF <- tweet_DF[!is.na(tweet_DF$score),]

for(i in 1:nrow(tweet_DF)){
  t <- tweet_DF$text[i]
  tweet_DF$pri[i] = grepl('pri',t) | grepl('PRI',t)
  tweet_DF$pan[i] = grepl('pan',t) | grepl('PAN',t)
  tweet_DF$prd[i] = grepl('prd',t) | grepl('PRD',t)
  tweet_DF$pt[i] = grepl('pt',t) | grepl('PT',t)
  tweet_DF$pvem[i] = grepl('verde',t) | grepl('Verde',t) | grepl('PVEM',t)
  tweet_DF$panal[i] = grepl('Nueva Alianza',t) | grepl('PANAL',t) | grepl('panal',t)
  tweet_DF$mc[i] = grepl('mc',t) | grepl('movimiento ciudadano',t) | 
    grepl('Movimiento Ciudadano',t)
  tweet_DF$ensoc[i] = grepl('encuentro social',t) | grepl('Encuentro Social',t)|
    grepl('Encuentro Social',t)
  tweet_DF$indep[i] = grepl('indep',t) | grepl('Indep',t)
  tweet_DF$morena[i] = grepl('morena',t) | grepl('Morena',t)| grepl('MORENA',t)
}

tweet_DF %>% filter(pri) %>% group_by(pri) %>% summarise(mean=mean(score))
tweet_DF %>% filter(pan) %>% group_by(pan) %>% summarise(mean=mean(score))
tweet_DF %>% filter(prd) %>% group_by(prd) %>% summarise(mean=mean(score))
tweet_DF %>% filter(pt) %>% group_by(pt) %>% summarise(mean=mean(score))
tweet_DF %>% filter(pvem) %>% group_by(pvem) %>% summarise(mean=mean(score))
tweet_DF %>% filter(panal) %>% group_by(panal) %>% summarise(mean=mean(score))
tweet_DF %>% filter(mc) %>% group_by(mc) %>% summarise(mean=mean(score))
tweet_DF %>% filter(ensoc) %>% group_by(ensoc) %>% summarise(mean=mean(score))
tweet_DF %>% filter(indep) %>% group_by(indep) %>% summarise(mean=mean(score))
tweet_DF %>% filter(morena) %>% group_by(morena) %>% summarise(mean=mean(score))

tweet_DF %>% filter(pri) %>% group_by(pri) %>% summarise(sum=sum(positive)/length(score))
tweet_DF %>% filter(pan) %>% group_by(pan) %>% summarise(sum=sum(positive)/length(score))
tweet_DF %>% filter(prd) %>% group_by(prd) %>% summarise(sum=sum(positive)/length(score))
tweet_DF %>% filter(pt) %>% group_by(pt) %>% summarise(sum=sum(positive)/length(score))
tweet_DF %>% filter(pvem) %>% group_by(pvem) %>% summarise(sum=sum(positive)/length(score))
tweet_DF %>% filter(panal) %>% group_by(panal) %>% summarise(sum=sum(positive)/length(score))
tweet_DF %>% filter(mc) %>% group_by(mc) %>% summarise(sum=sum(positive)/length(score))
tweet_DF %>% filter(ensoc) %>% group_by(ensoc) %>% summarise(sum=sum(positive)/length(score))
tweet_DF %>% filter(indep) %>% group_by(indep) %>% summarise(sum=sum(positive)/length(score))
tweet_DF %>% filter(morena) %>% group_by(morena) %>% summarise(sum=sum(positive)/length(score))

partidos_sent <- data.frame(partido = c('pri','pan','prd','pt','pvem','panal','mc','indep','morena'), 
                            score =c(-0.57,-0.6,0.44,-0.92,0.36,2,-0.94,1.27,-1.19),
                            positivos =c(36,10,20,4,7,1,0,16,3),
                            proporcion_positivos=c(0.09,0.28,0.4,0.3,0.6,1,0,0.43,0.10))

### Time series vs score
tweet_DF %>% group_by(created) %>% summarise(mean = mean(score)) %>%
  ggplot(aes(created,mean)) + geom_smooth() + theme_bw() + xlab('Fecha') + ylab('Score promedio') +
  ggtitle('Score promedio en el tiempo')
tweet_DF %>% group_by(created) %>% summarise(mean = sum(positive)) %>%
  ggplot(aes(created,mean)) + geom_smooth() + theme_minimal()


