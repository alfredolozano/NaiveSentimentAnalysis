score.sentiment = function(sentences, pos.words, neg.words)
{
	require(plyr)
	require(stringr)

	scores = lapply(sentences, function(sentence, pos.words, neg.words){

		sentence = gsub('[[:punct:]]','',sentence)
		sentence = gsub('[[:cntrl:]]','',sentence)
		sentence = gsub('\\d+','',sentence)

		sentence = tolower(sentence)

		word.list = str_split(sentence, '\\s+')

		words = unlist(word.list)

		pos.matches = match(words, pos.words)
		neg.matches = match(words, neg.words)


		pos.matches = !is.na(pos.matches)
		neg.matches = !is.na(neg.matches)

		score = sum(pos.matches) - sum(neg.matches)

		return(score)
		})

	scores.df = data.frame(score=scores, text=sentences)
	return(scores.df)
}