library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("quanteda")


text <- readLines(file.choose()) # выбираем файл, в котором содержатся данные
docs <- Corpus(VectorSource(text))


options(max.print=999999)
inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
#docs <- tm_map(docs, toSpace, "\")
docs <- tm_map(docs, toSpace, "—")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "–")

docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove russian common stopwords
docs <- tm_map(docs, removeWords, stopwords("russian"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("российский", "сколько","рт", "татарский", "татарстан", "тысруб", "татарстана", "татнефти", "большой", "никакой", "просто", "однако", "нашапочта", "деньга", "нампишут", "добрый", "день", "лицо", "лишь", "ваш", "https", "tme", "сообщает", "банка", "хотя", "тд", "млн", "миннихан", "причём", "менее", "считаю", "вид", "иметь", "самый", "смочь", "многий", "стать", "счёт", "должный", "наш", "ха", "свой", "свой", "часто", "идти", "часть", "весь", "уровень", "дать", "хотеть", "людей", "ещё", "мочь", "своей", "стран", "года", "лет", "таким", "хочу", "счет", "других", "также", "числе", "пока", "является", "свои", "этих", "образом", "лет", "годы", "таких", "будут", "тех", "будет", "году", "год","наша", "наши", "эта", "раза", "прежде", "раза", "нужно", "именно", "очень", "своих", "своим", "нашим", "необходимо", "будем", "наших", "те", "такую", "нашей", "я", "мой", "это", "должны", "должен", "должна", "которая", "которые", "который", "россии", "россия", "страны", "страна", "нам")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
docs <- unlist(docs, recursive = TRUE, use.names = TRUE)
text.tmp <- system2("/Users/aidarzinnatullin/Downloads/mystem", c("-c", "-l", "-d"), input = docs, stdout = TRUE)
text.tmp
cat(text.tmp,file="название файла после майстема.txt", encoding = "utf-8")


rm(list = ls()) # Удаляем все объекты
#Начинаем все заново
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("quanteda")
text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))

inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
#docs <- tm_map(docs, toSpace, "\")
docs <- tm_map(docs, toSpace, "—")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "–")

docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove russian common stopwords
docs <- tm_map(docs, removeWords, stopwords("russian"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("деньга", "нампишут", "добрый", "день", "лицо", "лишь", "ваш", "https", "tme", "сообщает", "банка", "хотя", "тд", "млн", "миннихан", "причём", "менее", "считаю", "вид", "иметь", "самый", "смочь", "многий", "стать", "счёт", "должный", "наш", "ха", "свой", "свой", "часто", "идти", "часть", "весь", "уровень", "дать", "хотеть", "людей", "ещё", "мочь", "своей", "стран", "года", "лет", "таким", "хочу", "счет", "других", "также", "числе", "пока", "является", "свои", "этих", "образом", "лет", "годы", "таких", "будут", "тех", "будет", "году", "год","наша", "наши", "эта", "раза", "прежде", "раза", "нужно", "именно", "очень", "своих", "своим", "нашим", "необходимо", "будем", "наших", "те", "такую", "нашей", "я", "мой", "это", "должны", "должен", "должна", "которая", "которые", "который", "россии", "россия", "страны", "страна", "нам")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

docs_matrix <- TermDocumentMatrix(docs)
m <- as.matrix(docs_matrix)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

head(d, 10)

# Облаков слов
wordcloud(words = d$word, freq = d$freq,scale = c(2, .2),  min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=.35, 
          colors=brewer.pal(8, "Dark2"))

# 
barplot(d[1:10,]$freq, las = 1, axes = T, names.arg = d[1:10,]$word,
        col = c("green", "red", "blue", "yellow", "orange", "lightblue", "lavender", "cornsilk", "lavender", "lightcyan"), main ="Самые используемые слова телеграм-канала Неудаща",
        xlab = "Частота слов", horiz = T, cex.names = 0.4, cex.axis = 0.8, xlim= c(0,500))
