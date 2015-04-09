library(ggplot2)
library(XML)
library(dplyr)
library(tidyr)

#URL (name) Collection ----
namesList <- NULL
for(i in LETTERS){
  url <- sprintf("http://afltables.com/afl/stats/players%s_idx.html", i)
  xmlData <- htmlParse(url)
  tables <- readHTMLTable(xmlData)
  names <- as.vector(t(tables[[1]]))
  namesList <- c(namesList, names)
}

urlData <- namesList %>%
  data.frame() %>%
  filter(namesList != '&nbsp') %>%
  na.omit() %>%
  separate(namesList, c('last', 'first'), sep = ', ') %>%
  unite(name, first, last, sep = '_') %>%
  mutate(first = substr(x = name, start = 1, stop = 1),
         url = sprintf("http://afltables.com/afl/stats/players/%s/%s.html", first, name),
         url = gsub(pattern = ' ', replacement = '_', x = url))

exclude <- table(urlData$url) %>% data.frame() %>% filter(Freq >= 2) %>% select(Var1) %>% t() %>% as.vector()

urlData <- urlData %>%
  filter(!(url %in% exclude))

#Stat Collection ----
features <- paste(c('Gm', 'Opponent', 'Rd', 'R', '#', 'KI', 'MK', 'HB', 'DI', 'GL', 'BH', 'HO', 'TK', 'RB', 
                    'IF', 'CL', 'CG', 'FF', 'FA', 'BR', 'CP', 'UP', 'CM', 'MI', '1%', 'BO', 'GA', '%P'), collapse = '')

playerData <- NULL
for(i in urlData$url[10000:11387]){
  
  xmlData <- htmlParse(i)
  tables <- readHTMLTable(xmlData)
  
  for(j in 1:length(tables)) {
    
    if(paste(names(tables[[j]]), collapse = '') == features) c(
      tables[[j]]$player <- i,
      playerData <- rbind(tables[[j]], playerData))
  }
  
  print(i)

}

#write.csv(playerData, file = 'playerData6.csv')

#Data collation ----

data1 <- read.csv('playerData1.csv')
data2 <- read.csv('playerData2.csv')
data3 <- read.csv('playerData3.csv')
data4 <- read.csv('playerData4.csv')
data5 <- read.csv('playerData5.csv')
data6 <- read.csv('playerData6.csv')

playerDataFull <- 
  rbind(data1, data2, data3, data4, data5, data6)  %>% 
  arrange(player, Gm) %>%
  select(-X)




