library(rvest)
library(data.table)

html <- read_html("http://www.mynhldraft.com/2019-nhl-draft/2019-nhl-draft-rankings/")

tables <- html %>%
  html_nodes("table") 

tables <- lapply(tables, html_table, fill=T)
tables <- tables[-1]

temp <- as.data.table(do.call(cbind, tables))[-1,]

head(temp)
names(temp) <- unlist(temp[1,])
names(temp) <- gsub("\n", "", names(temp))

str(temp)

temp <- temp[-1,]
ind <- c(which(temp[1,] == "1"),
  which(is.na(temp[1,])),
  which(grepl("European|NA Skaters", names(temp))))

temp <- subset(temp,select = -ind)

main <- data.frame(ranker = NULL, rank = NULL, player = NULL)

main <- rbindlist(lapply(1:ncol(temp), function(k){
  temp2 <- subset(temp,select=k)
  temp2 <- cbind(data.table(ranker = names(temp2) ,rank = 1:nrow(temp2)),temp2)
  names(temp2)[3] <- "player"
  temp2
}))

head(main)

correction_mapping <- data.table(
  player = c("Kakko Kaapo", "Vasily Podkolzin", "Ville Henola", "Mortiz Seider", "Ilya Nikolaev", "Brett RE Leason", "Phillip Tomasino"),
  right = c("Kaapo Kakko", "Vasili Podkolzin", "Ville Heinola", "Moritz Seider", "Ilya Nikolayev", "Brett Leason", "Philip Tomasino")
)

setkey(main, player)
setkey(correction_mapping, player)

main_temp <- merge(main, correction_mapping, all = TRUE)

main_temp[grepl("Dach", player), player:= "Kirby Dach"]
main <- main_temp[, player := ifelse(is.na(right), player, right)]




fwrite(main, "data/main.R")

