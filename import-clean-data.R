library(rvest)
library(data.table)

html <- read_html("http://www.mynhldraft.com/2019-nhl-draft/2019-nhl-draft-rankings/")

tables <- html %>%
  html_nodes("table") 

tables <- lapply(tables, html_table, fill=T)
tables <- tables[-1]

main <- as.data.table(do.call(cbind, tables))[-1,]

head(main)
names(main) <- unlist(main[1,])
names(main) <- gsub("\n", "", names(main))

str(main)

main <- main[-1,]
ind <- c(which(main[1,] == "1"),
  which(is.na(main[1,])))

main <- subset(main,select = -ind)


head(main)

fwrite(main, "data/main.R")

