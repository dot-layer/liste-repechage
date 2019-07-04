library(rvest)
library(data.table)

source("investigate-variance.R")

html <- read_html("http://www.hockeydb.com/ihdb/draft/nhl2019e.html")

draft_table <- html %>%
  html_nodes("table") 

draft_table <- lapply(draft_table, html_table, fill=T)
draft_table <- draft_table[1]

temp_table <- as.data.table(do.call(cbind, draft_table))[-1,]

draft_2019 <- subset(temp_table, select = 1:5)
setnames(draft_2019, colnames(draft_2019), c("round", "num", "drafted_by", "name", "position"))
# Minor corrections to names
draft_2019[name == "Nick Robertson", name := "Nicholas Robertson"]
draft_2019[name == "Ilya Nikolaev", name := "Ilya Nikolayev"]
draft_2019[name == "Egor Afanasyev", name := "Yegor Afanasyev"]
draft_2019[name == "Michal Teply", name := "Michael Teply"]

head(draft_2019)

predictions <- copy(temp)[, rank := nrow(temp) - rank(score) + 1]
# Just to clean up a name that have not been cleaned before (should have been bu anyways)
predictions <- predictions[!(player == "Nicolas Robertson"),]
predictions <- predictions[!(player == "Egor Afanasyev"),]
predictions <- predictions[, c("player", "rank", "score"), with = F][order(rank),]

data_plot <- merge(predictions, draft_2019, by.x = "player", by.y = "name", all.x = TRUE)[order(rank)]
data_plot[, num := as.integer(num)]
data_plot[, diff := rank - num]

data_plot[, color := "gray"]
data_plot[diff > 0, color := "green"]
data_plot[diff < 0, color := "red"]
data_plot[, diff_label := as.character(diff)]
data_plot[diff > 0, diff_label := paste0("+", diff_label)]
data_plot[, diff2 := pmin(pmax(diff,-25),25)]

ggplot(data_plot, aes(x=rank, y=sort(score))) +
  geom_point() +
  geom_line() +
  geom_text(aes(label=paste0(player[order(score)], " (", num[order(score)], " : ", diff_label[order(score)], ")"), color = diff2[order(score)]), vjust=-.25, hjust=-.25, size=3, angle=-45, check_overlap = T) +
  # geom_text(aes(label=paste0(player[order(score)], " (", num[order(score)], " : ", diff_label[order(score)], ")"), color = color[order(score)]), vjust=-.5, hjust=-.5, size=3, angle=-45, check_overlap = T) +
  #scale_color_manual("drafted", labels = c("Repêché au rang prévu", "Repêché plus tôt que prévu", "Repêché plus tard que prévu"), values = c("#808080", "#008000", "#ff0000"))+
  scale_color_gradientn("drafted", colours = c("#ff3333", "#808080", "#33FF33"))+
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none"#,
  #       legend.text = element_text(size=14)#,
  #       # legend.key.size = unit(1.5, "cm"),
  #       # legend.key.width = unit(0.5,"cm") 
  ) +
  xlim(1,70) +
  ylim(min(temp$score)-300,max(temp$score))

