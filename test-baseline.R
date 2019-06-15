library(tibble)
library(purrr)
library(magrittr)
library(dplyr)

main <- fread("data/main.R", encoding = "UTF-8")

by_player <- data.table(player = player <- unique(unlist(main[, player])),
                         ID = as.numeric(as.factor(player)))

main %<>% 
  left_join(by_player, by = "player")

by_player <- 
  by_player %>% 
  left_join(main %>%
              group_by(ID) %>%
              summarise(straight_average = mean(rank),
                        min_ranking = min(rank),
                        max_ranking = max(rank),
                        variance = var(rank),
                        count = n()),
            by = "ID")
