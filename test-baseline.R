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
                        count = n(),
                        median = median(rank),
                        corrected_average = (sum(rank) + (14 - count) * 33) / 14) %>% 
              mutate(final_selection = rank(corrected_average)),
            by = "ID")


saveRDS(by_player, "data/by_player.rds")

# player_pairs <- expand.grid(by_player$ID,
#                             by_player$ID) %>% 
#   unique() %>% 
#   rename(ID1 = Var1, ID2 = Var2) %>% 
#   filter(ID1 != ID2)
# 
# rank_comparison <- function(ranker, df){
#   rank1 <- tibble(by_player$ID) %>% 
#     left_join()
#     df %>%
#     select(ID1 = ID, rank1 = rank)
#   rank2 <- df %>%
#     select(ID2 = ID, rank2 = rank)
#   
#   merge(rank1, rank2, all = TRUE) %>% 
#     unique() %>% 
#     filter(ID1 != ID2) %>% 
#     mutate(rank1_lt_rank2 = (rank1 < rank2)) %>% 
#     mutate(ranker = ranker)
# }
# 
# results <- map_df(unique(main$ranker),
#                   ~ rank_comparison(.x, main %>% filter(ranker == .x)))
# 
# results %<>% group_by(ID1) %>% summarise(mean_beat = mean(rank1_lt_rank2),
#                                          count_beat = sum(rank1_lt_rank2))
# 
# by_player %<>% left_join(results, by = c("ID" = "ID1"))
