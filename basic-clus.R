main <- fread("data/main.R")

#** On fill les NA avec des 33 **
# rank.mat <- as.matrix(
#   dcast(data = main, formula = ranker ~ player, value.var="rank")[,-1]
#   )
rank.mat <- as.matrix(
  dcast(data = main, formula = ranker ~ player, value.var="rank", fill = 40)[,-1]
)

perm <- order(apply(rank.mat, 2, function(v) sum(is.na(v))))

Tau.hat <- cor(rank.mat[,perm],method = "kendall")
image(Tau.hat)

which(apply(Tau.hat, 1, function(v) sum(is.na(v)) > 2))

Tau.hat[is.na(Tau.hat)] <- 0

# heatmap(t(rank.mat))
# heatmap(t(Tau.hat))
hc <- hclust(dist(t(rank.mat)), "ward.D")

hc <- hclust(as.dist(1-abs(Tau.hat)))
par(mar = c(0,0,0,0))
plot(hc, xaxt = "n", yaxt = "n", ann = F)


