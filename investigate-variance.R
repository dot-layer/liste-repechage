library(ggplot2)
library(ggrepel)
## clean couple duplicates....

main <- fread("data/main.R")
main[, N := .N, .(player)]

main <- main[, m_rank := (sum(rank) + 33*(14-N))/14, .(player)]
main <- main[, v_rank := sum((rank - m_rank)^2 + (33-m_rank)^2*(14-N))/13, .(player)]

temp <- main[, .(m_rank = unique(m_rank), v_rank = unique(v_rank), unique(N)), .(player)]
temp[order(m_rank),]
ggplot(temp, aes(x = m_rank, y =sqrt(v_rank))) +
  geom_point()


kern  <- sapply(temp$m_rank, function(rr){
  sum(temp$v_rank*dnorm(temp$m_rank,rr,2))/sum(dnorm(temp$m_rank,rr,2))
})
temp[, kern := kern]
plot(x = sort(temp$m_rank), y = temp$kern[order(temp$m_rank)], type="l")


afun <- approxfun(x = temp$m_rank[temp$m_rank < 22], y = temp$kern[temp$m_rank < 22], method="linear",
          yleft = min(temp$kern), yright = max(temp$kern), rule = 1, f = 0, ties = mean)


mat <- as.matrix(subset(dcast(main, ranker ~ player, value.var = "rank"), select=-1))
ij.mat <- t(combn(ncol(mat),2))

mat[is.na(mat)] <- 33

D <- matrix(0,ncol(mat),ncol(mat))
D[ij.mat] <- apply(ij.mat, 1, function(ij){
  i <- ij[1]
  j <- ij[2]
  sum((mat[,i]-mat[,j])^2/sapply(rowMeans(mat[,ij]), afun))/sum(1/sapply(rowMeans(mat[,ij]), afun))
})
D <- as.dist(D + t(D))
fit <- cmdscale(D,eig=TRUE, k=1)

temp[, score := c(fit$points)]
temp[order(score,decreasing = T), c("player","score")]


ggplot(temp, aes(x=sort(rank(score)), y=sort(score))) +
  geom_point() +
  geom_line() +
  geom_text(aes(label=player[order(score)]), vjust=-.5, hjust=-.5, , size=3, angle=-45, check_overlap = T) +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  xlim(1,70) +
  ylim(min(temp$score)-200,max(temp$score))#+
  # geom_abline(slope=0,intercept=min(temp$score))






