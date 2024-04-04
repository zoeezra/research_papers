library(tidyverse)
library(FactoMineR)
library(ggplot2)
library(cluster)
library(NbClust)
library(fpc)

# Importing and Cleaning Data
basketball <- read.csv(file.choose(), header = TRUE)
basketball_pg <- basketball
basketball_pg[, c(5:26, 29)] <- (basketball_pg[, c(5:26, 29)])/basketball_pg$Games
basketball_pg <- basketball_pg[, -28]

basketball_pg$FG. <- basketball_pg$FGM/basketball_pg$FGA
basketball_pg$TwoP. <- basketball_pg$TwoPM/basketball_pg$TwoPA
basketball_pg$ThreeP. <- basketball_pg$ThreePM/basketball_pg$ThreePA
basketball_pg$FT. <- basketball_pg$FTM/basketball_pg$FTA
basketball_pg[is.na(basketball_pg)] <- 0

basketball_fg1 <- basketball_pg %>% select(Min, TwoPM, TwoPA, ThreePM, ThreePA, FTM, FTA, OREB, DREB, AST, STL, BLK, FD, AVGPM, PTS)
basketball_fg2 <- basketball_pg %>% select(Min, TwoP., ThreeP., FT., OREB, DREB, AST, STL, BLK, FD, AVGPM, PTS)
basketball_fg3 <- basketball_pg %>% select(Min, FGA, FGM, FTM, FTA, OREB, DREB, AST, STL, BLK, FD, AVGPM, PTS)
basketball_fg4 <- basketball_pg %>% select(Min, FG., FT., OREB, DREB, AST, STL, BLK, FD, AVGPM, PTS)

### DESCRIPTIVE STATISTICS ###
sapply(basketball_pg[,-c(1, 2, 3, 4, 20, 22, 25)], median)

### PRINCIPAL COMPONENT ANALYSIS ###

# PCA (Two and Three Points Made and Attempted)
basketball_pc1 <- prcomp(x = basketball_fg1, scale = T)
summary(basketball_pc1)
basketball_pc1$rotation

tibble(eigenvalues = (basketball_pc1$sdev)^2, PC = 1:19) %>%
  ggplot(aes(y = eigenvalues, x = PC)) +
  geom_point() +
  geom_bar(stat = "identity", fill = "tomato", alpha = 0.5) +
  geom_line() +
  scale_x_continuous(breaks = 1:19) +
  ggthemes::theme_gdocs()

basket1_scores <- basketball_pc1$x[, 1:4]

mahalanobis_dist_b1 <- mahalanobis(x = basket1_scores, center = c(0,0), cov = var(basket1_scores))

basket1_new <- bind_cols(basketball_pg[,1:2], basketball_fg1, dist = mahalanobis_dist_b1)

basket1_outliers <- basket1_new %>% 
  filter(dist > mean(mahalanobis_dist_b1)+(2*sd(mahalanobis_dist_b1))) %>%
  select(-18)

basket1_median <- tibble(Name = "MEDIAN", Team = "-", summarise_all(basketball_fg1, .funs = median))

rbind(basket1_outliers, basket1_median)

# PCA (Two and Three Points Percentage)
basketball_pc2 <- prcomp(x = basketball_fg2, scale = T)
summary(basketball_pc2)
basketball_pc2$rotation

tibble(eigenvalues = (basketball_pc2$sdev)^2, PC = 1:12) %>%
  ggplot(aes(y = eigenvalues, x = PC)) +
  geom_point() +
  geom_bar(stat = "identity", fill = "tomato", alpha = 0.5) +
  geom_line() +
  scale_x_continuous(breaks = 1:12) +
  ggthemes::theme_gdocs()

basket2_scores <- basketball_pc2$x[, 1:4]

mahalanobis_dist_b2 <- mahalanobis(x = basket2_scores, center = c(0,0), cov = var(basket2_scores))

basket2_new <- bind_cols(basketball_pg[,1:2], basketball_fg2, dist = mahalanobis_dist_b2)

basket2_outliers <- basket2_new %>% 
  filter(dist > mean(mahalanobis_dist_b2)+(2*sd(mahalanobis_dist_b2))) %>%
  select(-15)

basket2_median <- tibble(Name = "MEDIAN", Team = "-", summarise_all(basketball_fg2, .funs = median))

rbind(basket2_outliers, basket2_median)

basket2_combi <- bind_cols(basket2_new, basket2_scores)
  
ggplot() +
  geom_point(data = basket2_combi, aes(x = PC1, y = PC2, colour = Team))
  
# PCA (Field Goals Made and Attempted)
basketball_pc3 <- prcomp(x = basketball_fg3, scale = T)
summary(basketball_pc3)
basketball_pc3$rotation

tibble(eigenvalues = (basketball_pc3$sdev)^2, PC = 1:13) %>%
  ggplot(aes(y = eigenvalues, x = PC)) +
  geom_point() +
  geom_bar(stat = "identity", fill = "tomato", alpha = 0.5) +
  geom_line() +
  scale_x_continuous(breaks = 1:13) +
  ggthemes::theme_gdocs()

basket3_scores <- basketball_pc3$x[, 1:4]

mahalanobis_dist_b3 <- mahalanobis(x = basket3_scores, center = c(0,0), cov = var(basket3_scores))

basket3_new <- bind_cols(basketball_pg[,1:2], basketball_fg3, dist = mahalanobis_dist_b3)

basket3_outliers <- basket3_new %>% 
  filter(dist > mean(mahalanobis_dist_b3)+(2*sd(mahalanobis_dist_b3))) %>%
  select(-16)

basket3_median <- tibble(Name = "MEDIAN", Team = "-", summarise_all(basketball_fg3, .funs = median))

rbind(basket3_outliers, basket3_median)

# PCA (Field Goals Percentage)
basketball_pc4 <- prcomp(x = basketball_fg4, scale = T)
summary(basketball_pc4)
basketball_pc4$rotation

tibble(eigenvalues = (basketball_pc4$sdev)^2, PC = 1:11) %>%
  ggplot(aes(y = eigenvalues, x = PC)) +
  geom_point() +
  geom_bar(stat = "identity", fill = "tomato", alpha = 0.5) +
  geom_line() +
  scale_x_continuous(breaks = 1:11) +
  ggthemes::theme_gdocs()

basket4_scores <- basketball_pc4$x[, 1:4]

mahalanobis_dist_b4 <- mahalanobis(x = basket4_scores, center = c(0,0), cov = var(basket4_scores))

basket4_new <- bind_cols(basketball_pg[,1:2], basketball_fg4, dist = mahalanobis_dist_b4)

basket4_outliers <- basket4_new %>% 
  filter(dist > mean(mahalanobis_dist_b4)+(2*sd(mahalanobis_dist_b4))) %>%
  select(-14)

basket4_median <- tibble(Name = "MEDIAN", Team = "-", summarise_all(basketball_fg4, .funs = median))

rbind(basket4_outliers, basket4_median)

# Mahalanobis Function
mahal <- function(x, cx = NULL) {
  x <- as.data.frame(x)
  if(is.null(cx)) cx <- cov(x)
  out <- lapply(1:nrow(x), function(i) {
    mahalanobis(x = x, 
                center = do.call("c", x[i, ]),
                cov = cx,
                tol=1e-20) # needed when working with almost singular vcov matrix
  })
  return(as.dist(do.call("rbind", out)))
}

# Mahalanobis Distance Matrix
mahal_dist_pc <- mahal(basket2_scores)


##### Cluster Analysis Models #####

##### Internal Cluster Validation #####
# For AgNes using Ward's Linkage
for(i in c("ch", "silhouette", "ratkowsky", "ccc", "db", "friedman", "trcovw")) {
  cat("\n\n", i, "\n")
  
  if(i %in% c("friedman", "trcovw")){
    index <- NbClust(data = scale(basket2_scores), distance = NULL, diss=mahal_dist_pc,
                     min.nc = 1, max.nc = 10, method = "ward.D", index = i)$All.index
    diff <- abs(index[2:10] - index[1:9])
    print(diff)
  } else {
    index <- NbClust(data = scale(basket2_scores), distance = NULL, diss=mahal_dist_pc,
                     min.nc = 2, max.nc = 10, method = "ward.D", index = i)$All.index
    print(index)
  }
}

# For K-Means Clustering
for(i in c("ch", "silhouette", "ratkowsky", "ccc", "db", "friedman", "trcovw")) {
  cat("\n\n", i, "\n")
  index <- NbClust(data = scale(basket2_scores), distance = NULL, diss=mahal_dist_pc,
                   min.nc = 2, max.nc = 10, method = "kmeans", index = i)$All.index
  if(i %in% c("friedman", "trcovw")){
    diff <- abs(index[2:9] - index[1:8])
    print(diff)
  } else {
    print(index)
  }
}


##### Clustering Models #####
# HIERARCHICAL: AgNes using Ward's Linkage
ward_avg_pc <- cluster::agnes(x = mahal_dist_pc, diss = T, method = "ward")
ward_mahal_pc <- cutree(tree = ward_avg_pc, k = 5)
table(ward_mahal_pc)

# NON-HIERARCHICAL: K-Means Clustering
basket2_pcscore <- cbind(basketball_pg[,1:2], basket2_scores)

kcenters_pc <- basket2_pcscore[,-c(1:2)] %>%
  mutate_all(.funs = scale) %>%
  mutate(clust = ward_mahal_pc) %>%
  group_by(clust) %>%
  summarise_all(.funs = mean) %>%
  select(-clust)

kmeans_clus_pc <- kmeans(x = scale(basket2_scores), centers = kcenters_pc)
table(kmeans_clus_pc$cluster)

##### Cluster Interpretation #####
basket2_clusters <- basket2_pcscore %>%
  mutate(Cluster = kmeans_clus_pc$cluster)

## Means per cluster
basket2_clusters %>%
  group_by(Cluster) %>%
  summarise_all(.funs = mean)

# Plot of means
clus_means <- basket2_pcscore[,-c(1:2)] %>% 
  mutate_all(.funs = scale) %>%
  mutate(kmeans = kmeans_clus_pc$cluster) %>%
  group_by(kmeans) %>%
  summarise_all(.funs = mean) %>%
  pivot_longer(PC1:PC4, names_to = "vars", values_to = "mean") %>%
  mutate(Label = factor(mean < 0, labels = c("Above average", "Below average")))
clus_means[,2] <- factor(clus_means$vars, levels = c("PC4", "PC3", "PC2", "PC1"))

ggplot(clus_means, aes(x = vars, y = mean)) +
  geom_bar(aes(fill = Label), stat = "identity", position = "dodge") +
  facet_wrap(. ~ kmeans) +
  coord_flip() +
  ylim(-2,2)  + 
  scale_fill_manual(values = c("seagreen","maroon")) +
  ggthemes::theme_gdocs()

## Observations with Highest Silhouette
clus_sil <- silhouette(kmeans_clus_pc$cluster, mahal_dist_pc)

# Cluster 1
basket2_clusters %>%
  mutate(silhouette = clus_sil[,3]) %>% 
  filter(Cluster == 1) %>%
  top_n(n = 10, wt = silhouette)

# Cluster 2
basket2_clusters %>%
  mutate(silhouette = clus_sil[,3]) %>% 
  filter(Cluster == 2) %>%
  top_n(n = 10, wt = silhouette)

# Cluster 3
basket2_clusters %>%
  mutate(silhouette = clus_sil[,3]) %>% 
  filter(Cluster == 3) %>%
  top_n(n = 10, wt = silhouette)

# Cluster 4
basket2_clusters %>%
  mutate(silhouette = clus_sil[,3]) %>% 
  filter(Cluster == 4) %>%
  top_n(n = 10, wt = silhouette)

# Cluster 5
basket2_clusters %>%
  mutate(silhouette = clus_sil[,3]) %>% 
  filter(Cluster == 5) %>%
  top_n(n = 10, wt = silhouette)

## Scatterplot
basket2_clusters %>% 
  mutate(Cluster = as.factor(Cluster)) %>%
  ggplot(aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size=3)