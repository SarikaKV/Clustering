library(ggplot2)
library(plyr)
library(dplyr)
library(Rtsne) # for t-SNE plot
library(reshape2)
library(cluster) # for gower similarity and pam
directory =  '/home/sarika/BankDetails/rewards/'
df = read.csv( paste(directory ,'brand_features/csv/brandFeatures.csv',sep = '/'),sep= ',')
df$SectorID = as.factor(df$SectorID)
df$BrandID = as.factor(df$BrandID)
df[is.na(df)] = 0
head(df)
dim(df)
summary(df)
drops= c("SectorID")
df = df[ , !(names(df) %in% drops)]
#scaled.dat <- scale(df[2:56])
#df_scaled <- scaled.dat
#head(df_scaled)
# Running kmeans 
#tot.wss <- 0
#for (i in 1:100) {
#  tot.wss[i] <- kmeans(df_scaled,centers=i)$tot.withinss
#}
#plot(1:100, tot.wss, 
#     type="b", 
#     xlab="Number of Clusters",
#     ylab="Total within groups sum of squares") 

#Calculating distance for mixed types, Gower Distance
gower_dist <- daisy(df[, -1],metric = "gower")
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)
# Output most similar pair
df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),arr.ind = TRUE)[1, ], ]
# Output most dissimilar pair
df[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),arr.ind = TRUE)[1, ], ]
# Calculate silhouette width for many k using PAM to identify best cluster number
sil_width <- c(NA)
for(i in 2:100){
  pam_fit <- pam(gower_dist,diss = TRUE, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
# Plot sihouette width (higher is better)
plot(1:86, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:86, sil_width)
#Choose best cluster number and interpret
pam_fit <- pam(gower_dist, diss = TRUE, k = 8)
pam_results <- df %>%
  dplyr::select(-BrandID) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary
#Get medoid of each cluster
df[pam_fit$medoids, ]

#TSNE Plotting
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         BrandID = df$BrandID )

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))+
  #geom_text(data=tsne_data,aes(x=X,y=Y,label=SectorID),size = 3,color = 'black',check_overlap = TRUE,vjust =-0.5,hjust=0)+
  scale_x_continuous(limits=c(-40,40), breaks=seq(-40,40, by = 5),expand = c(0, 0)) 
#Print the brands and see
tsne_data %>%
  filter(X > -2.5 & X < 15,
         Y > -40 & Y < -30) %>%
  left_join(df, by = "BrandID") %>%
  collect %>%
  .[["BrandID","SectorID"]]