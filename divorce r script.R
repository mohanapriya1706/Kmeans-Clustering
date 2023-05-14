library(tidyverse)
library(readxl)
library(reshape2)
divorce_data <- read_excel("divorce.xlsx")
library(rmarkdown)

str(divorce_data)
summary(divorce_data)
sum(is.na(divorce_data))
library(ggplot2)

sum(duplicated(divorce_data))
dim(divorce_data)
divorce_data <- unique(divorce_data)
dim(divorce_data)

pivot_data <-pivot_longer(divorce_data,cols=everything())
ggplot(pivot_data,aes(x=value,y=..count..)) +
  geom_bar(stat='count',fill='blue') + 
  facet_wrap(~name,scales="free_x")



###subsetting divorce not and divorced
df_yes <- divorce_data %>% 
  filter(Class==1)
df_not <- divorce_data %>% 
  filter(Class==0)

pivot_data <-pivot_longer(df_yes,cols=everything())
ggplot(pivot_data,aes(x=value,y=..count..)) +
  geom_bar(stat='count',fill='red') + 
  facet_wrap(~name,scales="free_x")

pivot_data <-pivot_longer(df_not,cols=everything())
ggplot(pivot_data,aes(x=value,y=..count..)) +
  geom_bar(stat='count',fill='violet') + 
  facet_wrap(~name,scales="free_x")

###highly correlated features
library(reshape2)
corr_data <- cor(divorce_data,method="pearson")
melt_corr<- melt(corr_data)
ggplot(melt_corr, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Correlation Heatmap")

library(caret)
library(factoextra)
high_corr_vars <- findCorrelation(corr_data, cutoff = 0.8)
data_highcorr <- divorce_data[, high_corr_vars]
cluster_df <- data_highcorr[, -which(names(data_highcorr) == "Class")]
fviz_nbclust(cluster_df, kmeans, method = "wss")
fviz_nbclust(cluster_df, kmeans, method = "silhouette")

set.seed(123)
km <- kmeans(cluster_df, 2, nstart = 25)
fviz_cluster(km,data = cluster_df, geom = "point")

library(cluster)
silhouette_score <- silhouette(km$cluster, dist(cluster_df))
silhouette_avg <- mean(silhouette_score)
silhouette_avg

km$centers #evaluation using centers
class(km$cluster)
km$cluster
divorce_data$Class
divorce_data$Class[divorce_data$Class == 0] <- 2
confusionMatrix(as.factor(km$cluster),as.factor(divorce_data$Class))