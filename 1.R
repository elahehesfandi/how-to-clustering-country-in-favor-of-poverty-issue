
#data entry
library(readxl)
data_g2 <- read_excel("data_g2.xlsx", col_types = c("text", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric"))
View(data_g2)

#data standardization
library(dplyr)
glimpse(data_g2)
head(data_g2)
tail(data_g2)
str(data_g2)
summary(data_g2)
library(purrr)
library(tidyr)
library(ggplot2)
data_g2 %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
#######

summary(data_g2$SIC)
m1 <-mean(data_g2$SIC,na.rm = TRUE)
v1 <- var(data_g2$SIC,na.rm = TRUE)
SIC.s <- (data_g2$SIC - m1)/sqrt(v1) # standardization
mean(SIC.s,na.rm = TRUE)  
var(SIC.s,na.rm = TRUE)
######

summary(data_g2$CM)
m2 <-mean(data_g2$CM,na.rm = TRUE)
v2 <- var(data_g2$CM,na.rm = TRUE)
CM.s <- (data_g2$CM - m2)/sqrt(v2) # standardization
mean(CM.s,na.rm = TRUE)  
var(CM.s,na.rm = TRUE)

######

summary(data_g2$MMR)
m3 <-mean(data_g2$MMR,na.rm = TRUE)
v3 <- var(data_g2$MMR,na.rm = TRUE)
MMR.s <- (data_g2$MMR - m3)/sqrt(v3) # standardization
mean(MMR.s,na.rm = TRUE)  
var(MMR.s,na.rm = TRUE)

#####

summary(data_g2$U5M)
m4 <-mean(data_g2$U5M,na.rm = TRUE)
v4 <- var(data_g2$U5M,na.rm = TRUE)
U5M.s <- (data_g2$U5M - m4)/sqrt(v4) # standardization
mean(U5M.s,na.rm = TRUE)  
var(U5M.s,na.rm = TRUE)

#####

summary(data_g2$IH)
m5 <-mean(data_g2$IH,na.rm = TRUE)
v5 <- var(data_g2$IH,na.rm = TRUE)
IH.s <- (data_g2$IH - m5)/sqrt(v5) # standardization
mean(IH.s,na.rm = TRUE)  
var(IH.s,na.rm = TRUE)

#####

summary(data_g2$IT)
m6 <-mean(data_g2$IT,na.rm = TRUE)
v6 <- var(data_g2$IT,na.rm = TRUE)
IT.s <- (data_g2$IT - m6)/sqrt(v6) # standardization
mean(IT.s,na.rm = TRUE)  
var(IT.s,na.rm = TRUE)

#####

summary(data_g2$MCT)
m7 <-mean(data_g2$MCT,na.rm = TRUE)
v7 <- var(data_g2$MCT,na.rm = TRUE)
MCT.s <- (data_g2$MCT - m7)/sqrt(v7) # standardization
mean(MCT.s,na.rm = TRUE)  
var(MCT.s,na.rm = TRUE)

#####

summary(data_g2$PCR)
m8 <-mean(data_g2$PCR,na.rm = TRUE)
v8 <- var(data_g2$PCR,na.rm = TRUE)
PCR.s <- (data_g2$PCR - m8)/sqrt(v8) # standardization
mean(PCR.s,na.rm = TRUE)  
var(PCR.s,na.rm = TRUE)

#####

summary(data_g2$CFWfemale)
m9 <-mean(data_g2$CFWfemale,na.rm = TRUE)
v9 <- var(data_g2$CFWfemale,na.rm = TRUE)
CFWfemale.s <- (data_g2$CFWfemale - m9)/sqrt(v9) # standardization
mean(CFWfemale.s,na.rm = TRUE)  
var(CFWfemale.s,na.rm = TRUE)

#####

summary(data_g2$CFWmale)
m10 <-mean(data_g2$CFWmale,na.rm = TRUE)
v10 <- var(data_g2$CFWmale,na.rm = TRUE)
CFWmale.s <- (data_g2$CFWmale - m10)/sqrt(v10) # standardization
mean(CFWmale.s,na.rm = TRUE)  
var(CFWmale.s,na.rm = TRUE)

#####

summary(data_g2$Lp)
m11 <-mean(data_g2$Lp,na.rm = TRUE)
v11 <- var(data_g2$Lp,na.rm = TRUE)
Lp.s <- (data_g2$Lp - m11)/sqrt(v11) # standardization
mean(Lp.s,na.rm = TRUE)  
var(Lp.s,na.rm = TRUE)

#####

data.stand <- scale(data_g2[,-1])  # To standarize the variables
data.stand <- as.data.frame(data.stand)
rownames(data.stand) <- data_g2$country

######

library(tidyr)
library(ggplot2)
data.stand %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
summary(data.stand)

#First model# K-Means#

# how many clusters ?
# how many clusters ?#Choose the number of clusters (k) with using the elbow method 
# load required packages
library(factoextra)
library(NbClust)
library(cluster)
set.seed(6)
data.stand.s<- newdata <- na.omit(data.stand)

# Elbow method
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data.stand.s, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

##Gap Statistic Method
gap_stat <- clusGap(data.stand.s, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)




# K-Means

set.seed(123)
k.means.fit <- kmeans(na.omit(data.stand),4) # this helps in omitting NA 
str(k.means.fit)
###
k.means.fit
k2 <- kmeans(data.stand.s, centers = 2, nstart = 25)
str(k2)
fviz_cluster(k2, data = data.stand.s)
######

k3 <- kmeans(data.stand.s, centers = 3, nstart = 25)
k4 <- kmeans(data.stand.s, centers = 4, nstart = 25)
k5 <- kmeans(data.stand.s, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = data.stand.s) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = data.stand.s) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = data.stand.s) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = data.stand.s) + ggtitle("k = 5")



library(gridExtra)

grid.arrange(p1, p2, p3, p4, nrow = 2)

fviz_cluster(k4, data = data.stand.s)

# Fitting K-Means to the dataset
set.seed(30)
kmeans = kmeans(x = data.stand.s, centers = 4)
y_kmeans = kmeans$cluster
kmeans$cluster
data.frame(kmeans$cluster)

k4$centers

# hierarchical clustering 
# Dissimilarity matrix
d <- dist(data.stand.s, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

abline(h=15, col="red")
groups <- cutree(hc1, k=4) # cut tree into 5 clusters


#hierarchical clustering, k=4
library(cluster)
library(proxy)
simil(data.stand, method="Gower")
dendro_plot <- hclust(dist(data.stand, method="Gower"), method = "complete")
plot(dendro_plot, cex = 0.5)
groups <- cutree(dendro_plot, k=4)
groups
rect.hclust(dendro_plot, k=4, border= c("red", "green", "blue"))
# draw dendogram with red borders around the 5 clusters
rect.hclust(hc1, k = 4, border = 2:5)
########
cutree(hc1,4) 
data.frame(cutree(hc1,4)) 



##third method: princomp: Principal Components Analysis
library(stats)
acp<-princomp(data.stand.s, cor=T)
summary(princomp(data.stand.s, cor=T))
library(factoextra)
res.pca <- prcomp(data.stand.s, scale = TRUE)
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

##Graph of variables
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

##Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

library(factoextra)
# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 









