#Working with the data
setwd("C:/Users/Abhishek/Desktop/EACH/Computing/IR Data Analysis")
VR<- read.csv("VR.csv", header = T)
s<- as.matrix(VR[,1])
VR_new <- VR[,-1]
rownames(VR_new)<- s
VR_scaled<- as.data.frame(scale(VR_new))

#Performing cluster analysis using K-Means method
c1 <- kmeans(VR_scaled, 5)
plot(VR_scaled, col=c1$cluster)
library(cluster)
clusplot(VR_scaled, c1$cluster, color=TRUE, labels=2, lines=0)

#Performing principal component analysis
pca1<-princomp(VR_scaled, scores = T, cor = T)
summary(pca1)
loadings(pca1)
screeplot(pca1, type ="lines")


#Getting the biplot which explain the loading and score of the principal component

library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
g <- ggbiplot(pca1, obs.scale = 1, var.scale = 1, 
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
pca1$loadings

#Making the multiple linear regression model for density and MCR

Model_Density <- lm(Density~ Alf..IR.+ ACd..IR. + ACLg..IR., data=VR)
plot(Model_Density)
Model_MCR <- lm(MCR~ LACAR..IR.+ ACd..IR. + ACLg..IR., data=VR)
plot(Model_MCR)

