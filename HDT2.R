library(ggplot2)


#grafico de codos
wcss <- vector()
for(i in 1:20){
  wcss[i] <- sum(kmeans(movies[,3], i)$withinss)
}

ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:20, y = wcss), color = 'blue') + 
  ggtitle("M�todo del Codo popularidad") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')


#pruebas kmeans
irisCluster <- kmeans(movies[, 3:5], 3, nstart = 20)
irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(movies, aes(popularity, budget, color = rainbow(10866))) + geom_point()


library(ggplot2)
ggplot(movies, aes(movies$popularity, movies$runtime, color = movies$release_year)) + geom_point()



irisCluster <- kmeans(movies[, 19], 7, nstart = 20)
irisCluster$cluster <- as.factor(irisCluster$cluster)
table (irisCluster$cluster, movies$vote_average)
ggplot(movies, aes(vote_average, vote_count, color = release_year)) + geom_point()

#prueba HC


dataset <- (movies[,3:4])
library(ggdendro)
dendrogram <- hclust(dist(dataset, method = 'euclidean'), method = 'ward.D')
ggdendrogram(dendrogram, rotate = FALSE, labels = FALSE, theme_dendro = TRUE) + 
  labs(title = "Dendrograma movies 3,4")
agrupamientoJ <- hclust(dist(dataset, method = 'euclidean'), method = 'ward.D')
clases_aj <- cutree(agrupamientoJ, k = 7)
dataset$cluster <- clases_aj
ggplot() + geom_point(aes(x = popularity, y = budget, color = cluster), data = dataset, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  ggtitle('Clusters de Datos con k = 7 / Agrupamiento Jer�rquico') + 
  xlab('popularidad') + ylab('presupuesto')



#Paquete para saber el mejor n�mero de clusters
nb <- NbClust(movies[,3:4], distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")

#Visualizaci�n de cluster jer�rquico
hc.cut<-hcut(movies[,3:4], k=3, hc_method = "complete")
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
fviz_cluster(hc.cut, ellipse.type = "convex")



#`rueba para ambas variables segun manual de R 
dataset <- (movies[,3:4])

dataset <- na.omit(dataset) # listwise deletion of missing
dataset <- scale(dataset) # standardize variables


# Determine number of clusters
wss <- (nrow(dataset)-1)*sum(apply(dataset,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dataset, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# K-Means Cluster Analysis
fit1 <- kmeans(dataset, 7) # 7 cluster solution
# get cluster means 
aggregate(dataset,by=list(fit1$cluster),FUN=mean)
# append cluster assignment
dataset <- data.frame(dataset, fit1$cluster)

# Ward Hierarchical Clustering
d <- dist(dataset, method = "euclidean") # distance matrix
fit2 <- hclust(d, method="ward") 
plot(fit2) # display dendogram
groups <- cutree(fit2, k=7) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit2, k=7, border="red")


# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(dataset, method.hclust="ward",
               method.dist="euclidean")
plot(fit2) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit2, alpha=.95)


# K-Means Clustering with 5 clusters
fit1 <- kmeans(dataset, 7)

# Cluster Plot against 1st 2 principal components


# vary parameters for most readable graph
library(cluster) 
clusplot(dataset, fit1$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(dataset, fit1$cluster)




# comparing 2 cluster solutions
library(fpc)
cluster.stats(d, fit1$cluster, fit2$cluster)




#Método de la silueta para clustering jerárquico
#Clustering jerárquico
hc<-hclust(dist(movies[,3:4])) #Genera el clustering jerárquico de los datos
plot(hc) #Genera el dendograma
rect.hclust(hc,k=7) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=7) #corta el dendograma, determinando el grupo de cada fila

silch<-silhouette(groups,dist(movies[,3:4]))
mean(silch[,3]) #0.73, no es la mejor partición pero no está mal


g1HC<-dataset[dataset$gruposHC==1,]
g2HC<-dataset[dataset$gruposHC==2,]
g3HC<-dataset[dataset$gruposHC==3,]
g4HC<-dataset[dataset$gruposHC==4,]
g5HC<-dataset[dataset$gruposHC==5,]
g6HC<-dataset[dataset$gruposHC==6,]
g7HC<-dataset[dataset$gruposHC==7,]


hc.cut<-hcut(movies[,3:4], k=7, hc_method = "complete")
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
fviz_cluster(hc.cut, ellipse.type = "convex")

#pruebas kmeans

km<-kmeans(movies[,3:4],7)
dataset$grupo<-km$cluster
plotcluster(movies[,3:4],km$cluster) #grafica la ubicación de los clusters

#Método de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(movies[,3:4]))
mean(silkm[,3]) #0.55, no es la mejor partición pero no está mal


#Visualizaci�n de los clusters con factoextra
#Visualizaci�n de las k-medias
fviz_cluster(km, data = movies[,3:4],geom = "point", ellipse.type = "norm")
#preprocesamiento
#para poder ver las dimensiones
dim(movies)
#respuesta:[1] 10866    21
#chequear null data
is.null(movies)
#respuesta:[1] FALSE
wcss <- vector()
for(i in 1:20){
  wcss[i] <- sum(kmeans(movies[,19], i)$withinss)
}

ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:20, y = wcss), color = 'blue') + 
  ggtitle("Método del Codo") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')

#pregunta 3
#kmeans
irisCluster <- kmeans(movies[, 3:4], 20, nstart = 20)
irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(movies, aes(popularity, budget, color = movies$budget)) + geom_point()
#agrupamiento jerarquico
library(mclust)
fit <- Mclust(movies)
plot(fit)

library(factoextra)
fviz_mclust(object = fit, what = "BIC", pallete = "jco") + 
  scale_x_discrete(limits = c(1:10))
#agrupamiento gaussiano
mc<-Mclust(movies[,1:4],3)
plot(mc, what = "classification", main="MClust Classification")
movies$mxGau<-mc$classification
g1MC<-datos[datos$mxGau==1,]
g2MC<-movies[movies$mxGau==2,]
g3MC<-movies[movies$mxGau==3,]
#silhouette
mc<-Mclust(movies[,1:4],3)
silmg<-silhouette(mc$classification,dist(movies[,1:4]))
mean(silmg[,3])
#
