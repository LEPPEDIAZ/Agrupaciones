library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el n�mero de clusters �ptimo
library(factoextra) #Para hacer gr�ficos bonitos de clustering

#k-medias
datos<-(movies[,3:4])
km<-kmeans(movies[,3:4],7)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Species))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Species))*100
g3<- datos[datos$grupo==3,]
prop.table(table(g3$Species))*100

plotcluster(movies[,3:4],km$cluster) #grafica la ubicación de los clusters

#Clustering jerárquico
hc<-hclust(dist(movies[,3:4])) #Genera el clustering jerárquico de los datos
plot(hc) #Genera el dendograma
rect.hclust(hc,k=7) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=7) #corta el dendograma, determinando el grupo de cada fila
datos$gruposHC<-groups


g1HC<-dataset[dataset$gruposHC==1,]
g2HC<-dataset[dataset$gruposHC==2,]
g3HC<-dataset[dataset$gruposHC==3,]
g4HC<-dataset[dataset$gruposHC==4,]
g5HC<-dataset[dataset$gruposHC==5,]
g6HC<-dataset[dataset$gruposHC==6,]
g7HC<-dataset[dataset$gruposHC==7,]

#Fuzzy C-Means
fcm<-cmeans(movies[,3:4],7)
datos$FCGrupos<-fcm$cluster
datos<-cbind(datos,fcm$membership)

#Mixture of gaussians
mc<-Mclust(movies[,3:4],7)
plot(mc, what = "classification", main="MClust Classification")
datos$mxGau<-mc$classification
g1MC<-datos[datos$mxGau==1,]
g2MC<-datos[datos$mxGau==2,]
g3MC<-datos[datos$mxGau==3,]
g4MC<-datos[datos$mxGau==4,]
g5MC<-datos[datos$mxGau==5,]
g6MC<-datos[datos$mxGau==6,]
g7MC<-datos[datos$mxGau==7,]



#Método de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(movies[,3:4]))
mean(silkm[,3]) #0.55, no es la mejor partición pero no está mal

#Método de la silueta para clustering jerárquico
silch<-silhouette(groups,dist(movies[,3:4]))
mean(silch[,3]) #0.51, no es la mejor partición pero no está mal

#Método de la silueta para fuzzy cmeans
silfcm<-silhouette(fcm$cluster,dist(movies[,3:4]))
mean(silfcm[,3]) #0.54, no es la mejor partición pero no está mal

#Método de la silueta para mixture of gaussians
silmg<-silhouette(mc$classification,dist(movies[,3:4]))
mean(silmg[,3]) #0.50, no es la mejor partición pero no está mal

#Método de Ward para determinar el número correcto de clusteres con k-medias
#Para saber cual es el mejor numero de clusters
wss <- (nrow(movies[,3:4])-1)*sum(apply(movies[,3:4],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(movies[,3:4], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

#Paquete para saber el mejor n�mero de clusters
nb <- NbClust(movies[,3:4], distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")

#Visualizaci�n de los clusters con factoextra
#Visualizaci�n de las k-medias
fviz_cluster(km, data = movies[,3:4],geom = "point", ellipse.type = "norm")

#Visualizaci�n de cluster jer�rquico
hc.cut<-hcut(movies[,3:4], k=7, hc_method = "complete")
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
fviz_cluster(hc.cut, ellipse.type = "convex")



