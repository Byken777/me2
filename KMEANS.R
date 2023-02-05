{
library(factoextra)
a=iris[1:4]
sc=scale(a)
fviz_nbclust(
  sc,kmeans,method="wss"
)+labs(subtitle="Elbow method")
km=kmeans(sc,centers =3)
km
km.cluster=km$cluster
fviz_cluster(list(  data=sc,cluster=km.cluster)
)
}