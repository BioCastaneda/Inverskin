# Práctico 7: Análisis multivariado

En este práctico veremos:

## Contenido

1. [Análisis de componentes principales (PCA)](https://github.com/BioCastaneda/Inverskin/blob/main/Clase_7.md#1-an%C3%A1lisis-de-componentes-principales-pca)
2. [Escalamiento multidimensional (MDS)](https://github.com/BioCastaneda/Inverskin/blob/main/Clase_7.md#2-escalamiento-multidimensional-mds)
3. [Análisis de conglomerados (Clusters)](https://github.com/BioCastaneda/Inverskin/blob/main/Clase_7.md#3-an%C3%A1lisis-de-conglomerados-clusters)

---
## 1. Análisis de componentes principales (PCA)

Descargar los datos contenidos en el archivo de texto [Phylum](https://github.com/BioCastaneda/Inverskin/blob/main/archivos/DSdata_phylum.xlsx)

Este set de datos contiene las abundancias (realtivas y absolutas) de bacterias asociadas al intestito de <i>Drosophila subobscura</i> a nivel taxonómico de phylum.

1. Analizamos las correlaciones entre variables
```
library(rstatix)
library(readxl)
library(factoextra)
library(FactoMineR)

data1 <- read_xlsx("DSdata_phylum.xlsx")
head(data1)
dim(data1)
str(data1)
#
## Seleccionamos las columnas con las abundancias absolutas
data2 <- data1[,c(9:13)]
cor.mat <- data2 %>% cor_mat()
cor.mat %>% pull_lower_triangle() %>% cor_plot()
```

2. Realizamos el PCA
```
# Calculamos los componentes principales
ds.pca <- PCA(data2, graph=F)

# Visualizamos la varianza explicada por cada PC
fviz_eig(ds.pca, addlabels = TRUE, ylim = c(0, 80))
```

3. Graficar
```
plot.pca <- fviz_pca_biplot(ds.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = data1$Stress, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = c("red","blue"),
                addEllipses = TRUE,
                # Variables
                alpha.var=1, col.var = "black",
                gradient.cols = "black",
                legend.title = list(fill = "Stress"))

plot.pca
```

---
## 2. Escalamiento multidimensional (MDS)

Descargar los datos contenidos en el archivo de texto [Otus](https://github.com/BioCastaneda/Inverskin/blob/main/archivos/otu_table.xlsx)

Este set de datos contiene las abundancias absolutas de bacterias asociadas al intestito de <i>Drosophila subobscura</i> a nivel de OTU.

1. Cargamos y realizamos el MDS
```
library(vegan)
library(ggplot2)
library(ggpubr)
library(MASS)

otu <- read_xlsx("otu_table.xlsx")
head(otu)
#
## Removemos las columnas categóricas
mat.otu <- otu[,-c(1:3),]
#
# Realizamos el MDS
distancia <- vegdist(mat.otu, method="bray")
mds <- metaMDS(distancia)
```

2. Graficamos
```
## Obtenemos las coordenadas del MDS
scores(mds, display="sites")
#
## Creamos un dataframe nuevo
data.scores <- as.data.frame(scores(mds, display="sites"))
data.scores$sex <- otu$Sex
data.scores$stress <- otu$Stress
#
ggscatter(data.scores, x="NMDS1", y="NMDS2", color="blue", size=3)
ggscatter(data.scores, x="NMDS1", y="NMDS2", color="sex", size=3)
ggscatter(data.scores, x="NMDS1", y="NMDS2", color="stress", size=3)
ggscatter(data.scores, x="NMDS1", y="NMDS2", color="sex", shape="stress", size=3)
```

3. Realizamos un análisis de PERMANOVA sobre las matrices de distancia entre las muestras
```
distancia <- vegdist(mat.otu, method="bray")
adonis2(distancia ~ otu$Sex)
adonis2(distancia ~ otu$Stress)
adonis2(distancia ~ otu$Sex*otu$Stress)
```

---
## 3. Análisis de conglomerados (Clusters)

Descargar los datos contenidos en el archivo de texto [Otus](https://github.com/BioCastaneda/Inverskin/blob/main/archivos/otu_table.xlsx)

Este set de datos contiene las abundancias absolutas de bacterias asociadas al intestito de <i>Drosophila subobscura</i> a nivel de OTU.

1. Cargamos y realizamos el análisis
```
library(ape)
otu <- read_xlsx("otu_table.xlsx")
head(otu)
otu2 <- otu[order(otu$Stress),]
head(otu2)
#
## Removemos las columnas categóricas
mat.otu <- otu2[,-c(1:3),]
#
## Calculamos las distancias entre muestras...
distancia <- dist(mat.otu, method="euclidean")
## ... y el método de jerarquización
clust <- hclust(distancia, method="average")
```

2. Graficamos
```
## Opción 1
plot(as.phylo(clust),type="phylogram",cex=0.8,tip.col=c(rep("red",18),rep("blue",18)),font=2,main="Similitud entre muestras")
legend(800,30,pt.bg=c("red","blue"),c("Heat-stressed","Non-stressed"),pch=21,bty="n")
#
## Opción 2
t1 <- scale(mat.otu)
heatmap(t1, xlab="OTUs", ylab="Muestras", RowSideColors=rep(c("blue","red"),18))
```
