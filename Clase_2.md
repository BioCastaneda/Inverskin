# Clase 2: Comparación de dos grupos

En esta clase veremos análisis paramétricos y no paramétricos para comparar dos grupos, ya sea pareados como no pareados.

---

## Contenido

1. [Comparación de promedio contra un valor dado](https://github.com/BioCastaneda/Inverskin/edit/main/Clase_2.md#1-comparaci%C3%B3n-de-promedio-contra-un-valor-dado)
2. Comparación entre dos grupos no pareados
3. Comparación entre dos grupos pareados

---

## 1. Comparación de promedio contra un valor dado

La distancia canina (medida de cuspide a cuspide en sentido transversal) en la poblacion normal tiene un promedio de 34,8 mm. Usted quiere saber si la distancia canina de pacientes con fisura velopalatina distinta a la
distancia canina de pacientes sin esa condición. Para esto se toman muestras de 10 personas con fisura velopalatina.

| Distancia canina (mm) | 
| :---: |
| 28,5 | 
| 25,0 | 
| 26,8 | 
| 26,9 | 
| 25,1 | 
| 27,2 | 
| 25,9 | 
| 24,9 | 
| 25,2 | 
| 26,3 | 
```
rm(list=ls()) # limpia el ambiente
graphics.off() # Limpiar la lista de gráficos

fisura <-c(25.8,25,26.8,26.9,25.1,27.2,25.9,24.9,25.2,26.3)
length(fisura) # Número de datos
summary(fisura) # Estadísticas básicas: min, max, cuatiles, mediana y media 
sd(fisura)
var(fisura)
IQR(fisura) # Estadísticas básicas: variaza, desv. estandar y r. intercuartilico
```

Evaluar si los datos son normales. Es decir, evaluar si lo datos siguen una distribución normal
```
install.packages("ggpubr")
library(ggpubr)

gghistogram(fisura, bins=10, title="Histograma datos originales", fill="blue", add="mean")
gghistogram(fisura, bins=5, title="Histograma datos originales", fill="blue", add="mean")

ggqqplot(fisura, title="QQplot datos originales", col="blue")
```
También podemos hacer una prueba de hipótesis para poner a prueba si los datos son normales o no
H0: los datos son normales
H1: lo datos no son normales
```
shapiro.test(fisura)
```
Si los datos son normales, podemos aplicar la prueba paramétrica de test de t
```
t.test(fisura, mu=34.8)
```
Si los datos no son normales, podemos aplicar la prueba paramétrica de Wilcox
```
wilcox.test(fisura, mu=34.8)
```

---

## 2. Comparación entre dos grupos no pareados

Para este ejercicio vamos a utilizar datos publicados del trabajo "High-Fat and Low-Carbohydrate Diets Are Associated with Allergic Rhinitis But Not Asthma or Atopic Dermatitis in Children"
de los autores Kim y colaboradores publicados en PLoS One el 2016 (https://doi.org/10.1371/journal.pone.0150202).

![PLOS](https://github.com/BioCastaneda/Inverskin/blob/main/archivos/pone.0150202.t002.png)

Los tamaños muestreales de este estudios son los siguientes:

| Categoría | Rinitis alérgica | Asma | Dermatitis atópica |
| :-------: | :---: | :---: |:---: |
| No | 2369 | 2858 | 2569 | 
| Si | 671 | 182 | 471 |

Asumiendo que los datos son normales, vamos a reconstruir los datos tomando en cuenta los promedios, desviación estándar y tamaño muestreal

https://peerj.com/articles/1889/



















