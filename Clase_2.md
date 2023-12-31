# Clase 2: Comparación de dos grupos

En esta clase veremos análisis paramétricos y no paramétricos para comparar dos grupos, ya sea pareados como no pareados.

---

## Contenido

1. [Comparación de promedio contra un valor dado](https://github.com/BioCastaneda/Inverskin/edit/main/Clase_2.md#1-comparaci%C3%B3n-de-promedio-contra-un-valor-dado)
2. [Comparación entre dos grupos no pareados](https://github.com/BioCastaneda/Inverskin/blob/main/Clase_2.md#2-comparaci%C3%B3n-entre-dos-grupos-no-pareados)
3. [Comparación entre dos grupos pareados](https://github.com/BioCastaneda/Inverskin/blob/main/Clase_2.md#3-comparaci%C3%B3n-entre-dos-grupos-pareados)

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

Para este ejercicio vamos a utilizar datos publicados del trabajo "High-Fat and Low-Carbohydrate Diets Are Associated with Allergic Rhinitis But Not Asthma or Atopic Dermatitis in Children" de los autores Kim y colaboradores publicados en PLoS One el 2016 (https://doi.org/10.1371/journal.pone.0150202). El diseño de este estudio representa un muestreo no pareado, ya que las muestras de un grupo son completamente independientes a la del otro grupo.

![PLOS](https://github.com/BioCastaneda/Inverskin/blob/main/archivos/pone.0150202.t002.png)

Los tamaños muestreales de este estudios son los siguientes:

| Categoría | Rinitis alérgica | Asma | Dermatitis atópica |
| :-------: | :---: | :---: |:---: |
| No | 2369 | 2858 | 2569 | 
| Si | 671 | 182 | 471 |

Asumiendo que los datos son normales, vamos a reconstruir los datos tomando en cuenta los promedios, desviación estándar y tamaño muestreal
```
# crear 2369 datos con media 64.9 y desviación estándar 0.3
data1 <- rnorm(2369,64.9,0.3)
# crear 671 datos con media 63.3 y desviación estándar 0.5
data2 <- rnorm(671,63.3,0.5)
# juntamos con los datos
carbohidratos <- c(data1,data2)
# creamos el factor NO para los 2369 datos 
no <- factor(rep("no",2369))
# creamos el factor SI para los 671 datos 
si <- factor(rep("si",671))
# juntamos los factores
categorias <- c(no,si)
# creamos un dataframe con la variable agrupadora y la variable respuesta
rinitis <- data.frame(categorias,carbohidratos)
# revisamos el encabezado y la estructura de este dataframe
head(rinitis)
str(rinitis)
```
Vamos a ver las estadístics básicas del set de datos
```
install.packages("dplyr")
library(dplyr)
tabla1 <- group_by(rinitis, categorias) %>%
  summarise(muestras=n(),
            media=mean(carbohidratos, na.rm=T),
            mediana=median(carbohidratos, na.rm=T),
            varianza=var(carbohidratos, na.rm=T),
            DE=sd(carbohidratos, na.rm=T),
            EE=DE/sqrt(muestras))
tabla1
```

Ahora vamos a probar los supuestos de los análisis paramétricos
```
gghistogram(rinitis$carbohidratos, bins=10, title="Histograma datos originales", fill="blue", add="mean")
ggqqplot(rinitis$carbohidratos, title="Histograma datos originales", col="blue")
shapiro.test(rinitis$carbohidratos)
```

Si los datos no son normales, podemos internar transformarlos para que se distribuyan normalmente
```
# logaritmo de 10
ggqqplot(log10(rinitis$carbohidratos), title="Histograma datos originales", col="blue")
shapiro.test(log10(rinitis$carbohidratos))
# raíz cuadrada
ggqqplot(sqrt(rinitis$carbohidratos), title="Histograma datos originales", col="blue")
shapiro.test(sqrt(rinitis$carbohidratos))
# recíproco
ggqqplot(1/(rinitis$carbohidratos), title="Histograma datos originales", col="blue")
shapiro.test(1/(rinitis$carbohidratos))
```

También vamos a probar el supuestos de homocedasticidad u homogeneidad de varianzas
```
fligner.test(carbohidratos ~ categorias, data=rinitis)
```

Si los datos no cumples con estos supuestos paramétricos, debemos usar un análisis no paramétrico.
```
test1 <- wilcox.test(carbohidratos ~ categorias, data=rinitis, alternative="two.sided", paired=F)
test1
```

Vamos a hacer un gráfico de caja y bigote
```
ggboxplot(rinitis, x="categorias", y="carbohidratos", color="categorias")
ggboxplot(rinitis, x="categorias", y="carbohidratos", color="categorias", add="jitter")
ggboxplot(rinitis, x="categorias", y="carbohidratos", color="categorias", add="jitter",
          ylab="Porcentaje de carbohidratos (%)", shape="categorias", 
          palette=c("orange","darkgreen"), legend="none")
```

Ahora vamos a hacer un gráfico de violín
```
ggviolin(rinitis, x="categorias", y="carbohidratos", color="categorias", add="jitter",
          ylab="Porcentaje de carbohidratos (%)", shape="categorias", 
          palette=c("orange","darkgreen"), legend="none")
```

Podemos ir agregando elementos. Por ejemplo, la mediana de cada grupo.
```
ggviolin(rinitis, x="categorias", y="carbohidratos", color="categorias", add="jitter",
         ylab="Porcentaje de carbohidratos (%)", shape="categorias", 
         palette=c("orange","darkgreen"), legend="none")+
  stat_summary(fun=median, show.legend=F, geom="crossbar", position=position_dodge(width=0.5), width=0.5)
```

Vamos a volver al gráfico de caja y bigote para agregarle manualmente datos del análisis estadístico
```
#
# Vamos a crear el objeto "plot1" para después llamarlo cuando queramos
plot1 <- ggboxplot(rinitis, x="categorias", y="carbohidratos", color="categorias", add="jitter",
          ylab="Porcentaje de carbohidratos (%)", shape="categorias", 
          palette=c("orange","darkgreen"), legend="none")

# Agregamos una línea y asteríscos
plot1 + geom_line(data=tibble(x=c(1, 2), y=c(67, 67)),
                   aes(x=x, y=y),
                   inherit.aes=FALSE)+
  geom_text(data=tibble(x=1.5, y=67.3),
            aes(x=x, y=y, label="****"), size=4,
            inherit.aes=FALSE)

# Otra opción es agregar el valor de probabilidad exacto
plot1 + geom_line(data=tibble(x=c(1, 2), y=c(67, 67)),
                  aes(x=x, y=y),
                  inherit.aes=FALSE)+
  geom_text(data=tibble(x=1.5, y=67.3),
            aes(x=x, y=y, label="P < 2.2 e-16"), size=4,
            inherit.aes=FALSE)
```










