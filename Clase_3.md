# Práctico 3: Comparación de múltiples grupos

En este práctico realizaremos diversos análisis de comparación de múltiples grupos en R. Primero, realizaremos análisis de un factor (ANOVA paramétrico y Kruskal-Wallis no paramétrico). Luego, realizaremos un diseño factorial de dos vías (ANOVA de dos vías). 

---

## Contenido

1. [Análisis paramétrico de una vía](https://github.com/BioCastaneda/Inverskin/blob/main/Clase_3.md#1-an%C3%A1lisis-param%C3%A9trico-de-una-v%C3%ADa)
2. [Análisis no-paramétrico de una vía](https://github.com/BioCastaneda/Inverskin/blob/main/Clase_3.md#2-an%C3%A1lisis-no-param%C3%A9trico-de-una-v%C3%ADa)
3. [Análisis factorial](https://github.com/BioCastaneda/Inverskin/blob/main/Clase_3.md#3-an%C3%A1lisis-factorial)

---
## 1. Análisis paramétrico de una vía

Descargar los datos contenidos en el archivo Excel [Antibióticos](https://github.com/lecastaneda/Bioestadistica/blob/main/Antibioticos.xlsx)

Este set datos corresponde a un set de datos revisado en clases, en el cual se mide el tiempo (en horas) que tarda en desaparecer un infección después de la administración de tres antibióticos. ¿Hay diferencias en el tiempo de respuesta entre los distintos antibióticos?

```
rm(list=ls()) # limpia el ambiente
graphics.off() # Limpiar la lista de gráficos

# Cargar la librería readxl que nos permitirá leer archivos Excel
library(readxl)
AB <- read_xlsx("Antibioticos.xlsx")
head(AB)
str(AB)

# Indicamos que la columna llama "Antibiotico" contiene los distinos niveles del factor Antibiótico
#
AB$Antibiotico <- as.factor(AB$Antibiotico)
str(AB)
```

Calcularemos la estadística descriptiva

```
library(dplyr)
tabla1 <- group_by(AB, Antibiotico) %>%
  summarise(muestras=n(),
            media=mean(Tiempo.h, na.rm=T),
            DE=sd(Tiempo.h, na.rm=T),
            EE=DE/sqrt(muestras))
tabla1
```

### Supuestos del análisis de una vía

**1. Normalidad**
```
ggqqplot(AB$Tiempo.h, col="blue")
shapiro.test(AB$Tiempo.h)
```

El qqplot muestra que la mayoría de los datos cae dentro de lo esperado para una distribución, aunque la prueba de 
Shapiro-Wilk indica que los datos no se distribuyen normalmente.
Probemos ahora transformando los datos a logaritmo de 10 (log10).
```
ggqqplot(log10(AB$Tiempo.h), col="blue")
shapiro.test(AB(data1$Tiempo.h))
```
Los datos transformados se ajustan a una distribución normal.


**2. Homocedasticidad**
```
fligner.test(Tiempo.h ~ Antibiotico, data=AB)
fligner.test(log10(Tiempo.h) ~ Antibiotico, data=AB)
```

Los datos transformados cumplen con los supuestos de las pruebas paramétricas.

**3. Análisis de una vía (ANOVA)**

```
## Comando aov
test1 <- aov(log10(Tiempo.h) ~ Antibiotico, data=AB)
anova(test1)

## Comando lm
test2 <- lm(log10(Tiempo.h) ~ Antibiotico, data=AB)
anova(test2)

## Graficar los residuales del modelo también nos permite saber si se cumple con el supusto de normalidad
ggqqplot(test1$residuals, col="red")
ggqqplot(test2$residuals, col="red")
```

Podemos concluir que hay diferencias significativas en el tiempo de respuesta de los antibióticos. Sin embargo, no podemos saber (aún) si todos los antibióticos tienen una respuesta distinta entre ellos o solo algunos de ellos difieren.

**4. Comparaciones múltiple**

Para esto, vamos a realizar dos pruebas a posterior: una comparación múltiple a través de la prueba de significancia honesta de Tukey (Tukey HSD), y varias pruebas pareadas corregidas por Bonferroni
```
library(rstatix)
test1 %>% tukey_hsd() # Prueba de Tukey para los datos transformados
#
## Crearemos una nueva variable llamada "log10.tiempo"
AB$log10.tiempo <- log10(AB$Tiempo.h) 
AB %>% t_test(log10.tiempo ~ Antibiotico) %>% adjust_pvalue(method="bonferroni")
```

**5. Grafiquemos!!**

```
plot1 <- AB %>%
  ggplot(aes(y=Tiempo.h, x=Antibiotico, fill=Antibiotico)) +
  geom_jitter(show.legend=F, shape=21, color="black", size=4, 
              position=position_jitterdodge(jitter.width=0.3, dodge.width=0.8)) +
  stat_summary(fun=mean, show.legend=F, geom="crossbar", position=position_dodge(width=0.8), width=0.3) + 
  labs(x="Antibióticos", y="Tiempo de respuesta (h)")+
  theme_classic()+
  theme(axis.text = element_text(size=10, color="black"),
        axis.title = element_text(size=13))
plot1
```

Agreguemos los valores de probabilidad y significancia a este gráfico
```
# Primero fijamos los valores del eje y donde queremos que vayan las comparaciones (ver tukey.aov)
stat.test <- tukey.aov %>% mutate(y.position = c(100,110,90))
#
# Gráfico con los valores de probabilidad
plot1.1 <- plot1 + stat_pvalue_manual(stat.test, label = "P < {p.adj}", tip.length = 0.01,
                           inherit.aes=FALSE)
#
# Gráfico con los signos de significancia
plot1.2 <- plot1 + stat_pvalue_manual(stat.test, label = "{p.adj.signif}", tip.length = 0.01,
                                      inherit.aes=FALSE)
#
# Colomanos ambos gráficos en una misma figura y guardamos en formato PDF:
ggarrange(plot1.1, plot1.2, labels=c("A","B"), ncol=2, nrow=1)
ggsave("Figure_2.pdf", width=10, height = 4)
```

**6. Análisis de poder**
```
## Miremos la tabla de ANOVA para ver las medias cuadradas entre y dentro de grupos
anova(test1)
#
## Calcular el poder estadístico del diseño (probabilidad de aceptar H0 cuando es veradadera)
power.anova.test(groups=3, n=10, between.var= 0.11, within.var=0.019, sig.level=0.05)
#
## Calcular el número mínimo de replicar para lograr un poder del 95%
power.anova.test(groups=3, power=0.95, between.var=0.11, within.var=0.019, sig.level=0.05)

```

---
## 2. Análisis no-paramétrico de una vía

Descargar los datos contenidos en el archivo Excel [Notas](https://github.com/lecastaneda/Bioestadistica/blob/main/notas_pregrado.txt)

Este set datos corresponde a las notas obtenidas durante cuatro años consecutivos para una asignatura de pregrado de la Facultad de Medicina. ¿Hay diferencias en las notas obtenidas para las distintas generaciones?

```
## Cargar los datos
data2 <- read.table("Notas_pregrado.txt", header=T)
head(data2)
str(data2)
#
## Asignar a la columna "year" (año) como factor
data2$year <- as.factor(data2$year)
str(data2)
```

Estadística descriptiva
```
tabla2 <- group_by(data2, year) %>%
  summarise(muestras=n(),
            media=mean(score, na.rm=T),
            DE=sd(score, na.rm=T),
            EE=DE/sqrt(muestras),
            min=min(score),
            max=max(score))
tabla2
```

Probamos los supuestos paramétricos
```
## Normalidad
shapiro.test(data2$score)
ggqqplot(data2$score, col="blue")
#
## Homocedasticidad
fligner.test(score ~ year, data=data2)
```

Claramente los datos no se adjustan a una distribución normal por lo que debemos utilizar un análisis no paramétrico, a pesar que el supuesto de homocedasticidad si se cumple.
```
kruskal.test(score~year, data=data2)
```

Dado que los datos no son normales, la mejor opción de graficarlos es con un gráfico de caja-bigote.
```
set.seed(0)
plot2 <- data2 %>%
  ggplot(aes(y=score, x=year, fill=year)) +
  geom_boxplot(show.legend=F, outlier.shape = NA) +
  geom_point(show.legend=F, shape=21, color="black", size=3, 
              position=position_jitterdodge(jitter.width=0.8, dodge.width=0.8),
             aes(color=year)) +
  labs(x="Año", y="Nota final")+
  theme_classic()+
  theme(axis.text = element_text(size=10, color="black"),
        axis.title = element_text(size=13))
plot2
```

Ahora incluiremos los resultados de las comparaciones múltiples en el gráfico
```
post.fdr <- data2 %>% wilcox_test(score ~ year) %>% adjust_pvalue(method="fdr")
post.fdr
plot2 + stat_pvalue_manual(post.fdr,label="p.adj.signif",tip.length = 0.02, 
                           y.position=c(7,8,7.5), inherit.aes=FALSE)
```

---
## 3. Análisis factorial

Descargar los datos contenidos en el archivo Excel [Resistencia](https://github.com/lecastaneda/Bioestadistica/blob/main/Resistencia.xlsx)

Este set datos corresponde a un experimento para evaluar el efecto de la microbiota intestinal y el sexo sobre la resistencia térmica en *Drosophila melanogaster*. ¿Hay efectos de la microbiota y/o el sexo sobre la resistencia térmica? ¿Estos factores interactúan entre sí?

```
## Cargar los datos
data3 <- read_xlsx("Resistencia.xlsx")
head(data3)
str(data3)
#
## Asginar columnas como factores
data3$sex <- as.factor(data3$sex)
data3$treat <- as.factor(data3$treat)
str(data3)
```

Estadística descriptiva.
```
tabla3 <- data3 %>% group_by(treat,sex) %>%
  summarise(muestras=n(),
            media=mean(timeko, na.rm=T),
            DE=sd(timeko, na.rm=T),
            EE=DE/sqrt(muestras))
tabla3
```

Evaluamos los supuestos paramétricos.
```
# Normalidad
shapiro.test(data3$timeko)
ggqqplot(data3$timeko, col="blue")
#
# Homocedasticidad
library(car)
levene_test(timeko ~ sex*treat, data=data3)
levene_test(timeko ~ sex, data=data3)
levene_test(timeko ~ treat, data=data3)
````

Dado que los datos son normales y homocedásticos, procedemos a analizar los datos con una ANOVA de dos vías
```
m1 <- lm(timeko ~ treat*sex, data=data3)
anova(m1)
#
## Prueba a posteriori de Tukey
tukey.test <- data3 %>% tukey_hsd(timeko ~ treat*sex)
tukey.test
```

Graficamos
```
set.seed(0)
plot3 <- data3 %>%
  ggplot(aes(y=timeko, x=treat, fill=sex)) +
  geom_jitter(show.legend=TRUE, shape=21, color="black", size=3, 
              position=position_jitterdodge(jitter.width=0.3, dodge.width=0.8)) +
  stat_summary(fun=mean, show.legend=F, geom="crossbar", position=position_dodge(width=0.8), width=0.5) + 
  labs(x="Treatment", y="Knockdown time (min)")+
  scale_fill_discrete(name="")+
  theme_classic()+
  theme(axis.text = element_text(size=10, color="black"),
        axis.title = element_text(size=13))
plot3
```

 
Opción para graficar las líneas de tendencia

1. Graficamos.
```
plot4 <- ggplot(tabla3, aes(x=treat, y=media, group=sex, color=sex)) + 
  geom_line(position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=2)+
  geom_errorbar(aes(ymin=media-DE, ymax=media+DE), width=.1,
                position=position_dodge(0.1))+
  labs(x="Treatment", y = "Knockdown time (min)")+
  scale_fill_discrete(name="ss")+
  theme_classic()+
  theme(axis.text = element_text(size=10, color="black"),
        axis.title = element_text(size=13))
plot4
```
