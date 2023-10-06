# Práctico 4: Modelos lineales mixtos

En este práctico utilizaremos los modelos lineales mixtos para analizar dos tipos de diseños: medidas repetidas y anidados.

---

## Contenido

1. [Diseño de medidas repetidas](https://github.com/BioCastaneda/Inverskin/blob/main/Clase_4.md#1-dise%C3%B1o-de-medidas-repetidas)
2. [Diseño anidado]()

---
## 1. Diseño de medidas repetidas

El diseño de medidas repetidas es aquel en el cual una misma unidad muestreal es medida múltiples veces, repetidas veces. Cuando una unidad muestreal es medida dos veces (e.g., antes y después), 
nos enfrentamos un diseño pareado (ver Clase 2). Pero cuando tenemos más de dos mediciones temporales para una misma unidad muestreal, el diseño se denomina de medidas repetidas.

El siguiente set da datos [Chickenwight](https://github.com/BioCastaneda/Inverskin/blob/main/archivos/chicken.txt) tiene pesos corporales medidos en 50 pollos durante sus primeras 21 semenas de vida (0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20 y 21 semanas).
Estos pollos fueron asignados a 4 dietas distintas para evaluar su efecto sobre el peso corporal de los pollos.

Lo primero que vamos a hacer es cargar el set de datos y revisar su estructura
```
ChickWeight <- read.table("chicken.txt", header=T)
str(ChickWeight)
```

Como se puede observar, las variables categóricas están en formato de número entero y no de factor. Por esto, vamos a proceder a asignarlas como factores.
```
ChickWeight$Diet <- as.factor(ChickWeight$Diet)
ChickWeight$Chick <- as.factor(ChickWeight$Chick)
ChickWeight$Time <- as.factor(ChickWeight$Time)
str(ChickWeight)
```

Ahora probar la normalidad
```
shapiro.test(ChickWeight$weight)
gghistogram(ChickWeight$weight)
#
# Los datos no son normales
gghistogram(log10(ChickWeight$weight))
shapiro.test(log10(ChickWeight$weight))
```

Ahora probar homocedasticidad
```
leveneTest(weight ~ Diet, data=ChickWeight)
leveneTest(log10(weight) ~ Diet, data=ChickWeight)
```

Ahora usaremos una librería llamada `lmer` que permite incluir efectos fijos y aleatorios.
```
library(lme4)
m1 <- lmer(weight ~ Diet*Time + (1|Chick), data=ChickWeight)
summary(m1)
#
# Usaremos la librería `car` para calcular las probabilidades exactas
library(car)
Anova(m1, test="F")
```

El análisis indica que la interacción dieta*tiempo es significativa, sugiriendo que el efecto de la dieta sobre el peso corporal es distinta entre ellas y que varía en función del tiempo

Generar una tabla con la estadística básica
```
library(dplyr)
tabla1 <- ChickWeight %>% group_by(Diet,Time) %>%
  summarise(muestras=n(),
            media=mean(weight, na.rm=T),
            DE=sd(weight, na.rm=T),
            EE=DE/sqrt(muestras))
tabla1
```

Usamos la tabla 1 para generar un gráfico con las tendencias temporales para cada dieta
```
library(ggpubr)
plot1 <- ggplot(tabla1, aes(x=Time, y=media, group=Diet, color=Diet)) + 
  geom_line(position=position_dodge(0.5)) +
  geom_point(position=position_dodge(0.5), size=2)+
  geom_errorbar(aes(ymin=media-DE, ymax=media+DE), width=.1,
                position=position_dodge(0.5))+
  labs(x="Semanas", y = "Peso (g)")+
  scale_color_discrete(name="Dieta")+
  theme_classic()+
  theme(axis.text = element_text(size=10, color="black"),
        axis.title = element_text(size=13))
plot1

## Editamos un poco la leyenda
plot1 + scale_color_discrete(name="Dieta",
                             breaks=c("1","2","3","4"),
                             labels=c("Tipo 1", "Tipo 2", "Tipo 3", "Tipo 4"))
```

También podemos observar la tendencia de peso para cada pollo
```
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick))+
  theme_classic()+
  labs(x="Semanas", y = "Peso (g)")+
  scale_color_discrete(name="Dieta",
                       breaks=c("1","2","3","4"),
                       labels=c("Tipo 1", "Tipo 2", "Tipo 3", "Tipo 4"))
```

Ahora solo vamos a analizar si existe efecto de la dieta sobre el peso a las 21 semanas. Para estos vamos a genera un subset de datos que solo tenga información de esa semana
```
d21 <- subset(ChickWeight, Time=="21")
m2 <- lm(weight ~ Diet, data=d21)
anova(m2)
```

Dado que hay diferencias significativas entre las dietas, realizaremos una comparación de Tukey
```
library(rstatix)
tukey.aov <- m3 %>% tukey_hsd()
```

Graficamos
```
plot2 <- d21 %>%
  ggplot(aes(y=weight, x=Diet, fill=Diet)) +
  geom_jitter(show.legend=F, shape=21, color="black", size=4, 
              position=position_jitterdodge(jitter.width=0.3, dodge.width=0.8)) +
  stat_summary(fun=mean, show.legend=F, geom="crossbar", position=position_dodge(width=0.8), width=0.3) + 
  labs(x="Dieta", y="Peso a los 21 días (g)")+
  scale_x_discrete(breaks=c("1","2","3","4"),
                   labels=c("Tipo 1", "Tipo 2", "Tipo 3", "Tipo 4"))+
  theme_classic()+
  theme(axis.text = element_text(size=10, color="black"),
        axis.title = element_text(size=13))
plot2
```

... y agregamos la información del análisis de Tukey
```
stat.test <- tukey.aov %>% mutate(y.position = c(375,440,455,395,420,405))

plot2 + stat_pvalue_manual(stat.test, label = "{p.adj.signif}", tip.length = 0.01,
                                      inherit.aes=FALSE)
```

---
## 2. Diseño anidado

Los diseños aninados diferen de los diseños ortogonales en que no todos los niveles de un factor están en perfecta combinación con los niveles del
otro factor. Es decir, hay niveles del factor B que solo están presentes en combinación con algunos o uno de los niveles del factor A.

En el siguiente set de datos, Tabletas, tenemos datos del grosor de unas patillas de distintos lotes, fabricados en distintas fábricas. En est caso, las tabletas del lote 1 solo se producen en la fábrica A, mientras que las pastillas del lote 4 solo se producen en la fábrica B. Con este set de datos probaremos si existen o no diferencias entre los productor producidos por cada una de las fábricas. Además, estimaremos la variabilidad entre lotes.

![Tabla](https://github.com/BioCastaneda/Inverskin/blob/main/archivos/tableta.png)



