# Práctico 4: Modelos lineales mixtos

En este práctico utilizaremos los modelos lineales mixtos para analizar dos tipos de diseños: medidas repetidas y anidados.

---

## Contenido

1. [Diseño de medidas repetidas]()
2. [Diseño anidado]()

---
## 1. Diseño de medidas repetidas

El diseño de medidas repetidas es aquel en el cual una misma unidad muestreal es medida múltiples veces, repetidas veces. Cuando una unidad muestreal es medida dos veces (e.g., antes y después), 
nos enfrentamos un diseño pareado (ver Clase 2). Pero cuando tenemos más de dos mediciones temporales para una misma unidad muestreal, el diseño se denomina de medidas repetidas.

El siguiente set da datos (Chickenwight) tiene pesos corporales medidos en 50 pollos durante sus primeras 21 semenas de vida (0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20 y 21 semanas).
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
plot1 <- ggplot(tabla1, aes(x=Time, y=media, group=Diet, color=Diet)) + 
  geom_line(position=position_dodge(0.5)) +
  geom_point(position=position_dodge(0.5), size=2)+
  geom_errorbar(aes(ymin=media-DE, ymax=media+DE), width=.1,
                position=position_dodge(0.5))+
  labs(x="Días", y = "Peso (g)")+
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


