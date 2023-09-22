## Continuación clase 2

Vovlvemos a generar los datos
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
# guardamos un aechivo con los datos
write.table(rinitis, "rinitis.txt", sep="\t", row.names=F)
```

## Análisis paramétrico muestras no pareadas

**IMPORTANTE**: apesar de que los datos no cumplen los supuestos paramétricos, vamos a hacer una prueba de t-student no pareada para efectos didácticos.
```
test2 <- t.test(carbohidratos ~ categorias, data=rinitis, alternative = "two.sided", paired=F, var.equal=T)
test2
```

También podemos calcular el tamaño del efecto del tratamiento. Esto nos permite cuantificar la magnitud del efecto que tiene un tratamiento sobre una variable respuesta.
```
install.packages("rstatix")
library(rstatix)
rinitis  %>% cohens_d(carbohidratos ~ categorias, paired = F)
```

Podemos calcular el poder de la prueba estadística.
```
test2$estimate  # entrega los promedios de cada grupo usados en la prueba de t
diff <- 64.89131-63.29767
diff

# Cargaremos el paquete pwr para hacer cálculos de poder (la probabilidad de rechazar H0 cuando es falsa)
install.packages("pwr")
library(pwr)
pwr.t2n.test(n1 = 2369, n2 = 671, sig.level = 0.05, power = NULL, d = diff, alternative="two.sided")
```

También podemos calcular el tamaño mímino para alcanzar una potencia del 95%.
```
sample.size <- pwr.t.test(d=1.59, power=0.95, type="two.sample", alternative="two.sided")
sample.size
plot(sample.size, xlab="sample size per group")
```

Ahora vamos a hacer un gráfico con barras de error
```
# Primero calculamos los estadígrafos necesarios para graficar
library(dplyr)
data.summary <- rinitis %>%
  group_by(categorias) %>%
  summarise(
    mean = mean(carbohidratos, na.rm = TRUE),
    sd = sd(carbohidratos, na.rm = TRUE),
    len = mean(carbohidratos),
    se = sd/sqrt(len)
  )
data.summary

# Luego procedemos a graficar
plot2 <- ggplot(data.summary, aes(x=categorias, y=mean, group=categorias, color=categorias)) + 
  geom_point(position=position_dodge(0.1), size=3)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.1,
                position=position_dodge(0.1), size=1)+
  labs(x="Condición", y = "Porcentaje de carbohidratos (%)")+
  ggtitle("Rinitis alérgica")+
  scale_y_continuous(limit = c(61, 67))+
  theme_classic()+
  theme(axis.text = element_text(size=11, color="black"),
        axis.title = element_text(size=13),
        plot.title = element_text(size=15, face="bold", hjust=0.5),
        legend.position = "none")+
  geom_line(data=tibble(x=c(1, 2), y=c(66.5, 66.5)),
                  aes(x=x, y=y),
                  inherit.aes=FALSE)+
  geom_text(data=tibble(x=1.5, y=66.7),
            aes(x=x, y=y, label="P < 2.2 e-16"), size=4,
            inherit.aes=FALSE)
plot2
```

Probemos otra opción
```
plot3 <- rinitis %>%
  ggplot(aes(y=carbohidratos, x=categorias, fill=categorias)) +
  geom_jitter(show.legend=F, shape=21, color="black", size=4, 
              position=position_jitterdodge(jitter.width=0.4, dodge.width=0.9)) +
  stat_summary(fun=mean, show.legend=F, geom="crossbar", position=position_dodge(width=0.2), width=0.3) + 
  labs(x="Condición", y="Porcentaje de carbohidratos (%)")+
  scale_y_continuous(limit = c(61, 67))+
  ggtitle("Rinitis alérgica")+
  theme_classic()+
  theme(axis.text = element_text(size=10, color="black"),
        axis.title = element_text(size=13),
        plot.title = element_text(size=15, face="bold", hjust=0.5))+
  geom_line(data=tibble(x=c(1, 2), y=c(66.5, 66.5)),
                  aes(x=x, y=y),
                  inherit.aes=FALSE)+
  geom_text(data=tibble(x=1.5, y=66.7),
            aes(x=x, y=y, label="P < 2.2 e-16"), size=4,
            inherit.aes=FALSE)
plot3
```

Ahora grafiquemos todo junto para generar una figura lista para nuestro manuscrito, tesis, presentación o póster.
```
plot4 <- ggarrange(plot2, plot3, labels=c("A","B"), ncol=2, nrow=1)
plot4
ggsave("Figura_1.pdf")
ggsave("Figura_1.tiff", units="in", width=12.2, height=3.79)
```

---

## 3. Comparación entre dos grupos pareados

En esta sección vamos a comparar niveles plasmáticos de un inmunosupresor tomados en un grupo de pacientes después de 1 y 12 horas de su administración.
Los datos  que vamos a utilizar están guardados en diferentes tipos de archivos: [TXT](https://github.com/BioCastaneda/Inverskin/blob/main/archivos/tacrolimus.txt), [CSV](https://github.com/BioCastaneda/Inverskin/blob/main/archivos/tacrolimus.csv), y [EXCEL](https://github.com/BioCastaneda/Inverskin/blob/main/archivos/tacrolimus.xlsx). Descargue los tres archivos.

Carguemos los archivos
```
# TXT
data1 <- read.table("tacrolimus.txt", header=T)
head(data1)

# CSV
data2 <- read.csv("tacrolimus.csv", header=T, sep=";")
head(data2)

# EXCEL
install.packages("readxl")
library(readxl)
data3 <- read_xlsx("tacrolimus.xlsx")  
head(data3)

str(data1)
data1$tiempo.factor <- as.factor(data1$tiempo)
str(data1)
```

Estadísticas básicas
```
library(dplyr)
tabla2 <- group_by(data1, tiempo.factor) %>%
  summarise(muestras=n(),
            media=mean(C1, na.rm=T),
            mediana=median(C1, na.rm=T),
            varianza=var(C1, na.rm=T),
            DE=sd(C1, na.rm=T),
            EE=DE/sqrt(muestras))
tabla2
```

Pruebas de normalidad
```
## Visualizar el qqplot
library(ggpubr)
ggqqplot(data1$C1)

## Shapiro-Wilk funcionan bien para tamaños muestreales mayores a 30
shapiro.test(data1$C1)

# Los datos no son normales y vamos a transformar a logaritmo de 10,
# pero antes revisamos el rango de valores. Si hay valores menores a 1,
# sumar una constante para evitar datos indefinidos
range(data1$C1)
shapiro.test(log10(data1$C1))
ggqqplot(log10(data1$C1))
```

Pruebas de homocedasticidad
```
## Prueba de Barlett
bartlett.test(C1 ~ tiempo.factor, data=data1)
## Prueba de Levene
library(car)
leveneTest(C1 ~ tiempo.factor, data=data1)
## Prueba de Filgner-Killen
fligner.test(C1 ~ tiempo.factor, data=data1)
```

Todas las pruebas indican que las varianzas no son homogéneas entre ambos grupos, así que usaremos una transformación.
```
## Prueba de Barlett
bartlett.test(log10(C1) ~ tiempo.factor, data=data1)
## Prueba de Levene
library(car)
leveneTest(log10(C1) ~ tiempo.factor, data=data1)
## Prueba de Filgner-Killen
fligner.test(log10(C1) ~ tiempo.factor, data=data1)
```

Ahora realizaremos una prueba de t para muestras pareadas.
```
# Primero con el paquete básico
test3 <- t.test(log10(TAC) ~ tiempo, data=data1.nuevo, paired=TRUE)  # el argumento "paired" establece si las muestras son pareadas o no
test3

# Ahora utilizado el paquete rstatix.
library(rstatix)
data1.nuevo$log10.TAC <- log10(data1.nuevo$TAC)  # debemos crear una nueva variable para rstatix funcione
head(data1.nuevo)

# El paquete rstatix tiene la ventaja de que entrega información del análisis de manera tabulada que después podemos usar.
test4 <- data1.nuevo %>%
  t_test(log10.TAC ~ tiempo, paired=TRUE) %>%
  add_significance()
test4
```

Como podemos ver existen diferencias significativas en los niveles de TAC entre las muestras tomadas después de 1 y 12 horas de adminstrado el fármaco.
Sin embargo, el valor de P (i.e., la probabilidad de que la H0 sea verdadera) no nos informa sobre la magnitud del efecto. Para esto vamos a calcular
el tamaño del efecto de Cohens
```
data1.nuevo  %>% cohens_d(log10.TAC ~ tiempo, paired = TRUE)
# El tamaño del efecto es grande
```

Ahora vamos a graficar.
```
set.seed(0)    # Setear un valor semilla no permitira evitar la aleatoriedad de los valores en los gráficos
plot4 <- data1.nuevo %>%                                                              # set de datos
  ggplot(aes(y=TAC, x=tiempo, fill=tiempo)) +                                         # variables x e y. Además indica que el color sea distinto entre los grupos
  geom_jitter(show.legend=F, shape=21, color="black", size=4,                         # forma, color y tamaño de los símbolos
              position=position_jitterdodge(jitter.width=0.3, dodge.width=0.5)) +     # disperción de los símbolos
  stat_summary(fun=mean, show.legend=F, geom="crossbar",                              # agrega una línea horizontal con el valor del promedio
               position=position_dodge(width=0.2), width=0.5) +                       # grosor y ancho de la línea
  labs(x="Tiempo de muestra (h)", y="Niveles plasmáticos de TAC (ng/ml)")+            # títulos de los ejes
  scale_x_discrete(breaks=c("C1","C12"),  
                   labels=c(glue("1"),
                            glue("12")))+                                             # cambiar los valores del eje x
  theme_classic()+                                                                    # formato del fondo del gráfico
  theme(axis.text = element_text(size=10, color="black"),                             # formato de las unidades de los ejes
        axis.title = element_text(size=13))                                           # formato del título de los ejes
plot4axis.title = element_text(size=13))
plot4
```

Podemos agregar el resultado de la prueba estadística al gráfico
```
stat.test <- test4 %>% mutate(y.position = c(25))                                     # definimos la posición del resultado en el eje y
plot4 + stat_pvalue_manual(stat.test, label = "P < {p}", tip.length = 0.01, ,         # definimos dónde la información
                           inherit.aes=FALSE)
ggsave("Figura_2.pdf")                                                                # guardamos en formato pdf
```




















