## Continuación clase 2

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
test1$estimate  # entrega los promedios de cada grupo usados en la prueba de t
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
data1$tiempo <- as.factor(data1$tiempo)
```

Estadísticas básicas
```
tabla2 <- group_by(data1, tiempo) %>%
  summarise(muestras=n(),
            media=mean(C0, na.rm=T),
            mediana=median(C0, na.rm=T),
            varianza=var(C0, na.rm=T),
            DE=sd(C0, na.rm=T),
            EE=DE/sqrt(muestras))
tabla2
```
















