##############################################
#                                            #
# Diseño Bayesiano de experimentos para GLMs #
# Autor: Gian Carlo Diluvi                   #
# Asesor: Ernesto Barrios                    #
# Instituto Tecnológico                      #
# Autónomo de México                         #
# 2018                                       #
#                                            #
##############################################
#
# Descripción:
#
# Encuentra el diseño D-óptimo Bayesiano
# para el ejemplo de las papas estudiado por
# Woods et al. (2006, 2017), el cual explica
# el efecto de tres variables en una respuesta
# binaria y en 16 ensayos
# El predictor lineal incluye términos cruzados
# y cuadráticos para las 3 variables. 
# El diseño se encuentra utilizando el método
# ACE propuesto por Overstall y Woods (2016),
# con los parámetros utilizados por Woods et al.
# (2017).
#
##




# Semilla para reproducibilidad:
set.seed(10021995)


# Preámbulo ----
library(MASS)
library(acebayes)
library(tidyverse)
library(GGally)
ggplot2::theme_set(theme_bw())



# Parámetros iniciales ---- ### ### ### ####

# Tamaño de muestra ----
## Seleccionado acorde al artículo citado en la tesis
n <- 16




# Diseño inicial ----
## Éste se seleciona aleatoriamente. Es una matriz
## de 16 x 3, donde cada elemento es un número
## tomado de una distribución uniforme en (-1.2872, 1.2872)
start.d <- matrix(runif(n=3*n, -1.2872, 1.2872),
                  ncol = 3, nrow = n,
                  byrow = TRUE, 
                  dimnames = list(as.character(1:n),  c("x1", "x2", "x3")))






# Distribución inicial ----
## Ésta es una función que regresa una matriz de B x 10,
## donde las entradas se distribuyen (independientemente)
## como se menciona en la tesis
prior <- function(B){
  
  # Inicializa como ceros
  p <- matrix(rep(0, B*10), ncol = 10, nrow = B)
  
  # Utiliza distribuciones definidas en la tesis
  p[ , c(1, 4:10)] <- runif(B*8, -2, 2)
  p[ , 2:3] <- runif(B*2, 2, 6)
  
  return(p)
}





# Diseño óptimo ---- ### ### ### ###
## Se obtiene mediante el algoritmo ACE de Overstall y Woods
## con los parámetros especificados en la tesis
eta <- acebayes::aceglm(formula=~x1+x2+x3+I(x1^2)+I(x2^2)+I(x3^2)+I(x1*x2)+I(x1*x3)+I(x2*x3),
                        start.d = start.d, 
                        family = binomial,
                        prior = prior,
                        method = "MC",
                        B = c(20000, 1000),
                        criterion = "D",
                        Q = 10,
                        lower = -1.2872,
                        upper = 1.2872)

# Muestra el resumen del diseño óptimo
eta

# Muestra el diseño óptimo
eta$phase2.d

# Compara gráficamente las variables
GGally::ggpairs(as.data.frame(eta$phase2.d))
ggplot2::ggsave("log_reg_fig.png")


# Guarda el diseño en otra variable
eta1 <- eta



# Determina convergencia ---- ### ### ### ####


eta1.phase <- -c(eta1$phase1.trace, eta1$phase2.trace)
m <- length(eta1.phase)
eta1.phase <- data.frame("Número" = 1:m, "Utilidad_esperada" = eta1.phase)

# Genera y guarda imagen
ggplot2::ggplot(eta1.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
                y = "Pérdida esperada")

ggplot2::ggsave("fig_conv_log_reg.png")