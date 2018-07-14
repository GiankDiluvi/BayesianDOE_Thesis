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
# para el ejemplo de regresión Poisson
# de Atkinson y Woods (2015) y Woods et al.
# (2016).
# El diseño se encuentra utilizando el método
# ACE propuesto por Overstall y Woods (2016),
# con los parámetros utilizados por Woods et al.
# (2017).
#
##



# Semilla para reproducibilidad:
set.seed(19022013)


# Preámbulo ----
library(MASS)
library(acebayes)
library(tidyverse)
library(GGally)
ggplot2::theme_set(theme_bw())



# Parámetros iniciales ---- ### ### ### ####

# Tamaño de muestra ----
## Seleccionado acorde al artículo citado en la tesis
n <- 6




# Diseño inicial ----
## Éste se seleciona aleatoriamente. Es una matriz
## de 6 x 5, donde cada elemento es un número
## tomado de una distribución uniforme en (-1, 1)
start.d <- matrix(runif(5*n, -1, 1),
                  ncol = 5, nrow = n,
                  byrow = TRUE, 
                  dimnames = list(as.character(1:n),  
                                  c("x1", "x2", "x3", "x4", "x5")))


# Distribución inicial ----
## Ésta es una función que regresa una matriz de B x 5,
## donde las entradas se distribuyen (independientemente)
## como se menciona en la tesis


for(alfa in c(0.5, 0.75)){


prior <- function(B){
  
  # Inicializa como ceros
  p <- matrix(rep(0, B*5), ncol = 5, nrow = B)
  
  # Utiliza distribuciones definidas en la tesis
  for(i in c(1, 3, 5)){
    p[ , i] <- runif(B, 1, 1+alfa)
  }
  
  for(i in c(2, 4)){
    p[ , i] <- runif(B, -1-alfa, -1)
  }

  
  return(p)
}





# Diseño óptimo ---- ### ### ### ####
## Se obtiene mediante el algoritmo ACE de Overstall y Woods
## con los parámetros especificados en la tesis
eta <- acebayes::aceglm(formula=~0+x1+x2+x3+x4+x5,
                        start.d = start.d, 
                        family = poisson(link = "log"),
                        prior = prior,
                        method = "MC",
                        B = c(20000, 1000),
                        criterion = "D",
                        Q = 10,
                        lower = -1,
                        upper = 1)

# Muestra el resumen del diseño óptimo
eta

# Muestra el diseño óptimo
eta$phase2.d

# Compara gráficamente las variables
GGally::ggpairs(as.data.frame(eta$phase2.d))
ggplot2::ggsave(paste0("pois_reg_alfa", as.character(alfa), ".png"))



# Guarda el diseño en otra variable
ifelse(alfa == 0.5, eta2 <- eta, eta3 <- eta)

}



# Determina convergencia ---- ### ### ### ####


# alfa = 0.5
eta2.phase <- -c(eta2$phase1.trace, eta2$phase2.trace)
m <- length(eta2.phase)
eta2.phase <- data.frame("Número" = 1:m, "Utilidad_esperada" = eta2.phase)

ggplot2::ggplot(eta2.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
                y = "Pérdida esperada")
ggplot2::ggsave("fig_conv_pois_reg_alfa5.png")


# alfa = 0.75
eta3.phase <- -c(eta3$phase1.trace, eta3$phase2.trace)
m <- length(eta3.phase)
eta3.phase <- data.frame("Número" = 1:m, "Utilidad_esperada" = eta3.phase)

ggplot2::ggplot(eta3.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
                y = "Pérdida esperada")
ggplot2::ggsave("fig_conv_pois_reg_alfa75.png")
