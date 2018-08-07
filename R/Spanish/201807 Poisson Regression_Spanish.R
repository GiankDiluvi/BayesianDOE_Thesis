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
  
  
  # Ahora modifica la distribución inicial
  prior_2 <- function(B){
    
    # Inicializa como ceros
    p <- matrix(rep(0, B*5), ncol = 5, nrow = B)
    
    # Define las nuevas distribuciones inciales
    for(i in c(1, 3, 5)){
      p[ , i] <- truncnorm::rtruncnorm(B,
                                       a = 1,
                                       b = 1 + alpha,
                                       mean = 1 + alpha/2,
                                       sd = 0.1 * alpha)
    }
    
    for(i in c(2, 4)){
      p[ , i] <- truncnorm::rtruncnorm(B,
                                       a = -1 - alpha,
                                       b = -1,
                                       mean = -1 - alpha/2,
                                       sd = 0.1 * alpha)
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
  ggplot2::ggsave(paste0("pois_reg_alfa", 
                         as.character(alpha),
                         ".png"))
  
  
  
  # Guarda el diseño en otra variable
  ifelse(alpha == 0.5, eta_5_original <- eta, eta_75_original <- eta)
  
  
  
  
  
# Diseño óptimo con iniciales modificadas ---- ### ### ### ####
## Se obtiene mediante el algoritmo ACE de Overstall y Woods
## con los parámetros especificados en la tesis
  eta <- acebayes::aceglm(formula=~0+x1+x2+x3+x4+x5,
                          start.d = start.d, 
                          family = poisson(link = "log"),
                          prior = prior_2,
                          method = "MC",
                          B = c(20000, 1000),
                          criterion = "D",
                          Q = 20,
                          lower = -1,
                          upper = 1)
  
  # Show the optimal design's summary
  #eta
  
  # Print the optimal design
  eta$phase2.d
  
  # Generate graph comparing variables
  GGally::ggpairs(as.data.frame(eta$phase2.d))
  ggplot2::ggsave(filename = paste0("alpha",
                                    as.character(alpha),
                                    "modified.png"))
  
  
  
  # Save the design in another variable
  ifelse(alpha == 0.5, eta_5_modified <- eta, eta_75_modified <- eta)
  
  
  
  
  
}



# Determina convergencia ---- ### ### ### ####

# Distribuciones iniciales originales
# alpha = 0.5
eta_5_original.phase <- -c(eta_5_original$phase1.trace, eta_5_original$phase2.trace)
m <- length(eta_5_original.phase)
eta_5_original.phase <- data.frame("Número" = 1:m, "Utilidad_esperada" = eta_5_original.phase)

# Genera y guarda gráfica
ggplot2::ggplot(eta_5_original.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
                y = "Pérdida esperada")
ggplot2::ggsave("fig_conv_pois_reg_alfa5.png")


# alpha = 0.75
eta_75_original.phase <- -c(eta_75_original$phase1.trace, eta_75_original$phase2.trace)
m <- length(eta_75_original.phase)
eta_75_original.phase <- data.frame("Número" = 1:m, "Utilidad_esperada" = eta_75_original.phase)

# Genera y guarda gráfica
ggplot2::ggplot(eta_75_original.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
                y = "Pérdida esperada")
ggplot2::ggsave("fig_conv_pois_reg_alfa75.png")





# Distribuciones iniciales modificadas
# alpha = 0.5
eta_5_modified.phase <- -c(eta_5_modified$phase1.trace, eta_5_modified$phase2.trace)
m <- length(eta_5_modified.phase)
eta_5_modified.phase <- data.frame("Número" = 1:m, "Utilidad_esperada" = eta_5_modified.phase)

# Genera y guarda gráfica
ggplot2::ggplot(eta_5_modified.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
                y = "Pérdida esperada")
ggplot2::ggsave("fig_conv_pois_reg_alfa5_modified.png")


# alpha = 0.75
eta_75_modified.phase <- -c(eta_75_modified$phase1.trace, eta_75_modified$phase2.trace)
m <- length(eta_75_modified.phase)
eta_75_modified.phase <- data.frame("Número" = 1:m, "Utilidad_esperada" = eta_75_modified.phase)

# Genera y guarda gráfica
ggplot2::ggplot(eta_75_modified.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
                y = "Pérdida esperada")
ggplot2::ggsave("fig_conv_pois_reg_alfa75_modified.png")
