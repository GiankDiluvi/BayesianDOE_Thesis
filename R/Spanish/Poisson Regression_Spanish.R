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
ptm <- proc.time()


# Semilla para reproducibilidad:
set.seed(19022013)


# Preámbulo ----
library(MASS)
library(acebayes)
library(tidyverse)
library(GGally)
library(rjags)
library(grid)
library(gridExtra)
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


for(alpha in c(0.5, 0.75)){
  
  
  prior <- function(B){
    
    # Inicializa como ceros
    p <- matrix(rep(0, B*5), ncol = 5, nrow = B)
    
    # Utiliza distribuciones definidas en la tesis
    for(i in c(1, 3, 5)){
      p[ , i] <- runif(B, 1, 1+alpha)
    }
    
    for(i in c(2, 4)){
      p[ , i] <- runif(B, -1-alpha, -1)
    }
    
    
    return(p)
  }
  
  
  # Ahora modifica la distribución inicial
  prior_2 <- function(B){
    
    # Define matriz inicial
    p <- matrix(2 * (1 + alpha) * (rbeta(5*B, 2, 2) - 0.5),
                ncol = 5, 
                nrow = B)
    
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
                          Q = 20,
                          lower = -1,
                          upper = 1)
  
  # Muestra el resumen del diseño óptimo
  #eta
  
  # Muestra el diseño óptimo
  eta$phase2.d
  
  # Compara gráficamente las variables
  GGally::ggpairs(as.data.frame(eta$phase2.d))
  ggplot2::ggsave(paste0("alpha_", 
                         as.character(round(100*alpha)),
                         "_original.png"))
  
  
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
  
  
  # Muestra el resumen del diseño óptimo
  #eta
  
  # Muestra el diseño óptimo
  eta$phase2.d
  
  # Compara gráficamente las variables
  GGally::ggpairs(as.data.frame(eta$phase2.d))
  ggplot2::ggsave(paste0("alpha_", 
                         as.character(round(100*alpha)),
                         "_modified.png"))
  
  
  
  # Guarda el diseño en otra variable
  ifelse(alpha == 0.5, eta_5_modified <- eta, eta_75_modified <- eta)
  
}






# Determina convergencia ---- ### ### ### ####

# Distribuciones iniciales originales
# alpha = 0.5
eta_5_original.phase <- -c(eta_5_original$phase1.trace, 
                           eta_5_original$phase2.trace)
m <- length(eta_5_original.phase)
eta_5_original.phase <- data.frame("Número" = 1:m, 
                                   "Utilidad_esperada" = eta_5_original.phase)

# Genera y guarda gráfica
ggplot2::ggplot(eta_5_original.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
                y = "Pérdida esperada")
ggplot2::ggsave("fig_conv_pois_reg_alfa5.png")


# alpha = 0.75
eta_75_original.phase <- -c(eta_75_original$phase1.trace, 
                            eta_75_original$phase2.trace)
m <- length(eta_75_original.phase)
eta_75_original.phase <- data.frame("Número" = 1:m, 
                                    "Utilidad_esperada" = eta_75_original.phase)

# Genera y guarda gráfica
ggplot2::ggplot(eta_75_original.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
                y = "Pérdida esperada")
ggplot2::ggsave("fig_conv_pois_reg_alfa75.png")





# Distribuciones iniciales modificadas
# alpha = 0.5
eta_5_modified.phase <- -c(eta_5_modified$phase1.trace, 
                           eta_5_modified$phase2.trace)
m <- length(eta_5_modified.phase)
eta_5_modified.phase <- data.frame("Número" = 1:m, 
                                   "Utilidad_esperada" = eta_5_modified.phase)

# Genera y guarda gráfica
ggplot2::ggplot(eta_5_modified.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
                y = "Pérdida esperada")
ggplot2::ggsave("fig_conv_pois_reg_alfa5_modified.png")


# alpha = 0.75
eta_75_modified.phase <- -c(eta_75_modified$phase1.trace, 
                            eta_75_modified$phase2.trace)
m <- length(eta_75_modified.phase)
eta_75_modified.phase <- data.frame("Número" = 1:m,
                                    "Utilidad_esperada" = eta_75_modified.phase)

# Genera y guarda gráfica
ggplot2::ggplot(eta_75_modified.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
                y = "Pérdida esperada")
ggplot2::ggsave("fig_conv_pois_reg_alfa75_modified.png")




### Análisis de la distribución posterior #### --- --- ---

# Ingresa de nuevo semilla inicial
set.seed(19022013)

# Simula betas
beta1 <- runif(1, 1, 1.5)
beta2 <- runif(1, -1.5, -1)
beta3 <- runif(1, 1, 1.5)
beta4 <- runif(1, -1.5, -1)
beta5 <- runif(1, 1, 1.5)
beta <- c(beta1, beta2, beta3, beta4, beta5)

# Modelo D-óptimo ###
# Simula datos
x_original <- eta_5_original$phase2.d
z_original <- x_original
for(i in 1:5){
  z_original[ , i] <- beta[i] * x_original[ , i]
}
z_original <- rowSums(z_original)
l_original <- exp(z_original)
y <- NULL
for(i in 1:6){
  y[i] <- rpois(1, lambda = l_original[i])
}


# Prepara datos para el modelo de JAGS
x1 <- x_original[ , 1]
x2 <- x_original[ , 2]
x3 <- x_original[ , 3]
x4 <- x_original[ , 4]
x5 <- x_original[ , 5]
N <- 6

# Ajusta modelo y obtén distribuciones posteriores con JAGS
cat("
    
    model{
    for (i in 1:N){
    y[i] ~ dpois(l[i])
    log(l[i]) <- z[i]
    z[i] <- b1*x1[i] + b2*x2[i] + b3*x3[i] + b4*x4[i] + b5*x5[i]
    }
    b1 ~ dunif(1, 1.5)
    b2 ~ dunif(-1.5, -1)
    b3 ~ dunif(1, 1.5)
    b4 ~ dunif(-1.5, -1)
    b5 ~ dunif(1, 1.5)
    }
    ", fill = TRUE, file = "Modelo_D.txt")

original <- jags.model("Modelo_D.txt",
                       data = list('x1' = x1,
                                   'x2' = x2,
                                   'x3' = x3,
                                   'x4' = x4,
                                   'x5' = x5,
                                   'y' = y,
                                   'N' = N),
                       n.chains = 1)

original <- coda.samples(original,
                         c('b1', 'b2', 'b3', 'b4', 'b5'),
                         10000)
# Guarda muestras
b1 <- original[[1]][, 1]
b2 <- original[[1]][, 2]
b3 <- original[[1]][, 3]
b4 <- original[[1]][, 4]
b5 <- original[[1]][, 5]


# Crea tibble con muestras de las distribuciones posteriores
posterior <- tibble(Diseño = rep("D-óptimo", 50000),
                    Beta = rep(c("Beta1", "Beta2",
                                 "Beta3", "Beta4",
                                 "Beta5"), each = 10000),
                    Valor = c(b1, b2,b3, b4, b5))




# Modelo SIL-óptimo ###
# Define diseño óptimo (tomado de Woods et al., 2017)
x_mod <- matrix(c(-0.5, rep(1, 5),
                  -1, 0.56, rep(-1, 4),
                  rep(1, 2), -0.31, rep(1, 3),
                  rep(-1, 3), 0.33, rep(-1, 2),
                  rep(1, 4), -0.38, 1),
                ncol = 5)

# Simula datos
z_mod <- x_mod
for(i in 1:5){
  z_mod[ , i] <- beta[i] * x_mod[ , i]
}
z_mod <- rowSums(z_mod)
l_mod <- exp(z_mod)
y <- NULL
for(i in 1:6){
  y[i] <- rpois(1, lambda = l_mod[i])
}


# Prepara datos para el modelo de JAGS
x1 <- x_mod[ , 1]
x2 <- x_mod[ , 2]
x3 <- x_mod[ , 3]
x4 <- x_mod[ , 4]
x5 <- x_mod[ , 5]
N <- 6

# Ajusta modelo y obtén distribuciones posteriores con JAGS
cat("
    
    model{
    for (i in 1:N){
    y[i] ~ dpois(l[i])
    log(l[i]) <- z[i]
    z[i] <- b1*x1[i] + b2*x2[i] + b3*x3[i] + b4*x4[i] + b5*x5[i]
    }
    b1 ~ dunif(1, 1.5)
    b2 ~ dunif(-1.5, -1)
    b3 ~ dunif(1, 1.5)
    b4 ~ dunif(-1.5, -1)
    b5 ~ dunif(1, 1.5)
    }
    ", fill = TRUE, file = "Modelo_SIL.txt")

mod <- jags.model("Modelo_SIL.txt",
                  data = list('x1' = x1,
                              'x2' = x2,
                              'x3' = x3,
                              'x4' = x4,
                              'x5' = x5,
                              'y' = y,
                              'N' = N),
                  n.chains = 1)

mod <- coda.samples(mod,
                    c('b1', 'b2', 'b3', 'b4', 'b5'),
                    10000)
# Guarda muestras
b1 <- mod[[1]][, 1]
b2 <- mod[[1]][, 2]
b3 <- mod[[1]][, 3]
b4 <- mod[[1]][, 4]
b5 <- mod[[1]][, 5]


# Crea tibble con muestras de las distribuciones posteriores
posterior_mod <- tibble(Diseño = rep("SIL-óptimo", 50000),
                        Beta = rep(c("Beta1", "Beta2",
                                     "Beta3", "Beta4",
                                     "Beta5"), each = 10000),
                        Valor = c(b1, b2,b3, b4, b5))


posterior <- bind_rows(posterior, posterior_mod)




# Grafica densidades marginales de 
# las distribuciones posteriores
text_size <- 25

# Beta 1
beta1_graph <- posterior %>% 
  dplyr::filter(Beta == "Beta1") %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_density(aes(x = Valor,
                            fill = Diseño),
                        alpha = 0.3) +
  ggplot2::geom_vline(xintercept = beta1,
                      linetype = "dashed") + 
  ggplot2::xlim(1, 1.5) +
  ggplot2::labs(x = "",
                y = "",
                title = "Beta 1") +
  ggplot2::theme(text = element_text(size = text_size))



# Beta 2
beta2_graph <- posterior %>% 
  dplyr::filter(Beta == "Beta2") %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_density(aes(x = Valor,
                            fill = Diseño),
                        alpha = 0.3) +
  ggplot2::geom_vline(xintercept = beta2,
                      linetype = "dashed") + 
  ggplot2::xlim(-1.5, -1) +
  ggplot2::labs(x = "",
                y = "",
                title = "Beta 2") +
  ggplot2::theme(text = element_text(size = text_size))



# Beta 3
beta3_graph <- posterior %>% 
  dplyr::filter(Beta == "Beta3") %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_density(aes(x = Valor,
                            fill = Diseño),
                        alpha = 0.3) +
  ggplot2::geom_vline(xintercept = beta3,
                      linetype = "dashed") + 
  ggplot2::xlim(1, 1.5) +
  ggplot2::labs(x = "",
                y = "",
                title = "Beta 3") +
  ggplot2::theme(text = element_text(size = text_size))




# Beta 4
beta4_graph <- posterior %>% 
  dplyr::filter(Beta == "Beta4") %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_density(aes(x = Valor,
                            fill = Diseño),
                        alpha = 0.3) +
  ggplot2::geom_vline(xintercept = beta4,
                      linetype = "dashed") + 
  ggplot2::xlim(-1.5, -1) +
  ggplot2::labs(x = "",
                y = "",
                title = "Beta 4") +
  ggplot2::theme(text = element_text(size = text_size))




# Beta 5
beta5_graph <- posterior %>% 
  dplyr::filter(Beta == "Beta5") %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_density(aes(x = Valor,
                            fill = Diseño),
                        alpha = 0.3) +
  ggplot2::geom_vline(xintercept = beta5,
                      linetype = "dashed") + 
  ggplot2::xlim(1, 1.5) +
  ggplot2::labs(x = "",
                y = "",
                title = "Beta 5") +
  ggplot2::theme(text = element_text(size = text_size))







# Define función para crear grid.arrange 
# con leyenda compartida (Wickham, 2016)
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), 
                                       nrow = 1, 
                                       position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}


# Genera grid de gráficas y guarda
graph_arrange <- grid_arrange_shared_legend(beta1_graph,
                                            beta2_graph, 
                                            beta3_graph, 
                                            beta4_graph, 
                                            beta5_graph,
                                            ncol=2, nrow=3)
ggsave(filename = "posterior_graphs.png",
       plot = graph_arrange,
       dpi = 300)



# Reporta el tiempo que tardó en correr el código
proc.time() - ptm