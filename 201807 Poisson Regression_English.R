##############################################
#                                            #
# Bayesian Design of Experiments for GLMs    #
# Author: Gian Carlo Diluvi                  #
# Supervisor: Ernesto Barrios                #
# Instituto Tecnológico                      #
# Autónomo de México (ITAM)                  #
# 2018                                       #
#                                            #
##############################################
#
# Description:
#
# Find the D-optimal Bayesian design for
# the Poisson regression example from
# Atkinson and Woods (2015) and Woods et al.
# (2016).
# The optimal design is found using Overstall
# and Woods' ACE method with the parameters used
# in Woods et al. (2017).
#
##



# Set seed for reproducibility
set.seed(19022013)


# Preamble ----
library(MASS)
library(acebayes)
library(tidyverse)
library(GGally)
ggplot2::theme_set(theme_bw())



# Initial parameters ---- ### ### ### ####

# Sample size ----
## Selected according to the article cited in the thesis
n <- 6




# Initial design ----
## Randomly selected. It's a 6 x 5 matrix where
## each element is taken from a uniform dist
# in (-1, 1)
start.d <- matrix(runif(5*n, -1, 1),
                  ncol = 5, nrow = n,
                  byrow = TRUE, 
                  dimnames = list(as.character(1:n),  
                                  c("x1", "x2", "x3", "x4", "x5")))


# Initial distribution ----
## Function that takes an integer B and returns a B x 5
## where each entry is distributed (independently) as 
## is mentioned in the thesis


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





# Optimal design ---- ### ### ### ###
## Obtained with Overstall and Woods' ACE algorithm
## with the parameters specified in the thesis
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

# Show the optimal design's summary
eta

# Print the optimal design
eta$phase2.d

# Generate graph comparing variables
GGally::ggpairs(as.data.frame(eta$phase2.d))
ggplot2::ggsave(paste0("alpha", alpha))



# Save the design in another variable
ifelse(alpha == 0.5, eta2 <- eta, eta3 <- eta)

}



# Assess convergence ---- ### ### ### ####


# alpha = 0.5
eta2.phase <- -c(eta2$phase1.trace, eta2$phase2.trace)
m <- length(eta2.phase)
eta2.phase <- data.frame("Número" = 1:m, "Utilidad_esperada" = eta2.phase)

# Generate and save plot
ggplo2::ggplot(eta2.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplo2::geom_line()+
  ggplo2::labs(x = "Número de iteración",
               "Pérdida esperada")
ggplo2::ggsave("fig_conv_pois_reg_alfa5.png")


# alpha = 0.75
eta3.phase <- -c(eta3$phase1.trace, eta3$phase2.trace)
m <- length(eta3.phase)
eta3.phase <- data.frame("Número" = 1:m, "Utilidad_esperada" = eta3.phase)

# Generate and save plot
ggplo2::ggplot(eta3.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplo2::geom_line()+
  ggplo2::labs(x = "Número de iteración",
               y = "Pérdida esperada")
ggplo2::ggsave("fig_conv_pois_reg_alfa75.png")
