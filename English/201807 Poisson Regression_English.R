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
library(truncnorm)
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
  
  # Initialize as zeros
  p <- matrix(rep(0, B*5), ncol = 5, nrow = B)
  
  # Define same prior distributions as the authors
  for(i in c(1, 3, 5)){
    p[ , i] <- runif(B, 1, 1 + alpha)
  }
  
  for(i in c(2, 4)){
    p[ , i] <- runif(B, -1 - alpha, -1)
  }

  
  return(p)
}


# Now modify original prior distribution to compare results
prior_2 <- function(B){
  
  # Initialize as zeros
  p <- matrix(rep(0, B*5), ncol = 5, nrow = B)
  
  # Define new prior distributions
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




# Original optimal design ---- ### ### ### ###
## Obtained with Overstall and Woods' ACE algorithm
## with the parameters specified in the thesis
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

# Show the optimal design's summary
#eta

# Print the optimal design
eta$phase2.d

# Generate graph comparing variables
GGally::ggpairs(as.data.frame(eta$phase2.d))
ggplot2::ggsave(filename = paste0("alpha", 
                                  as.character(alpha),
                                  "original.png"))



# Save the design in another variable
ifelse(alpha == 0.5, eta_5_original <- eta, eta_75_original <- eta)





# Optimal design w/modified priors ---- ### ### ### ###
## Obtained with Overstall and Woods' ACE algorithm
## with the parameters specified in the thesis
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



# Assess convergence ---- ### ### ### ####

# Original prior distributions
# alpha = 0.5
eta_5_original.phase <- -c(eta_5_original$phase1.trace, eta_5_original$phase2.trace)
m <- length(eta_5_original.phase)
eta_5_original.phase <- data.frame("Número" = 1:m, "Utilidad_esperada" = eta_5_original.phase)

# Generate and save plot
ggplot2::ggplot(eta_5_original.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
                y = "Pérdida esperada")
ggplot2::ggsave("fig_conv_pois_reg_alfa5.png")


# alpha = 0.75
eta_75_original.phase <- -c(eta_75_original$phase1.trace, eta_75_original$phase2.trace)
m <- length(eta_75_original.phase)
eta_75_original.phase <- data.frame("Número" = 1:m, "Utilidad_esperada" = eta_75_original.phase)

# Generate and save plot
ggplot2::ggplot(eta_75_original.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
               y = "Pérdida esperada")
ggplot2::ggsave("fig_conv_pois_reg_alfa75.png")





# Modified prior distributions
# alpha = 0.5
eta_5_modified.phase <- -c(eta_5_modified$phase1.trace, eta_5_modified$phase2.trace)
m <- length(eta_5_modified.phase)
eta_5_modified.phase <- data.frame("Número" = 1:m, "Utilidad_esperada" = eta_5_modified.phase)

# Generate and save plot
ggplot2::ggplot(eta_5_modified.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
                y = "Pérdida esperada")
ggplot2::ggsave("fig_conv_pois_reg_alfa5_modified.png")


# alpha = 0.75
eta_75_modified.phase <- -c(eta_75_modified$phase1.trace, eta_75_modified$phase2.trace)
m <- length(eta_75_modified.phase)
eta_75_modified.phase <- data.frame("Número" = 1:m, "Utilidad_esperada" = eta_75_modified.phase)

# Generate and save plot
ggplot2::ggplot(eta_75_modified.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
               y = "Pérdida esperada")
ggplot2::ggsave("fig_conv_pois_reg_alfa75_modified.png")
