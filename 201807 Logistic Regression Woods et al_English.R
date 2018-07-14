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
# the potato-packing example in Woods et
# al. (2006, 2017), which studies the impact
# of three variables in a binary variable
# throughout 16 runs.
# The linear predictor includes interactions
# between the three variables.
# The optimal design is found using Overstall
# and Woods' ACE method with the parameters used
# in Woods et al. (2017).
#
##




# Set seed for reproducibility
set.seed(10021995)


# Preamble ----
library(MASS)
library(acebayes)
library(tidyverse)
library(GGally)
ggplot2::theme_set(theme_bw())



# Initial parameters ---- ### ### ### ####

# Sample size ----
## Selected according to the article cited in the thesis
n <- 16




# Initial design ----
## Randomly selected. It's a 16 x 3 matrix where
## each element is taken from a uniform dist
# in (-1.2872, 1.2872)
start.d <- matrix(runif(n=3*n, -1.2872, 1.2872),
                  ncol = 3, nrow = n,
                  byrow = TRUE, 
                  dimnames = list(as.character(1:n),  c("x1", "x2", "x3")))






# Initial distribution ----
## Function that takes an integer B and returns a B x 10
## where each entry is distributed (independently) as 
## is mentioned in the thesis
prior <- function(B){
  
  # Initialize as ceros
  p <- matrix(rep(0, B*10), ncol = 10, nrow = B)
  
  # Use distributions defined in thesis
  p[ , c(1, 4:10)] <- runif(B*8, -2, 2)
  p[ , 2:3] <- runif(B*2, 2, 6)
  
  return(p)
}





# Optimal design ---- ### ### ### ###
## Obtained with Overstall and Woods' ACE algorithm
## with the parameters specified in the thesis
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

# Show the optimal design's summary
eta

# Print the optimal design
eta$phase2.d

# Generate graph comparing variables
GGally::ggpairs(as.data.frame(eta$phase2.d))
ggplot2::ggsave("log_reg_fig.png")


# Save the design in another variable
eta1 <- eta



# Assess convergence ---- ### ### ### ####


eta1.phase <- -c(eta1$phase1.trace, eta1$phase2.trace)
m <- length(eta1.phase)
eta1.phase <- data.frame("Número" = 1:m, "Utilidad_esperada" = eta1.phase)

# Generate and save plot
ggplot2::ggplot(eta1.phase, aes(x = Número, y = Utilidad_esperada))+
  ggplot2::geom_line()+
  ggplot2::labs(x = "Número de iteración",
                y = "Pérdida esperada")

ggplot2::ggsave("fig_conv_log_reg.png")