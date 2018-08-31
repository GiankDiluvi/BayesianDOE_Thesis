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
ptm <- proc.time()


# Set seed for reproducibility
set.seed(19022013)


# Preamble ----
library(MASS)
library(acebayes)
library(tidyverse)
library(GGally)
library(rjags)
library(grid)
library(gridExtra)
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
  
  # Define initial matrix
  p <- matrix(2 * (1 + alpha) * (rbeta(5*B, 2, 2) - 0.5),
              ncol = 5, 
              nrow = B)

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
ggplot2::ggsave(paste0("alpha_", 
                       as.character(round(100*alpha)),
                       "_original.png"))



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
ggplot2::ggsave(paste0("alpha_", 
                       as.character(round(100*alpha)),
                       "_modified.png"))



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





### Posterior distributions analysis #### --- --- ---

set.seed(19022013)

# Simulate betas
beta1 <- runif(1, 1, 1.5)
beta2 <- runif(1, -1.5, -1)
beta3 <- runif(1, 1, 1.5)
beta4 <- runif(1, -1.5, -1)
beta5 <- runif(1, 1, 1.5)
beta <- c(beta1, beta2, beta3, beta4, beta5)

# D-optimal model ###
# Simulate data
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


# Prepare data for JAGS model
x1 <- x_original[ , 1]
x2 <- x_original[ , 2]
x3 <- x_original[ , 3]
x4 <- x_original[ , 4]
x5 <- x_original[ , 5]
N <- 6

# Fit model and get posterior distributions w/JAGS
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
# Save samples
b1 <- original[[1]][, 1]
b2 <- original[[1]][, 2]
b3 <- original[[1]][, 3]
b4 <- original[[1]][, 4]
b5 <- original[[1]][, 5]


# Create tibble w/samples from posterior
posterior <- tibble(Diseño = rep("D-óptimo", 50000),
                    Beta = rep(c("Beta1", "Beta2",
                                 "Beta3", "Beta4",
                                 "Beta5"), each = 10000),
                    Valor = c(b1, b2,b3, b4, b5))




# SIL-optimal model ###
# Define data
x_mod <- matrix(c(-0.5, rep(1, 5),
                  -1, 0.56, rep(-1, 4),
                  rep(1, 2), -0.31, rep(1, 3),
                  rep(-1, 3), 0.33, rep(-1, 2),
                  rep(1, 4), -0.38, 1),
                ncol = 5)

# Simulate data
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


# Prepare data for JAGS model
x1 <- x_mod[ , 1]
x2 <- x_mod[ , 2]
x3 <- x_mod[ , 3]
x4 <- x_mod[ , 4]
x5 <- x_mod[ , 5]
N <- 6

# Fit model and get posterior distributions w/JAGS
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
# Save samples
b1 <- mod[[1]][, 1]
b2 <- mod[[1]][, 2]
b3 <- mod[[1]][, 3]
b4 <- mod[[1]][, 4]
b5 <- mod[[1]][, 5]


# Create tibble w/samples from posterior
posterior_mod <- tibble(Diseño = rep("SIL-óptimo", 50000),
                        Beta = rep(c("Beta1", "Beta2",
                                     "Beta3", "Beta4",
                                     "Beta5"), each = 10000),
                        Valor = c(b1, b2,b3, b4, b5))


posterior <- bind_rows(posterior, posterior_mod)




# Graph posteriors
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







# Define shared legend function
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
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


# Generate graph grid and save
graph_arrange <- grid_arrange_shared_legend(beta1_graph,
                                            beta2_graph, 
                                            beta3_graph, 
                                            beta4_graph, 
                                            beta5_graph,
                                            ncol=2, nrow=3)
ggsave(filename = "posterior_graphs.png",
       plot = graph_arrange,
       dpi = 300)



# Report time taken to run
proc.time() - ptm