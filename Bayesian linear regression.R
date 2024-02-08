#---------------------------------------------------------
# Bayesian linear regression (with inla)
#---------------------------------------------------------

library(tidyverse)
data(cars)

# 0.1 Split the dataset into train and test sets
set.seed(2024)
sample = sample(c(TRUE, FALSE), nrow(cars), replace=T, prob=c(0.7, 0.3))
#Split your data into training (70%) and test (30%) sets
train = cars[sample, ]
test = cars[!sample, ]

# 0.2 Check Normality of the response

ggplot(cars, aes(x = dist)) +
  geom_histogram(aes(y = ..density..), binwidth = 5,  colour = 1, fill = "white") +
  geom_density() +
  labs(title = 'Histogram with overlying density of the response',
       subtitle = 'Cars data set',
       y="density", x="speed (mph)") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 1.1 Create frequentist linear model, log linear model and plot
freq.lin.mod = lm(dist~speed, data=train)
freq.log.lin.mod = lm(log(dist)~speed, data = train)

summary(freq.lin.mod)
summary(freq.log.lin.mod)

# linear model
plot1 = train %>% 
  ggplot(aes(speed, dist)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(se = FALSE, color="blue") +
  labs(title = 'Plot of observations and the fitted Linear model',
       subtitle = 'Cars train data set',
       y="distance", x="speed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

plot1

# log-linear model
plot2 = train %>% 
  ggplot(aes(speed, log(dist))) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(se = FALSE, color="blue") +
  labs(title = 'Plot of observations and the fitted Log-Linear model',
       subtitle = 'Cars train data set',
       y="log(distance)", x="speed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

plot2

# 1.2 Summary of estimates

# table of results
library(xtable)
library(stargazer)

setwd("C:/Users/julia/OneDrive/Desktop/github/64. Bayesian Linear Regression")

# print results to LaTeX
stargazer(freq.lin.mod, title="Linear model", align=TRUE, single.row = TRUE, olumn.sep.width = "1pt")
stargazer(freq.log.lin.mod, title="Log-Linear model", align=TRUE, single.row = TRUE, olumn.sep.width = "1pt")


# 1.3 Add predictions, residuals and plot the residuals
train2 = data.frame(train, estimate = predict(freq.lin.mod), 
                    residuals = residuals(freq.lin.mod))

plot3 = train2 %>% 
  ggplot(aes(speed, dist)) +
  geom_point(aes(size = abs(residuals))) +
  geom_smooth(method = "lm")+
  geom_smooth(se = FALSE, color="blue")+
  geom_segment(aes(xend = speed, yend = estimate), color="red") +
  labs(title = 'Plot of the residuals and their distance to the fitted Linear model',
       subtitle = 'Cars train data set',
       y="distance", x="speed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

plot3

train3 = data.frame(train, estimate = predict(freq.log.lin.mod), 
                    residuals = residuals(freq.log.lin.mod))

plot4 = train3 %>% 
  ggplot(aes(speed, log(dist))) +
  geom_point(aes(size = abs(residuals))) +
  geom_smooth(method = "lm")+
  geom_smooth(se = FALSE, color="blue")+
  geom_segment(aes(xend = speed, yend = estimate), color="red") +
  labs(title = 'Plot of the residuals and their distance to the fitted Log-Linear model',
       subtitle = 'Cars train data set',
       y="log(distance)", x="speed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

plot4

# 2.1 Bayesian modeling

cars$dist %>% summary() # centered around 45 with sd = 30

qnorm(c(.25, .75), mean = 45, sd = 30)
# [1] 24.76531 65.23469 # similar to our data, prior on Beta0, informative prior
# prior on sigma N(0, 1000)


# 2.2 Define the likelihood, posterior and proposal function
library(brms)
set.seed(2024)

# Linear  model
bay.lin.mod = brm(dist ~ speed, data = train, family="gaussian") # reference non-informative priors

# Linear model 2
bay.lin.mod.2 = brm(dist ~ speed,
                   data = train, family = gaussian(),
                   prior = c(
                     prior(normal(45, 30), class = Intercept),
                     prior(normal(0, 1000), class = sigma),
                     prior(normal(0, 100), class = b)))
# Linear  model
bay.log.lin.mod = brm(log(dist) ~ speed, data = train, family="gaussian")

plot5 = plot(bay.lin.mod)
plot5

plot6 = plot(bay.log.lin.mod)
plot6

# 2.3 Plot the fitted and prediction interval

train %>%
  ggplot(aes(x = speed,
             y = dist)) +
  geom_point(position = "jitter",
             alpha    = .8)+ #to add some random noise for plotting purposes
  geom_smooth(method = "lm",  # to add  the linear relationship
              se = TRUE) +
  geom_smooth(se = FALSE, color="blue") +
  labs(title = 'Plot of the observations and the fitted Bayesian model',
       subtitle = 'Cars train data set',
       y="distance", x="speed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


# Prediction intervals

BayModel =  data.frame(
  Model=factor(c(rep("Linear model", length(train$speed)), 
                 rep("Log-Linear model", length(train$speed))),
               levels=c("Linear model", 
                        "Log-Linear model"), 
               ordered = TRUE),
  Speed=rep(train$speed, 2),
  Distance=c(train$dist, log(train$dist)),
  rbind(predict(bay.lin.mod),
        predict(bay.log.lin.mod)
  ))


# Plot using ggplot2 with facet_wrap
ggplot(BayModel, aes(x = Speed, y = Distance)) +
  geom_point(aes(x = Speed, y = Distance)) +
  geom_smooth(method = "lm", se=FALSE) +
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5), alpha=0.1, fill = "grey50",  
              color = "black", linetype = "dotted") +
  facet_wrap(~Model, scales = "free") +
  labs(title = 'Scatterplots, Regression lines and Prediction intervals',
       subtitle = 'Cars train data set',
       y="Speed", x="Distance") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


#----


#-----





# 3,1 Bayesian linear regression with INLA
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE) 
library(INLA)

inla.model <- inla(dist~speed, data=train, family = "gaussian",
                   control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE))
summary(inla.model)

# Fixed effects:
#              mean    sd     0.025quant 0.5quant 0.975quant  mode   kld
# (Intercept)  16.49   5.698      5.227     16.49     27.745  16.49   0
# x            1.25    0.321      0.615     1.25      1.884   1.25    0


# plot of the marginal posterior densities for the parameters
library(dplyr)
data.inla.fixed <- reshape2:::melt(inla.model$marginals.fixed) %>%
  reshape2:::dcast(L1+Var1~Var2, value='value')
ggplot(data.inla.fixed, aes(y=y, x=x)) + geom_line() +
  facet_wrap(~L1, scales='free', nrow=1) +
  labs(title = 'Time series plot',
       subtitle = 'Tesla stock',
       y="Adjusted closing price", x="time") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

ggplot(data = data.inla.fixed, aes(y=y, x=x)) +
  geom_line() +
  facet_wrap(~L1, scales='free', nrow=1) +
  labs(title = 'Marginal posterior densities for the intercept and the slope',
       subtitle = 'Tesla stock',
       y="Adjusted closing price", x="time") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# frequentist linear regression
mod2 <- lm(y~x)
summary(mod2)


# plot of the regression line with confidence region
dataplot <- data.frame(x=seq(min(data$x, na.rm=TRUE), max(data$x, na.rm=TRUE), 
                            length=100), y=NA)
data.pred <- rbind(data, dataplot)

# regression with INLA
data.inla <- inla(y~x, data=data.pred, family = "gaussian",
                   control.predictor=list(compute=TRUE))
summary(data.inla)

dataplot <- cbind(dataplot, 
                 data.inla$summary.linear.predictor[(nrow(data)+1):nrow(data.pred),])

# rename columns dataframe for ploting 
dataplot <- dataplot %>%
  rename('l'='0.025quant', 'u'='0.975quant')

# plot of the regression line with confidence region
ggplot(dataplot, aes(y=mean, x=x)) + geom_point(data=data, aes(y=y),shape=18) +
  geom_ribbon(aes(ymin=l, ymax=u), fill='red', alpha=0.4) +
  ggtitle("Bayesian Linear Regression and credibility bounds with INLA") +
  labs(y= "Posterior mean", x = "x") +
  geom_line() +
  theme_minimal()
























# plot of the marginal posterior densities for the parameters
library(tidyverse)
library(reshape2)
library(reshape)
data.mod1 <- melt(data.inla$marginals.fixed) %>%
  dcast(L1+Var1~Var2, value='value')

data.mod1.est <- data.frame(beta= c(mod1$summary.fixed[1,1],
                                    data.inla$summary.fixed[2,1]))

ggplot(data = data.mod1, aes(y=y, x=x)) +
  geom_line() +
  facet_wrap(~L1, scales='free', nrow=1) +
  ggtitle("Marginal posterior densities for the intercept and the slope") +
  theme_minimal()









































set.seed(2021)
n <- 50
lambda <- 0.5
data <- rexp(n = n, rate = lambda)
lambda_mle <- 1/mean(data)
lambda_mle
# 0.5643369

# 95% asymptotic C.I.
lower_bound <-  lambda_mle * (1 - ( qnorm(1-(0.05/2), mean=0, sd=1)) / sqrt(n))
upper_bound <-  lambda_mle * (1 + ( qnorm(1-(0.05/2), mean=0, sd=1)) / sqrt(n))
cbind(lower_bound, upper_bound)
# 0.4079136   0.7207603

set.seed(2021)
n <- 5000
lambda <- 0.5
data <- rexp(n = n, rate = lambda)
lambda_mle <- 1/mean(data)
lambda_mle
# [1] 0.5084782

# 95% asymptotic C.I.
lower_bound <-  lambda_mle * (1 - ( qnorm(1-(0.05/2), mean=0, sd=1)) / sqrt(n))
upper_bound <-  lambda_mle * (1 + ( qnorm(1-(0.05/2), mean=0, sd=1)) / sqrt(n))
cbind(lower_bound, upper_bound)
# 0.4943842   0.5225723




