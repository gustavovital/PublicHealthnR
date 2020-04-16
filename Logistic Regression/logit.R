# Script para estimações de modelos logit
# 
# Autor: Gustavo de Oliveira
# Data: 14/04/2020

source('Logistic Regression/analise_exploratoria.R')

# Estimação do modelo logit ----

m <- glm(dm ~ 1, family = binomial (link = logit))
summary(m)

exp(-1.7047)/(1 + exp(-1.7047))

table(m$y) # Isso é, estimamos num vetor com 60 yes e 330 no
60/(60 + 330)

# Estimando um logit: diabetes ~ genero ----

m <- glm(dm ~ gender, family = binomial (link = logit))
summary(m)

exp(m$coefficients)
# Estimando um logit: diabetes ~ idade ----

m <- glm(dm ~ age, family = binomial (link = logit))
summary(m)

exp(m$coefficients[1])
exp(m$coefficients)# prob de diabetes ao nascer

 # Analisando a linearidade ----

dm_by_age <- table(age, dm)
freq_table <- prop.table(dm_by_age, margin = 1)

prob <- freq_table[, 'yes']/freq_table[, 'no']
log_prob <- log(prob)

plot(rownames(freq_table), log_prob)

hist(age)

d <- density(age)
plot(d)

# logit - regressão multipla ----


m <- glm(dm ~ age + gender + imc, family = binomial (link = logit))
summary(m)

exp(m$coefficients)
exp(confint(m)) # intervalos de confiança

# diabetes ~ idade + colesterol + tipo de seguro ----

insurance <- g[, 'insurance']

model <- glm(dm ~ age + chol + insurance, family = binomial (link = logit))
summary(model)

exp(model$coefficients)
exp(confint(model))

# O R2de mcFadden é dado por = 1- lnL(Mfull)/lnL(modelo restrito)

null_model <- glm(dm ~ 1, family = binomial (link = logit))

R2 <- 1 - logLik(model)/logLik(null_model)
R2

# Estatistica C ----

install.packages('DescTools')
library('DescTools')

Cstat(model) # 0.7641725

# Hosmer-Lemeshow statistic and test: ----

install.packages("ResourceSelection") 
library(ResourceSelection) 

hl <- hoslem.test(x = model$y, y = fitted(model), g = 10)

# plot the observed vs expected number of cases for each of the 10 groups 
plot(hl$observed[, 'y1'], hl$expected[, 'yhat1'])

# plot the observed vs expected number of noncases for each of the 10 groups 
plot(hl$observed[,"y0"], hl$expected[,"yhat0"]) 

# plot observed vs. expected prevalence for each of the 10 groups 
plot(x = hl$observed[,"y1"]/(hl$observed[,"y1"]+hl$observed[,"y0"]), 
     y = hl$expected[,"yhat1"]/(hl$expected[,"yhat1"]+hl$expected[,"yhat0"])) 

# Hosmer and Lemeshow goodness of fit (GOF) test ----

install.packages("generalhoslem") 
library(generalhoslem) 

logitgof(obs = model$y, exp = fitted(model), g = 10) 

# anova ----

anova(model, test = 'Chisq')


