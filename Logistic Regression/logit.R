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

exp(m$coefficients[1]) # prob de diabetes ao nascer

 # Analisando a linearidade ----

dm_by_age <- table(age, dm)
freq_table <- prop.table(dm_by_age, margin = 1)

prob <- freq_table[, 'yes']/freq_table[, 'no']
log_prob <- log(prob)

plot(rownames(freq_table), log_prob)
