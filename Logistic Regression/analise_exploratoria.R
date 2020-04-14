# Script para analise exploratória dos dados. Dados de saude pública de Londres
# 
# Autor: Gustavo Vital
# Data: 13/04/2020

# Base de dados ----

g <- read.csv('Logistic Regression/data.csv', header = TRUE, sep = ',')

# Analise exploratória ----

dim(g) # 403 obs, 24 variaveis
colnames(g)

# Sobre as dummys ----

# insurance: 0=none, 1=government, 2=private
# fh = family history of diabetes (yes/no, where 1=yes, 0=no)
# smoking: 1=current, 2=never and 3=ex

# Criando variaveis ----

chol <- g[,'chol']
gender <- as.factor(g[, 'gender'])
dm <- as.factor(g[, 'dm'])

height <- g[,'height']
weight <- g[,'weight']

# Analisando ----

t <- table(gender)
addmargins(t) # add a soma total na tabela

round(prop.table(t), digits = 3) # tabela de porcentagem
                                 # 58% da amostra analisada são mulheres

summary(chol) # est de colesterol
dm2 <- factor(dm, exclude=NULL) # diabetes?

# Calculando o IMC ----

height.si <- height*0.0254   # Convertendo para kg e metro..*
weight.si <- weight*0.453592

imc <- weight.si/height.si^2
summary(imc)

# Criando uma categoria para IMC ----

imc_categorised <- ifelse(imc < 18.5, "underweight", 
                          ifelse(imc >= 18.5 & imc <= 25, "normal", 
                                 ifelse(imc > 25 & imc <= 30, "overweight", 
                                        ifelse(imc > 30, "obese", NA))))

table(imc_categorised, exclude = NULL)

# Comparando o IMC com diabetes: ----

diabetes_imc <- table(imc_categorised, dm2, exclude = NULL)
diabetes_imc

# Comparando com as porcentagens ----

diabetes_imc_perc <- round(100*prop.table(diabetes_imc, margin = 1), digits = 2)
diabetes_imc_perc

#margin = 1 nos da a porcentagem pela linha, e nao pela coluna.

# * dicionário: http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/Cdiabetes.html

# Outras informações: ----

age <- g[, 'age']
summary(gender)

age_categories <- ifelse(age < 45, '< 45',
                         ifelse(age >= 45 & age < 65, '[45-65)',
                                ifelse(age >= 65 & age < 75, '[65-75)', '> 75')))


table(age_categories, gender)
round(100*prop.table(table(age_categories, gender), margin = 1), digits = 2)
