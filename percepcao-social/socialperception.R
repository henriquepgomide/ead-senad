# Libraries ----
library(car) # Function Recode
library(psych) # Function Describe
library(mirt) # Function mirt IRT 

# Import data ----
## Var names
vars  <- read.csv("percepcaosocial_labels.csv")
varnames  <- names(vars); rm(vars)

## Dataframe
socialPer  <- read.csv("percepcaosocial.csv", col.names=varnames, na.strings=c(NA, "-")); rm(varnames)


## Recode Social Perception Scale 
for (i in 41:79){
  socialPer[,i]   <-  Recode(socialPer[,i], "'Concordo'=4 ; c('Concordo totalmente', 'Concordo Totalmente')=5 ; 'Discordo' = 2; c('Discordo totalmente','Discordo Totalmente') = 1;  'Nem discordo, nem concordo' = 3")                         
}

# Drop id variables
socialPer  <- subset(socialPer, select = -c(1,2,3,6,11,12,13,14,72))
write.csv(socialPer, "percepcaosocial_df.csv")

# Questions
questions  <- read.csv("percepcaosocial_questions.csv")
questionsLabels  <- as.vector(questions[1:39,]); rm(questions)

# Analysis----
## Import dataframe
socialPer  <- read.csv("percepcaosocial_df.csv")
## Summing scales to remove NA's
socialPer$scaleSum  <- rowSums(socialPer[,34:71])
## Subset completed observations and consented participation
socialPer  <- subset(socialPer, subset=socialPer$termo=="Sim" & socialPer$estado=="Finalizadas" & !is.na(socialPer$scaleSum))

# Demographics
## Age
### Clean data
idade  <- as.character(socialPer$idade)
idade[28]  <- "42"
socialPer$age  <- as.numeric(gsub("anos(.*)", "", idade)) 
describe(socialPer$age)

### Descriptives
summary(socialPer$age) # all
by(socialPer$age, socialPer$sexo, describe) #by sex

## Sex
cbind(round(prop.table(table(socialPer$sexo)),2))

## Degree
cbind(round(prop.table(table(socialPer$escolaridade)),2))

## Marital Staus
cbind(round(prop.table(table(socialPer$estadocivil)),2))

## Education
cbind(round(prop.table(table(socialPer$formacao)),2)) # Broken, needs manual recoding

## Ocupação
cbind(round(prop.table(table(socialPer$ocupacao)),2)) # Broken, needs manual recoding

## Time  working
timeWorking  <- as.character(socialPer$tempodeservico)
socialPer$timeWorking  <- as.numeric(gsub("anos(.*)", "", timeWorking)) 
describe(socialPer$timeWorking)

## Religion 
cbind(round(prop.table(table(socialPer$religiao)),2)) 

## Contact 
cbind(round(prop.table(table(socialPer$contato.tema)),2))  

## Deal with
cbind(round(prop.table(table(socialPer$lida.com)),2)) 

## Where deal with
cbind(round(prop.table(table(socialPer$onde.lida.com)),2))

### Others
table(socialPer$lida.com.outros)

# Scale analysis ---

# Full scale
fullScale  <- socialPer[,34:71]

# descriptives
describe(fullScale)

# alpha
cronbach  <- alpha(fullScale) # Cronbach's alpha = .87

# EFA ----
## All items ----

## KMO
KMO(fullScale) # KMO = .92

# Barlett test of homogeneity # OK
bartlett.test(fullScale) 

# Defining factors
fa.parallel(fullScale, fm="minres", fa="both", ylabel="Eigenvalues") # yields 2 factors

# Factor analysis using polychoric correlations
faAll <- fa.poly(fullScale, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faAll, digits=2, cut=0.3)

# Diagram
fa.diagram(faAll)

# RESULTADOS #
# Sem fator  9, 13, 25, 32
# Dois fatores e com cargas baixas  18, 36
# MR1  : 1,2,3,4,5,6,7,8,10,15,17,20,22,30,
# MR2  : 11,12,-14,16,-19,21,23,24,26,27,-28,-29,-31,-33,-34,-35, 37,38,39

# Recode negative items
for (i in c(14,19,28,29,31,33,34,35)){
    fullScale[,i]   <-  Recode(fullScale[,i], "5=1 ; 4=2 ; 3 = 3; 2 = 4; 1 = 5; else = NA")                         
}

fullScale[1:5,c(31,33,34,35)]


# Revista de validação de São Francisco
# a fazer -  Inverter itens da escala
# Confirmatoria - Não implementado ainda.
cfa <- bfactor(fullScale)

