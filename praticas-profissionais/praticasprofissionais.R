# Libraries ----
library(car) # Function Recode
library(psych) # Function Describe

# Import data ----

## Var names
vars  <- read.csv("praticasprofissionais_labels.csv", sep=",")
varnames  <- names(vars); rm(vars)

## Dataframe
praticasPro  <- read.csv("praticasprofissionais.csv", col.names=varnames, na.strings=c(NA, "-")); rm(varnames)

## Recode Social Perception Scale 
for (i in 30:72){
  praticasPro[,i]   <-  Recode(praticasPro[,i], "'Concordo'=4 ; c('Concordo totalmente', 'Concordo Totalmente')=5 ; 'Discordo' = 2; c('Discordo totalmente','Discordo Totalmente') = 1;  'Nem discordo, nem concordo' = 3")                         
}

praticasPro  <- subset(praticasPro, select = -c(1,2,3,4,5,6,11,12,13,14))
write.csv(praticasPro, "praticasprofissionais_df.csv")

# Questions - not implemented yet.
# questions  <- read.csv("percepcaosocial_questions.csv")

# Analysis----

## Import dataframe
praticasPro  <- read.csv("praticasprofissionais_df.csv")

## Summing scales to remove NA's
praticasPro$scaleSum  <- rowSums(praticasPro[,21:63])
## Subset completed observations and consented participation
praticasPro  <- subset(praticasPro, subset=praticasPro$termo=="Sim" & praticasPro$estado=="Finalizadas" & !is.na(praticasPro$scaleSum))

# Demographics

## Age

### Clean data

idade  <- as.character(praticasPro$idade)
idade[9]  <- "35"; idade[44] <- "29"; idade[69]  <- "31"; idade[111]   <-  42;
praticasPro$age  <- as.numeric(gsub("anos(.*)", "", idade))

### Descriptives
summary(praticasPro$age) # all
by(praticasPro$age, praticasPro$sexo, describe) #by sex

## Sex
cbind(round(prop.table(table(praticasPro$sexo)),2))

## Degree
cbind(round(prop.table(table(praticasPro$escolaridade)),2))

## Marital Staus
cbind(round(prop.table(table(praticasPro$estadocivil)),2))

## Education
cbind(round(prop.table(table(praticasPro$formacao)),2)) # Broken, needs manual recoding

## Ocupação
cbind(round(prop.table(table(praticasPro$ocupacao)),2)) # Broken, needs manual recoding

## Time  working
timeWorking  <- as.character(praticasPro$tempo.atuacao)
praticasPro$timeWorking  <- as.numeric(gsub("anos(.*)", "", timeWorking)) 
describe(praticasPro$timeWorking)

## Religion 
cbind(round(prop.table(table(praticasPro$religiao)),2)) 

## Contact 
cbind(round(prop.table(table(praticasPro$contato.tema)),2))  

## Deal with
cbind(round(prop.table(table(praticasPro$lida.com)),2)) 

## Where deal with
cbind(round(prop.table(table(praticasPro$onde.lida.com)),2))

# Scale analysis ---

# Full scale
fullScale  <- praticasPro[,21:63]

# descriptives
describe(fullScale)

# correlations
round(cor(fullScale, method="kendal", use="complete.obs"),2) # kendall correlation coef
cor.plot(cor(fullScale, method="kendal", use="complete.obs"), numbers= TRUE)

# alpha
cronbach  <- alpha(fullScale)
cronbach

# EFA ----

## All items ----

## KMO
KMO(fullScale)

# Barlett test of homogeneity
bartlett.test(fullScale)

# Defining factors
auto  <- fa.parallel(fullScale, fm="minres", fa="both", ylabel="Eigenvalues") # yields 4 components and 4 factors
VSS(fullScale, rotate="none") # VSS = 2; MAP = 4 factors

# Factor Analysis using polychoric correlations
faAll <- fa.poly(fullScale, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faAll, digits=2, cut= .3)


## V1 - Items with good loadings ----
# V1 - Version
v1Scale  <- subset(fullScale, select = -c(3,8,15,31,36,41))

# Factor analysis using polychoric correlations
fav1 <- fa.poly(v1Scale, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(fav1, digits=2, cut=0.3)

# Diagram
fa.diagram(fav1)

