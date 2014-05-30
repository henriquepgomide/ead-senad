# Libraries ----
library(car) # Function Recode
library(psych) # Function Describe

# Import data ----

## Var names
vars  <- read.csv("percepcaosocial_labels.csv")
varnames  <- names(vars); rm(vars)

## Dataframe
socialPer  <- read.csv("percepcaosocial.csv", col.names=varnames, na.strings=c(NA, "-")); rm(varnames)

## Recode Social Perception Scale 
for (i in 30:71){
  socialPer[,i]   <-  Recode(socialPer[,i], "'Concordo'=4 ; c('Concordo totalmente', 'Concordo Totalmente')=5 ; 'Discordo' = 2; c('Discordo totalmente','Discordo Totalmente') = 1;  'Nem discordo, nem concordo' = 3")                         
}

# Drop id variables
socialPer  <- subset(socialPer, select = -c(1,2,3,6,11,12,13,72))
write.csv(socialPer, "percepcaosocial_df.csv")

# Questions
questions  <- read.csv("percepcaosocial_questions.csv")

# Analysis----

## Import dataframe
socialPer  <- read.csv("percepcaosocial_df.csv")
## Summing scales to remove NA's
socialPer$scaleSum  <- rowSums(socialPer[,24:65])
## Subset completed observations and consented participation
socialPer  <- subset(socialPer, subset=socialPer$termo=="Sim" & socialPer$estado=="Finalizadas" & !is.na(socialPer$scaleSum))

# Demographics
## Age

### Clean data
idade  <- as.character(socialPer$idade)
idade[24]  <- "42"
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
# descriptives
describe(socialPer[,24:65])

# correlations
round(cor(socialPer[,24:65], method="kendal", use="complete.obs"),2) # kendall correlation coef
cor.plot(cor(socialPer[,24:65], method="kendal", use="complete.obs"), numbers= TRUE)

# alpha
cronbach  <- alpha(socialPer[,24:65])

# EFA ----

## KMO
KMO(socialPer[,24:65])

# Barlett test of homogeneity
bartlett.test(socialPer[,24:65])

# Defining factors
fa.parallel(socialPer[,24:65], fm="minres", fa="both", ylabel="Eigenvalues") # yields 4 components
VSS(socialPer[,24:65], rotate="none") # VSS = 3 factors MAP = 4 components

# Principal components analysis
pca <- fa.poly(socialPer[,24:65], nfactors = 2, rotate = "none", fm="minres")
print.psych(pca, digits=2, cut=0.3)

# Diagrama
fa.diagram(pca)


# Fazer EFA com rotação e sem rotação - tirando os itens 10,20,24.
# Ver itens que estão polarizando nas respostas pela escala
# REvista de validação de São Francisco

