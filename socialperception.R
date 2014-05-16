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

str(socialPer[,24:65])

# Defining factors
fa.parallel(socialPer[,24:65], fm="pa", fa="pc", ylabel="Eigenvalues", show.legend=FALSE) # yields 4 components
VSS(socialPer[,24:65], rotate="none") # VSS = 3 factors MAP = 4 components

# Principal components analysis
pca <- fa.poly(socialPer[,24:65], nfactors = 4, rotate = "oblimin", fm="pa")

# Observing loadings
print.psych(pca, cut = 0.3, sort = FALSE)
