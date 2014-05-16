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
## Subset completed observations and consented participation
rowsum(socialPer[,24:64])
socialPer  <- subset(socialPer, subset=socialPer$termo=="Sim" & socialPer$estado=="Finalizadas")

# descriptives
describe(socialPer[,24:64])
sapply(socialPer[,24:64], summary)

# correlations
round(cor(socialPer[,24:64], method="kendal", use="complete.obs"),2) # kendall correlation coef
cor.plot(cor(socialPer[,24:64], method="kendal", use="complete.obs"), numbers= TRUE)

# alpha
alpha(socialPer[,24:64])

# EFA

## KMO
# KMO Kaiser-Meyer-Olkin Measure of Sampling Adequacy - Function by G. Jay Kerns, Ph.D., Youngstown State University (http://tolstoy.newcastle.edu.au/R/e2/help/07/08/22816.html)
kmo = function( data ){
  library(MASS) 
  X <- cor(as.matrix(data)) 
  iX <- ginv(X) 
  S2 <- diag(diag((iX^-1)))
  AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
  IS <- X+AIS-2*S2                         # image covariance matrix
  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a) 
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b/(b+a)                        # indiv. measures of sampling adequacy
  AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the correlation matrix. That is the  negative of the partial correlations, partialling out all other variables.
  kmo <- BB/(AA+BB)                     # overall KMO statistic
  # Reporting the conclusion 
  if (kmo >= 0.00 && kmo < 0.50){test <- 'The KMO test yields a degree of common variance unacceptable for FA.'} 
  else if (kmo >= 0.50 && kmo < 0.60){test <- 'The KMO test yields a degree of common variance miserable.'} 
  else if (kmo >= 0.60 && kmo < 0.70){test <- 'The KMO test yields a degree of common variance mediocre.'} 
  else if (kmo >= 0.70 && kmo < 0.80){test <- 'The KMO test yields a degree of common variance middling.' } 
  else if (kmo >= 0.80 && kmo < 0.90){test <- 'The KMO test yields a degree of common variance meritorious.' }
  else { test <- 'The KMO test yields a degree of common variance marvelous.' }
  
  ans <- list( overall = kmo,
               report = test,
               individual = MSA,
               AIS = AIS,
               AIR = AIR )
  return(ans)
}
kmo(socialPer[,24:64])

# Barlett test of homogeneity
bartlett.test(socialPer[,24:64])

# Defining factors
fa.parallel(socialPer[,24:64], fm="pa") # yields 4 components
VSS(socialPer[,24:64], rotate="none") # VSS = 3 factors MAP = 4 components

# Principal components analysis
pca <- principal(rcqValid[34:45], nfactors = 2, rotate = "varimax")

# Observing loadings
print.psych(pca, cut = 0.4, sort = FALSE) # data revealed that item 5 was not related with any factors. As a possible solution, we decided to remove 1 item for each subscale. Therefore, we removed 3 items: 2,5,8.



# Revista para enviar - implementations science