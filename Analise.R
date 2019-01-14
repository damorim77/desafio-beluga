
X <- read.csv(url("https://s3-sa-east-1.amazonaws.com/beluga-challenge/train.csv"), na.strings = "")


X$id <- factor(X$id)
X$feature_0 <- as.Date(X$feature_0)

summary(X)

#Teste Chi-Quadrado para detectar dependências entre variáveis

chisq.test( X$feature_1, X$target )
chisq.test( X$feature_2, X$target )
chisq.test( X$feature_3, X$target )
chisq.test( X$feature_4, X$target )
chisq.test( X$feature_5, X$target )
chisq.test( X$feature_6, X$target )
chisq.test( X$feature_7, X$target )
chisq.test( X$feature_8, X$target )
chisq.test( X$feature_9, X$target )
chisq.test( X$feature_10, X$target )
chisq.test( X$feature_11, X$target )
chisq.test( X$feature_12, X$target )

install.packages("rcompanion")

#Teste V de Kramer, correlação entre variáveis categóricas

require(rcompanion)

cramerV( X$feature_1, X$target )
cramerV( X$feature_2, X$target )
cramerV( X$feature_3, X$target )
cramerV( X$feature_4, X$target )
cramerV( X$feature_5, X$target )
cramerV( X$feature_6, X$target )
cramerV( X$feature_7, X$target )
cramerV( X$feature_8, X$target )
cramerV( X$feature_9, X$target )
cramerV( X$feature_10, X$target )
cramerV( X$feature_11, X$target )
cramerV( X$feature_12, X$target )
