
X <- read.csv(url("https://s3-sa-east-1.amazonaws.com/beluga-challenge/train.csv"), na.strings="" )

Y <- read.csv(url("https://s3-sa-east-1.amazonaws.com/beluga-challenge/test.csv"), na.strings="" )

X$id <- factor(X$id)
X$target <- factor(X$target)
X$feature_0 <- as.Date(X$feature_0)

Y$id <- factor(Y$id)
Y$feature_0 <- as.Date(Y$feature_0)


if(! require(sqldf)) { 
  install.packages("sqldf") 
  library(sqldf)
}

do.analysis <- function () {
  
  cat("\014")
  
  # Análise exploratória univariada
  
  for (i in names(X))
  {
    print( summary(X[i])  )
    print('')
  }
  
  # Níveis das variáveis categóricas
  
  length(levels(X$feature_1))
  length(levels(X$feature_2))
  length(levels(X$feature_3))
  length(levels(X$feature_4))
  length(levels(X$feature_5))
  length(levels(X$feature_6))
  length(levels(X$feature_7))
  length(levels(X$feature_8))
  length(levels(X$feature_9))
  length(levels(X$feature_10))
  length(levels(X$feature_11))
  length(levels(X$feature_12))
  
  # Contagens das top 20 categorias pra cada variável
  
  print( sqldf("SELECT feature_1, count(1) count FROM X GROUP BY feature_1 ORDER BY count(1) DESC LIMIT 20")   )
  print( sqldf("SELECT feature_2, count(1) count FROM X GROUP BY feature_2 ORDER BY count(1) DESC LIMIT 20")   )
  print( sqldf("SELECT feature_3, count(1) count FROM X GROUP BY feature_3 ORDER BY count(1) DESC LIMIT 20")   )
  print( sqldf("SELECT feature_4, count(1) count FROM X GROUP BY feature_4 ORDER BY count(1) DESC LIMIT 20")   )
  print( sqldf("SELECT feature_5, count(1) count FROM X GROUP BY feature_5 ORDER BY count(1) DESC LIMIT 20")   )
  print( sqldf("SELECT feature_6, count(1) count FROM X GROUP BY feature_6 ORDER BY count(1) DESC LIMIT 20")   )
  print( sqldf("SELECT feature_7, count(1) count FROM X GROUP BY feature_7 ORDER BY count(1) DESC LIMIT 20")   )
  print( sqldf("SELECT feature_8, count(1) count FROM X GROUP BY feature_8 ORDER BY count(1) DESC LIMIT 20")   )
  print( sqldf("SELECT feature_9, count(1) count FROM X GROUP BY feature_9 ORDER BY count(1) DESC LIMIT 20")   )
  print( sqldf("SELECT feature_10, count(1) count FROM X GROUP BY feature_10 ORDER BY count(1) DESC LIMIT 20") )
  print( sqldf("SELECT feature_11, count(1) count FROM X GROUP BY feature_11 ORDER BY count(1) DESC LIMIT 20") )
  print( sqldf("SELECT feature_12, count(1) count FROM X GROUP BY feature_12 ORDER BY count(1) DESC LIMIT 20") )  
  
  # Analise exploratória bivariada
  
  # Analisando diferenças significantes entre as contagens de cada feature para os grupos target e não-target
  
  tb1 <- table( X$feature_1 , X$target)
  tb2 <- table( X$feature_2 , X$target)
  tb3 <- table( X$feature_3 , X$target)
  tb4 <- table( X$feature_4 , X$target)
  tb5 <- table( X$feature_5 , X$target)
  tb6 <- table( X$feature_6 , X$target)
  tb7 <- table( X$feature_7 , X$target)
  tb8 <- table( X$feature_8 , X$target)
  tb9 <- table( X$feature_9 , X$target)
  tb10 <- table( X$feature_10 , X$target)
  tb11 <- table( X$feature_11 , X$target)
  tb12 <- table( X$feature_12 , X$target)
  
  # Teste de Kruskal-Wallis - diferenças nas médias das contagens
  
  print( 	  kruskal.test( tb1[,2] ~ tb1[,1],  data  = tb1 )   	 )
  print( 	  kruskal.test( tb2[,2] ~ tb2[,1],  data  = tb2 )   	 )
  print( 	  kruskal.test( tb3[,2] ~ tb3[,1],  data  = tb3 )   	 )
  print( 	  kruskal.test( tb4[,2] ~ tb4[,1],  data  = tb4 )   	 )
  print( 	  kruskal.test( tb5[,2] ~ tb5[,1],  data  = tb5 )   	 )
  print( 	  kruskal.test( tb6[,2] ~ tb6[,1],  data  = tb6 )   	 )
  print( 	  kruskal.test( tb7[,2] ~ tb7[,1],  data  = tb7 )   	 )
  print( 	  kruskal.test( tb8[,2] ~ tb8[,1],  data  = tb8 )   	 )
  print( 	  kruskal.test( tb9[,2] ~ tb9[,1],  data  = tb9 )   	 )
  print( 	  kruskal.test( tb10[,2] ~ tb10[,1], data  = tb10 )	   )
  print( 	  kruskal.test( tb11[,2] ~ tb11[,1], data  = tb11 )	   )
  print( 	  kruskal.test( tb12[,2] ~ tb12[,1], data  = tb12 )	   )
  
  # Teste Chi-Quadrado - detectar dependências entre variáveis
  
  print( 	  chisq.test( X$feature_1, X$target )	 )
  print( 	  chisq.test( X$feature_2, X$target )	 )
  print( 	  chisq.test( X$feature_3, X$target )	 )
  print( 	  chisq.test( X$feature_4, X$target )	 )
  print( 	  chisq.test( X$feature_5, X$target )	 )
  print( 	  chisq.test( X$feature_6, X$target )	 )
  print( 	  chisq.test( X$feature_7, X$target )	 )
  print( 	  chisq.test( X$feature_8, X$target )	 )
  print( 	  chisq.test( X$feature_9, X$target )	 )
  print( 	  chisq.test( X$feature_10, X$target )	 )
  print( 	  chisq.test( X$feature_11, X$target )	 )
  print( 	  chisq.test( X$feature_12, X$target )	 )
  
  # Teste V de Kramer -> correlação entre variáveis categóricas
  
  if(! require(rcompanion)) { 
    install.packages("rcompanion") 
    library(rcompanion)
  }
  
  print( 	  cramerV( table( X$feature_1, X$target ))	 )
  print( 	  cramerV( table( X$feature_2, X$target ))	 )
  print( 	  cramerV( table( X$feature_3, X$target ))	 )
  print( 	  cramerV( table( X$feature_4, X$target ))	 )
  print( 	  cramerV( table( X$feature_5, X$target ))	 )
  print( 	  cramerV( table( X$feature_6, X$target ))	 )
  print( 	  cramerV( table( X$feature_7, X$target ))	 )
  print( 	  cramerV( table( X$feature_8, X$target ))	 )
  print( 	  cramerV( table( X$feature_9, X$target ))	 )
  print( 	  cramerV( table( X$feature_10, X$target ))	 )
  print( 	  cramerV( table( X$feature_11, X$target ))	 )
  print( 	  cramerV( table( X$feature_12, X$target ))	 )
  
  ## end do.analysis
}

do.analysis()

do.fix <- function(){
  
  # Preenche missing values com o valor mais frequente por coluna
  for( c in c('5','8','9','10','11','12')  )
  {
    col_name <- paste("feature_",c,sep="") 
    sql <- paste("SELECT ",col_name," FROM X WHERE ",col_name," <> '' GROUP BY ",col_name," ORDER BY count(1) DESC LIMIT 1 ")
    
    X[is.na(X[col_name]),][col_name] <- sqldf(sql)[col_name][1]
    
  }
  
  ## Redução do número de categorias - preservando somente as 20 maiores para cada variável
  
  
  top20f4 <- data.frame(sqldf("SELECT feature_4, count(1) count FROM X GROUP BY feature_4 ORDER BY count(1) DESC LIMIT 20"))
  top20f5 <- data.frame(sqldf("SELECT feature_5, count(1) count FROM X GROUP BY feature_5 ORDER BY count(1) DESC LIMIT 20"))
  top20f7 <- data.frame(sqldf("SELECT feature_7, count(1) count FROM X GROUP BY feature_7 ORDER BY count(1) DESC LIMIT 20"))
  
  sql= "
  SELECT X.id
  , X.feature_0
  , X.target
  , X.feature_1
  , case when X.feature_2 is null then '' else X.feature_2 end feature_2
  , X.feature_3
  , case when top20f4.feature_4 is null then '' else top20f4.feature_4 end feature_4
  , case when top20f5.feature_5 is null then '' else top20f5.feature_5 end feature_5
  , X.feature_6
  , case when top20f7.feature_7 is null then '' else top20f7.feature_7 end feature_7
  , case when X.feature_8 is null then '' else X.feature_8 end feature_8 
  , case when X.feature_9 is null then '' else X.feature_9 end feature_9
  , case when X.feature_10 is null then '' else X.feature_10 end feature_10
  , case when X.feature_11 is null then '' else X.feature_11 end feature_11
  , case when X.feature_12 is null then '' else X.feature_12 end feature_12
  FROM X
  LEFT JOIN top20f4 ON top20f4.feature_4 = X.feature_4
  LEFT JOIN top20f5 ON top20f5.feature_5 = X.feature_5
  LEFT JOIN top20f7 ON top20f7.feature_7 = X.feature_7
  "
  
  X <- sqldf(sql, stringsAsFactors= TRUE)
  
  X$feature_4  <- factor(X$feature_4 )
  X$feature_5  <- factor(X$feature_5 )
  X$feature_7  <- factor(X$feature_7 )
  X$feature_8  <- factor(X$feature_8 )
  X$feature_9  <- factor(X$feature_9 )
  X$feature_10 <- factor(X$feature_10)
  X$feature_11 <- factor(X$feature_11)
  X$feature_12 <- factor(X$feature_12)
  
  rm(top20f4)
  rm(top20f5)
  rm(top20f7)
  rm(sql)
  
  
  
  # Equalização dos níveis das variáveis categóricas em ambos datasets
  levels(Y$feature_1) <- union( levels(X$feature_1), levels(Y$feature_1)  )
  levels(Y$feature_2) <- union( levels(X$feature_2), levels(Y$feature_2)  )
  levels(Y$feature_3) <- union( levels(X$feature_3), levels(Y$feature_3)  )
  levels(Y$feature_4) <- union( levels(X$feature_4), levels(Y$feature_4)  )
  levels(Y$feature_5) <- union( levels(X$feature_5), levels(Y$feature_5)  )
  levels(Y$feature_6) <- union( levels(X$feature_6), levels(Y$feature_6)  )
  levels(Y$feature_7) <- union( levels(X$feature_7), levels(Y$feature_7)  )
  levels(Y$feature_8) <- union( levels(X$feature_8), levels(Y$feature_8)  )
  levels(Y$feature_9) <- union( levels(X$feature_9), levels(Y$feature_9)  )
  levels(Y$feature_10) <- union( levels(X$feature_10), levels(Y$feature_10)  )
  levels(Y$feature_11) <- union( levels(X$feature_11), levels(Y$feature_11)  )
  levels(Y$feature_12) <- union( levels(X$feature_12), levels(Y$feature_12)  )
  
  levels(X$feature_1) <- union( levels(X$feature_1), levels(Y$feature_1)  )
  levels(X$feature_2) <- union( levels(X$feature_2), levels(Y$feature_2)  )
  levels(X$feature_3) <- union( levels(X$feature_3), levels(Y$feature_3)  )
  levels(X$feature_4) <- union( levels(X$feature_4), levels(Y$feature_4)  )
  levels(X$feature_5) <- union( levels(X$feature_5), levels(Y$feature_5)  )
  levels(X$feature_6) <- union( levels(X$feature_6), levels(Y$feature_6)  )
  levels(X$feature_7) <- union( levels(X$feature_7), levels(Y$feature_7)  )
  levels(X$feature_8) <- union( levels(X$feature_8), levels(Y$feature_8)  )
  levels(X$feature_9) <- union( levels(X$feature_9), levels(Y$feature_9)  )
  levels(X$feature_10) <- union( levels(X$feature_10), levels(Y$feature_10)  )
  levels(X$feature_11) <- union( levels(X$feature_11), levels(Y$feature_11)  )
  levels(X$feature_12) <- union( levels(X$feature_12), levels(Y$feature_12)  )
  
  
  
}

do.fix()

# Modelo : Decision tree

if(! require(party)) { 
  install.packages("party") 
  library(party)
}


cat("\014")
set.seed(3.14)
vn <- sample(NROW(X))
train <- X[ vn[1:round(0.6*NROW(X),0)], ]

mod <- ctree( target ~  feature_1 + feature_7 + feature_2 + feature_11  , data = train  )

prd <- predict(mod, newdata = Y)

table(prd,train$target)

NROW(train)
length(prd)

sqldf("SELECT ")




if(! require(MASS)) { 
  install.packages("MASS") 
  library(MASS)
}


# Linear discriminant analysis

cat("\014")
set.seed(3.14)
vn <- sample(NROW(X))
train <- X[ vn[1:round(0.2*NROW(X),0)], ]

mod <- lda( target ~  feature_1 + feature_2, data = train  )

prd <- predict(mod)

table(prd$class,train$target)


# Support Vector Machine

if(! require(e1071)) { 
  install.packages("e1071") 
  library(e1071)
}


cat("\014")
set.seed(3.14)
vn <- sample(NROW(X))
train <- X[ vn[1:round(0.6*NROW(X),0)], ]

mod <- svm( target ~  feature_8 , data = train  )

prd <- predict(mod)

table(prd,train$target)










