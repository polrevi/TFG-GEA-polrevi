
# Input: supervised_data
# Output: taula_models

################
## Llibreries ##
################

library(readxl)
library(writexl)
# Gestio dades + grafics + pipes
library(tidyverse)
library(stringr)
# Estudi de missings
library(visdat)
library(naniar)
# Clustering HCPC : 
library(factoextra)
library(FactoMineR)
library(Factoshiny)
# ML supervisat :
library(caret)
library(randomForest)
library(e1071)
library(MASS)
library(bnlearn)
library(gRain)
library(xgboost)
library(Matrix)


###########
## DADES ##
###########

# Carrego les dades:
dades <- read.csv("supervised_data.csv")
str(dades)

# Dades amb imputacio a la demografia, cap missing
complete.cases(dades) %>% table()

# Convertir character a factor
for(i in colnames(dades)){
  if( is.character(dades[,i])==TRUE ){
    dades[,i] <- as.factor(dades[,i])
  }
}
# Convertim el Cluster {1,2,3} a factor
dades$Clusters <- as.factor(dades$Clusters)


# - Output: 
# 3 categories, cluster 3 catgeoria majoritaria
dades$Clusters %>% table()
dades$Clusters %>% table() %>% prop.table()*100
dades$Clusters %>% table() %>% prop.table() %>% barplot(col=c('blue','purple','green'),ylim=c(0,1))


#######################################
## ANALISI EXPLORATORI A LA RESPOSTA ##
#######################################


# LLOC NAIXEMENT: Molt bo!! Serveix per predir el Cluster 1, Cluster 2 i Cluster 3.
# Nascuts Fora d'Espanya -> Cluster 2 
# Catalunya -> Cluster 3
# Comunitats autonomes -> Cluster 1
table(dades$Clusters,dades$LLOC_NAIX) %>% prop.table(2) %>%
  barplot(col=c("blue","purple","green"),xlim=c(0,5),main="LLOC NAIXEMENT")
legend(x=3.75,y=0.8,legend=c("Cluster 1","Cluster 2","Cluster 3"),
       fill=c("blue","purple","green") )
abline(a=0.5,b=0,lwd=2,col='red')


# LLENGUA IDENTIFICACIO: Serveix per predir el Cluster 3, Cluster 2 i 1 pot confondre
# Ho fem pels 3 nivells mes representatius
data_sup <- filter(dades,LLENGUA_IDENTIFICACIO=="Català (valencià / balear)"|
                     LLENGUA_IDENTIFICACIO=="Castellà"|
                     LLENGUA_IDENTIFICACIO=="Totes dues igual: català (valencià / balear) i castellà")

table(data_sup$Clusters,as.character(data_sup$LLENGUA_IDENTIFICACIO)) %>% prop.table(2) %>%
  barplot(col=c("blue","purple","green"),xlim=c(0,5),main="LLENGUA IDENTIFICACIO")
abline(a=0.5,b=0,lwd=2,col='red')
legend(x=3.75,y=0.8,legend=c("Cluster 1","Cluster 2","Cluster 3"),
       fill=c("blue","purple","green") )
rm(data_sup)


# PROVINCIA: Per a totes les Provincies, el Cluster 3 el majoritari.
# S'aprecia que el Cluster 3 (indies-catalans) es troben a  
# la provincia de Girona i Lleida sent mes del 50%.
table(dades$Clusters,dades$PROVINCIA) %>% prop.table(2) %>%
  barplot(col=c("blue","purple","green"),xlim=c(0,6.5),main="PROVINCIA")
legend(x=5,y=0.8,legend=c("Cluster 1","Cluster 2","Cluster 3"),
       fill=c("blue","purple","green") )
abline(a=0.5,b=0,lwd=2,col='red')


## CANAL DE TELEVISIO: Serveix per identificar molt be els 3 clusters.
# TV3 | 3/24 | La Sexta | No veu la TV -> Cluster 3
# Antena3 | No mira els informatius -> Cluster 1 
# Telecinco | La1 -> Cluster 2

data_sup <- filter(dades,INF_POL_TV_CANAL=="TV3"|INF_POL_TV_CANAL=="Antena 3"|
                     INF_POL_TV_CANAL=="La 1"|INF_POL_TV_CANAL=="Telecinco"|
                     INF_POL_TV_CANAL=="No mira els informatius"|INF_POL_TV_CANAL=="La Sexta"|
                     INF_POL_TV_CANAL=="No veu la TV / No té TV"|INF_POL_TV_CANAL=="Canal 3/24"
                   )

table(data_sup$Clusters,as.character(data_sup$INF_POL_TV_CANAL)) %>% prop.table(2) %>%
  barplot(col=c("blue","purple","green"),main="CANAL TELEVISIO INFO POLITICA")
abline(a=0.5,b=0,lwd=2,col='red')
rm(data_sup)

## CANAL DE RADIO: 
data_sup <- filter(dades,INF_POL_RADIO_EMISORA=="No escolta els informatius"|
                     INF_POL_RADIO_EMISORA=="No escolta la ràdio / No té ràdio"|
                     INF_POL_RADIO_EMISORA=="Rac 1"|
                     INF_POL_RADIO_EMISORA=="Catalunya Ràdio"|
                     INF_POL_RADIO_EMISORA=="SER"|
                     INF_POL_RADIO_EMISORA=="COPE"
                   )

table(data_sup$Clusters,as.character(data_sup$INF_POL_RADIO_EMISORA)) %>% prop.table(2) %>%
  barplot(col=c("blue","purple","green"),main="CANAL RADIO INFO POLITICA")
abline(a=0.5,b=0,lwd=2,col='red')
rm(data_sup)

table(dades$INF_POL_RADIO_EMISORA) %>% sort()

## EDAT
boxplot(dades$EDAT ~ dades$Clusters,col=c("blue","purple","green"),ylab="Edat",
        xlab="Cluster",main="Edat",ylim=c(0,100))


## RELIGIO
tau_religio <- table(dades$Clusters,dades$RELIGIO) %>% prop.table(2) 
colnames( tau_religio ) <- c("Altres","Budisme","Agnosticisme","Ateisme","Catolicisme",
                             "Protestants","Islam","Judaisme","NC","NS","Jehova")
tau_religio %>% barplot( col = c("blue","purple","green"),main="Religio" )
abline(a=0.5,b=0,lwd=2,col='red')
rm(tau_religio)


###################
## PROVAR MODELS ##
###################

# Farem 5 particions de train i test, i provarem diferents models.
# Aixi identifiquem models i metriques de comportament.

# - Preliminars:

## TAULA PER DESAR METRIQUES AJUST MODELS ##
taula_models <- data.frame(
  tipus_model=c(),
  sensC1=c(),
  sensC2=c(),
  sensC3=c(),
  acc=c()
)

## FUNCIO PER AFEGIR FILA A LA TAULA ANTERIOR ##
introduir_model <- function(nomModel,s1,s2,s3,acc){
  taula_models <-
    rbind(
      taula_models,
      data.frame(
        model_name=nomModel,
        sensC1=mean(s1),
        sensC2=mean(s2),
        sensC3=mean(s3),
        acc=mean(acc)
      )
    )
  return(taula_models)
}


## FUNCIO CREAR DATASET TRAIN I TEST BALANCEJATS ##
# train: 800, 250 (C1), 300 (C2), 250 (C3)
# test: 914, 119 (C1), 218 (C2), 577(C3) 

split_train_test <- function(dades,llavor){
  set.seed(llavor)
  # 1- Parteixo dades cluster 1,2,3
  C1 <- filter(dades,Clusters==1)
  C2 <- filter(dades,Clusters==2)
  C3 <- filter(dades,Clusters==3)
  # 2.1- Train i test pel grup 1
  id <- sample(1:nrow(C1),250)
  trainC1 <- C1[id,]
  testC1 <- C1[-id,]
  # 2.2- Train i test pel grup 2
  id <- sample(1:nrow(C2),300)
  trainC2 <- C2[id,]
  testC2 <- C2[-id,]
  # 2.3- Train i test pel grup 3
  id <- sample(1:nrow(C3),250)
  trainC3 <- C3[id,]
  testC3 <- C3[-id,]
  # 3- Ajuntar en un sol el train i test
  train_data <- rbind(trainC1,trainC2,trainC3)
  test_data <- rbind(testC1,testC2,testC3)
  # Retornem el dataset per entrenar model i sense entrenar model
  return(
    list(train_data=train_data,
         test_data=test_data)
  )
}

### -- PROVA DE MODELS -- ##

# (1) NAIVE BAYES amb totes les variables categoriques
s1 <- numeric(5)
s2 <- numeric(5)
s3 <- numeric(5)
acc <- numeric(5)
quali_dades <- dplyr::select(dades,-c("PERSONES_LLAR","EDAT") )

for(i in 1:5){
  # Particio de train balancejada i el seu respectiu test
  particio <- split_train_test(quali_dades,i) # provem 5 llavors
  train1 <- particio$train_data
  test1 <- particio$test_data
  # Entrenem model
  mod_nb_full <- naiveBayes(Clusters ~ . , train1)
  # Metriques prediccio
  s1[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$byClass[1,1]
  s2[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$byClass[2,1]
  s3[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$byClass[3,1]
  acc[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$overall[1]
}

taula_models <- introduir_model("Naive Bayes totes les variables",s1,s2,s3,acc)
taula_models


# (2) NAIVE BAYES amb variables seleccionades
s1 <- numeric(5)
s2 <- numeric(5)
s3 <- numeric(5)
acc <- numeric(5)

for(i in 1:5){
  # Particio de train balancejada i el seu respectiu test
  particio <- split_train_test(dades,i) # provem 5 llavors
  train1 <- particio$train_data
  test1 <- particio$test_data
  # Entrenem model
  mod_nb_full <- naiveBayes(Clusters ~ INF_POL_TV_CANAL+INF_POL_RADIO_EMISORA+
                              INF_POL_PREMSA_DIARI+INGRESSOS_1_15+LLENGUA_IDENTIFICACIO
                              ,train1)
  # Metriques prediccio 
  s1[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$byClass[1,1]
  s2[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$byClass[2,1]
  s3[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$byClass[3,1]
  acc[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$overall[1]
  
}

taula_models <- introduir_model("Naive Bayes seleccio variables",s1,s2,s3,acc)
taula_models


# (3) RANDOM FOREST: ntree=100, mtry=10
# Afegim mes variables
s1 <- numeric(5)
s2 <- numeric(5)
s3 <- numeric(5)
acc <- numeric(5)

for(i in 1:5){
  # Particio de train balancejada i el seu respectiu test
  particio <- split_train_test(dades,i) # provem 5 llavors
  train1 <- particio$train_data
  test1 <- particio$test_data
  # Entrenem model
  set.seed(i)
  mod_nb_full <- randomForest(Clusters ~ . ,train1, ntree=100,mtry=10)
  # Metriques prediccio
  s1[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$byClass[1,1]
  s2[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$byClass[2,1]
  s3[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$byClass[3,1]
  acc[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$overall[1]
  
}

taula_models <- introduir_model("Random Forest (numvar=10, ntree=100)",s1,s2,s3,acc)
taula_models


# (4) RANDOM FOREST: ntree=100, mtry=15
# Afegim mes variables
s1 <- numeric(5)
s2 <- numeric(5)
s3 <- numeric(5)
acc <- numeric(5)

for(i in 1:5){
  # Particio de train balancejada i el seu respectiu test
  particio <- split_train_test(dades,i) # provem 5 llavors
  train1 <- particio$train_data
  test1 <- particio$test_data
  # Entrenem model
  set.seed(i)
  mod_nb_full <- randomForest(Clusters ~ . ,train1, ntree=100,mtry=15)
  # Metriques prediccio
  s1[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$byClass[1,1]
  s2[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$byClass[2,1]
  s3[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$byClass[3,1]
  acc[i] <- confusionMatrix( predict(mod_nb_full,test1) , test1$Clusters)$overall[1]
  
}

taula_models <- introduir_model("Random Forest (numvar=15, ntree=100)",s1,s2,s3,acc)
taula_models

predict(mod_nb_full,test1)


#  (5) XGBOOST:
# Afegim mes variables
s1 <- numeric(5)
s2 <- numeric(5)
s3 <- numeric(5)
acc <- numeric(5)

for(i in 1:5){
  # Particio de train balancejada i el seu respectiu test
  particio <- split_train_test(dades,i) # provem 5 llavors
  train1 <- particio$train_data
  test1 <- particio$test_data
  # Creacio de matrius pel XGBoost
  trainm <- sparse.model.matrix(Clusters ~ . ,data=train1)
  train_Y <- as.numeric(train1[,"Clusters"])-1
  train_matrix <- xgb.DMatrix(data = as.matrix(trainm),label = train_Y)
  valm <- sparse.model.matrix(Clusters ~ . ,data=test1)
  val_Y <- test1[,"Clusters"]
  val_matrix = xgb.DMatrix(data = as.matrix(valm),label = val_Y)
  # Hiperparametres model XGBoost
  xgb_params = list(objective   = "multi:softmax",
                    num_class="3",
                    eval_metric = "error",
                    max_depth   = 10, # Profunditat de l'arbre
                    eta         = 0.05, # learning rate
                    gammma      = 0, # Reducció de pèrdues mínima necessària per fer una partició addicional en un node fulla de l'arbre.
                    colsample_bytree = 0.5,
                    min_child_weight = 1
                    )
  # Entrenament model XGboost
  model_xgb <- xgb.train(params=xgb_params,data=train_matrix,nrounds = 100)
  # Prediccio model
  pred_xgb <- predict(model_xgb,newdata = val_matrix)
  # Metriques :
  s1[i] <- confusionMatrix( as.factor(pred_xgb+1), as.factor(val_Y) )$byClass[1,1]
  s2[i] <- confusionMatrix( as.factor(pred_xgb+1), as.factor(val_Y) )$byClass[2,1]
  s3[i] <- confusionMatrix( as.factor(pred_xgb+1), as.factor(val_Y) )$byClass[3,1]
  acc[i] <- confusionMatrix( as.factor(pred_xgb+1), as.factor(val_Y) )$overall[1]
}

taula_models <- introduir_model("XGBoost",s1,s2,s3,acc)
taula_models


## GUARDAR TAULA AMB ELS MODELS ##

# BALANCED ACCURACY
taula_models$balanced_acc <- (taula_models$sensC1+taula_models$sensC2+taula_models$sensC3)/3
taula_models

## GUARDEM LA TAULA DE METRIQUES TOTS ELS MODELS 
taula_models[,2:6] <- round( taula_models[,2:6]*100,2)
colnames(taula_models) <- c("Nom Model","Sens.C1","Sens.C2","Sens.C3","Accuracy","Bal.Accuracy")
taula_models
write_xlsx(taula_models,"taula_models.xlsx")


##################################################################
##################################################################

## FIGURA. Exemple d'Arbre classificacio ##

library(rpart)
library(rpart.plot)

# Renombrar variable resposta
copiatrain1 <- train1
cont <- 1
copiatrain1$Clusters <- as.numeric(copiatrain1$Clusters)
for(i in copiatrain1$Clusters){
  if(i==1){copiatrain1$Clusters[cont]="Cluster 1"}
  if(i==2){copiatrain1$Clusters[cont]="Cluster 2"}
  if(i==3){copiatrain1$Clusters[cont]="Cluster 3"}
  cont <- cont+1
}
copiatrain1$Clusters <- as.factor(copiatrain1$Clusters)

# Recodifico variable CANAL TV
copiatrain1$INF_POL_TV_CANAL <- as.character(copiatrain1$INF_POL_TV_CANAL)
copiatrain1$INF_POL_TV_CANAL[copiatrain1$INF_POL_TV_CANAL=="Altres canals estatals"] <- "Altres canals"
copiatrain1$INF_POL_TV_CANAL[copiatrain1$INF_POL_TV_CANAL=="Altres canals estrangers"] <- "Altres canals"
copiatrain1$INF_POL_TV_CANAL[copiatrain1$INF_POL_TV_CANAL=="Altres canals locals"] <- "Altres canals"
copiatrain1$INF_POL_TV_CANAL[copiatrain1$INF_POL_TV_CANAL=="Antena 3"] <- "A3"
copiatrain1$INF_POL_TV_CANAL[copiatrain1$INF_POL_TV_CANAL=="No mira els informatius"] <- "No mira informatius"
copiatrain1$INF_POL_TV_CANAL[copiatrain1$INF_POL_TV_CANAL=="No té cap canal habitual"] <- "Cap habitual"
copiatrain1$INF_POL_TV_CANAL[copiatrain1$INF_POL_TV_CANAL=="Canal 3/24"] <- "3/24"
copiatrain1$INF_POL_TV_CANAL[copiatrain1$INF_POL_TV_CANAL=="Canal 33"] <- "33"
copiatrain1$INF_POL_TV_CANAL[copiatrain1$INF_POL_TV_CANAL=="Telecinco"] <- "T5"
copiatrain1$INF_POL_TV_CANAL[copiatrain1$INF_POL_TV_CANAL=="No veu la TV / No té TV"] <- "No veu/té TV"

copiatrain1$INF_POL_TV_CANAL <- as.factor(copiatrain1$INF_POL_TV_CANAL)

# Recodifico variable LLENGUA
copiatrain1$LLENGUA_IDENTIFICACIO <- as.character(copiatrain1$LLENGUA_IDENTIFICACIO)
copiatrain1$LLENGUA_IDENTIFICACIO[copiatrain1$LLENGUA_IDENTIFICACIO=="No contesta"] <- "No Contesta"
copiatrain1$LLENGUA_IDENTIFICACIO[copiatrain1$LLENGUA_IDENTIFICACIO=="Altres llengües o altres combinacions"] <- "Altres"
copiatrain1$LLENGUA_IDENTIFICACIO[copiatrain1$LLENGUA_IDENTIFICACIO=="Català (valencià / balear)"] <- "Català"
copiatrain1$LLENGUA_IDENTIFICACIO[copiatrain1$LLENGUA_IDENTIFICACIO=="Totes dues igual: català (valencià / balear) i castellà"] <- "Les 2: Castellà i Català"

copiatrain1$LLENGUA_IDENTIFICACIO <- as.factor(copiatrain1$LLENGUA_IDENTIFICACIO)

# Model senzill + grafic
mod_arbre <- rpart( formula=Clusters ~ LLENGUA_IDENTIFICACIO+INF_POL_TV_CANAL,
                    data=copiatrain1, method="class")
rpart.plot(x=mod_arbre,type=5,fallen.leaves = FALSE)



##################################################################
##################################################################

## (6) RANDOM CLASIFIER 
# Model que prediu clusters al atzar seguint Uniforme Discreta per {1,2,3}
s1 <- numeric(100)
s2 <- numeric(100)
s3 <- numeric(100)
acc <- numeric(100)

for(i in 1:100){
  # Particio de train balancejada i el seu respectiu test
  particio <- split_train_test(dades,i) # provem 5 llavors
  train1 <- particio$train_data
  test1 <- particio$test_data
  # Classificacions aleatories
  # set.seed(i)
  pred <- sample(x = c(1,2,3),prob = c(1/3,1/3,1/3),size=nrow(test1),replace=TRUE )
  pred <- as.factor(pred)
  
  # Metriques prediccio
  s1[i] <- confusionMatrix( pred , test1$Clusters)$byClass[1,1]
  s2[i] <- confusionMatrix( pred , test1$Clusters)$byClass[2,1]
  s3[i] <- confusionMatrix( pred , test1$Clusters)$byClass[3,1]
  acc[i] <- confusionMatrix( pred , test1$Clusters)$overall[1]
  
}

summary( s1 )
summary( s2 )
summary( s3 )
summary( acc )

