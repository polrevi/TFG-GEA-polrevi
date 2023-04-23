
# Input: ip_data_demo, ip_data_ordinal, index_ccomplet
# Output: supervised_data, clusters_regL_ordinal

################
## LLIBRERIES ##
################

library(readxl)
# Gestio dades + grafics + pipes
library(tidyverse)
library(ggpubr)
library(ggridges)
library(ggfortify)
library(stringr)
# Estudi de missings
library(visdat)
library(naniar)
# Imputacio NA
library(missForest)
# Clustering HCPC : 
library(factoextra)
library(FactoMineR)
library(Factoshiny)
library(cluster)
# ML supervisat :
library(randomForest)
library(e1071)



###########
## DADES ##
###########

# Carrego dades:
ip_data_demo <- read_excel("ip_data_demo.xlsx")
ip_data_op <- read_excel("ip_data_ordinal.xlsx")
# Carrego l'index dels casos complets:
index_ccomplet <- read_excel("index_ccomplet.xlsx")
index_ccomplet <- index_ccomplet$id

################
## -- HCPC -- ##
################


#########
## PCA ##
#########

# Prendrem fins al 12e component principal per despres els clusters HCPC
# La justificacio es que volem fer clusters sobre els components que guarden 70% info
# els components que guarden soroll no els volem pel clustering
res.PCA <- PCA(ip_data_op,ncp = 12)

# - Numero components i variabilitat explicada:
plot(y=res.PCA$eig[,3],x=1:length(res.PCA$eig[,3]),"b",pch=20,
     xlab="Numero components PCA",ylab="% variabilitat explicada Total",col="darkblue")
abline(a=50,b=0,col='purple',lwd=2)
abline(a=70,b=0,col='red',lwd=2)
legend(x=0,y=100,legend=c("50% tall variabilitat","70% tall variabilitat"),fill=c("purple","red"))
# Amb 12 components expliquem el 70% de la variabilitat. Podem passar de 46 a 12 variables!
# Amb nomes 5 components expliquem el 50% de la variabilitat.

# Grafic de la variabilitat
fviz_eig(res.PCA,ncp=46,choice="variance")+ylab("% Variabilitat Explicada")+
  xlab("Número de Components")+ggtitle("Variabilitat Explicada per cada component")


# - Grafic variables :
plot.PCA(res.PCA, choix='var',axes=c(1,2)) # PC1-PC2

# Grafic variables, agrupant tipus de pregunta per color
fviz_pca_var(res.PCA,
             axes=c(1,2),
             geom=c("point"),
             col.var = c(
               "Posicionament polític personal",
               rep("Posicionament partits ESP-CAT",8),
               "Posicionament polític personal",
               rep("Posicionament partits ESQ-DRET",8),
               "Pregunta Nivell de Riscs disposat a assumir",
               rep("Simpaties i valoracions de líders, partits i governs",22),
               rep("Valoracions Partits/Polítics",5)
               )
             ) 

# - Grafic variables influents
fviz_contrib(res.PCA,choice="var")

# % informacio que es conserva de cada variable
res.PCA$var$cos2 %>% dim()
(res.PCA$var$cos2)[1:12,] %>% round(2) 
quali_rep_taula <- apply( round(res.PCA$var$cos2[1:46,],2),1,sum ) 
quali_rep_taula <- as.data.frame( quali_rep_taula )
colnames(quali_rep_taula) <- "qualitat"
quali_rep_taula


# - Grafics individus a espai PCA :
a1 <- plot.PCA(res.PCA,axes=c(1,2),invisible= 'var',cex=1,cex.main=0.55,
               cex.axis=0.55,label ='none',xlim=c(-8,6),ylim=c(-4.5,6)); a1
a2 <- plot.PCA(res.PCA,axes=c(1,3),invisible= 'var',cex=1,cex.main=0.55,
               cex.axis=0.55,label ='none',xlim=c(-8,6.5),ylim=c(-5,4)); a2
a3 <- plot.PCA(res.PCA,axes=c(2,3),invisible= 'var',cex=1,cex.main=0.55,
               cex.axis=0.55,label ='none',xlim=c(-4.5,4.5),ylim=c(-4,3.5)); a3

# Grafic densitat components principals
individuals_pca <- res.PCA$ind$coord
individuals_pca <- as.data.frame(individuals_pca)
# 1 vs 2
b1<- ggplot(data=individuals_pca)+
  aes(x=Dim.1,y=Dim.2)+
  stat_density2d(aes(fill=..level..), geom="polygon",bins=10)+
  scale_fill_gradient(low="purple", high="yellow",name="densitat")+
  labs(title="Densitat conjunta PC1-PC2")
b1
# 1 vs 3
b2<- ggplot(data=individuals_pca)+
  aes(x=Dim.1,y=Dim.3)+
  stat_density2d(aes(fill=..level..), geom="polygon",bins=10)+
  scale_fill_gradient(low="purple", high="yellow", name="densitat")+
  labs(title="Densitat conjunta PC1-PC3")
b2
# 2 vs 3
b3 <- ggplot(data=individuals_pca)+
  aes(x=Dim.2,y=Dim.3)+
  stat_density2d(aes(fill=..level..), geom="polygon",bins=10)+
  scale_fill_gradient(low="purple", high="yellow", name="densitat")+
  labs(title="Densitat conjunta PC2-PC3")
b3

grafic_individus_pca <- ggarrange(a1,a2,a3,b1,b2,b3)
grafic_individus_pca
rm(a1,a2,a3,b1,b2,b3)


# Individus al espai PCA. Valor de cada component per cada individu
res.PCA$ind$coord



##############
## CLUSTERS ##
##############

# CLUSTERS : Algoritme de clustering
set.seed(3)
res.HCPC <- HCPC(res.PCA,nb.clust = 3,iter.max=10,method="ward")


########################
## NUMERO DE CLUSTERS ##
########################

# Documentacio de la llibreria, 
# TOTAL INERTIA = BETWEEN INERTIA + WITHIN INERTIA

# - res.HCPC$call$t$within : WITHIN GROUP INERTIA (WWS)
# - res.HCPC$call$t$inert.gain : DIFERENCIA BETWEEN GROUP INERTIA 
# Calcula, la diferencia (guany) entre la Between Inertia passant de k a k+1 clusters
# - res.HCPC$call$t$quot : Ratio entre 2 within k-1 i k
# - res.HCPC$call$t$nb.clust : Numero clusters

### DIAGRAMA COLZE - SCREE GRAPH ###

# -- WSS: res.HCPC$call$t$within --
plot(y=res.HCPC$call$t$within[1:20],
     x=1:20,"b",xlab="k Clusters",ylab="Within Sum of Squares (WSS)",main="WSS per k clusters",
     col="darkblue",lwd=2)
abline(a=22.5,b=0,col='red',lwd=2,lty=2)
# Veiem que augmentar a 4 clusters ja no millora molt el WSS


# -- interia.gain: Inercia Between Grup K - Inercia Between K-1 --

# Veiem que per a 4 clusters el guany d'Inercia no canvia i s'estabilitza
# Aixo equival a dir que afegir mes clusters no fa explicar mes variabilitat.
plot(y=res.HCPC$call$t$inert.gain,
     x=1:length(res.HCPC$call$t$inert.gain),
     xlim=c(0.75,16),"b",ylab="Guany d'inercia",
     xlab="Nombre de clusters",col='darkblue',lwd=2,main="Diferencia BSS per k clusters")
abline(a=1.08,b=0,col='red',lwd=1,lty=2)
# Grafic equivalent
plot.HCPC(res.HCPC,choice="bar",title="Guany d'Inèrcia Entre Grups (BSS) de k-1 a k clústers")

# Dendograma veiem a l'eix 'y' que apareix el 'inertia gain':
plot.HCPC(res.HCPC,choice="tree")


# (*) Descripcio de cada variable segons cluster:
descvars_clusters <- res.HCPC$desc.var

# Grafic valors v.test de la variable per cluster:
plot.catdes(descvars_clusters,
            col.upper = "green",col.lower = "red",barplot = FALSE)
# Valor v.test per cluster
plot.catdes(descvars_clusters,
            col.upper = "green",col.lower = "red",barplot = TRUE)
par(mfrow=c(1,1))

# Ordenats de mes a menys per l'estadistic v.test:
# - S'ordena per v.test de gran a petit
# - Mean in category, es la mitjana de la puntuacio pel cluster
# - La overall mean, es la mitjana global sense distingir clusters
des_quantivar_clust <- descvars_clusters$quanti
des_quantivar_clust$`1` # variables per cluster 1
des_quantivar_clust$`2` # variables per cluster 2
des_quantivar_clust$`3` # variables per cluster 3

# (*) Descripcio de cada individu. Individus caracteristics de cada cluster.
descind_clusters <- res.HCPC$desc.ind
descind_clusters


# - Visualitzar Clusters
# 1 vs 2:
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Factor map',
          axes=c(1,2)) # PC1-PC2
# 2 vs 3 :
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Factor map',
          axes=c(2,3)) # PC2-PC3
# 1 vs 3 :
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Factor map',
          axes=c(1,3)) # PC1-PC3

# Cluster amb ggplot
a <- fviz_cluster(res.HCPC,axes=c(1,2),alpha = 1.3, geom = c("point"),
                  palette = c("blue", "purple", "green"),main="Clusters a l'espai PC1-PC2"); a
b <- fviz_cluster(res.HCPC,axes=c(1,3),alpha = 1.3, geom = c("point"),
                  palette = c("blue", "purple", "green"),main="Clusters a l'espai PC1-PC3"); b
c <- fviz_cluster(res.HCPC,axes=c(2,3),alpha = 1.3, geom = c("point"),
                  palette = c("blue", "purple", "green"),main="Clusters a l'espai PC2-PC3"); c

grafic_individus_HCPC <- ggarrange(a,b,c,ncol = 3)
grafic_individus_HCPC
ggarrange(a,b,ncol=2)
rm(a,b,c)


# - Clusters :
Clusters <- res.HCPC$data.clust$clust
table(Clusters)
prop.table(table(Clusters))*100


### Components Principals univariants per Clusters ###
data_grafboxplot <- cbind(res.PCA$ind$coord[,1:3],Clusters)
data_grafboxplot <- as.data.frame(data_grafboxplot)
colnames(data_grafboxplot) <- c("d1","d2","d3","Clusters")
data_grafboxplot$Clusters <- as.factor( data_grafboxplot$Clusters )

h1 <- ggplot(data=data_grafboxplot)+aes(x=d1,y=Clusters,fill=Clusters)+
  geom_density_ridges(alpha=0.8,bandwidth=0.7)+
  labs(x="Score PC1",y="Clusters")+theme_ridges()+
  scale_fill_manual(values=c("blue", "purple","green"))+
  xlim(-10,10)
h1

h2 <- ggplot(data=data_grafboxplot)+aes(x=d2,y=Clusters,fill=Clusters)+
  geom_density_ridges(alpha=0.8,bandwidth=0.7)+
  labs(x="Score PC2",y="Clusters")+theme_ridges()+
  scale_fill_manual(values=c("blue", "purple","green"))+
  xlim(-10,10)
h2

grafic_histograma_pca <- ggarrange(h1,h2,nrow = 2)
grafic_histograma_pca

rm(data_grafboxplot)


#############################################
## DESCRIPTIVA CLUSTERS RESPECTE VARIABLES ##
#############################################


### GRAFICS Seccio Resultats ### 
data_AED_clusters <- cbind( ip_data_op, Clusters )

# ESP_CAT_0_10, SIMPATIA_INDEPENDENTISTES_0_10, SIMPATIA_UNIONISTES_0_10
# IDEOL_0_10, SIMPATIA_FEMINISTES_0_10, SIMPATIA_ESQUERRES_0_10, SIMPATIA_PSC_0_10,
# VAL_GOV_ESP, VAL_GOV_CAT, SIMPATIA_CEC_0_10, RISCOS

z1 <- ggplot(data=data_AED_clusters)+
  aes(y=ESP_CAT_0_10,x=Clusters,fill=Clusters)+geom_boxplot()+
  scale_fill_manual(values=c("blue", "purple","green"))+
  ylab("Identificació ESP-CAT")+guides(fill="none"); z1

z2 <- ggplot(data=data_AED_clusters)+
  aes(y=SIMPATIA_INDEPENDENTISTES_0_10,x=Clusters,fill=Clusters)+geom_boxplot()+
  scale_fill_manual(values=c("blue", "purple","green"))+
  ylab("Simpatia Independentistes")+guides(fill="none"); z2

z3 <- ggplot(data=data_AED_clusters)+
  aes(y=IDEOL_0_10,x=Clusters,fill=Clusters)+geom_boxplot()+
  scale_fill_manual(values=c("blue", "purple","green"))+
  ylab("Ideologia ESQ-DRETA")+guides(fill="none"); z3
 
z4 <- ggplot(data=data_AED_clusters)+
  aes(y=SIMPATIA_FEMINISTES_0_10,x=Clusters,fill=Clusters)+geom_boxplot()+
  scale_fill_manual(values=c("blue", "purple","green"))+
  ylab("Simpatia Feministes")+guides(fill="none"); z4

z5 <- ggplot(data=data_AED_clusters)+
  aes(y=SIMPATIA_ESQUERRES_0_10,x=Clusters,fill=Clusters)+geom_boxplot()+
  scale_fill_manual(values=c("blue", "purple","green"))+
  ylab("Simpatia Esquerres")+guides(fill="none"); z5

z6 <- ggplot(data=data_AED_clusters)+
  aes(y=SIMPATIA_UNIONISTES_0_10,x=Clusters,fill=Clusters)+geom_boxplot()+
  scale_fill_manual(values=c("blue", "purple","green"))+
  ylab("Simpatia Unionistes")+guides(fill="none"); z6

z7 <-ggplot(data=data_AED_clusters)+
  aes(y=SIMPATIA_PSC_0_10,x=Clusters,fill=Clusters)+geom_boxplot()+
  scale_fill_manual(values=c("blue", "purple","green"))+
  ylab("Simpatia partit PSC")+guides(fill="none"); z7

z8 <- ggplot(data=data_AED_clusters)+
  aes(y=VAL_GOV_ESP,x=Clusters,fill=Clusters)+geom_boxplot()+
  scale_fill_manual(values=c("blue", "purple","green"))+
  ylab("Valoració Govern Espanyol")+guides(fill="none"); z8

z9 <- ggplot(data=data_AED_clusters)+
  aes(y=SIMPATIA_CEC_0_10,x=Clusters,fill=Clusters)+geom_boxplot()+
  scale_fill_manual(values=c("blue", "purple","green"))+
  ylab("Simpatia partit Podemos-CEC")+guides(fill="none"); z9

z10 <- ggplot(data=data_AED_clusters)+
  aes(y=VAL_GOV_CAT,x=Clusters,fill=Clusters)+geom_boxplot()+
  scale_fill_manual(values=c("blue", "purple","green"))+
  ylab("Valoració Govern Català")+guides(fill="none"); z10

#
grafic_boxplots_clusters <- ggarrange(z1,z2,z3,z4,z5,z6,z7,z8,z10,nrow=3,ncol = 3)
grafic_boxplots_clusters


##################
# Deso el fitxer amb el que farem aprenentatge supervisat:

# Variables demografia+Cluster
supervised_data <- cbind(ip_data_demo,Clusters)
# El desem:
write.csv(supervised_data,file="supervised_data.csv",row.names = FALSE)

###############
## 
# Fitxer comparacio clusters: Obtinc els individus sense missings
data_ccomplet_clusters_regL <- cbind(ip_data_op[index_ccomplet,] , Clusters[index_ccomplet] )
colnames( data_ccomplet_clusters_regL )[ncol(data_ccomplet_clusters_regL)] <- "Clusters"
# Desem clusters creats amb metode imputacio Regressio Lineal:
write.csv( data_ccomplet_clusters_regL , file="clusters_regL_ordinal.csv")

