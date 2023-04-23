
library(tidyverse)
library(caret)

# - Obtenim els 2 vectors de Clusters per cada metode:

# Clusters casos complets:
clust_cc <- read.csv("clusters_ccomplet_ordinal.csv")
clust_cc$Clusters
# Clusters imputacio:
clust_regl <- read.csv("clusters_RegL_ordinal.csv")
clust_regl$Clusters

# Comparacio preliminar que estem creuant be els individus:
all.equal( select(clust_cc,-Clusters), select(clust_regl,-Clusters) )
# TRUE: vol dir que tenen els mateixos atributs


### - Comparacio % de cada cluster per metode: - ###
# freq absoluta
table(clust_cc$Clusters) # c.complet
table(clust_regl$Clusters) # imp.regL
# freq relativa
table(clust_cc$Clusters) %>% prop.table() # c.complet
table(clust_regl$Clusters) %>% prop.table() # imp.regL
# Diferencia freq relativa
( table(clust_cc$Clusters) %>% prop.table() - table(clust_regl$Clusters) %>% prop.table() )*100


### - Comparacio dels 2 metodes: - ###
mat_confussio <- table(clust_regl$Clusters,clust_cc$Clusters)
rownames(mat_confussio) <- c("Imp C1","Imp C2","Imp C3")
colnames(mat_confussio) <- c("CasComp C1","CasComp C2","CasComp C3")

# Matriu comparacio: Annex de la memoria
mat_confussio

confusionMatrix(
  as.factor( clust_regl$Clusters ),
  as.factor( clust_cc$Clusters )
  )

