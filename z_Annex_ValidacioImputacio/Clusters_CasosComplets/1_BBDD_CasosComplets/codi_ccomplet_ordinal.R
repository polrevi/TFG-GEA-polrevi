

################
## LLIBRERIES ##
################

library(readxl)
# Gestio dades + grafics + pipes
library(tidyverse)
library(stringr)
# Estudi de missings
library(visdat)
library(naniar)
# Imputacio NA
library(mice)


###########
## DADES ##
###########

# (*) Carrego dades:
original_opdata <- read_excel("preprocessed_OPdata.xlsx") # Opinio Publica (Clusters)
original_demodata <- read_excel("preprocessed_DEMOdata.xlsx") # Demografia (Covariables)
# Informacio variables:
variables_info <- read_excel("update_variablesinfo.xlsx")

# (*) Recordem els missings:
# Opinio publica:
gg_miss_var(original_opdata) # opinio publica
vis_miss(original_opdata)
miss_sum_op <- miss_var_summary(original_opdata)
# Demografia:
gg_miss_var(original_demodata) # demografia
vis_miss(original_demodata)
miss_sum_demo <- miss_var_summary(original_demodata)


###########################
##  ELIMINACIO MISSINGS  ##
###########################

# (*) Gestio de Missings: Treure variables >20% Missings
# Eliminacio d'individus amb molts missings

###  OPINIO PUBLICA ###
# - Trec 20% Variables Missings -
name_miss_op <- filter(miss_sum_op,pct_miss<=20) %>% select(variable)
name_miss_op <- name_miss_op$variable
data_op <- select(original_opdata,name_miss_op)
vis_miss(data_op) # ara hi ha 95.4% de dades sense missings

## - VARIABLES ORDINALS CLUSTERS  ##

# - Identificacio d'individus amb masses missings, necessari per excloure de BBDD demografia
# Dataset variables ordinals:
ordinal_name <- filter(variables_info,Rang=="[0-10] , Discret") %>% select(NOM_VARIABLE)
ordinal_name <- ordinal_name$NOM_VARIABLE
ordinal_name <- intersect(ordinal_name , colnames(data_op) )
ordinal_op_data <- select(data_op,ordinal_name)
vis_miss(ordinal_op_data)

perc_ccases_ind <- numeric(2000)
for(i in 1:2000){
  perc_ccases_ind[i] <- ordinal_op_data[i,] %>% as.numeric() %>% complete.cases() %>% sum()/46
}
miss_individuals <- data.frame(
  id=1:2000,
  perc_ccases=perc_ccases_ind
)
# distribucio % casos complets a cada variable
hist(miss_individuals$perc_ccases,
     main="% casos complets a cada variable",xlab="%",breaks=30,col='yellow') 

# Individus amb mes del 80 % de informacio a cada variable
miss_individuals$perc_ccases[miss_individuals$perc_ccases>0.8] %>% length()
id_noeliminats <- miss_individuals$id[miss_individuals$perc_ccases>0.8]

new_ordinal_opdata <- ordinal_op_data[id_noeliminats,]

vis_miss(new_ordinal_opdata) # Nova visualitzacio dels missings a la BBDD

# ID dels casos complets, el necessitarem per comparar :
index_cas_complet <- numeric()

for( i in 1:nrow(new_ordinal_opdata) ){
  if( complete.cases(new_ordinal_opdata)[i] == TRUE ){
    index_cas_complet <- c( index_cas_complet, i )
  }
}
index_cas_complet %>% length()


### DEMOGRAFIA  ###
# - Trec 20% Variables Missings -
name_miss_demo <-  filter(miss_sum_demo,pct_miss<=20) %>% select(variable)
name_miss_demo <- name_miss_demo$variable
data_demo <- select(original_demodata,name_miss_demo)
data_demo <- data_demo[id_noeliminats,]
vis_miss(data_demo) # 99.5% sense NA

# Ara no hi ha casi missings!

#######

## GUARDEM BBDD :

ordinal_cascomplet <- new_ordinal_opdata[index_cas_complet,]
complete.cases(ordinal_cascomplet) %>% table()

new_data_demo <- data_demo[index_cas_complet,]
new_data_demo <- select(new_data_demo,-c(SIT_LAB_ACTIU,SECTOR) )
complete.cases(new_data_demo) %>% table()
vis_miss(new_data_demo)



###########
## FINAL ##
###########


# Guardem datasets imputats de opinio publica i demografia
writexl::write_xlsx(ordinal_cascomplet,"ip_data_ordinal.xlsx")
writexl::write_xlsx(new_data_demo,"ip_data_demo.xlsx")

