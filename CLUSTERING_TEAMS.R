setwd('C:/Users/nicol/OneDrive/Desktop/TESI/team_data')
library(tidyverse)
library(mclust)
library(clustvarsel)
library(colorr)
library(cartography)
library(ggsci)

data = as.tibble(read.csv('team_all_data.csv', header=T))
data = data %>% mutate(id = X.1) %>% select(-X.1)
data[,c(6:50,52:56)] = scale(data[,c(6:50,52:56)])

#Scelta variabili---------------------------------------------------------------

scelta_variabili = clustvarsel(data = data[,c(6:50,52:56)],
                               sampsize = nrow(data),
                               itermax = 200)

scelta_variabili$model$modelName
scelta_variabili$model$G
scelta_variabili$subset

subset = c('Opp_Mid','Isolation','Opp_LC3','DREB','Opp_ITP')

#CLustering --------------------------------------------------------------------

modello = Mclust(data[,subset], model = 'EVE', G = 3)


data = data %>% mutate(gruppo = modello$classification)
data = data %>% mutate(soft = round(modello$uncertainty,2))
data = data %>% mutate(prob = round(modello$z,3))

table(data$gruppo)


#Grafico di dispersione---------------------------------------------------------
clp = clPairs(modello$data, modello$classification, 
              color = pal_jama()(3), lower.panel = NULL)

clPairsLegend(0.1, 0.4, class = clp$class, col = clp$col, pch = clp$pch, box = F,
              title = "Gruppo")


#Altri grafici -----------------------------------------------------------------
final = c('TEAM_ID','SEASON','TEAM_NAME','TEAM_ABBREVIATION','id','NET_RATING',
          subset,'gruppo','soft','prob')

df = data[,final]

train_long = df %>%  #con pivot longer allunghiamo il dataset
  pivot_longer(cols = subset, #prima colonna nome variabile seconda colonna valore
               names_to = 'var',
               values_to = 'valore') %>%
  mutate(gruppo=as.factor(gruppo))



ggplot(data = df, aes(x = SEASON)) +
  geom_bar(aes(fill = as.factor(gruppo)))+
  scale_fill_jama()+
  labs(
    title = "Numero di squadre per gruppo e stagione",
    x = "Stagione",
    y = "Numero di squadre",
    fill = "Gruppo"  
  )

ggplot(data = train_long, aes(x=var, y=valore))+
  geom_boxplot(aes(fill = as.factor(gruppo)))+
  scale_fill_jama()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")+
  labs(
    title = "Boxplot distribuzione delle variabili rilevanti per gruppo",
    x = "Variabile",
    y = "Valore standardizzato")+
  theme(legend.position = "none")+
  facet_wrap(gruppo ~.)

df_1 = df[df$gruppo==1, c('TEAM_ABBREVIATION','SEASON','soft','NET_RATING','prob')]
df_2 = df[df$gruppo==2, c('TEAM_ABBREVIATION','SEASON','soft','NET_RATING','prob')]
df_3 = df[df$gruppo==3, c('TEAM_ABBREVIATION','SEASON','soft','NET_RATING','prob')]


df_riassunto = df[, c('TEAM_ABBREVIATION','SEASON','gruppo',
                      'soft','NET_RATING','prob')]

ggplot(df) + geom_density(aes(x =1-modello$uncertainty),
                          color="darkblue", fill="lightblue") +
  labs(x = 'Probabilità', 
       y = 'Numero squadre',
       title = 'Probabilità di appartenere a un certo cluster')


#-------------------------------------------------------------------------------
dft = df_riassunto %>% select(-c(gruppo, soft))
dft$team_NET = dft$NET_RATING
dft = dft %>% select(-NET_RATING)
write.csv(dft, file ='soft_teams.csv')
rm(scelta_variabili, subset, modello, clp, final, train_long,
   df_1, df_2, df_3, df_riassunto)


