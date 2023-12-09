setwd('C:/Users/nicol/OneDrive/Desktop/TESI/player_data')
library(tidyverse)
library(mclust)
library(colorr)
library(cartography)
library(ggsci)
library(clustvarsel)

df = read.csv('players_complete_data.csv')
df1 = df[df$GP>=30,]
df1[,9:30] = scale(df1[,9:30])

#Scelta variabili --------------------------------------------------------------

scelta_variabili = clustvarsel(data = df1[,9:30],
                               sampsize = nrow(data),
                               itermax = 200)


scelta_variabili$model$modelName
scelta_variabili$model$G
scelta_variabili$subset

subset = c('THREE_FG_AST_PCT','THREE_FGA_PCT','CORNER3','THREE_FG_PCT')
rm(scelta_variabili)

#Scelta seed -------------------------------------------------------------------

modelli = tibble(seed = rep(0,100),
                 modelName = rep('',100),
                 G = rep(0,100),
                 classification = matrix(rep(0,310000),nrow=100,),
                 BIC = rep(0,100))


for (i in 1:100){
  set.seed(i)
  modello = Mclust(df1[,subset], model = 'VEV', G = 9)
  modelli$seed[i] = i
  modelli$modelName[i] = modello$modelName
  modelli$G[i] = modello$G
  modelli$classification[i,] = modello$classification
  modelli$BIC[i] = modello$BIC
}

c=0
similarità = list()
for (i in 1:99){
  for (j in (i+1):100){
    c = c+1
    similarità[c] = adjustedRandIndex(modelli$classification[bestmodels[i],], 
                                      modelli$classification[bestmodels[j],])
    
    
  }
} 
similarità = as.numeric(similarità)
mean(similarità)
boxplot(similarità)


bestmodels = order(modelli$BIC)[91:100]

c=0
similarità = list()
for (i in 1:9){
  for (j in (i+1):10){
    c = c+1
    similarità[c] = adjustedRandIndex(modelli$classification[bestmodels[i],], 
                                      modelli$classification[bestmodels[j],])
    
    
  }
} 

similarità = as.numeric(similarità)
mean(similarità)
boxplot(similarità)

scelta_modello = modelli[modelli$BIC == max(modelli$BIC),]


scelta_modello$BIC
scelta_modello$modelName
scelta_modello$G

rm(scelta_modello, modelli, modello, similarità, bestmodels, c, i ,j)

#Clustering -------------------------------------------------------------------- 

set.seed(32)
#modello1 = Mclust(scale(df1[,9:30]), model = 'VEV', G = 9)
modello1 = Mclust(scale(df1[,subset]), model = 'VEV', G = 9)

df1 = df1 %>% mutate(gruppo = modello1$classification)
df1 = df1 %>% mutate(soft = round(modello1$uncertainty,2))
df1 = df1 %>% mutate(prob = round(modello1$z,3))


ggplot(df1, aes(x = as.factor(gruppo))) +
  geom_bar(fill = pal_simpsons()(9))+
  labs(x='Gruppo', y = 'Numero giocatori', 
       title ='Distribuzione dei giocatori nei cluster')+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)


ggplot(df1) + geom_density(aes(x =1-modello1$uncertainty),color="darkblue", 
                           fill="lightblue") +
  labs(x = 'Probabilità', y = 'Numero giocatori',
       title = 'Probabilità di appartenere a un certo cluster')


train_long = df1 %>%  #con pivot longer allunghiamo il dataset
  pivot_longer(cols = -c('gruppo','SEASON','TEAM_ABBREVIATION','TEAM_ID',
                         'PLAYER_ID', 'PLAYER_NAME','soft','AGE','GP','X','prob'), #prima colonna nome variabile seconda colonna valore
               names_to = 'var',
               values_to = 'valore') %>%
  mutate(gruppo=as.factor(gruppo))

sum((1-modello1$uncertainty>=0.9))/3100


#GRAFICI SUBSET-----------------------------------------------------------------
df_subset = df1[,c('gruppo','SEASON','TEAM_ABBREVIATION','TEAM_ID',
                  'PLAYER_ID', 'PLAYER_NAME','soft','AGE','GP','X','prob',subset)]

subset_long = df_subset %>%  #con pivot longer allunghiamo il dataset
  pivot_longer(cols = -c('gruppo','SEASON','TEAM_ABBREVIATION','TEAM_ID',
                         'PLAYER_ID', 'PLAYER_NAME','soft','AGE','GP','X','prob'), #prima colonna nome variabile seconda colonna valore
               names_to = 'var',
               values_to = 'valore') %>%
  mutate(gruppo=as.factor(gruppo)) 

ggplot(data = subset_long, aes(x=var, y=valore))+
  geom_boxplot(aes(fill = as.factor(gruppo)))+
  scale_fill_simpsons()+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")+
  labs(
    title = "Boxplot per gruppo delle variabili rilevanti",
    x = "",
    y = "Valore standardizzato")+
  theme(legend.position = "none")+
  facet_wrap(gruppo ~.)


#GRUPPO 1 Role Player-----------------------------------------------

ggplot(data = train_long[train_long$gruppo == 1,], aes(x=var, y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot()+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")

df_prova = df1[df1$gruppo == 1, c('SEASON','PLAYER_NAME','soft','gruppo','prob')]

#GRUPPO 2 Lunghi versatili --------------------------------------------------------
ggplot(data = train_long[train_long$gruppo == 2,], aes(x=var, y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot()+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")


vars_to_plot = c('HEIGHT','DREB_PCT','OREB_PCT','BLOCK_PCT','RA','TWO_FG_PCT',
                 'AST_PCT','CORNER3','THREE_FGA_PCT')
ggplot(data = train_long[train_long$gruppo == 2& 
                           train_long$var %in% vars_to_plot,], aes(x= var , y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot(fill = c('#C80813FF','#46732EFF','#C80813FF', rep('#46732EFF',3),
                        '#46732EFF','#C80813FF', '#46732EFF'))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")+
  labs(
    title = "Lunghi versatili",
    x = "Variabile",
    y = "Valore standardizzato")+
  theme(legend.position = "none")
  

df_prova = df1[df1$gruppo == 2, c('SEASON','PLAYER_NAME','soft','gruppo','prob')]


#Gruppo3 Generali in campo -----------------------------------------------------

ggplot(data = train_long[train_long$gruppo == 3,], aes(x=var, y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot()+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")

vars_to_plot = c('HEIGHT','DREB_PCT','OREB_PCT','BLOCK_PCT','AST_PCT')

ggplot(data = train_long[train_long$gruppo == 3& 
                           train_long$var %in% vars_to_plot,], aes(x= var , y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot(fill = c('#46732EFF',rep('#C80813FF',4)))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")+
  labs(
    title ='Generali in campo',
    x = "Variabile",
    y = "Valore standardizzato")+
  theme(legend.position = "none")



df_prova = df1[df1$gruppo == 3, c('SEASON','PLAYER_NAME','soft','gruppo','prob')]


#GRUPPO4 Guardie offensive -----------------------------------------------------

ggplot(data = train_long[train_long$gruppo == 4,], aes(x=var, y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot()+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")

vars_to_plot = c('HEIGHT','DREB_PCT','OREB_PCT','BLOCK_PCT','AST_PCT',
                 'USG_PCT', 'FGA')

ggplot(data = train_long[train_long$gruppo == 4& 
                           train_long$var %in% vars_to_plot,], aes(x= var , y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot(fill = c('#46732EFF',rep('#C80813FF',2),'#46732EFF',rep('#C80813FF',2),'#46732EFF'))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")+
  labs(
    title = "Guardie offensive",
    x = "Variabile",
    y = "Valore standardizzato")+
  theme(legend.position = "none")


df_prova = df1[df1$gruppo == 4, c('SEASON','PLAYER_NAME','soft','gruppo','prob')]


#GRUPPO5 Guardie specialiste da 3-----------------------------------------------

ggplot(data = train_long[train_long$gruppo == 5,], aes(x=var, y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot()+
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")

vars_to_plot = c('THREE_FGA_PCT','THREE_FG_AST_PCT', 'THREE_FG_PCT',
                 'TOV_PCT', 'OREB_PCT')

ggplot(data = train_long[train_long$gruppo == 5& 
                           train_long$var %in% vars_to_plot,], aes(x= var , y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot(fill = c('#C80813FF',rep('#46732EFF',3),'#C80813FF'))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")+
  labs(
    title = "Guardie specialiste da 3 punti",
    x = "Variabile",
    y = "Valore standardizzato")+
  theme(legend.position = "none")


df_prova = df1[df1$gruppo == 5, c('SEASON','PLAYER_NAME','soft','gruppo','prob')]

#GRUPPO6 Lunghi classici--------------------------------------------------------

ggplot(data = train_long[train_long$gruppo == 6,], aes(x=var, y=valore))+
  facet_wrap(gruppo ~.)+
  geom_boxplot()+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")

vars_to_plot = c('THREE_FGA_PCT','THREE_FG_AST_PCT', 'THREE_FG_PCT','AST_PCT',
                 'HEIGHT', 'OREB_PCT','RA','TWO_FG_AST_PCT')

ggplot(data = train_long[train_long$gruppo == 6& 
                           train_long$var %in% vars_to_plot,], aes(x= var , y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot(fill = c('#C80813FF',rep('#46732EFF',3),rep('#C80813FF',3),'#46732EFF'))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")+
  labs(
    title = "Lunghi classici",
    x = "Variabile",
    y = "Valore standardizzato")+
  theme(legend.position = "none")

df_prova = df1[df1$gruppo == 6, c('SEASON','PLAYER_NAME','soft','gruppo','prob')]

ggplot(data = df_prova, aes(x = SEASON)) +
  geom_bar(fill = '#D5E4A2FF')+
  labs(
    title = "Lunghi classici per stagione",
    x = "Stagione",
    y = "Lunghi classici",
  )

#GRUPPO7 Stretch forwards-------------------------------------------------------

ggplot(data = train_long[train_long$gruppo == 7,], aes(x=var, y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot()+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")

vars_to_plot = c('THREE_FGA_PCT','CORNER3', 'POINTS','AST_PCT','TOV_PCT','USG_PCT')

ggplot(data = train_long[train_long$gruppo == 7& 
                           train_long$var %in% vars_to_plot,], aes(x= var , y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot(fill = c('#C80813FF','#46732EFF','#C80813FF','#46732EFF',rep('#C80813FF',2)))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")+
  labs(
    title = "Stretch forwards",
    x = "Variabile",
    y = "Valore standardizzato")+
  theme(legend.position = "none")


df_prova = df1[df1$gruppo == 7, c('SEASON','PLAYER_NAME','soft','gruppo','prob')]

#GRUPPO8 Skilled Forwards-------------------------------------------------------

ggplot(data = train_long[train_long$gruppo == 8,], aes(x=var, y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot()+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")

vars_to_plot = c('THREE_FG_AST_PCT','THREE_FG_PCT','HEIGHT', 'STL_PCT','AST_PCT')

ggplot(data = train_long[train_long$gruppo == 8& 
                           train_long$var %in% vars_to_plot,], aes(x= var , y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot(fill = c('#C80813FF','#46732EFF','#C80813FF',rep('#46732EFF',2)))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")+
  labs(
    title = "Skilled Forwards",
    x = "Variabile",
    y = "Valore standardizzato")+
  theme(legend.position = "none")

df_prova = df1[df1$gruppo == 8, c('SEASON','PLAYER_NAME','soft','gruppo','prob')]

#GRUPPO9 Marcatori -------------------------------------------------------------

ggplot(data = train_long[train_long$gruppo == 9,], aes(x=var, y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot()+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")

vars_to_plot = c('USG_PCT','TOV_PCT','POINTS', 'PIE','CORNER3', 'THREE_FGA_PCT')

ggplot(data = train_long[train_long$gruppo == 9& 
                           train_long$var %in% vars_to_plot,], aes(x= var , y=valore))+
  #facet_wrap(gruppo ~.,scales='free')+
  geom_boxplot(fill = c('#C80813FF',rep('#46732EFF',2),'#C80813FF',rep('#46732EFF',2)))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")+
  labs(
    title = "Marcatori",
    x = "Variabile",
    y = "Valore standardizzato")+
  theme(legend.position = "none")

df_prova = df1[df1$gruppo == 9, c('SEASON','PLAYER_NAME','soft','gruppo','prob')]

#-------------------------------------------------------------------------------
dfp = df1 %>% select(PLAYER_ID, PLAYER_NAME, TEAM_ABBREVIATION, SEASON, prob)
write.csv(dfp,'soft_players.csv')
rm(modello1, train_long, df_subset, subset_long, df_prova, vars_to_plot)





?mclust
modello_prova_sub = Mclust(df1[,subset], model = 'VEV', G = 9)
modello_prova = Mclust(df1[,9:30], model = 'VEV', G = 9)
