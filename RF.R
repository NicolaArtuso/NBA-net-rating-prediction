setwd('C:/Users/nicol/OneDrive/Desktop/TESI/lineup_data')
library(tidyverse)
library(randomForest)
library(ggplot2)
library(gridExtra)


data = read.csv('soft_lineups.csv')
setwd('C:/Users/nicol/OneDrive/Desktop/TESI/lineup_data/RF')
data = data %>% select(-X)
x = data %>% select(-c('TEAM_ABBREVIATION','GROUP_ID','GP',
                       'NET_RATING','POSS','id', 'team_NET','somma',))

#Training set: tutte le stagione fino al 2022
(unique(x$id))

set.seed(100)
id_train = sample(13800, size = 12081)
train =  x[id_train, c(3:15)]

#Test set: la stagione 22-23
test = x[-id_train, c(3:15)]



set.seed(100)
#Random forest con valori di default (500 alberi, m = p/3 = 4)
rf = randomForest(B_NET ~., data = train)
plot(rf, ylim = c(11,12))
plot(rf)
ggplot(tibble(error = rf$mse, trees = c(1:500)), aes(x=trees,y = error))+
  geom_line()+
  labs(x = 'Numero di alberi',
       y = 'errore',
       title = 'Andamento MSE per numero di alberi nel RF')

rf$importance
rf$mse[200] -rf$mse[500]

#Dati per valori predetti VS valori osservati
data_gr <- train %>%
  mutate(set="train") %>%
  bind_rows(test %>% mutate(set="test"))


set.seed(100)
#Vediamo come modificare l'albero scegliendo il giusto m
oob_err <- double(12)
test_err <- double(12)

#mtry è il numero di variabili scelte caualmente ad ogni split
for(mtry in 1:12) {
  rf_m <- randomForest(B_NET ~ . , data = train, mtry=mtry, ntree = 300) 
  oob_err[mtry] <- rf_m$mse[300] #Errore per tutti gli alberi adattati
  
  pred <- predict(rf_m,test) #Previsioni sul set di test per ciascun albero
  test_err[mtry] <- with(test, mean( (B_NET - pred)^2)) #Mean Squared Error per l'insieme di test
}


test_err
oob_err

test_err + oob_err

ds_gr <- data_frame(type=c(rep("test", length(test_err)), rep("oob", length(oob_err))),
                    mtry = c(1:length(test_err), 1:length(oob_err)),
                    error=c(test_err, oob_err))


ggp <- ggplot(data = ds_gr, mapping = aes(x=mtry,y=error)) +
  geom_line(aes(colour=type)) +
  geom_point(aes(colour=type)) + 
  labs(x = 'm', y='errore')+
  ggtitle("OOB error e test error per numero di variabili negli split (m)")

print(ggp)



rf_fit1 = randomForest(B_NET ~., data = train, ntree=300)
data_gr$fit1 <- predict(rf_fit1, data_gr)

mse1 <- data_gr %>%
  filter(set=="test") %>%
  summarise(mse = mean((fit1-B_NET)^2)) %>%
  pull()

print(mse1)

plot2 <- ggplot(data = data_gr, mapping = aes(x=fit1, y=B_NET)) +
  geom_point(aes(colour=set), alpha=0.6) +
  geom_abline(slope=1, intercept = 0)+
  #geom_smooth(method = "lm", se = FALSE, aes(colour=set), alpha=0.6)+
  labs(x='Valori predetti', y = 'Valori osservati', 
       title ='Valori predetti VS Valori osservati, 300 alberi, m = 4')+
  xlim(-30,30)+
  ylim(-30,30)

plot2



#nodesize-----------------------------------------------------------------------
oob_err_node = double(7)
test_err_node = double(7)

set.seed(100)
for(nodesize in 1:7) {
  rf_m <- randomForest(B_NET ~ . , data = train, nodesize = nodesize,ntree=300) 
  oob_err_node[nodesize] <- rf_m$mse[300] #Errore per tutti gli alberi adattati
  
  pred <- predict(rf_m,test) #Previsioni sul set di test per ciascun albero
  test_err_node[nodesize] <- with(test, mean( (B_NET - pred)^2)) #Mean Squared Error per l'insieme di test
}


test_err_node
oob_err_node

test_err + oob_err

dst_gr <- data_frame(type=c(rep("test", length(test_err_node)), 
                            rep("oob", length(oob_err_node))),
                     nodesize = c(1:length(test_err_node), 1:length(oob_err_node)),
                     error=c(test_err_node, oob_err_node))


ggpt <- ggplot(data = dst_gr, mapping = aes(x=nodesize,y=error)) +
  geom_line(aes(colour=type)) +
  geom_point(aes(colour=type)) + 
  ggtitle("OOB e Errore di test error Vs. Node size")

print(ggpt)

#FIT1---------------------------------------------------------------------------
test_data = data_gr[data_gr$set == "test",]

test_fit1=test_data$fit1
test_BNET = test_data$B_NET

summary(test_fit1)
summary(test_BNET)

df = tibble(x = test_fit1 - test_BNET)
df$value

ggplot(df) + geom_density(aes(x = x),color="darkblue", fill="lightblue") +
  labs(x = 'Valore previsto - valore osservato', y = 'Densità', 
       title = 'Distribuzione errori sul test set')

ggplot(data = data_err, aes(x = error)) +
  geom_histogram(bins = 200, fill = "lightblue" , color = 'blue')+
  labs(title = "Istogramma degli errori", x = "Valore dell'errore", y = "Frequenza")

ggplot(data_err) + geom_density(aes(x = sqrt(error2)),color="darkblue", fill="lightblue") +
  labs(x = 'RMSE', y = 'Densità', 
       title = 'Distribuzione errori sul test set')

mean(sqrt(data_err$error2))

hist((test_fit1 - test_BNET), breaks = 100, freq = F)

data_gr$error = data_gr$fit1 - data_gr$B_NET


data_err = merge(data_gr, data)
data_err = data_err[-id_train,]
data_err[data_err$error == max(data_err$error),]
data_err[data_err$error == min(data_err$error),]
data_err$error2 = data_err$error^2

data_err[order(abs(data_err$error))[1:5], 
         c('TEAM_ABBREVIATION','SEASON','error','fit1','B_NET','POSS','GROUP_NAME')]

data_err[order(abs(data_err$error),decreasing = T)[1:5], 
         c('TEAM_ABBREVIATION','SEASON','error','fit1','B_NET','POSS','GROUP_NAME')]



ggplot(data_err[data_err$POSS>=600,]) + geom_density(aes(x = error),color="darkblue", fill="lightblue") +
  labs(x = 'Valore previsto - valore osservato', y = 'Densità', 
       title = 'Distribuzione errori sul test set per quintetti con più di 600 possessi')


ggplot(data_err[data_err$POSS<600,]) + geom_density(aes(x = error),color="darkblue", fill="lightblue") +
  labs(x = 'Valore previsto - valore osservato', y = 'Densità', 
       title = 'Distribuzione errori sul test set per quintetti con meno di 600 possessi')

ggplot(data_err[]) + geom_density(aes(x = log(POSS)),color="darkblue", fill="lightblue")
  
quantile(data_err$POSS, probs = seq(0,1,0.1))
quantili_poss = c(20,28,31,35,40,46,55,68,94,162,2771)

media_errori = tibble(quantile=c(1:10), media = double(10))
for (i in 2:11){
  media_errori$media[i-1] = mean(data_err[data_err$POSS>quantili_poss[i-1] & 
                                            data_err$POSS<=quantili_poss[i], 
                                          data_err$error2])
}


for (i in 2:11) {
  if (all(c("POSS", "error2") %in% colnames(data_err))) {
    subset_data = data_err[data_err$POSS > quantili_poss[i - 1] & data_err$POSS <= quantili_poss[i], ]
    media_errori$media[i - 1] = mean(abs(subset_data$error))#, na.rm = TRUE)
  } else {
    # Gestisce le colonne mancanti
    print("Una o più colonne mancano in data_err")
  }
}

ggplot(data = media_errori, aes(x = quantile, y = media)) +
  geom_point(size = 2) +
  geom_line()

ggplot(data = data_err, aes(x = POSS, y = abs(error))) +
  geom_point(size = 1) 
#labs(x = "Stagione", y = "Tiri tentati a partita") +
    
#Quantile net-------------------------------------------------------------------
quantile_bnet = quantile(data_err$B_NET, probs = seq(0,1,0.1))
quantili_poss = c(20,28,31,35,40,46,55,68,94,162,2771)

media_errori = tibble(quantile = quantile_bnet, media = double(10))


for (i in 2:11) {
  if (all(c("B_NET", "error2") %in% colnames(data_err))) {
    subset_data = data_err[data_err$B_NET > quantile_bnet[i - 1] & data_err$B_NET <= quantile_bnet[i], ]
    media_errori$media[i - 1] = mean(abs(subset_data$error))#, na.rm = TRUE)
  } else {
    # Gestisce le colonne mancanti
    print("Una o più colonne mancano in data_err")
  }
}

ggplot(data = media_errori, aes(x = quantile, y = media)) +
  geom_point(size = 2) +
  geom_line()

ggplot(data = data_err, aes(x = POSS, y = abs(error))) +
  geom_point(size = 1) 
labs(x = "Stagione", y = "Tiri tentati a partita") +

