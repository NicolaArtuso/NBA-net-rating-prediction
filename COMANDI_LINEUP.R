library(tidyverse)
setwd('C:/Users/nicol/OneDrive/Desktop/TESI/lineup_data')


dfl = as.tibble(read.csv('nba_lineups_data.csv', header=T))
dfl = dfl %>% select(c('X','GROUP_ID','GROUP_NAME', 'TEAM_ABBREVIATION','GP',
                     'SEASON','NET_RATING','POSS'))
dfl$id = c(1:16000)
dfl = dfl %>% select(-X)


ggplot(dfl) + geom_density(aes(x = POSS),color="darkblue", fill="lightblue") +
  labs(x = 'Possessi', y = 'Densità', title = 'Distribuzione possessi giocati')

x = merge(dfl,dft)
x$team_prob = x$prob
x = x %>% select(-prob)  

x$B_NET = ifelse(x$POSS >= 600,
                 x$NET_RATING, 
                 x$POSS/600 * x$NET_RATING + (1-x$POSS/600)*x$team_NET)

ggplot(x) + geom_density(aes(x = NET_RATING, 
                             color = "Net rating", 
                             fill = 'Net rating'))+
  geom_density(aes(x = B_NET, color = "Net rating bayesiano", 
                   fill = "Net rating bayesiano"), alpha = 0.5)+
  labs(x = 'Rating', y = 'Densità', title = 'Confronto net ratings')+
  scale_color_manual(values = c("Net rating" = "darkblue", 
                                "Net rating bayesiano" = "orange")) +
  scale_fill_manual(values = c("Net rating" = "lightblue", 
                               "Net rating bayesiano" = "yellow")) +
  guides(color = guide_legend(title = "Tipo di net rating"), 
         fill = guide_legend(title = "Tipo di net rating"))


nrow(x[x$SEASON == '2022-23',])
nrow(x[x$POSS>= 600,])/ nrow(x)



x$player_prob = matrix(0, ncol = 9, nrow = nrow(x))


for (i in 1:nrow(x)) {
  players = unlist(strsplit(x$GROUP_ID[i], "-"))
  player_prob_sum <- rep(0,9)  # Inizializza a 0
  
  for (j in 2:6) {
    season = x$SEASON[i]
    player = players[j]
    # Trova il punteggio del giocatore dal dataframe dfp
    matching_rows <- dfp[dfp$SEASON == season & dfp$PLAYER_ID == player, ]
    
    if (nrow(matching_rows) > 0) {
      player_prob_sum <- matching_rows$prob + player_prob_sum
    }
  }
  
  x$player_prob[i,] <- round(player_prob_sum,3) #Assegna la somma al dataframe x
}

x$somma =  apply(x$player_prob, 1, sum)
x = subset(x, somma > 4.5)

rm(dfl,dfp,dft,i,j,player_prob_sum,season,player_matching_rows)

write.csv(x,'soft_lineups.csv')
