setwd('C:/Users/nicol/OneDrive/Desktop/TESI/player_data')
library(tidyverse)

p1 = as.tibble(read.csv('nba_player1_PERGAME_data.csv', header=T))
p2 = as.tibble(read.csv('nba_player2_PER100_data.csv', header=T))
p3 = as.tibble(read.csv('nba_player3_PER100_data.csv', header=T))
p4 = as.tibble(read.csv('nba_player4_PERGAME_data.csv', header=T))
adv = as.tibble(read.csv('nba_player_adv_PER100_data.csv', header=T))

df = tibble(PLAYER_ID = p1$PLAYER_ID,
            PLAYER_NAME = p1$PLAYER_NAME,
            TEAM_ID = p1$TEAM_ID,
            TEAM_ABBREVIATION = p1$TEAM_ABBREVIATION,
            AGE = p1$AGE,
            SEASON = p1$SEASON,
            GP = p1$GP,
            HEIGHT = p1$PLAYER_HEIGHT_INCHES,
            OREB_PCT = adv$OREB_PCT,
            DREB_PCT = adv$DREB_PCT,
            AST_PCT = adv$AST_PCT,
            STL_PCT = p3$STL,
            BLOCK_PCT = p3$BLK, 
            TOV_PCT = p3$TOV,
            POINTS = p1$PTS*p1$GP*100/adv$POSS,
            USG_PCT = adv$USG_PCT,
            PIE = adv$PIE,
            FT_RATE = p3$FTM/p3$FGA,
            FT_PCT = p3$FT_PCT,
            FGA = p3$FGA,
            TWO_FG_PCT = (p3$FGM-p3$FG3M)/(p3$FGA-p3$FG3A),
            THREE_FG_PCT = p3$FG3_PCT,
            TWO_FG_AST_PCT = p2$PCT_AST_2PM,
            THREE_FGA_PCT = p2$PCT_FGA_3PT,
            CORNER3 = p4$Corner.3*p1$GP/(p2$FGA),
            THREE_FG_AST_PCT = p2$PCT_AST_3PM,
            RA = p4$Restricted.Area*p1$GP/(p2$FGA),
            ITP = p4$In.The.Paint..Non.RA.*p1$GP/(p2$FGA),
            MID = p4$Mid.Range*p1$GP/(p2$FGA)
)

write.csv(df, file = 'players_complete_data.csv')
df = read.csv('players_complete_data.csv')

ggplot(df, aes(x = GP)) +
  labs(x='Partite giocate', y = 'Numero giocatori')+
  geom_bar(fill = 'lightblue') #+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

df1 = df[df$GP>=30,]

ggplot(df1) + geom_density(aes(x = HEIGHT),color="darkblue", fill="lightblue") +
  labs(x = 'Altezza', y = 'Densità', title = 'Distribuzione altezza')


df1$HEIGHT1 = df1$HEIGHT
df1$HEIGHT1[df1$HEIGHT1 >= 85] = 85
df1$HEIGHT1[df1$HEIGHT1 <= 72] = 72

ggplot(df1, aes(x = HEIGHT1)) +
  labs(x='Altezza in pollici', y = 'Numero giocatori')+
  geom_bar(fill = 'lightblue') +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

#PUNTI -------------------------------------------------------------------------
top_points = df1 %>% 
  group_by(HEIGHT1) %>% 
  filter(row_number(desc(POINTS)) == 1)

mean_points <- df1 %>%
  group_by(HEIGHT1) %>%
  summarize(POINTS = mean(POINTS))

ggplot(df1, aes(HEIGHT1, POINTS)) + 
  labs(x = 'Altezza in pollici', y = 'Punti per 100 possessi' )+
  geom_boxplot(aes(group = HEIGHT1))+
  geom_label(aes(label = PLAYER_NAME), data = top_points) #+
  geom_line(data = mean_points, aes(group = 1), color = "red")

#USG----------------------------------------------------------------------------
top_usg = df1 %>% 
  group_by(HEIGHT1) %>% 
  filter(row_number(desc(USG_PCT)) == 1)

mean_usg <- df1 %>%
  group_by(HEIGHT1) %>%
  summarize(USG_PCT = mean(USG_PCT))

ggplot(df1, aes(HEIGHT1, USG_PCT)) + 
  labs(x = 'Altezza in pollici', y = 'Usage%' )+
  geom_boxplot(aes(group = HEIGHT1)) +
  geom_label(aes(label = PLAYER_NAME), data = top_usg) #+
  geom_line(data = mean_usg, aes(group = 1), color = "red")


#ASSIST ------------------------------------------------------------------------
top_assist = df1 %>% 
  group_by(HEIGHT1) %>% 
  filter(row_number(desc(AST_PCT)) == 1)

mean_assist <- df1 %>%
  group_by(HEIGHT1) %>%
  summarize(AST_PCT = mean(AST_PCT))


ggplot(df1, aes(x = HEIGHT1, y = AST_PCT)) + 
  labs(x = 'Altezza', y = 'Assist%' )+
  geom_boxplot(aes(group = HEIGHT1)) +
  geom_label(aes(label = PLAYER_NAME), data = top_assist) 
  geom_line(data = mean_assist, aes(group = 1), color = "red")

#RIMBALZI ----------------------------------------------------------------------
df1$TREB_PCT = df1$OREB_PCT + df1$DREB_PCT

top_reb = df1 %>% 
  group_by(HEIGHT1) %>% 
  filter(row_number(desc(OREB_PCT+DREB_PCT)) == 1)

mean_reb <- df1 %>%
  group_by(HEIGHT1) %>%
  summarize(TREB_PCT = mean(OREB_PCT+DREB_PCT))


ggplot(df1, aes(HEIGHT1, TREB_PCT))+  
  labs(x = 'Altezza', y = 'Reb%' )+
  geom_boxplot(aes(group = HEIGHT1)) +
  geom_label(aes(label = PLAYER_NAME), data = top_reb) #+
  geom_line(data = mean_reb, aes(group = 2), color = "red")

#RUBATE-------------------------------------------------------------------------
top_stl = df1 %>% 
  group_by(HEIGHT1) %>% 
  filter(row_number(desc(STL_PCT)) == 1)

mean_stl <- df1 %>%
  group_by(HEIGHT1) %>%
  summarize(STL_PCT = mean(STL_PCT))

ggplot(df1, aes(HEIGHT1, STL_PCT)) + 
  labs(x = 'Altezza', y = 'Steal%' )+
  geom_boxplot(aes(group = HEIGHT1))+
  geom_label(aes(label = PLAYER_NAME), data = top_stl) #+
  geom_line(data = mean_stl, aes(group = 1), color = "red")


#%tENTATI DA TRE PUNTI ---------------------------------------------------------
top_three = df1 %>% 
  group_by(HEIGHT1) %>% 
  filter(row_number(desc(THREE_FGA_PCT)) == 1)

mean_three <- df1 %>%
  group_by(HEIGHT1) %>%
  summarize(THREE_FGA_PCT = mean(THREE_FGA_PCT))

ggplot(df1, aes(HEIGHT1, THREE_FGA_PCT)) + 
  labs(x = 'Altezza', y = '3FGA%' )+
  geom_boxplot(aes(group = HEIGHT1)) +
  geom_label(aes(label = PLAYER_NAME), data = top_three) #+
  geom_line(data = mean_three, aes(group = 1), color = "red")

#PIE ---------------------------------------------------------------------------
top_pie = df1 %>% 
  group_by(HEIGHT1) %>% 
  filter(row_number(desc(PIE)) == 1)

mean_pie <- df1 %>%
  group_by(HEIGHT1) %>%
  summarize(PIE = mean(PIE))

ggplot(df1, aes(HEIGHT1, PIE)) + 
  labs(x = 'Altezza', y = 'PIE' )+
  geom_boxplot(aes(group = HEIGHT1))+
  geom_label(aes(label = PLAYER_NAME), data = top_pie) #+
  geom_line(data = mean_pie, aes(group = 1), color = "red")

  
#-------------------------------------------------------------------------------

  