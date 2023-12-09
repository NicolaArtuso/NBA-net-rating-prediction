setwd('C:/Users/nicol/OneDrive/Desktop/TESI/team_data')
library(tidyverse)
library(ggplot2)
library(corrplot)
library(plotly)
library(colorr)
library(cartography)
library(ggsci)

#GRUPPI DI DATI ----------------------------------------------------------------
#Advanced stat
adv = as.tibble(read.csv('nba_advanced_data.csv', header=T))
#Tipo di giocata 
type = as.tibble(read.csv('nba_play_type_data.csv', header=T))
#Punti su 100 possessi per tipo di punto
eff = as.tibble(read.csv('nba_efficiency_data.csv', header=T))
eff = eff %>% select(-ends_with("PCT"))
eff = eff %>% select(-c('GP','W','L','MIN'))

#Tiro su 100 possessi
shot = as.tibble(read.csv('nba_shot_data.csv', header=T))
shot = shot %>% select(- Backcourt)

#Tiri subiti su 100 possessi
opp = as.tibble(read.csv('nba_oppshot_data.csv', header=T))
opp = opp %>% mutate(Opp_RA = Restricted.Area,
                     Opp_ITP = In.The.Paint..Non.RA.,
                     Opp_Mid = Mid.Range,
                     Opp_LC3 = Left.Corner.3,
                     Opp_RC3 = Right.Corner.3,
                     Opp_AB3 = Above.the.Break.3,
                     Opp_C3 = Corner.3)
opp = opp %>%  select(-c(Restricted.Area, In.The.Paint..Non.RA.,Mid.Range,
                         Left.Corner.3, Right.Corner.3, Above.the.Break.3,
                         Backcourt, Corner.3))


#Statistiche tradizionali su 100 possessi
stat_percento = read.csv('nba_traditional_PER100_data.csv',header=T)
stat_percento = stat_percento[,c(1:28,56)]
stat_percento = stat_percento[,-c(4,5,6,7)]
stat_percento = stat_percento %>% select(-ends_with("PCT"))
stat_percento = stat_percento %>% select(-c('MIN','FGM','FG3M','FTM','PTS'))


#MERGE--------------------------------------------------------------------------
tot = merge(eff,opp)
tot = merge(tot,shot)
tot = merge(tot,type)
tot = merge(tot, stat_percento, by.x = c('TEAM_ID','SEASON','X','TEAM_NAME'),
            by.y =c('TEAM_ID','SEASON','X','TEAM_NAME') )
tot = merge(tot,adv)
str(tot)
tot = tot %>%  select(-'SEASON_ID')
write.csv(tot, file = 'team_all_data.csv')

#ANALISI -----------------------------------------------------------------------
datac = as.tibble(read.csv('team_all_data.csv', header=T))
datac = datac %>% mutate(id = X.1) %>% select(-X.1)

#TIRI DA TRE

top_Three = datac %>% 
  group_by(SEASON) %>% 
  filter(row_number(desc(Corner.3 + Above.the.Break.3)) == 1)

low_Three = datac %>% 
  group_by(SEASON) %>% 
  filter(row_number(desc(Corner.3 + Above.the.Break.3)) == 30)

mean_three <- datac %>%
  group_by(SEASON) %>%
  summarize(FG3A = mean(Corner.3 + Above.the.Break.3))

ggplot(datac, aes(SEASON, FG3A)) + 
  labs(x = 'Stagione', y = 'Tiri da 3 tentati' )+
  geom_boxplot(aes(group = SEASON))+
  geom_label(aes(label = TEAM_ABBREVIATION), data = top_Three) +
  geom_label(aes(label = TEAM_ABBREVIATION), data = low_Three) #+
  geom_line(data = mean_three, aes(group = 1), color = "red")


#TIRI DALLA MEDIA
top_mid = datac %>% 
  group_by(SEASON) %>% 
  filter(row_number(desc(Mid.Range)) == 1)

low_mid = datac %>% 
  group_by(SEASON) %>% 
  filter(row_number(desc(Mid.Range)) == 30)

mean_mid <- datac %>%
  group_by(SEASON) %>%
  summarize(Mid.Range = mean(Mid.Range))

ggplot(datac, aes(SEASON, Mid.Range)) + 
  labs(x = 'Stagione', y = 'Tiri dalla media tentati' )+
  geom_boxplot(aes(group = SEASON))+
  geom_label(aes(label = TEAM_ABBREVIATION), data = top_mid) +
  geom_label(aes(label = TEAM_ABBREVIATION), data = low_mid) #+
  geom_line(data = mean_mid, aes(group = 1), color = "red")



#TIRI RESTRICTED AREA
top_RA = datac %>% 
  group_by(SEASON) %>% 
  filter(row_number(desc(Restricted.Area)) == 1)

low_RA = datac %>% 
  group_by(SEASON) %>% 
  filter(row_number(desc(Restricted.Area)) == 30)

mean_RA <- datac %>%
  group_by(SEASON) %>%
  summarize(Restricted.Area = mean(Restricted.Area))


ggplot(datac, aes(SEASON, Restricted.Area)) + 
  labs(x = 'Stagione', y = 'Tiri in restricted area tentati' )+
  geom_boxplot(aes(group = SEASON))+
  geom_label(aes(label = TEAM_ABBREVIATION), data = top_RA) +
  geom_label(aes(label = TEAM_ABBREVIATION), data = low_RA) #+
  geom_line(data = mean_RA, aes(group = 1), color = "red")


#TIRI LIBERI
top_ft = datac %>% 
  group_by(SEASON) %>% 
  filter(row_number(desc(FTA)) == 1)

low_ft = datac %>% 
  group_by(SEASON) %>% 
  filter(row_number(desc(FTA)) == 30)

mean_ft <- datac %>%
  group_by(SEASON) %>%
  summarize(FTA = mean(FTA))


ggplot(datac, aes(SEASON, FTA)) + 
  labs(x = 'Stagione', y = 'Tiri liberi tentati')+
  geom_boxplot(aes(group = SEASON))+
  geom_label(aes(label = TEAM_ABBREVIATION), data = top_ft) +
  geom_label(aes(label = TEAM_ABBREVIATION), data = low_ft) #+
  geom_line(data = mean_ft, aes(group = 1), color = "red")

#PACE
top_pace = datac %>% 
  group_by(SEASON) %>% 
  filter(row_number(desc(PACE)) == 1)

low_pace = datac %>% 
  group_by(SEASON) %>% 
  filter(row_number(desc(PACE)) == 30)

mean_pace <- datac %>%
  group_by(SEASON) %>%
  summarize(PACE = mean(PACE))


ggplot(datac, aes(SEASON, PACE)) + 
  labs(x = 'Stagione', y = 'PACE' )+
  geom_boxplot(aes(group = SEASON))+
  geom_label(aes(label = TEAM_ABBREVIATION), data = top_pace) +
  geom_label(aes(label = TEAM_ABBREVIATION), data = low_pace) #+
  geom_line(data = mean_pace, aes(group = 1), color = "red")

#Confronto 3 pt, mid range e RA
medie_tiri <- datac %>%
  group_by(SEASON) %>%
  summarize(Media_FG3A = mean(Corner.3 + Above.the.Break.3),
            Media_MidRange = mean(Mid.Range),
            Media_RA = mean(Restricted.Area))

# Crea il grafico delle medie dei tiri per ciascuna variabile
ggplot(data = medie_tiri, aes(x = SEASON)) +
  geom_point(aes(y = Media_FG3A, color = "Tiro da 3 punti"), size = 3) +
  geom_line(aes(y = Media_FG3A, group = 1, color = "Tiro da 3 punti")) +
  
  geom_point(aes(y = Media_MidRange, color = "Mid Range"), size = 3) +
  geom_line(aes(y = Media_MidRange, group = 1, color = "Mid Range")) +
  
  geom_point(aes(y = Media_RA, color = "Restricted Area"), size = 3) +
  geom_line(aes(y = Media_RA, group = 1, color = "Restricted Area")) +
  
  labs(x = "Stagione", y = "Tiri tentati a partita") +
  
  scale_color_manual(values = c("Tiro da 3 punti" = "black", "Mid Range" = "red",
                                "Restricted Area" = "blue"),'Tipo di tiro') +
  
  theme(legend.position = "top")  # Posiziona la legenda in alto



#PLAYTYPE

medie_type = datac %>% 
  group_by(SEASON) %>%
  summarize(Iso = mean(Isolation),
            Trans = mean(Transition),
            PRM = mean(PRRollman),
            Cut = mean(Cut),
            PRBH = mean(PRBallHandler),
            Post = mean(Postup),
            Spot = mean(Spotup),
            HO = mean(Handoff),
            OScreen = mean(OffScreen),
            OReb = mean(OffRebound),
            Misc = mean(Misc))


df_long <- gather(medie_type, key = "Tipo", value = "Valore", -SEASON)

# Crea il barplot
ggplot(df_long, aes(x = Tipo, y = Valore, fill = Tipo)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values= pal_d3('category20b') (11))+
  labs(
    x = "Giocata",
    y = "Valore") +
  facet_wrap(~SEASON)#, scales = 'free')

#PUNTI GENERATI
medie_points = datac %>% 
  group_by(SEASON) %>%
  summarize(Drive = mean(DRIVE_PTS/POINTS),
            Catch = mean(CATCH_SHOOT_PTS/POINTS),
            PullUp = mean(PULL_UP_PTS/POINTS),
            Paint = mean(PAINT_TOUCH_PTS/POINTS),
            Post = mean(POST_TOUCH_PTS/POINTS),
            Elbow = mean(ELBOW_TOUCH_PTS/POINTS))


df_long2 <- gather(medie_points, key = "Tipo", value = "Valore", -SEASON)

# Crea il barplot
ggplot(df_long2, aes(x = Tipo, y = Valore, fill = Tipo)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values= pal_jama()(6))+
  labs(
    x = "Tipo",
    y = "Valore") +
  facet_wrap(~SEASON)
