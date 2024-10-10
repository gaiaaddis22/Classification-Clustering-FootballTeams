# FONTE DATABASE: https://www.kaggle.com/datasets/technika148/football-database

# Import delle librerie necessarie per far funzionare il codice
library(tidyverse)
library(GGally)
library(ggcorrplot)
library(gridExtra)
library(grid)
library(Rmixmod)
library(mclust)
library(mixtools)
library(cowplot)
library(ggpubr)

appearances <- read_csv("~/Desktop/Football Database/appearances.csv")
#games <- read_csv("~/Desktop/Football Database/games.csv")
leagues <- read_csv("~/Desktop/Football Database/leagues.csv")
#players <- read_csv("~/Desktop/Football Database/players.csv")
#shots <- read_csv("~/Desktop/Football Database/shots.csv")
teams <- read_csv("~/Desktop/Football Database/teams.csv")
teamstats <- read_csv("~/Desktop/Football Database/teamstats.csv")

####################### 1. GESTIONE DEL DATABASE E CREAZIONE DEL DATASET ##############################

# Obiettivo: creare un dataset contentente le principali statistiche descrittive sulla 
# performance di una squadra in una determinata stagione calcistica.

# Per farlo, utilizzeremo 4 delle 7 tabelle disponibili nel database "Football Database"
# Rispettivamente: teamstats, teams, leagues & apparances

#### NA ####

sum(is.na(teamstats)) # 1 solo NA
which(apply(is.na(teamstats),2,any)) # 14: yellowCards (colonna NA)
which(is.na(teamstats$yellowCards)) # 8281 (riga NA)

teamstats[8281,] 
# L'NA è relativo al numero di cartellini gialli dati alla Roma nella partita del 2 Marzo 2015 contro la Juventus.
# Essendo un'informazione facilmente reperibile su internet imputiamo il reale valore dei cartellini mancanti (4):

teamstats[8281,14] <- 4 # 4 cartellini gialli 

# Fonte: https://www.google.com/search?q=2+marzo+2015+roma&rlz=1C5CHFA_enIT830IT830&oq=2+marzo+2015+ro&aqs=chrome.0.69i59j69i57.2413j0j7&sourceid=chrome&ie=UTF-8#sie=m;/g/11f53r4c4d;2;/m/03zv9;ln;fp;1;;;
# Nota: se si prende un cartellino rosso per somma di 2 gialli, quei gialli non vengono conteggiati come ammonizione

sum(is.na(teams)) # 0
sum(is.na(appearances)) # 0 
sum(is.na(leagues)) # 0

##### IMPUTAZIONE NOMI SQUADRE E NOMI CAMPIONATI #####

# Il dataset teamstats presenta soltanto gli ID della partita e della squadra, ma noi vogliamo 
# anche il nome della squadra e del campionato in cui milita

# Per farlo è necessario effettuare alcuni merge con altri dataset disponibili nel db, in particolare:

# Uniamo teamstats e teams (dove sono contenuti i nomi delle squadre) 
# attraverso la variabile TeamID così da inserire il nome della squadra nel dataset teamstats

teamstats <- full_join(teamstats,teams) %>%
  rename(team_name=name)

# Creiamo apparances2 inventando una statistica fittizia per poter usare il comando group_by

appearances2 <- appearances %>%
  group_by(gameID,leagueID) %>%
  summarise(tot_key_passes = sum(keyPasses)) 

# Uniamo teamstats e appearances2 per ottenere leagueID in teamstats
teamstats <-  appearances2 %>%
  full_join(teamstats)

# Uniamo ora teamstats e leagues (dove sono contenuti i nomi delle leghe) per ottenere il nome della lega in teamstats

teamstats <- full_join(teamstats,leagues) %>%
  rename(league_name=name)

#### CREAZIONE DEL DATASET FINALE #### 

# Creiamo la variabile points, dove assegnamo 3 punti per la vittoria, 1 per il pareggio e 0 per la sconfitta

points <- ifelse(teamstats$result == "W", 3, ifelse(teamstats$result == "D", 1, 0))
teamstats <- cbind(teamstats,points = points)

# Creiamo un indicatore di accuratezza del tiro, dato da: shotsOnTarget / shots
# (i.e. : se l'Inter in una partita ha fatto 10 tiri di cui 2 nello specchio della porta, 
# l'indicatore assumerà il valore 0.2)

sum(is.na(teamstats)) # 0

teamstats %>%
  filter(shots == 0) # solo 10 partite in cui la squadra non ha tirato (importante da valutare essendo al denominatore)

shots_accuracy <- teamstats$shotsOnTarget/teamstats$shots 
shots_accuracy <- ifelse(teamstats$shots !=0  ,shots_accuracy,0)
teamstats <- cbind(teamstats,shots_accuracy=shots_accuracy)

# Creiamo la variabile con il numero totale dei cartellini (gialli + rossi)

teamstats <- teamstats %>%
  mutate(Cards = redCards+yellowCards)

# Creiamo le statistiche descrittive di performance per ogni squadra e per ogni stagione disponibile

stats_summary <- teamstats %>%
  group_by(team_name, season, league_name) %>%
  summarise(tot_points = sum(points),
            tot_goals = sum(goals),
            tot_x_goals = sum(xGoals),
            mean_shots_acc = mean(shots_accuracy),
            tot_deep = sum(deep),
            mean_ppda = mean(ppda),
            tot_fouls = sum(fouls),
            tot_corners = sum(corners),
            tot_cards = sum(Cards))

# Ordiniamo il dataset per stagione, lega e punti così da ottenere le classifiche dei campionati
stats_summary <- stats_summary %>%
  arrange(season, league_name, desc(tot_points))

# Per attribuire la posizione in classifica alle squadre creiamo un dataset con le posizioni possibili
# da 1 a 20 per tutti i campionati, tranne per la Bundesliga, la quale ha solo 18 squadre
position <- rep(c(1:18, rep(1:20,4)),7)
stats_summary <- cbind(stats_summary,position = position)

# Creiamo la variabile target che utilizzeremo per la classificazione 

# La target è suddivisa in 4 categorie: HS, MHS, MLS, LS
# dove: HS = alta classifica, MHS = medio alta classifica, MLS = medio bassa classifica e 
# LS = bassa classifica 

# Dividiamo tutti i campionati, eccetto la Bundesliga, in fasce da 5 squadre (20/5 = 4).
# La Bundesliga viene divisa in 4-5-5-4 squadre

stats_summary_notB <- stats_summary %>%
  filter(league_name != "Bundesliga") %>%
  mutate(stand_pos = ifelse(position %in% 1:5, "HS", 
                            ifelse(position %in% 6:10, "MHS",
                                   ifelse(position %in% 11:15, "MLS", "LS"))))

stats_summary_B <- stats_summary %>%
  filter(league_name == "Bundesliga") %>%
  mutate(stand_pos = ifelse(position %in% 1:4, "HS", 
                            ifelse(position %in% 5:9, "MHS",
                                   ifelse(position %in% 10:14, "MLS", "LS"))))

stats_summary <- rbind(stats_summary_notB,stats_summary_B) %>%
  arrange(season, league_name, desc(tot_points))

# Le osservazioni presentano dei nomi ripetuti più volte 
# (i.e. l'Inter ha sempre militato in serie A, quindi Inter è presente 7 volte nel dataset)
# pertanto, per garantire una maggiore comprensione dei grafici che faremo in seguito,
# assegnamo l'anno del campionato all'osservazione (i.e. Inter -> Inter__2014, Inter__2015, ... , Inter__2020)

stats_summary$team_name_season <- paste(stats_summary$team_name, substr(stats_summary$season, 1, 4), sep = "__")
stats_summary <- stats_summary %>%
  select(team_name_season, everything())

# Abbiamo ora ottenuto il dataset su cui lavorare

################################ 2. ANALISI ESPLORATIVA #################################### 

sum(is.na(stats_summary)) # 0 
str(stats_summary)
stats_summary$season <- as.character(stats_summary$season)

# Correlazioni e Stime di Densità Non Parametriche

ggpairs(stats_summary[,c(5:13)], ggplot2::aes(alpha=0.5,color=stats_summary$stand_pos))+
  scale_colour_manual(values=c("forestgreen","red2","yellow3","orange2"))+
  scale_fill_manual(values=c("forestgreen","red2","yellow3","orange2"))+
  theme(panel.background = element_rect(fill = "#F5F5F5"))

# Esiste una forte correlazione fra tot_goals e x_tot_goals (expected tot goals)
# Rimuoviamo dal dataset x_tot_goals che risulta ridondante, infatti
# x_tot_goals rappresenta la stima dei gol che una squadra potrebbe segnare nella stagione 

stats_summary <- stats_summary[,-7]

# Matrice di correlazione

stats_quant <- stats_summary[,5:12] # dataset con le sole variabile quantitative
correlations <- round(cor(stats_quant),3)

ggcorrplot(correlations, type = "upper", lab = T, colors = c("red3","white","green4"),
           title = "Correlation Matrix") + 
  theme(plot.title = element_text(size=22, color="black", face = "bold",family = "Tahoma"), axis.text.x = element_text(angle = 90)) + 
  labs(x= " " , y = " ") 

# Commento:
# Dalla matrice di correlazione si nota che tot_cards e tot_fouls sono le variabili meno correlate 
# con le altre, mentre tot_goals e tot_points è la coppia di variabili più correlata,
# questo risultato non è sorprendente, infatti le squadre che segnano di più tendenzialmente vincono anche di più

# Istogrammi e stime di densità non parametriche per le variabili quantitative (non differenziate per stand_pos)

g1 <- ggplot(stats_summary, mapping = aes(x=tot_points))+
  geom_histogram(aes(y=after_stat(density)), fill = "darkgreen" , color = "black", bins = 20, alpha = 0.7) +
  geom_density(alpha = 0.5, fill = "lightgreen", kernel = "gaussian") +
  theme_test()
g2 <- ggplot(stats_summary, mapping = aes(x=tot_goals))+
  geom_histogram(aes(y=after_stat(density)), fill = "darkgreen" , color = "black", bins = 20, alpha = 0.7) +
  geom_density(alpha = 0.5, fill = "lightgreen", kernel = "gaussian") +
  labs(y=" ") +
  theme_test()
g3 <- ggplot(stats_summary, mapping = aes(x=mean_shots_acc))+
  geom_histogram(aes(y=after_stat(density)), fill = "darkgreen" , color = "black", bins = 20, alpha = 0.7) +
  geom_density(alpha = 0.5, fill = "lightgreen", kernel = "gaussian") +
  labs(y=" ") +
  theme_test()
g4 <- ggplot(stats_summary, mapping = aes(x=tot_deep))+
  geom_histogram(aes(y=after_stat(density)), fill = "darkgreen" , color = "black", bins = 20, alpha = 0.7) +
  geom_density(alpha = 0.5, fill = "lightgreen", kernel = "gaussian") +
  labs(y=" ") +
  theme_test()
g5 <- ggplot(stats_summary, mapping = aes(x=mean_ppda))+
  geom_histogram(aes(y=after_stat(density)), fill = "darkgreen" , color = "black", bins = 20, alpha = 0.7) +
  geom_density(alpha = 0.5, fill = "lightgreen", kernel = "gaussian") +
  theme_test()
g6 <- ggplot(stats_summary, mapping = aes(x=tot_fouls))+
  geom_histogram(aes(y=after_stat(density)), fill = "darkgreen" , color = "black", bins = 20, alpha = 0.7) +
  geom_density(alpha = 0.5, fill = "lightgreen", kernel = "gaussian") +
  labs(y=" ") +
  theme_test()
g7 <- ggplot(stats_summary, mapping = aes(x=tot_corners))+
  geom_histogram(aes(y=after_stat(density)), fill = "darkgreen" , color = "black", bins = 20, alpha = 0.7) +
  geom_density(alpha = 0.5, fill = "lightgreen", kernel = "gaussian") +
  labs(y=" ") +
  theme_test()
g8 <- ggplot(stats_summary, mapping = aes(x=tot_cards))+
  geom_histogram(aes(y=after_stat(density)), fill = "darkgreen" , color = "black", bins = 20, alpha = 0.7) +
  geom_density(alpha = 0.5, fill = "lightgreen", kernel = "gaussian") +
  labs(y=" ") +
  theme_test()

window1 <- arrangeGrob(g1, g2, g3, g4, ncol = 4)
window2 <- arrangeGrob(g5, g6, g7, g8, ncol = 4)

grid.arrange(window1, window2, ncol = 1, top=textGrob("Kernel Density & Histogram : Overlay", gp=gpar(fontsize=22,font=8)))

# Commento:
# Valutiamo la distribuzione delle variabili in esame attraverso gli istogrammi
# ai quali viene sovrapposta la stima non parametrica della densità utilizzando un kernel gaussiano.
# Le variabili mean_shots_acc e mean_ppda appaiono unimodali a primo impatto, mentre le restanti variabili 
# quantitative mostrano multimodalità anche se leggera. Soltanto la variabile tot_fouls mostra una 
# bimodalità più marcata.

# Istogrammi e stime di densità non parametriche per le variabili quantitative (differenziate per stand_pos)

# tot_points
ggplot(stats_summary, mapping = aes(x=tot_points))+
  geom_histogram(aes(y=after_stat(density), fill = stand_pos),color = "black", bins = 20, alpha = 0.9) +
  geom_density(aes(fill = stand_pos),alpha = 0.4, kernel = "gaussian", size = 0.7) +
  facet_wrap(~stand_pos) +
  theme(strip.background = element_rect(fill = "white"))  +
  scale_fill_manual(values=c("forestgreen","red2","yellow2","orange2"))

# tot_goals
ggplot(stats_summary, mapping = aes(x=tot_goals))+
  geom_histogram(aes(y=after_stat(density), fill = stand_pos),color = "black", bins = 20, alpha = 0.9) +
  geom_density(aes(fill = stand_pos),alpha = 0.4, kernel = "gaussian", size = 0.7) +
  facet_wrap(~stand_pos) +
  theme(strip.background = element_rect(fill = "white"))  +
  scale_fill_manual(values=c("forestgreen","red2","yellow2","orange2"))

# mean_shots_acc
ggplot(stats_summary, mapping = aes(x=mean_shots_acc))+
  geom_histogram(aes(y=after_stat(density), fill = stand_pos),color = "black", bins = 20, alpha = 0.9) +
  geom_density(aes(fill = stand_pos),alpha = 0.4, kernel = "gaussian", size = 0.7) +
  facet_wrap(~stand_pos) +
  theme(strip.background = element_rect(fill = "white")) +
  scale_fill_manual(values=c("forestgreen","red2","yellow2","orange2"))

# tot_deep
ggplot(stats_summary, mapping = aes(x=tot_deep))+
  geom_histogram(aes(y=after_stat(density), fill = stand_pos),color = "black", bins = 20, alpha = 0.9) +
  geom_density(aes(fill = stand_pos),alpha = 0.4, kernel = "gaussian", size = 0.7) +
  facet_wrap(~stand_pos) +
  theme(strip.background = element_rect(fill = "white"))  +
  scale_fill_manual(values=c("forestgreen","red2","yellow2","orange2"))

# mean_ppda
ggplot(stats_summary, mapping = aes(x=mean_ppda))+
  geom_histogram(aes(y=after_stat(density), fill = stand_pos),color = "black", bins = 20, alpha = 0.9) +
  geom_density(aes(fill = stand_pos),alpha = 0.4, kernel = "gaussian", size = 0.7) +
  facet_wrap(~stand_pos) +
  theme(strip.background = element_rect(fill = "white"))  +
  scale_fill_manual(values=c("forestgreen","red2","yellow2","orange2"))

# tot_fouls
ggplot(stats_summary, mapping = aes(x=tot_fouls))+
  geom_histogram(aes(y=after_stat(density), fill = stand_pos),color = "black", bins = 20, alpha = 0.9) +
  geom_density(aes(fill = stand_pos),alpha = 0.4, kernel = "gaussian", size = 0.7) +
  facet_wrap(~stand_pos) +
  theme(strip.background = element_rect(fill = "white"))  +
  scale_fill_manual(values=c("forestgreen","red2","yellow2","orange2"))

# tot_corners
ggplot(stats_summary, mapping = aes(x=tot_corners))+
  geom_histogram(aes(y=after_stat(density), fill = stand_pos),color = "black", bins = 20, alpha = 0.9) +
  geom_density(aes(fill = stand_pos),alpha = 0.4, kernel = "gaussian", size = 0.7) +
  facet_wrap(~stand_pos) +
  theme(strip.background = element_rect(fill = "white")) +
  scale_fill_manual(values=c("forestgreen","red2","yellow2","orange2"))

# tot_cards
ggplot(stats_summary, mapping = aes(x=tot_cards))+
  geom_histogram(aes(y=after_stat(density), fill = stand_pos),color = "black", bins = 20, alpha = 0.9) +
  geom_density(aes(fill = stand_pos),alpha = 0.4, kernel = "gaussian", size = 0.7) +
  facet_wrap(~stand_pos) +
  theme(strip.background = element_rect(fill = "white")) +
  scale_fill_manual(values=c("forestgreen","red2","yellow2","orange2"))

# Commento:
# Da questi grafici si nota che le variabili che risultano discriminare maggiormente per stand_pos
# sono: tot_points, tot_goals, mean_shots_acc, tot_deep e tot_corners.
# Questo aspetto verrà analizzato meglio successivamente tramite i boxplot condizionati

# Boxplot condizionati a stand_pos

# boxplot per ogni classe di stand_pos con tot_points
gg1 <- ggplot(stats_summary, mapping = aes(x=tot_points, fill = stand_pos))+
  geom_boxplot() +
  theme_test() + 
  coord_flip() +
  #theme(legend.position = "none") +
  scale_fill_manual(values = c("forestgreen","red2","yellow2","orange2"))

# boxplot per ogni classe di stand_pos con tot_goals
gg2 <- ggplot(stats_summary, mapping = aes(x=tot_goals, fill = stand_pos))+
  geom_boxplot()+
  theme_test()+ 
  coord_flip()+
  #theme(legend.position = "none")+
  scale_fill_manual(values = c("forestgreen","red2","yellow2","orange2"))

# boxplot per ogni classe di stand_pos con mean_shots_acc
gg3 <- ggplot(stats_summary, mapping = aes(x=mean_shots_acc, fill = stand_pos))+
  geom_boxplot()+
  theme_test()+ 
  coord_flip()+
  #theme(legend.position = "none")+
  scale_fill_manual(values = c("forestgreen","red2","yellow2","orange2"))

# boxplot per ogni classe di stand_pos con tot_deep
gg4 <- ggplot(stats_summary, mapping = aes(x=tot_deep, fill = stand_pos))+
  geom_boxplot()+
  theme_test()+ 
  coord_flip()+
  scale_fill_manual(values = c("forestgreen","red2","yellow2","orange2"))
  #theme(legend.position = "none")

# boxplot per ogni classe di stand_pos con mean_ppda
gg5 <- ggplot(stats_summary, mapping = aes(x=mean_ppda, fill = stand_pos))+
  geom_boxplot()+
  theme_test()+ 
  coord_flip()+
  #theme(legend.position = "none")+
  scale_fill_manual(values = c("forestgreen","red2","yellow2","orange2"))

# boxplot per ogni classe di stand_pos con tot_fouls
gg6 <- ggplot(stats_summary, mapping = aes(x=tot_fouls, fill = stand_pos))+
  geom_boxplot()+
  theme_test()+ 
  coord_flip()+
  #theme(legend.position = "none")+
  scale_fill_manual(values = c("forestgreen","red2","yellow2","orange2"))

# boxplot per ogni classe di stand_pos con tot_corners
gg7 <- ggplot(stats_summary, mapping = aes(x=tot_corners, fill = stand_pos))+
  geom_boxplot()+
  theme_test()+ 
  coord_flip()+
  #theme(legend.position = "none")+
  scale_fill_manual(values = c("forestgreen","red2","yellow2","orange2"))

# boxplot per ogni classe di stand_pos con tot_cards
gg8 <- ggplot(stats_summary, mapping = aes(x=tot_cards, fill = stand_pos))+
  geom_boxplot()+
  theme_test()+ 
  coord_flip()+
  scale_fill_manual(values = c("forestgreen","red2","yellow2","orange2"))
  #theme(legend.position = "none")

# Griglia grafici con titolo e legenda comune
gg_tot <- list(gg1, gg2, gg3, gg4, gg5, gg6, gg7, gg8)
griglia <- ggarrange(plotlist = gg_tot, ncol = 4, nrow = 2, common.legend = TRUE, legend = "right")
titolo <- ggdraw() + draw_label("Conditioned Boxplot to Target", fontface = "bold", x = 0.5, y = 0.5, hjust = 0.5)
gg_titolo <- plot_grid(titolo, griglia, ncol = 1, rel_heights = c(0.1, 0.9))
legenda <- get_legend(griglia)
griglia_legenda <- plot_grid(gg_titolo, legenda, ncol = 2, rel_widths = c(0.8, 0.2), align = "h")
griglia_legenda

# Commento:
# L'analisi grafica dei boxplot condizionati conferma le ipotesi tratte precedentemente dallo studio delle stime
# di densità condizionate delle variabili.
# In particolare si nota che la classe HS risulta essere quella più distante dalle altre classi


############################ 3. CLASSIFICATION #################################

n <- nrow(stats_summary)
set.seed(222)
test.set.labels<-sample(1:n,206) # 70% training set (480/686)
perm = sample(n)
test.set.labels<-perm[480:n]  # 30% test set (206/686)

stats_summary_features <- stats_summary[,5:12]   # features
stats_summary_target <- as.factor(stats_summary$stand_pos) # variabile target
# Servono per il ciclo del CV del MDA

training <- stats_summary[-test.set.labels,]
test <- stats_summary[test.set.labels,]

training_features <- training[,5:12]
training_class <- as.factor(training$stand_pos)

test_features <- test[,5:12]
test_class <- as.factor(test$stand_pos)

prop.table(table(training_class)) # bilanciamento nel training
prop.table(table(test_class)) # bilanciamento nel test
# In entrambi i set le classi sembrano equibilanciate

dataset <- stats_summary[,c(5:12,14)]  # dataset con le features e la classe

# Dividiamo le osservazioni in base alla classe di stand_pos
dataset_HS <- dataset %>%
    filter(stand_pos=="HS") %>%
    select(-stand_pos)

dataset_MHS <- dataset %>%
    filter(stand_pos=="MHS") %>%
    select(-stand_pos)

dataset_MLS <- dataset %>%
    filter(stand_pos=="MLS") %>%
    select(-stand_pos)

dataset_LS <- dataset %>%
    filter(stand_pos=="LS") %>%
    select(-stand_pos)

# Supponendo che le osservazioni rappresentino una mistura di 4 normali 8-variate/multivariate
# Calcoliamo baricentri e matrici di varianze (cioè parametri) di ogni classe

mu_HS <- colMeans(dataset_HS); Sigma_HS <- var(dataset_HS)

mu_MHS <- colMeans(dataset_MHS); Sigma_MHS <- var(dataset_MHS)

mu_MLS <- colMeans(dataset_MLS); Sigma_MLS <- var(dataset_MLS)

mu_LS <- colMeans(dataset_LS); Sigma_LS <- var(dataset_LS)

# K-L simmetryzed distance function

KLs <- function(mu1,mu2,sigma1,sigma2) {
  
  d <- length(mu1)
  
  0.5*t(mu1-mu2)%*%(solve(sigma1)+solve(sigma2))%*%(mu1-mu2)+
    +0.5*sum(diag(sigma1%*%solve(sigma2)+solve(sigma1)%*%sigma2))-d
}


KLs(mu_HS,mu_LS,Sigma_HS,Sigma_LS)     #41.59787
KLs(mu_HS,mu_MLS,Sigma_HS,Sigma_MLS)   #49.08913   
KLs(mu_HS,mu_MHS,Sigma_HS,Sigma_MHS)   #11.73493
KLs(mu_MHS,mu_MLS,Sigma_MHS,Sigma_MLS) #6.679185 
KLs(mu_MHS,mu_LS,Sigma_MHS,Sigma_LS)   #14.19953
KLs(mu_MLS,mu_LS,Sigma_MLS,Sigma_LS)   #7.383853

# gruppi più distanti: HS,MLS
# gruppi meno distanti: MHS,MLS

#### EDDA ####


# itermax <- 10000 , da noi fatto con 10000 giri (computazionalmente pesantissimo)  
itermax <- 1000

# Creiamo models, un dataframe vuoto che conterr? i nomi dei migliori modelli stimati 
# dal classificatore e i rispettivi valori di CV e BIC
models <- data.frame(mod_name=NULL, CV=NULL, BIC=NULL)

for (iter in 1 : itermax){
  
  classif <- mixmodLearn(training_features, 
                         training_class,
                         models=mixmodGaussianModel(family = "all", equal.proportions = F),
                         # solo i modelli con mixing weights variabili, campionamemto NON retrospettivo
                         criterion = c("CV","BIC"))
  # stimiamo EDDA per tutti i 14 modelli di misture di normali
  
  mod <- classif@bestResult@model  # salviamo il nome del modello risultato migliore (con CV minore)
  
  CV <- classif@results[[1]]@criterionValue[1] # salviamo il valore del CV del modello migliore
  
  BIC <- classif@results[[1]]@criterionValue[2]  # salviamo il valore del BIC del modello migliore
  
  models <- rbind(models, data.frame(mod_name=mod, CV=CV, BIC=BIC)) 
  # Inseriamo la nuova osservazione composta dal nome del modello migliore, il suo CV ed il suo BIC.
  # Alla fine models sar? un data frame con 3 colonne ed un numero di osservazioni pari ad itermax
}

# Trasformiamo models in tibble cos? da poter raggruppare il data frame per nome del modello (mod_name).
# Viene riportato solo il CV minimo che si ? ottenuto nelle diverse stime del classificatore per ogni modello.
# Viene riportato anche il BIC, che per? ? identico nelle diverse stime dello stesso modello
(models <- models %>%
    as_tibble() %>%
    group_by(mod_name) %>%
    summarize(CV=min(CV), BIC=min(BIC)) %>%
    arrange(CV))

# Risultati con 10000 giri del ciclo:

#mod_name                 CV    BIC

#1 Gaussian_pk_Lk_Ck     0.203 27222.   VVV aka QDA 
#2 Gaussian_pk_Lk_C      0.207 27003.   VEE
#3 Gaussian_pk_L_Ck      0.211 27265.   EVV
#4 Gaussian_pk_Lk_D_Ak_D 0.213 26924.   VVE
#5 Gaussian_pk_L_D_Ak_D  0.215 36732.   EVE
#6 Gaussian_pk_L_C       0.232 27077.   LDA

# Si nota che QDA ha CV pi? basso (0.203), mentre il BIC privilegia VVE.


# Grafici andamento CV e BIC al variare dei modelli

graph1 <- ggplot(models, aes(x=mod_name, y=CV, group=1)) +
  geom_vline(aes(xintercept = mod_name[1]), colour="olivedrab3", linetype=2, linewidth=.6)+
  geom_line(linewidth=1,color="olivedrab3")+
  geom_point(size=3,color="darkgreen")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="", title="TOP Classification Models based on CV")+
  theme(plot.title = element_text(hjust = 0, size = 14, face = "bold"))

graph2 <- ggplot(models, aes(x=mod_name, y=BIC, group=1)) +
  geom_vline(aes(xintercept = mod_name[which.min(BIC)]), colour="olivedrab3", linetype=2, linewidth=.6)+
  geom_line(linewidth=1,color="olivedrab3")+
  geom_point(size=3,color="darkgreen")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="", title="TOP Classification Models based on BIC")+
  theme(plot.title = element_text(hjust = 0, size = 14, face = "bold"))

grid.arrange(graph1,graph2)

# Il modello VVE presenta il minor BIC, mentre QDA presenta il minor CV.
# Priviegiando il CV come criterio di scelta, optiamo per Gaussian_pk_Lk_Ck, cio? VVV, cio? QDA 

# Alleniamo il classificatore sul training usando il TOP model VVV
best_mod <- mixmodLearn(training_features, 
                        training_class,
                        models=mixmodGaussianModel(listModels="Gaussian_pk_Lk_Ck", equal.proportions = F))@bestResult 

# Utilizziamo il classificatore sulle osservazioni del test
prediction <- mixmodPredict(data=test_features, classificationRule=best_mod)

# Utilizzando la funzione classError calcoliamo l'Accuracy del modello
# Nota: La funzione classError considera direttamente il valore minimo tra le proporzioni
# dei punti misclassificati rispetto a quelli ottenuti permutando le labels
Accuracy <- 1 - classError(test_class,prediction["partition"])$errorRate #0.7922705
round(Accuracy*100,2)

# Si salvano le osservazioni del test che vengono classificate erroneamente
misclass <- classError(test_class,prediction["partition"])$misclassified

# Si calcolano i valori dell'incertezza per ogni unità del test set
uncertainty <- 1-apply(prediction@proba,1,max)

# data_graph è un data frame composto da tutte le unità del test set (e le rispettive variabili) + le relative incertezze, 
# è utile per i grafici successivi
data_graph <- data.frame(test,uncertainty)

# Unità classificate erroneamente per ogni classe
table(data_graph[misclass,]$stand_pos)

# Scatterplot con colori in base alla classe assegnata dal classificatore.
# Punti di grandezza proporzionale all'incertezza e punti misclassificati in nero. 
# Si nota che il classificatore fa più fatica ad allocare le unità delle classi MHS e MLS perchè molto simili tra di loro

ggg1 <- ggplot(data_graph, aes(x=tot_corners, y=tot_points, color=stand_pos)) +  
  scale_colour_manual(values=c("forestgreen","red2","yellow2","orange2"))+
  geom_point(aes(size=uncertainty))+
  geom_point(data_graph[misclass,], mapping=aes(x=tot_corners, y=tot_points, size=uncertainty),colour="black")+ 
  theme_minimal()+
  labs(x="tot corners", y="tot points")+
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust =0, size = 14, face = "bold"))

ggg2 <- ggplot(data_graph, aes(x=mean_ppda, y=tot_points, colour=stand_pos)) + 
  scale_colour_manual(values=c("forestgreen","red2","yellow2","orange2"))+
  geom_point(aes(size=uncertainty))+
  geom_point(data_graph[misclass,], mapping=aes(x=mean_ppda, y=tot_points, size=uncertainty),colour="black")+ 
  theme_minimal()+
  labs(x="mean ppda", y="tot points")+
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust =0, size = 14, face = "bold"))

ggg3 <- ggplot(data_graph, aes(x=tot_goals, y=tot_points, colour=stand_pos)) +
  scale_colour_manual(values=c("forestgreen","red2","yellow2","orange2"))+
  geom_point(aes(size=uncertainty))+
  geom_point(data_graph[misclass,], mapping=aes(x=tot_goals, y=tot_points, size=uncertainty),colour="black")+ 
  theme_minimal()+
  labs(x="tot goals", y="tot points")+
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust =0, size = 14, face = "bold"))

ggg4 <- ggplot(data_graph, aes(x=tot_fouls, y=tot_points, colour=stand_pos)) + 
  scale_colour_manual(values=c("forestgreen","red2","yellow2","orange2"))+
  geom_point(aes(size=uncertainty))+
  geom_point(data_graph[misclass,], mapping=aes(x=tot_fouls, y=tot_points, size=uncertainty),colour="black")+ 
  theme_minimal()+
  labs(x="tot fouls", y="tot points")+
  theme(plot.title = element_text(hjust =0, size = 14, face = "bold"))

grid.arrange(ggg1,ggg2,ggg3,ggg4, top=textGrob("Misclassified Units & Uncertainty", gp=gpar(fontsize=22,font=8)))

#### MDA ####

## BIC ##
mod2 <- MclustDA(training_features, training_class) # si stima con il classificatore MDA
summary(mod2)

# Scegliendo i modelli in base al BIC, il classificatore suggerisce di utilizzare EDDA.
# Infatti le classi hanno una sola componente per mistura 

## CV ##

# Calcoliamo il CV manualmente:
# Dividiamo il dataset in 14 sottoinsiemi.
# Togliamo dal dataset le unit? di un sottoinsieme e stimiamo il 
# classificatore utilizzando come possibili modelli LDA e QDA.
# Successivamente calcoliamo il MER sulle unit? del sottoinsieme rimosso.
# Si fa questo per tutti i 14 sottoinsiemi del dataset e si fa la media 
# dei MER per ottenere il CV del classificatore. 
# Si fa ci? considerando MDA con misture interne aventi componenti da 1 a 6 

# Ciclo per scegliere il numero delle componenti delle misture interne del MDA in base al CV
# (computazionalmente pesante)

set.seed(222)
G <- 6        # fino a 6 componenti per mistura interna
V <- 14       # si divide il training in 14 
perm <- sample(n)  # si riordinano casualmente i numeri da 1 a n
B <- round(n/V)    # numero di unit? per sottoinsieme
err <- matrix(NA ,G,V)  # matrice vuota che conterr? i MER per ogni G e V

for (g in 1:G){
  for (v in 1:V){
    test_labels <-  perm[(B*(v-1)+1):(B*v)]
    mod <- MclustDA(stats_summary_features[-test_labels,], stats_summary_target[-test_labels],G=g, 
                   modelNames = c("EEE", "VVV")) #LDA, QDA
     err[g,v]  <- classError(predict(mod, stats_summary_features[test_labels,])$class, stats_summary_target[test_labels])$errorRate
    }
}

# Dataset contente il CV per ogni numero di componenti delle misture interne
err <- data.frame(components=1:G, CV = rowMeans(err)) 

ggplot(err, aes(x=components, y=CV)) + 
  geom_vline(aes(xintercept = components[which.min(CV)]), colour="olivedrab3", linetype=2, linewidth=.6)+
  geom_line(linewidth=1,color="olivedrab3")+
  geom_point(size=3,color="darkgreen")+
  scale_x_continuous(breaks = 1:10)+
  theme_minimal()+
  labs(x="number of components", title="MDA models with 1:6 mixture components & CV")+
  theme(plot.title = element_text(hjust =0, size = 14, face = "bold"))

# Anche scegliendo in base al CV si opta per EDDA
# Infatti il CV è minimo quando le 4 misture interne hanno una sola componente ciascuna

######################## 4. CLUSTERING #############################################

# Creiamo il dataset su cui effetueremo il clustering
data_clust <- stats_quant

# Trasformazione per grafici
data_clust_df <- as.data.frame(data_clust) 
stats_summary_df <- as.data.frame(stats_summary)

# Effettuiamo il clustering senza imporre i gruppi
stats_summary_BIC <- Mclust(data_clust)
summary(stats_summary_BIC) 

# VVE con 3 componenti risulta essere il miglior modello in base al BIC

# Risultati

#---------------------------------------------------- 
#Gaussian finite mixture model fitted by EM algorithm 
#---------------------------------------------------- 

# Mclust VVE (ellipsoidal, equal orientation) model with 3 components: 

# log-likelihood n df     BIC       ICL
#.  -18524.19.  686 78 -37557.79 -37794.24

# Clustering table:
# 1   2   3 
# 164 316 206 

# Scegliamo il miglior modello utilizzando il criterio ICL
b_models_ICL <- mclustICL(data_clust)
summary(b_models_ICL) 

# EVE con 2 componenti risulta essere il miglior modello in base all'ICL

# Best ICL values:
# EVE,2 VVE,2 EEV,2
# ICL-37670.52 -3.767056e+04 -37680.52486
# ICL diff0.00 -3.399461e-02 -10.00228

# Grafici per valutare tutti i modelli provati (BIC & CV)
plot(x=b_models_ICL,what="ICL", legendArgs=list(x="bottomright",inset=0.01,ncol=3,cex=0.7))
plot(x=stats_summary_BIC,what="BIC", legendArgs=list(x="bottomright",inset=0.01,ncol=3,cex=0.7))

### Clustering con modello scelto secondo criterio BIC ### 

stats_cluster <- stats_summary_BIC$classification 

# Plot mclust classification 
# Proiezione delle coordinate delle variabili mean_ppda e tot_goals
coordProj (
  data_clust_df,
  dimens = c("mean_ppda", "tot_goals"),
  what = "classification",
  classification = stats_summary_BIC$classification,
  col = c("forestgreen",
          "red2",
          "yellow2"),
  symbols = c(15, 17, 16),
  cex = 1.2,
  xlim=c(5,23),
  main = F
) + title(main="Clustering VVE,3 : (mean ppda , tot goals) ")

# Interpretazione cluster:

# Scegliamo tot_goals e mean_ppda come variabili da mostrare perchè risultano utili nell'interpretazione dei cluster. 
# tot_goals rappresenta una statistica offensiva (gol segnati), mentre mean_ppda una statistica difensiva (pressing). 
# Nota: un valore di ppda basso indica un pressing efficace e quindi una buona difesa (e viceversa)
# Attraverso questa osservazione è possibile acquisire un'idea del modo in cui le squadre giocano e valutare le loro abilità.
# Ogni gruppo sarà quindi caratterizzato da un livello di abilità di gioco sancito dall'attacco e dalla difesa.

# Dall'analisi del grafico è possibile dedurre che:
# Il gruppo verde rappresenta le squadre di alto livello (forte attacco e forte difesa);
# Il gruppo giallo le squadre di medio livello (modesto attacco e modesta difesa);
# Il gruppo rosso le squadre di basso livello (scarso attacco e scarsa difesa).

# Vogliamo aggiungere al grafico il nome della squadra che è più vicina al centro del cluster

# Medie e matrici di varianza e covarianza per le tre componenti della mistura
center <- stats_summary_BIC$parameters$mean
cov1 <-as.matrix((stats_summary_BIC$parameters$variance$sigma)[,,1])
cov2 <- as.matrix((stats_summary_BIC$parameters$variance$sigma)[,,2])
cov3 <- as.matrix((stats_summary_BIC$parameters$variance$sigma)[,,3])

# Calcolo della distanza di mahalanobis tra le osservazioni e il centro del gruppo a cui sono associate
distanze1 <- mahalanobis(data_clust_df[stats_cluster==1,c("tot_goals","mean_ppda")], center[c("tot_goals","mean_ppda"),1], cov1[c("tot_goals","mean_ppda"),c("tot_goals","mean_ppda")])
distanze2 <- mahalanobis(data_clust_df[stats_cluster==2,c("tot_goals","mean_ppda")],center[c("tot_goals","mean_ppda"),2], cov2[c("tot_goals","mean_ppda"),c("tot_goals","mean_ppda")])
distanze3 <- mahalanobis(data_clust_df[stats_cluster==3,c("tot_goals","mean_ppda")], center[c("tot_goals","mean_ppda"),3], cov3[c("tot_goals","mean_ppda"),c("tot_goals","mean_ppda")])

# Scegliamo l'osservazione più vicina ad ogni cluster
punti1 <- as.numeric(names(sort(distanze1,dec=F)[1]))
punti2 <- as.numeric(names(sort(distanze2,dec=F)[1]))
punti3 <- as.numeric(names(sort(distanze3,dec=F)[1]))

# Aggiungiamole al grafico
text(
  x = stats_summary_df[c(punti1,punti2,punti3),"mean_ppda"],
  y = stats_summary_df[c(punti1,punti2,punti3),"tot_goals"] ,
  labels = stats_summary$team_name_season[c(punti1,punti2,punti3)],
  cex = 0.8,
  pos = 3,
  col = "black",
  font = 4
)

# L'aggiunta dei nomi delle squadre supporta l'interpretazione dei gruppi:

# Il Chelsea del 2019 è arrivato terzo qualificandosi dunque in champions league;
# ha disputato un ottimo campionato sia dal punto di vista offensivo che difensivo
# L' Espanyol del 2018 è invece riuscito a qualificarsi in europa league conquistando
# la settima posizione del campionato spagnolo;
# L'Union Berlin del 2019 si è invece posizionato nella parte destra del tabellone
# non qualificandosi in nessuna competizione europea e consolidando il suo posto in Bundesliga
# per l'anno successivo

# Grafico Incertezza
coordProj(
  data_clust_df,
  dimens = c("mean_ppda", "tot_goals"),
  what = "uncertainty",
  parameters = stats_summary_BIC$parameters ,
  z = stats_summary_BIC$z,
  addEllipses = TRUE,
  col = c("forestgreen",
          "red2",
          "yellow2"),
  main=F
) + title(main="Uncertainty Clustering VVE,3 : (mean ppda , tot goals)")

# Come ci si poteva aspettare c'è più incertezza nelle unità vicine a più gruppi contemporaneamente.

# Stimiamo dunque una misura della bontà dei cluster

# Misura di distanza simmetrica tra le componenti della mistura (gruppi)

KLs(center[,1],center[,2],cov1,cov2) # distanza tra cluster "alto livello" e "basso livello" = 15.05742
KLs(center[,1],center[,3],cov1,cov3) # distanza tra cluster "alto livello" e "medio livello" = 11.08272
KLs(center[,2],center[,3],cov2,cov3) # distanza tra cluster "basso livello" e "medio livello" = 7.288221

# Commenti:
# Come ci si poteva aspettare, c'è una maggiore distanza tra il gruppo rappresentante le squadre di alto 
# livello e quello rappresentante le squadre di basso livello.
# La distanza minore è, invece, tra il gruppo rappresentante le squadre di medio livello e quello
# rappresentantente squadre di basso livello.
# Queste misure ci suggeriscono che le squadre di medio e basso livello sono più simili tra loro,
# mentre le squadre di alto livello si differenziano di più dagli altri 2 gruppi

# R^2
p <- stats_summary_BIC$parameters$pro 
mu <- rowSums(p*center)
sigma <- ((p[1]*cov1)+(p[2]*cov2)+(p[3]*cov3))+ 
  ((p[1]*(center[,1]-mu) %*% t(center[,1]-mu))+
     (p[2]*(center[,2]-mu) %*% t(center[,2]-mu))+
     (p[3]*(center[,3]-mu) %*% t(center[,3]-mu)))

# R^2_tr
R2_tr <- 1- sum(diag(p[1]*cov1+p[2]*cov2+p[3]*cov3))/(sum(diag(sigma)))
round(R2_tr,4) # 0.3639

# R^2_det
R2_det <- 1- det(p[1]*cov1+p[2]*cov2+p[3]*cov3)/det(sigma)
round(R2_det,4) # 0.8482

# L'R^2_det viene calcolato considerando il determimante della matrice sigma, 
# cioè raccoglie più informazione dell' R^2 calcolato con la sola traccia.
# Diamo dunque più importanza all'R^2_det.
# R^2_det = 0.8482 : il valore si avvicina ad 1, cioè indica una buona clusterizzazione

# Entropia
Z <- stats_summary_BIC$z
(EN <- -sum(apply(Z, 1, function(x) x%*%log(x))) / (n*log(3)))
# Si ottiene un valore inferiore a 1 non troppo distante dallo 0.
# Anche l'entropia indica una buona classificazione


### Clustering con modello scelto secondo criterio ICL ###

stats_summary_ICL <- Mclust(data_clust,modelNames = "EVE",G=2)
summary(stats_summary_ICL)
coordProj (
  data_clust_df,
  dimens = c("mean_ppda", "tot_goals"),
  what = "classification",
  classification = stats_summary_ICL$classification,
  col = c("forestgreen",
          "red2"),
  symbols = c(15, 17),
  cex = 1.2,
  xlim=c(5,23),
  main = F,
) 

stats_cluster2 <- stats_summary_ICL$classification
center2 <- stats_summary_ICL$parameters$mean
covv1 <-as.matrix((stats_summary_ICL$parameters$variance$sigma)[,,1])
covv2 <- as.matrix((stats_summary_ICL$parameters$variance$sigma)[,,2])

# Calcolo della distanza di mahalanobis tra le osservazioni e il centro del gruppo a cui sono associate
ddistanze1 <- mahalanobis(data_clust_df[stats_cluster2==1,c("tot_goals","mean_ppda")], center2[c("tot_goals","mean_ppda"),1], covv1[c("tot_goals","mean_ppda"),c("tot_goals","mean_ppda")])
ddistanze2 <- mahalanobis(data_clust_df[stats_cluster2==2,c("tot_goals","mean_ppda")],center2[c("tot_goals","mean_ppda"),2], covv2[c("tot_goals","mean_ppda"),c("tot_goals","mean_ppda")])

# Scegliamo l'osservazione più vicina ad ogni cluster
ppunti1 <- as.numeric(names(sort(ddistanze1,dec=F)[1]))
ppunti2 <- as.numeric(names(sort(ddistanze2,dec=F)[1]))

# Aggiungiamole al grafico
text(
  x = stats_summary_df[c(ppunti1,ppunti2),"mean_ppda"],
  y = stats_summary_df[c(ppunti1,ppunti2),"tot_goals"] ,
  labels = stats_summary$team_name_season[c(ppunti1,ppunti2)],
  cex = 0.8,
  pos = 3,
  col = "black",
  font = 4
) + title(main="Clustering EVE,2 : (mean ppda , tot goals) ")

# Commenti: 
# Dall'analisi del grafico è possibile dedurre che:
# Il gruppo verde rappresenta le squadre di alto livello, (top attacco, top difesa);
# Il gruppo rosso le squadre di medio-basso livello (mediocre/scarso attacco, mediocre/scarsa difesa)

# Rispetto al precedente cluster a 3 gruppi sembra quasi che abbia isolato le squadre di altissimo
# livello e abbia unito le squadre di medio e basso livello

# L'aggiunta dei nomi delle squadre supporta l'interpretazione dei gruppi:

# L'Arsenal del 2017 si è qualificato in Europa League, mentre 
# il Monaco del 2019 si è piazzato a metà classifica

# Grafico incertezza
coordProj(
  data_clust_df,
  dimens = c("mean_ppda", "tot_goals"),
  what = "uncertainty",
  parameters = stats_summary_ICL$parameters ,
  z = stats_summary_ICL$z,
  addEllipses = TRUE,
  col = c("forestgreen",
          "red2"),
  main= F
) + title(main="Uncertainty Clustering EVE,2 : (mean ppda , tot goals) ")

# Misura di distanza tra i gruppi
KLs(center2[,1],center2[,2],covv1,covv2) # 11.26616

# R^2
p2 <- stats_summary_ICL$parameters$pro 
mu2 <- rowSums(p2*center2)
sigma2 <- ((p2[1]*covv1)+(p2[2]*covv2))+ 
  (p2[1]*(center2[,1]-mu2) %*% t(center2[,1]-mu2))+
     (p2[2]*(center2[,2]-mu2) %*% t(center2[,2]-mu2))
     
# R^2_tr
R2_tr2 <- 1 - (sum(diag(p2[1]*covv1+p2[2]*covv2))/sum(diag(sigma2)))
round(R2_tr2,4) # 0.9193

# R^2_det
R2_det2 <- 1- det(p2[1]*covv1+p2[2]*covv2)/det(sigma2)
round(R2_det2,4) # 0.9985

# Sembrerebbe una clusterizzazione migliore di quella precedente

# Entropia
Z2 <- stats_summary_ICL$z
(EN <- -sum(apply(Z2, 1, function(x) x%*%log(x)))/(n*log(3))) #0.1205748

# Si ottiene un valore inferiore a 1 e vicino a 0.
# Anche l'entropia indica una buona clusterizzazione


