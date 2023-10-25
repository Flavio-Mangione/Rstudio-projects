#PULIZIA ENVIROMENT 
rm(list=ls())
#IMPOSTARE LA PROPRIA WORKING DIRECTORY SU CUI EFFETTUARE L'ANALISI
setwd("C:/Users/flavi/Downloads")
#LEGGI IL FILE EXCEL DA ANALIZZARE 
studenti<-read.csv('StudentiEBD Anonimo 5.csv', header=T,sep=';')

#RINOMINIAMO LE VARIABILI
maturità<-studenti$Voto.maturità
matematica<-studenti$Matematica
Economia_aziendale<-studenti$EcoNOmia.Aziendale
Fondamenti_giuridici<-studenti$Fond..Giuridici
Informatica<-studenti$Informatica.e.programmazione
Statistica<-studenti$StatiStica
Metodi_matematici<-studenti$Metodi.Matematici.per.la.finanza
Scienze_Finanze<-studenti$Scienza.della.Finanza
Politica_economica<-studenti$Politica.EcoNOmica
Economia_Politica<-studenti$EcoNOmia.Politica
indirizzo<-studenti$Indirizzo

#NUMERO DI SODDISFATTI PER ECONOMIA E BIG DATA
table(studenti$SoddiSfazione.per.EBD)

#ETÀ E NUMERO FRATELLI E SORELLE PER STUDENTE 
table(studenti$età,studenti$n.....fratelli.Sorelle)

#GRAFICO A TORTA CON RELATIVA LEGGENDA PER INDIRIZZI SCOLASTICI
conteggi <- table(studenti$Indirizzo)
colori <- c("paleturquoise1","palevioletred1","seagreen1","turquoise3")
nomi <- c("Altro: 10 studenti", "Indirizzo classico: 9 studenti","indirizzo Tecnico: 17 studenti", "Indirizzo scientifico: 74 studenti")
percentuale_indirizzi<-paste(round(100*conteggi/sum(conteggi),1),'%',sep='')
pie(conteggi, labels=percentuale_indirizzi, col = colori, main = "Grafico a torta per gli indirizzi scolastici di provenienza degli studenti")
legend("topright",nomi,cex=0.9,fill=colori)

#VERSIONE 3D DEL GRAFICO A TORTA
library(plotrix)
pie3D(conteggi, labels = percentuale_indirizzi, main = "Grafico a torta 3D per indirizzi scolastici",col = colori)  
legend("topright", nomi, cex = 1, fill=colori)  

#PROCEDIAMO A FARE UN ANALISI DESCRITTIVA DEGLI INDIRIZZI SCOLASTICI PER VEDERE I VOTI DI MATURITÀ E FARCI DEI BOXPLOT 
means <-aggregate(studenti$Voto.maturità, by=list(studenti$Indirizzo), mean)
colnames(means) <- c("Indirizzo", "Media")
means <- aggregate(studenti$Voto.maturità, by=list(studenti$Indirizzo), mean)
colnames(means) <- c("Indirizzo", "Media")
Al <- subset(studenti$Voto.maturità, studenti$Indirizzo == 'Al')
C <- subset(studenti$Voto.maturità, studenti$Indirizzo == 'C')
IT <- subset(studenti$Voto.maturità, studenti$Indirizzo == 'IT')
S <- subset(studenti$Voto.maturità, studenti$Indirizzo == 'S')

#ANALISI DESCRITTIVA INDIRIZZI
library(psych)
library(summarytools)
maturità.indirizzi <- data.frame(
  Voti = c(Al, C, IT, S), 
  Indirizzo = rep(c("Altro", "Classico", "Tecnico", "Scientifico"), times = c(length(Al), length(C), length(IT), length(S)))
)

descr_by_indirizzo <- stby(data = maturità.indirizzi$Voti, 
                           INDICES = maturità.indirizzi$Indirizzo, 
                           FUN = descr)
print(descr_by_indirizzo)

#BOXPLOT VOTI MATURITÀ PER INDIRIZZI SCOLASTICI
library(ggplot2)
ggplot(maturità.indirizzi, aes(x = Indirizzo , y = Voti, fill = Indirizzo)) +
  geom_boxplot() +
  geom_point(aes(x = "Altro", y = means$Media[1]), color = "white", shape=8) +
  geom_point(aes(x = "Classico", y = means$Media[2]), color = "white", shape=8) +
  geom_point(aes(x = "Tecnico", y = means$Media[3]), color = "white", shape=8)+
  geom_point(aes(x = "Scientifico", y = means$Media[4]), color = "white", shape=8) +
  geom_text(aes(x = "Altro", y = means$Media[1], label = paste("Media:", round(means$Media[1], 2))), 
            color = "gray0", vjust = 1.2) +
  geom_text(aes(x = "Classico", y = means$Media[2], label = paste("Media:", round(means$Media[2], 2))), 
            color = "gray0", vjust = 1) +
  geom_text(aes(x = "Tecnico", y = means$Media[3], label = paste("Media:", round(means$Media[3], 2))), 
            color = "gray0", vjust = -1) +
  geom_text(aes(x = "Scientifico", y = means$Media[4], label = paste("Media:", round(means$Media[4], 2))), 
            color = "gray0", vjust = -1) +
  scale_fill_manual(values = c("firebrick2", "chartreuse2",'cyan2','darkgoldenrod2')) +
  xlab("Indirizzi scolastici") +
  ylab("Voti Maturità") +
  theme(legend.text = element_text(size = 13))+
  ggtitle("Boxplot voto maturità per indirizzi scolastici")


#BARPLOT NUMERO DI ESAMI SUPERATI E MEDIE DEGLI ESAMI
descr(studenti[10:18])
esami <- colnames(studenti[10:18])
superati<-apply(!is.na(studenti[10:18]), 2, sum, na.rm = TRUE)
medie <- apply(studenti[10:18], 2, mean, na.rm = TRUE)
esami.superati<- data.frame(Esami = esami,superati)

library(ggplot2)
ggplot(esami.superati, aes(x=Esami, y=superati, fill=Esami)) + 
  geom_bar(stat="identity") +
  theme_set(theme_classic())+
  labs(x="Esami", y="Numero di studenti che hanno superato l'esame", fill="Esami") +
  geom_text(aes(label = superati), position = position_stack(vjust = 0.5))+
  theme(legend.text = element_text(size = 17), legend.title = element_text(size = 17),
        legend.key.size = unit(1.5, "lines")) +
  ggtitle("Numero di studenti che hanno superato ogni esame")


esami.superati <- data.frame(esami.superati, medie)
ggplot(esami.superati, aes(x=Esami, y=medie, fill=Esami)) + 
  geom_bar(stat="identity") +
  theme_set(theme_classic())+
  labs(x="Esami", y="Medie per esame", fill="Esami") +
  geom_text(aes(label = sprintf("%.2f", medie)), position = position_stack(vjust = 0.5)) +
  theme(legend.text = element_text(size = 17), legend.title = element_text(size = 17),
        legend.key.size = unit(1.5, "lines")) +
  ggtitle("Grafico a barre medie per ogni esame")
  
  

#BOXPLOT CONFRONTO MEDIE PER MATERIE SCIENTIFICHE 
matematica.na <- studenti$Matematica[!is.na(studenti$Matematica)]
Statistica.na <- studenti$StatiStica[!is.na(studenti$StatiStica)]
Metodi_matematici.na <- studenti$Metodi.Matematici.per.la.finanza[!is.na(studenti$Metodi.Matematici.per.la.finanza)]
media1 <- mean(matematica.na)
media2 <- mean(Metodi_matematici.na)
media3 <- mean(Statistica.na)
materie_scientifiche <- data.frame(Voti = c(matematica, Metodi_matematici, Statistica), Esami = rep(c("matematica", "Metodi matematici", "Statistica"), each = c(length(matematica), length(Metodi_matematici), length(Statistica))))
descr_by_scientifiche <- stby(data = materie_scientifiche$Voti, 
                           INDICES = materie_scientifiche$Esami, 
                           FUN = descr)
print(descr_by_scientifiche)
library(ggplot2)
ggplot(materie_scientifiche, aes(x = Esami, y = Voti, fill = Esami)) +
  geom_boxplot() +
  geom_point(aes(x = "matematica", y = media1), color = "white", shape=8) +
  geom_point(aes(x = "Metodi matematici", y = media2), color = "white", shape=8) +
  geom_point(aes(x = "Statistica", y = media3), color = "white", shape=8) +
  geom_text(aes(x = "matematica", y = media1, label = paste("Media:", round(media1, 2))), 
            color = "gold", vjust = -1) +
  geom_text(aes(x = "Metodi matematici", y = media2, label = paste("Media:", round(media2, 2))), 
            color = "blue", vjust = -1) +
  geom_text(aes(x = "Statistica", y = media2, label = paste("Media:", round(media3, 2))), 
            color = "green3", vjust = -2) +
  scale_fill_manual(values = c("deeppink1", "cyan3",'red')) +
  xlab("Esami") +
  ylab("Voti degli esami") +
  ggtitle("Boxplot materie scientifiche")+ theme_light()+
  scale_y_continuous(limits = c(18, 31), breaks = seq(18, 31, by = 1))


#TRASFORMIAMO TUTTI I 31 IN 30 PER IL CALCOLO DELLE MEDIE ARITMETICHE E PER LE MEDIE PER IL CONSEGUIMENTO
matematica<-ifelse(studenti$Matematica==31,30,studenti$Matematica)
Economia_aziendale<-ifelse(studenti$EcoNOmia.Aziendale==31,30,studenti$EcoNOmia.Aziendale)
Fondamenti_giuridici<-ifelse(studenti$Fond..Giuridici==31,30,studenti$Fond..Giuridici)
Informatica<-ifelse(studenti$Informatica.e.programmazione==31,30,studenti$Informatica.e.programmazione)
Statistica<-ifelse(studenti$StatiStica==31,30, studenti$StatiStica)
Metodi_matematici<-ifelse(studenti$Metodi.Matematici.per.la.finanza==31,30,studenti$Metodi.Matematici.per.la.finanza)
Scienze_Finanze<-ifelse(studenti$Scienza.della.Finanza==31,30,studenti$Scienza.della.Finanza)
Politica_economica<-ifelse(studenti$Politica.EcoNOmica==31,30,studenti$Politica.EcoNOmica)
Economia_Politica<-ifelse(studenti$EcoNOmia.Politica==31,30,studenti$EcoNOmia.Politica)

#CREIAMO DUE NUOVI VETTORI: MEDIE ARITMETICHE E MEDIE PER IL CONSEGUIMENTO
medie_aritmetiche =c()
medie_per_il_conseguimento=c()
for(i in 1:nrow(studenti)){
  medie_aritmetiche[i] = mean(c(Economia_aziendale[i],
                                Economia_Politica[i],matematica[i],Politica_economica[i],
                                Metodi_matematici[i],Scienze_Finanze[i],Fondamenti_giuridici[i],
                                Informatica[i],Statistica[i]),na.rm=T)
  medie_per_il_conseguimento[i]= mean(c(Economia_aziendale[i],Economia_Politica[i],matematica[i],Politica_economica[i],
                                        Metodi_matematici[i],Scienze_Finanze[i],Fondamenti_giuridici[i],
                                        Informatica[i],Statistica[i])*110/30,na.rm=T)
}
studenti<-data.frame(studenti,medie_aritmetiche,medie_per_il_conseguimento)

#BOXPLOT MEDIE ARITMETICHE PER INDIRIZZI
Al <- subset(medie_aritmetiche, studenti$Indirizzo == 'Al')
C <- subset(medie_aritmetiche, studenti$Indirizzo == 'C')
IT <- subset(medie_aritmetiche, studenti$Indirizzo == 'IT')
S <- subset(medie_aritmetiche, studenti$Indirizzo == 'S')
S<-S[!is.na(S)]
Medie.indirizzi <- data.frame(
  Voti = c(Al, C, IT, S), 
  Indirizzo = rep(c("Altro", "Classico", "Tecnico", "Scientifico"), times = c(length(Al), length(C), length(IT), length(S))))
ggplot(Medie.indirizzi, aes(x = Indirizzo , y = c(Al,C,IT,S), fill = Indirizzo)) +
  geom_boxplot() +
  geom_point(aes(x = "Altro", y = mean(Al)), color = "white", shape=8) +
  geom_point(aes(x = "Classico", y = mean(C)), color = "white", shape=8) +
  geom_point(aes(x = "Tecnico", y = mean(IT)), color = "white", shape=8)+
  geom_point(aes(x = "Scientifico", y = mean(S)), color = "white", shape=8) +
  geom_text(aes(x = "Altro", y = mean(Al), label = paste("Media:", round(mean(Al), 2))), 
            color = "gray0", vjust = -1) +
  geom_text(aes(x = "Classico", y = mean(C), label = paste("Media:", round( mean(C), 2))), 
            color = "gray0", vjust = -1) +
  geom_text(aes(x = "Tecnico", y = mean(IT), label = paste("Media:", round(mean(IT), 2))), 
            color = "gray0", vjust = -1) +
  geom_text(aes(x = "Scientifico", y = mean(S), label = paste("Media:", round( mean(S), 2))), 
            color = "gray0", vjust = -1) +
  scale_fill_manual(values = c("lightcoral", "mediumspringgreen", "orchid1", "lightsalmon1"))+
  xlab("Indirizzi scolastici") +
  ylab("Medie aritmetiche") +
  ggtitle("Boxplot Medie aritmetiche per indirizzi scolastici")


descr_by_indirizzi<- stby(data = Medie.indirizzi$Voti, 
                            INDICES = Medie.indirizzi$Indirizzo, 
                            FUN = descr)
print(descr_by_indirizzi)

#NUOVO DATASET CON SOLO GLI STUDENTI CHE HANNO SUPERATO I 9 ESAMI E DIVISIONE MEDIE ARITMETICHE PER SESSO 
studenti2<-subset(studenti,studenti$Numero.eSami.Superati==9)
descr(studenti2[10:18])
genere<-studenti2$Genere
medie_maschi<-subset(studenti2$medie_aritmetiche,genere=='M')
medie_femmine<-subset(studenti2$medie_aritmetiche,genere=='F')
medie1<-mean(medie_femmine)
medie2<-mean(medie_maschi)
medie.genere <- data.frame(medie = c(medie_maschi, medie_femmine), genere = rep(c("Maschi", "Femmine"), times = c(length(medie_maschi), length(medie_femmine))))
descr_by_stud2<- stby(data = medie.genere$medie, 
                              INDICES = medie.genere$genere, 
                              FUN = descr)
print(descr_by_stud2)

t.test(medie_maschi, medie_femmine, paired = FALSE, var.equal = TRUE, alternative = "less")

#BOXPLOT PER LE MEDIE ARITMETICHE PER GLI STUDENTI CHE HANNO SUPERATO I 9 ESAMI DIVISI PER SESSO 
library(ggplot2)
boxplot37<-ggplot(medie.genere, aes(x = genere, y = medie, fill = genere)) +
  geom_boxplot() +
  geom_point(aes(x = "Femmine", y = medie1), color = "white", shape=8) +
  geom_point(aes(x = "Maschi", y = medie2), color = "white", shape=8) +
  geom_text(aes(x = "Femmine", y = medie1, label = paste("Media:", round(medie1, 2))), 
            color = "gold", vjust = -1) +
  geom_text(aes(x = "Maschi", y = medie2, label = paste("Media:", round(medie2, 2))), 
            color = "blue", vjust = -1) +
  scale_fill_manual(values = c("deeppink1", "cyan3")) +
  xlab("Genere") +
  ylab("medie aritmetiche") +
  ggtitle("Boxplot medie aritmetiche studenti per genere con 9 esami superati")+ theme_light()

#ANALISI MEDIE ARITMETICHE PER GENERE DEGLI STUDENTI CON MENO DI 9 ESAMI SUPERATI 
studenti.ultimi <- subset(studenti,studenti$Numero.eSami.Superati>=3 & studenti$Numero.eSami.Superati<=6)
descr(studenti.ultimi[10:18])
genere<-studenti.ultimi$Genere
medie_maschi<-subset(studenti.ultimi$medie_aritmetiche,genere=='M')
medie_femmine<-subset(studenti.ultimi$medie_aritmetiche,genere=='F')
medie3<-mean(medie_femmine)
medie4<-mean(medie_maschi)
medie.genere <- data.frame(medie = c(medie_maschi, medie_femmine), genere = rep(c("Maschi", "Femmine"), times = c(length(medie_maschi), length(medie_femmine))))
descr_by_stud26<- stby(data = medie.genere$medie, 
                       INDICES = medie.genere$genere, 
                       FUN = descr)
print(descr_by_stud26)

#T.TEST MEDIE STUDENTI CON NUMERO ESAMI PASSATI MAGGIORE DI 9 A CONFRONTO CON GLI STUDENTI CON NUMERO DI ESAMI PASSATI COMPRESI TRA 3 E 6
t.test(studenti.ultimi$medie_aritmetiche, studenti2$medie_aritmetiche, paired = FALSE, var.equal = TRUE, alternative = "less")

#BOXPLOT DELLE MEDIE ARITMETICHE PER GENERE DEGLI STUDENTI TRA I 3 E 6 ESAMI SUPERATI

library(ggplot2)
boxplot26<-ggplot(medie.genere, aes(x = genere, y = medie, fill = genere)) +
  geom_boxplot() +
  geom_point(aes(x = "Femmine", y = medie3), color = "white", shape=8) +
  geom_point(aes(x = "Maschi", y = medie4), color = "white", shape=8) +
  geom_text(aes(x = "Femmine", y = medie3, label = paste("Media:", round(medie3, 2))), 
            color = "gold", vjust = -1) +
  geom_text(aes(x = "Maschi", y = medie4, label = paste("Media:", round(medie4, 2))), 
            color = "orangered3", vjust = 1.5) +
  scale_fill_manual(values = c("orchid2", "steelblue3")) +
  xlab("Genere") +
  ylab("medie aritmetiche") +
  ggtitle("Boxplot medie aritmetiche studenti con numero di esami superati tra 3 e 6")+ theme_light()

#UNIAMO I BOXPLOT DEL SUBSET DA 35 STUDENTI E IL SUBSET DA 26 STUDENTI
library(cowplot)
plot_grid(boxplot37, boxplot26, ncol = 2, align = "h")


#BOXPLOT CONFRONTO MEDIE ARTITMETICHE STUDENTI LAVORATORI E NON LAVORATORI
studenti.lavoratori<-subset(studenti,studenti$medie_aritmetiche!='NaN')
lavoratori<-subset(studenti.lavoratori$medie_aritmetiche,studenti.lavoratori$Lavoratore.Lavoratrice..SI.NO.=='SI')
non.lavoratori<-subset(studenti.lavoratori$medie_aritmetiche,studenti.lavoratori$Lavoratore.Lavoratrice..SI.NO.=='NO')
medie.lavoratori<-mean(lavoratori)
medie.non.lavoratore<-mean(non.lavoratori)
work<- data.frame(lavoro = c(lavoratori, non.lavoratori), Studente = rep(c("Lavoratore", "Non Lavoratore"), times = c(length(lavoratori), length(non.lavoratori))))
descr_by_work<- stby(data = work$lavoro, 
                     INDICES = work$Studente, 
                     FUN = descr)
print(descr_by_work)

library(ggplot2)
ggplot(work, aes(x = Studente, y = lavoro, fill = Studente)) +
  geom_boxplot() +
  geom_point(aes(x = "Lavoratore", y = medie.lavoratori), color = "white", shape=8) +
  geom_point(aes(x = "Non Lavoratore", y = medie.non.lavoratore), color = "white", shape=8) +
  geom_text(aes(x = "Lavoratore", y = medie.lavoratori, label = paste("Media:", round(medie.lavoratori, 2))), 
            color = "gold1", vjust = -1) +
  geom_text(aes(x = "Non Lavoratore", y = medie.non.lavoratore, label = paste("Media:", round(medie.non.lavoratore, 2))), 
            color = "seagreen1", vjust = -1) +
  scale_fill_manual(values = c("orchid2", "darkslategray4")) +
  xlab("Tipo di studente universitario") +
  ylab("Medie aritmetiche") +
  ggtitle("Boxplot confronto medie aritmetiche studenti lavoratori e non lavoratori")+
  theme_minimal()

#T-TEST PER CONFRONTO TRA MEDIE TRA LAVORATORI E NON LAVORATORI 
t.test(lavoratori, non.lavoratori, paired = FALSE, var.equal = TRUE, alternative = "less")
lavoratori<-subset(studenti,studenti$Lavoratore.Lavoratrice..SI.NO.=='SI')
lavoratori.femmine<-subset(lavoratori,lavoratori$Genere=='F')
lavoratori.maschio<-subset(lavoratori,lavoratori$Genere=='M')

#BARPLOT STUDENTI LAVORATORI E STUDENTI NON LAVORATORI
library(ggplot2)
freq <- table(lavoratori$Lavoratore.Lavoratrice..SI.NO., lavoratori$Numero.eSami.Superati)
freq_df <- data.frame(lavoratori = rownames(freq),
                      Numero.eSami.Superati = as.numeric(colnames(freq)),
                      freq = as.numeric(freq))
non.lavoratori <- subset(studenti, studenti$Lavoratore.Lavoratrice..SI.NO.=='NO')
freq2 <- table(non.lavoratori$Lavoratore.Lavoratrice..SI.NO., non.lavoratori$Numero.eSami.Superati)
freq2_df <- data.frame(Lavoratore = rownames(freq2),Numero.eSami.Superati = as.numeric(colnames(freq2)),freq = as.numeric(freq2))
exams<- data.frame(
  esami.work = c(freq_df$Numero.eSami.Superati, freq2_df$Numero.eSami.Superati), 
  lavoratore= rep(c("SI", "NO"), times = c(length(freq_df$Numero.eSami.Superati), length( freq2_df$Numero.eSami.Superati))))
descr_by_esami.lavoratore<- stby(data = exams$esami.work, 
                                 INDICES = exams$lavoratore, 
                                 FUN = descr)
print(descr_by_esami.lavoratore)
t.test(freq_df$Numero.eSami.Superati, freq2_df$Numero.eSami.Superati, paired = FALSE, var.equal = TRUE, alternative = "less")

#BARPLOT LAVORATORI
plot1 <- ggplot(freq_df, aes(x = Numero.eSami.Superati, y = freq, fill = factor(Numero.eSami.Superati))) +
  geom_bar(stat = "identity", width=0.5) +
  xlab("Numero esami superati") +
  ylab("Frequenza") +
  theme_set(theme_classic())+
  scale_fill_discrete(name = "numero esami superati") + 
  geom_text(aes(label = freq), position = position_stack(vjust = 0.5)) +
  scale_y_continuous(name = "Frequenza", limits = c(0, max(freq2_df$freq)), breaks = seq(0, max(freq2_df$freq), 5))+
  scale_x_continuous(name = "Numero esami superati", limits = c(3, 10), breaks = seq(3,9, 1))+
  theme(legend.text = element_text(size = 18), legend.title = element_text(size = 18),
        legend.key.size = unit(1.5, "lines"),legend.position = c(0.1, 0.9), legend.justification = c(0, 1))+
  ggtitle("Numero esami superati da studenti lavoratori")
 

#BARPLOT NON LAVORATORI
plot2 <- ggplot(freq2_df, aes(x = Numero.eSami.Superati, y = freq, fill = factor(Numero.eSami.Superati))) +
  geom_bar(stat = "identity", width=0.5) +
  theme_set(theme_classic())+
  xlab("Numero esami superati") +
  ylab("Frequenza") +
  scale_fill_discrete(name = "numero esami superati") + 
  geom_text(aes(label = freq), position = position_stack(vjust = 0.5)) +
  scale_x_continuous(name = "Numero esami superati", limits = c(2, 10), breaks = seq(3,9, 1))+
  theme(legend.text = element_text(size = 18), legend.title = element_text(size = 18),
        legend.key.size = unit(1.5, "lines"),legend.position = c(0.1, 0.9), legend.justification = c(0, 1))+
  ggtitle("Numero esami superati da studenti non Lavoratori")

#SOVRAPPOSIZIONE DEI DUE BARPLOT 
library(cowplot)
plot_grid(plot1, plot2, ncol = 2, align = "h")

#BOXPLOT MEDIE DEGLI STUDENTI FUMATORI E NON FUMATORI
studenti.fumatori<-subset(studenti,studenti$Fumatore!="")
Fumatori<-studenti.fumatori$Fumatore
fumo<-subset(studenti.fumatori$medie_aritmetiche,studenti.fumatori$Fumatore=='SI')
non.fumo<-subset(studenti.fumatori$medie_aritmetiche,studenti.fumatori$Fumatore=='NO')
media.fumatori<-mean(fumo)
media.non.fumatori<-mean(non.fumo)
medie.fumatori<- data.frame(medie.fumo = c(fumo, non.fumo), Fumatore = rep(c("SI", "NO"), times = c(length(fumo), length(non.fumo))))
descr_by_fumatori<- stby(data = medie.fumatori$medie.fumo, 
                      INDICES = medie.fumatori$Fumatore, 
                      FUN = descr)
print(descr_by_fumatori)

ggplot(medie.fumatori, aes(x = Fumatori, y = medie.fumo, fill = Fumatori)) +
  geom_boxplot() +
  geom_point(aes(x = "SI", y = media.fumatori), color = "white", shape = 8) +
  geom_point(aes(x = "NO", y = media.non.fumatori), color = "white", shape = 8) +
  geom_text(aes(x = "SI", y = media.fumatori, label = paste("Media:", round(media.fumatori, 2))), 
            color = "indianred3", vjust = -1) +
  geom_text(aes(x = "NO", y = media.non.fumatori, label = paste("Media:", round(media.non.fumatori, 2))), 
            color = "gold", vjust = -1) +
  scale_fill_manual(values = c("lightsalmon3", "navajowhite1")) +
  xlab("Fumatore?") +
  ylab("Medie aritmetiche") +
  ggtitle("Boxplot medie aritmetiche studenti fumatori e non fumatori") +
  theme_light()
t.test(fumo, non.fumo, paired = FALSE, var.equal = TRUE, alternative = "less")

#INTERVALLO DI CONFIDENZA PER STIMARE DOVE CADRÀ LA MEDIA DEL ESAME DI STATISTICA PER BIG DATA DEL SECONDO SEMESTRE 
Statistica.na<-studenti$StatiStica[!is.na(studenti$StatiStica)]
intervallo <- t.test(Statistica.na, conf.level = 0.95)$conf.int
print(intervallo)

#ISTOGRAMMA MEDI ARITMETICHE A CONFRONTO PER GENERE 
ggplot(studenti.lavoratori, aes(x=medie_aritmetiche, fill=Genere)) + 
  geom_histogram(alpha=0.5, position="dodge", bins=40) +
  scale_x_continuous(limits = c(18, 30), breaks = seq(18, 30, by = 1)) +
  labs(x="Medie aritmetiche", y="Frequenze", fill="Genere") +
  ggtitle("Confronto tra le distribuzioni delle medie aritmetiche per genere")

#GRAFICI DI DENSITÀ
ggplot(studenti, aes(studenti$Numero.eSami.Superati)) + 
  geom_density(aes(studenti$Numero.eSami.Superati, fill = studenti$Indirizzo), position = 'identity', alpha = 0.5) +
  labs(x = 'Numero esami superati', y = 'Density') + scale_fill_discrete(name = 'Indirizzo scolastico') + scale_x_continuous(limits = c(0, 9))+
  ggtitle("Grafico di densità numero di esami superati per indirizzo scolastico")
  

ggplot(studenti, aes(studenti$medie_aritmetiche)) + 
  geom_density(aes(studenti$medie_aritmetiche, fill = studenti$Indirizzo), position = 'identity', alpha = 0.5) +
  labs(x = 'Medie aritmetiche', y = 'Density') + scale_fill_discrete(name = 'Indirizzo scolastico') + scale_x_continuous(limits = c(18, 30))+
  ggtitle("Medie aritmetiche per indirizzo scolastico")

ggplot(studenti, aes(studenti$medie_aritmetiche)) + 
  geom_density(aes(studenti$medie_aritmetiche, fill = studenti$Genere), position = 'identity', alpha = 0.5) +
  labs(x = 'Medie aritmetiche', y = 'Densità') + scale_fill_discrete(name = 'Genere') + scale_x_continuous(limits = c(18, 30))+
  ggtitle("Confronto tra le distribuzione delle medie aritmetiche per genere")

#SCATTERPLOT MEDIE ARITMETICHE STUDENTI IN RIFERIMENTO AL NUMERO DI ESAMI SUPERATI
studenti3<-subset(studenti.lavoratori,studenti.lavoratori$Numero.eSami.Superati!='NA')
colori.bravura <- cut(studenti3$Numero.eSami.Superati, 
                      breaks = c(2.5, 5.5, 8.5, Inf), 
                      labels = c("da 3 a 5 esami", "da 6 a 8 esami", "9 esami"), 
                      include.lowest = TRUE)
colore_palette <- c("orange1", "darkturquoise", "plum3")
library(ggplot2)
ggplot(studenti3, aes(x = studenti3$Numero.eSami.Superati, y = studenti3$medie_aritmetiche, col=factor(colori.bravura))) +
  geom_point(size=3) +
  ggrepel::geom_text_repel(aes(label = identificativo), size = 3.5, show.legend = FALSE,col="black",max.overlaps = 60) +
  scale_x_continuous(name = "Numero esami superati", limits = c(3, max(studenti3$Numero.eSami.Superati)), breaks = seq(3, max(studenti3$Numero.eSami.Superati), 1)) +
  scale_y_continuous(name = "Medie aritmetiche", limits = c(19, max(studenti3$medie_aritmetiche)), breaks = seq(19, max(studenti3$medie_aritmetiche), 0.5))+
  ggtitle("Grafico a Dispersione medie aritmetiche studenti") +
  scale_color_manual(values = colore_palette, labels = c("da 3 a 5 esami", "da 6 a 8 esami", "9 esami"))+
  labs(col = "Numero esami superati:")+
  theme_gray()

#ANALISI PER TRE DIVERSI GRUPPI DEL NUMERO DI ESAMI SUPERATI
medie.3.5<-subset(studenti3, Numero.eSami.Superati >= 3 & Numero.eSami.Superati <= 5)$medie_aritmetiche  
medie.6.8<-subset(studenti3, Numero.eSami.Superati >= 6 & Numero.eSami.Superati <= 8)$medie_aritmetiche  
medie.9<-subset(studenti3,Numero.eSami.Superati==9)$medie_aritmetiche
medie.esami.superati<- data.frame(
  medie = c(medie.3.5, medie.6.8,medie.9), 
  numero.esami.superati = rep(c("superati tra i 3 e 5 esami", "superati tra i 6 e 8 esami", "superarati 9 esami"), times = c(length(medie.3.5), length(medie.6.8), length(medie.9))))
descr_by_superamenti<- stby(data = medie.esami.superati$medie, 
                            INDICES = medie.esami.superati$numero.esami.superati, 
                            FUN = descr)
print(descr_by_superamenti)

#MODELLI LOGISTICI E LINEARI

model.scientifico<-lm(Metodi_matematici~matematica+Statistica+studenti$Voto.maturità)
summary(model.scientifico)
studenti62 <- subset(studenti, !is.na(Metodi.Matematici.per.la.finanza) & !is.na(StatiStica))
studenti62<-data.frame(studenti62,resid(model.scientifico))
colore_sesso <- ifelse(studenti62$Genere== "F", "deeppink2", "darkturquoise")
par(mfrow=c(2,2))
plot(model.scientifico,pch=18,col=colore_sesso)
title(main = "Grafico Modello Lineare per la varibile risposta Metodi Matematici per la Finanza", line=-2,outer = TRUE)


medie_alte<-ifelse(medie_aritmetiche>=27,1,0)
medie_alte[is.na(medie_alte)]<-0

model.maturità<-glm(medie_alte~maturità+studenti$Numero.eSami.Superati)
summary(model.maturità)
p=exp(-1.186656 + 0.009394*101+9*0.090519)/(1+exp(-1.186656+ 0.009394*101+9*0.090519))
p

model.maturità<-lm(medie_aritmetiche~maturità+studenti$Numero.eSami.Superati)
summary(model.maturità)
par(mfrow=c(2,2))
colore<-ifelse(studenti3$Genere== "F", "deeppink2", "darkturquoise")
plot(model.maturità,col=colore)
title(main = "Grafico Modello Lineare per la varibile risposta Medie Aritmetiche", line=-2,outer = TRUE)
data.frame(studenti3$identificativo,resid(model.maturità))

Statistica<-studenti$StatiStica
model2<-lm(Statistica~matematica+maturità+studenti$Numero.eSami.Superati)
summary(model2)
studenti4<-subset(studenti3,studenti3$StatiStica!='NA'& studenti3$Matematica!='NA')
colore.stat<-ifelse(studenti4$Genere== "F", "lightcoral", "seagreen2")
par(mfrow=c(2,2))
plot(model2,col=colore.stat,pch=19)
title(main = "Grafico Modello Lineare per la varibile risposta Statistica", line=-2,outer = TRUE)

#MODELLO DI REGRESSIONE LOGISTICA PER IL SUPERAMENTO DEL ESAME DI METODI MATEMATICI PER LA FINANZA E CALCOLO PROBABILITÀ
Metodi_matematici<-studenti$Metodi.Matematici.per.la.finanza
voti_metodi<-ifelse(Metodi_matematici>=18,1,0)
voti_metodi[is.na(voti_metodi)]<-0
matematica<-studenti$Matematica
model1<-glm(voti_metodi~matematica,family = binomial)
summary(model1)
p=exp(-4.54394+0.20608*31)/(1+exp(-4.54394+0.20608*31))
p

#ANOVA
modello.anova <- aov(studenti$Numero.eSami.Superati~studenti$Indirizzo)
summary(modello.anova)
test.tukey <- TukeyHSD(modello.anova)
par(mfrow=c(1,1))
plot(test.tukey)


#ANALISI FATTORIALE E CLUSTER

#CLUSTER PER I 37 STUDENTI CHE HANNO SUPERATO TUTTI E 9 GLI ESAMI
library(cluster)
library(factoextra)
library(dendextend)
data_corsi<-studenti2[10:18]
scale_data<-scale(data_corsi)
dist_data<-dist(scale_data)
k.means<-kmeans(scale_data, centers=2,nstart=20)
print(k.means)
data.clusters<-k.means$cluster
distances = get_dist(scale_data,method="euclidean")
sil_k_means = silhouette(k.means$cluster,distances)
fviz_nbclust(x = scale_data, method = "silhouette", FUNcluster = kmeans, k.max = 10)+
  ggtitle("Numero ottimale di cluster da utilizzare per l'analisi")
dendogramma<- hclust(dist_data, method = "complete")
colori<- c("red", "blue")  
plot(dendogramma, hang = -1, main = "Dendrogramma")
rect.hclust(dendogramma, k = 2, border = colori)
rownames(scale_data)<-paste(studenti2$Genere, studenti2$identificativo,sep=':')
fviz_cluster(list(data=scale_data, cluster =data.clusters))+
  ggtitle("Grafico di clustering per gli studenti che hanno superato 9 esami")
table(data.clusters, studenti2$Genere,studenti2$medie_aritmetiche)

#ANALISI FATTORIALE E CORRELAZIONI 
cor(studenti2[10:18])
fa<- factanal((studenti2[10:18]),factors = 3,rotation = "varimax")
fa





