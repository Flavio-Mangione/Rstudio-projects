#Impostiamo la working directory dove dovremo lavorare
setwd("C:/Users/flavi/Downloads")
#Leggiamo il file CAST.csv
cast<-read.csv('CAST.csv', header=T,sep=',')
altezza<-cast$altezza
peso<-cast$peso
età<-cast$eta.
genere<-cast$genere
table(genere)

#Analisi descrittiva del peso in base al genere con la funzione descr del pacchetto summarytools
library(summarytools)
descr(peso)
peso1<-subset(peso,genere=='1')
descr(peso1)
peso2<-subset(peso,genere=='2')
descr(peso2)


#Diagramma a dispersione dell'altezza in base al età
library(ggplot2)
ggplot(cast, aes(x = età, y = altezza, col=factor(genere))) +
  geom_point(size=3) +
  scale_x_continuous(name = "Età a partire dai 18 anni", limits = c(18, max(età)), breaks = seq(18, max(età), 5)) +
  scale_y_continuous(name = "Altezza in cm", limits = c(min(altezza), max(altezza)), breaks = seq(min(altezza), max(altezza), 10)) +
  ggtitle("Grafico a Dispersione Età-Altezza genere 1-2") +
  scale_color_manual(values = c("red", "blue"), labels = c("genere 1", "genere 2"))


#Boxplot peso in base al genere 1 e genere 2
pesi <- data.frame(peso = c(peso1, peso2), genere = rep(c("1", "2"), times = c(length(peso1), length(peso2))))
media1=mean(peso1)
media2=mean(peso2)
library(ggplot2)
ggplot(pesi, aes(x = genere, y = peso, fill = genere)) +
  geom_boxplot() +
  geom_point(aes(x = "1", y = media1), color = "white", shape=8) +
  geom_point(aes(x = "2", y = media2), color = "white", shape=8) +
  geom_text(aes(x = "1", y = media1, label = paste("Media:", round(media1, 2))), 
            color = "gold", vjust = -1) +
  geom_text(aes(x = "2", y = media2, label = paste("Media:", round(media2, 2))), 
            color = "blue", vjust = -1) +
  scale_fill_manual(values = c("deeppink1", "cyan3")) +
  xlab("Genere") +
  ylab("Peso in kg") +
  ggtitle("Boxplot peso in base al genere")+ theme_light()


#Individuiamo gli outlier del boxplot
IQR1<-21.25
Linf<-quantile(peso1,0.25) - 1.5*IQR1
Lsup<-quantile(peso1,0.75) + 1.5*IQR1
outlier1<-peso1[peso1< Linf| peso1>Lsup]
IQR2<-19.00
Linf<-quantile(peso2,0.25) - 1.5*IQR2
Lsup<-quantile(peso2,0.75) + 1.5*IQR2
outlier2<-peso2[peso2< Linf| peso2>Lsup]
data_outlier<-data.frame(outlier1,outlier2)


#Analisi descrittiva della altezza in base al genere con il comando descr del pacchetto summarytools
altezza1<-subset(altezza,genere=='1')
altezza2<-subset(altezza,genere=='2')
descr(altezza1)
descr(altezza2)


#Boxplot altezza in base al genere 1 e 2
altezze<- data.frame(altezza = c(altezza1, altezza2), genere = rep(c("1", "2"), times = c(length(altezza1), length(altezza2))))
media1=mean(altezza1)
media2=mean(altezza2)
library(ggplot2)
ggplot(altezze, aes(x = genere, y = altezza, fill = genere)) +
  geom_boxplot() +
  geom_point(aes(x = "1", y = media1), color = "white", shape=8) +
  geom_point(aes(x = "2", y = media2), color = "white", shape=8) +
  geom_text(aes(x = "1", y = media1, label = paste("Media:", round(media1, 2))), 
            color = "gold", vjust = -1) +
  geom_text(aes(x = "2", y = media2, label = paste("Media:", round(media2, 2))), 
            color = "firebrick1", vjust = -1) +
  scale_fill_manual(values = c("mediumorchid2", "orange")) +
  xlab("Genere") +
  ylab("Altezze") +
  ggtitle("Boxplot altezze in base al genere")+ theme_gray()

#Scatterplot peso vs altezza per il genere 1
dati<-data.frame(peso1,altezza1)
cor(peso1,altezza1)
library(ggplot2)
ggplot(dati, aes(x = peso1, y = altezza1)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Diagramma a dispersione Peso vs Altezza genere 1",
       x = "Peso (kg)",
       y = "Altezza (cm)")


#Scatterplot peso vs altezza per il genere 2
dati2<-data.frame(altezza2,peso2)
cor(peso2,altezza2)
library(ggplot2)
ggplot(dati2, aes(x = peso2, y = altezza2)) +
geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Diagramma a dispersione Peso vs Altezza genere 2",
       x = "Peso (kg)",
       y = "Altezza (cm)")


#Confronto istogrammi altezza per il genere 1 e 2 con età magggiore o uguale a 18
par(mfrow=c(1,2))
dati_filtrati <- subset(cast, eta. >= 18 & genere == 1)
library(summarytools)
descr(dati_filtrati$altezza)
breaks <- seq(145.0, 190.0, by=5.0)
freq <- table(cut(dati_filtrati$altezza, breaks, right=T))
cbind(freq, freq/nrow(dati_filtrati))
hist(dati_filtrati$altezza, main="Istogramma delle altezze del genere 1 con età maggiore o uguale a 18", xlab="Altezza in cm",ylab="Densità",freq=F,col='pink', breaks = seq(145, 190, by = 5),xlim=c(145, 190))
lines(density(dati_filtrati$altezza), lwd = 3, col = "purple")
abline(v = mean(dati_filtrati$altezza), col = "blue", lwd = 2) 
abline(v = median(dati_filtrati$altezza), col = "green", lwd = 2) 
abline(v = quantile(dati_filtrati$altezza, 0.25), col = "orange", lty = 2, lwd = 2) 
abline(v = quantile(dati_filtrati$altezza, 0.75), col = "yellow", lty = 2, lwd = 2) 
legend("topright",c("Primo quartile","Media","Mediana","Terzo quartile"),fill=c("orange","blue","green","yellow"),cex=0.5) 
lines(density(dati_filtrati$altezza,bw="SJ"),lty=3,col="blue",lwd=2)

dati_filtrati2 <- subset(cast, eta. >= 18 & genere == 2)
library(summarytools)
descr(dati_filtrati2$altezza)
breaks <- seq(160.0, 205.0, by=5.0)
freq <- table(cut(dati_filtrati2$altezza, breaks, right=T))
cbind(freq, freq/nrow(dati_filtrati2))
hist(dati_filtrati2$altezza, main="Istogramma delle altezze del genere 2 con età maggiore o uguale a 18", xlab="Altezza in cm",ylab="Densità",freq=F,col="lightblue", breaks = seq(160, 205, by = 5),xlim=c(160, 205))
lines(density(dati_filtrati2$altezza), lwd = 3, col = "purple")
abline(v = mean(dati_filtrati2$altezza), col = "blue", lwd = 2) 
abline(v = median(dati_filtrati2$altezza), col = "green", lwd = 2) 
abline(v = quantile(dati_filtrati2$altezza, 0.25), col = "orange", lty = 2, lwd = 2) 
abline(v = quantile(dati_filtrati2$altezza, 0.75), col = "yellow", lty = 2, lwd = 2) 
lines(density(dati_filtrati2$altezza,bw=3),lty=3,col="blue",lwd=2)
legend("topright",c("Primo quartile","Media","Mediana","Terzo quartile"),fill=c("orange","blue","green","yellow"),cex=0.5)


#Studiamo anche indice di massa corporea degli individui IMC
IMC<-data.frame(IMC=peso/(altezza/100)^2)
cast_new<-data.frame(cast,IMC)
indicatori <- ifelse(età >= 18,ifelse(IMC >= 18.50 & IMC <= 24.99, "normopeso",ifelse(IMC < 18.50, "sottopeso", "sovrappeso")),0)
stato_peso <- data.frame(
  Genere = rep(c("Genere 1 (Donne)", "Genere 2 (Uomini)"), each = 3),
  Status = c("Sottopeso", "Normopeso", "Sovrappeso"),
  Numero = c(
    sum(indicatori == "sottopeso" & genere == "1"),
    sum(indicatori == "normopeso" & genere == "1"),
    sum(indicatori == "sovrappeso" & genere == "1"),
    sum(indicatori == "sottopeso" & genere == "2"),
    sum(indicatori == "normopeso" & genere == "2"),
    sum(indicatori == "sovrappeso" & genere == "2")))


# Creazione del grafico a barre con ggplot2 con lo stato di peso degli individui in base all'indice di massa corporea
library(ggplot2)
ggplot(stato_peso, aes(x = Status, y = Numero, fill = Genere)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Stato", y = "Numero di persone", fill = "Genere") +
  scale_fill_manual(values = c("indianred1", "cornflowerblue")) +
  ggtitle("Diagramma a barre IMC genere 1 e genere 2")+
  theme_minimal()


#Modelli di regressione lineare multipla della variabile dipendente peso 
model_peso<-lm(peso~altezza+genere+età,data=cast)
summary(model_peso)
par(mfrow=c(2,2))
plot(model_peso,col='dodgerblue2')
stimato<-fitted(model_peso)
residui<-resid(model_peso)
residual1<-data.frame(osservato=peso,stimato,residui,genere)
#Equazione del modello di regressione lineare multipla della variabile dipendete peso
p<-(-54.34071)+(0.76330)*altezza+(-3.99430)*genere+(0.24205)*età 
#Applichiamo equazione del modello di regressione multipla per trovare il peso stimato di Jane
peso_Jane<-(-54.34071)+(0.76330)*165+(-3.99430)*1+(0.24205)*68
peso_Jane 
Residuo_Jane<-55-peso_Jane #un residuo molto alto!


#Modello di regressione lineare multipla della variabile dipendente altezza
model_Altezza<-lm(altezza~peso+età+genere,data=cast)
summary(model_Altezza)
par(mfrow=c(2,2))
plot(model_Altezza,col='darkgoldenrod1')
stimato<-fitted(model_Altezza)
residui<-resid(model_Altezza)
residual2<-data.frame(osservato=altezza,stimato,residui,genere)
#Equazione del modello di regressione lineare multipla della variabile dipendete altezza
q<-144.57 + (12.228)*genere + (0.1641)*peso - (0.1298)*età


#ANALIZZIAMO IL SECONDO DATASET 
crop<-read.csv('crop1.data.csv', header=T,sep=';')  
resa<-crop$resa
library(summarytools)
descr(resa)
fertilizzante<-crop$fertilizante
densità<-crop$densita
blocco<-crop$blocco
table(fertilizzante)
table(blocco)
table(densità)


#Facciamo un boxplot della resa rispetto alla densità
resa1<-subset(resa,densità=='1')
library(summarytools)
descr(resa1)
resa2<-subset(resa,densità=='2')
library(summarytools)
descr(resa2)
rese<- data.frame(resa = c(resa1, resa2), densità = rep(c("Rado", "Fitto"), times = c(length(resa1), length(resa2))))
media1=mean(resa1)
media2=mean(resa2)
library(ggplot2)
ggplot(rese, aes(x = densità, y = resa, fill = densità)) +
  geom_boxplot() +
  geom_point(aes(x = "Rado", y = media1), color = "white", shape=8) +
  geom_point(aes(x = "Fitto", y = media2), color = "white", shape=8) +
  geom_text(aes(x = "Rado", y = media1, label = paste("Media:", round(media1, 2))), 
            color = "gold", vjust = -1) +
  geom_text(aes(x = "Fitto", y = media2, label = paste("Media:", round(media2, 2))), 
            color = "gold", vjust = -1) +
  scale_fill_manual(values = c("chartreuse3", "forestgreen")) +
  xlab("Densità della piantagione") +
  ylab("Resa del mais") +
  ggtitle("Boxplot resa del mais in base alla densità di piantagione")+ theme_light()


#Facciamo un boxplot della resa rispetto al tipo di fertilizzante
resa1<-subset(resa,fertilizzante=='1')
descr(resa1)
resa2<-subset(resa,fertilizzante=='2')
descr(resa2)
resa3<-subset(resa,fertilizzante=='3')
descr(resa3)
rese<- data.frame(resa = c(resa1, resa2,resa3), fertilizzante = rep(c("1", "2","3"), times = c(length(resa1), length(resa2),length(resa3))))
media1=mean(resa1)
media2=mean(resa2)
media3=mean(resa3)
library(ggplot2)
ggplot(rese, aes(x = fertilizzante, y = resa, fill = fertilizzante)) +
  geom_boxplot() +
  geom_point(aes(x = "1", y = media1), color = "white", shape=8) +
  geom_point(aes(x = "2", y = media2), color = "white", shape=8) +
  geom_point(aes(x = "3", y = media3), color = "white", shape=8) +
  geom_text(aes(x = "1", y = media1, label = paste("Media:", round(media1, 2))), 
            color = "gold", vjust = -2) +
  geom_text(aes(x = "2", y = media2, label = paste("Media:", round(media2, 2))), 
            color = "gold", vjust = -2) +
  geom_text(aes(x = "3", y = media3, label = paste("Media:", round(media3, 2))), 
            color = "gold", vjust = -2) +
  scale_fill_manual(values=c("gray49", "darkgoldenrod4", "aquamarine3")) +
  xlab("Fertilizzante") +
  ylab("Resa del mais") +
  ggtitle("Boxplot resa in base al tipo di fertilizzante")+ theme_gray()


#Facciamo un boxplot della resa rispetto ai blocchi di trattamento
resa1<-subset(resa,blocco=='1')
descr(resa1)
resa2<-subset(resa,blocco=='2')
descr(resa2)
resa3<-subset(resa,blocco=='3')
descr(resa3)
resa4<-subset(resa,blocco=='4')
descr(resa4)
rese<- data.frame(resa = c(resa1, resa2,resa3, resa4), blocco = rep(c("1", "2","3","4"), times = c(length(resa1), length(resa2),length(resa3),length(resa4))))
media1=mean(resa1)
media2=mean(resa2)
media3=mean(resa3)
media4=mean(resa4)
library(ggplot2)
ggplot(rese, aes(x = blocco, y = resa, fill = blocco)) +
  geom_boxplot() +
  geom_point(aes(x = "1", y = media1), color = "white", shape=8) +
  geom_point(aes(x = "2", y = media2), color = "white", shape=8) +
  geom_point(aes(x = "3", y = media3), color = "white", shape=8) +
  geom_point(aes(x = "4", y = media4), color = "white", shape=8) +
  geom_text(aes(x = "1", y = media1, label = paste("Media:", round(media1, 2))), 
            color = "gold", vjust = -1) +
  geom_text(aes(x = "2", y = media2, label = paste("Media:", round(media2, 2))), 
            color = "gold", vjust = -1) +
  geom_text(aes(x = "3", y = media3, label = paste("Media:", round(media3, 2))), 
            color = "gold", vjust = -1) +
  geom_text(aes(x = "4", y = media4, label = paste("Media:", round(media4, 2))), 
            color = "gold", vjust = -1) +
  scale_fill_manual(values=c("gray49", "burlywood3", "deepskyblue3","red")) +
  xlab("Blocchi di trattamento") +
  ylab("Resa del mais") +
  ggtitle("Boxplot resa del mais in base al blocco di trattamento")+ theme_gray()

#Modello di regressione lineare della variabile dipendente resa in funzione della variabile esplicativa fertilizzante
model_resa1=lm(resa~fertilizzante)
par(mfrow=c(2,2))
plot(model_resa1,col=c('blue','indianred1'),pch=19)
summary(model_resa1)
plot(model_resa1)


#Modello di regressione lineare della variabile dipendente resa 
model_resa2=lm(resa~fertilizzante+densità)
summary(model_resa2)
par(mfrow=c(2,2))
plot(model_resa2,col=c('lightpink1','aquamarine'),pch=19)
stimato<-fitted(model_resa2)
residui<-resid(model_resa2)
residual3<-data.frame(osservato=resa,stimato,residui)


#Modello di regressione lineare della variabile dipendente resa con interazione tra fertilizzante e densità
model_resa3=lm(resa~fertilizzante*densità)
summary(model_resa3)
par(mfrow=c(2,2))
plot(model_resa3,col='green2', pch=19)
stimato<-fitted(model_resa3)
residui<-resid(model_resa3)
residual4<-data.frame(osservato=resa,stimato,residui)


#Modello di regressione lineare della variabile dipendente resa
model_resa4=lm(resa~fertilizzante+blocco+densità)
summary(model_resa4)
stimato<-fitted(model_resa4)
residui<-resid(model_resa4)
residual5<-data.frame(osservato=resa,stimato,residui)





