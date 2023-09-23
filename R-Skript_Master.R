getwd()
setwd("/Users/hanneswirnsberger/Documents/Master_Thesis/R_thesis")
getwd()

#Datensätze einlesen
biomass <- read.csv("/Users/hanneswirnsberger/Documents/Master_Thesis/Data/daten_geprueft/Biomasse.csv",
                    header = TRUE, sep =";", dec = ",", na ="NA")
values <- read.csv("/Users/hanneswirnsberger/Documents/Master_Thesis/Finale Datensätze/Biomasse_Erosion.csv",
                   header = TRUE, sep =";", dec = ",", na ="NA")
rusle <- read.csv("/Users/hanneswirnsberger/Documents/Master_Thesis/Data/daten_geprueft/20230512_RUSLE_veg.csv",
                  header = TRUE, sep =",", dec = ".", na ="NA")
erosion <- read.csv("/Users/hanneswirnsberger/Documents/Master_Thesis/Finale Datensätze/Erosionswannen_Werte.csv",
                    header = TRUE, sep =";", dec = ",", na ="NA")

attach(biomass)
biomass_01 <- subset (biomass, site==1)
biomass_02 <- subset (biomass, site==2)
biomass_03 <- subset (biomass, site==3)
biomass_04 <- subset (biomass, site==4)

#Datensatz ohne Extremwerte bei AGB
#data_filter <- subset (data, id!="s4_p04" & id!="s4_p07" & id!="s4_p10" & id!="s4_p11")
#hang_filter <- subset (hang, id!="P4_04" & id!="P4_07" & id!="P4_10" & id!="P4_11")

library(ggplot2)
library(stargazer)
library(ggpubr)
library (EnvStats)
library(ggstatsplot)

#Statistischer Unterschied der AGB zwischen den verschiedenen Sukzesssionszonen

#prüfung auf Normalverteilung
qqnorm(no_outliers_agb01$adj_agb)
hist(no_outliers_agb01$adj_agb)

#Wilcox-test Prüfung
wilcox.test(biomass_02$adj_agb~biomass_03$adj_agb)

#Boxplots, AGB with Moss, Adjusted AGB

my_comparisons <- list(c("1","2"),c("2","3"),c("3","4"))

#Boxplots AGB

ggboxplot(biomass, x = "site", y = "agb_g_m2", fill = "#CCCCCC", outlier.shape = 4,
          bxp.errorbar = TRUE, bxp.errorbar.width = 0.5, outlier.size = 4, 
          xlab = "Site", ylab = "AGB (g/m²)")+
  stat_n_text(y.pos = -100, size = 6)+
  stat_mean_sd_text(y.pos = 1900, size = 6)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", size = 6)+
  theme(text = element_text(size = 25))

#Boxplot Erosionswannen

ggboxplot(erosion, x = "zone", y = "ssy", fill = "#CCCCCC", outlier.shape = NA,
          bxp.errorbar = TRUE, bxp.errorbar.width = 0.5, outlier.size = NA, 
          xlab = "Site", ylab = "AGB (g/m²)")+
  stat_n_text(y.pos = -100, size = 6)+
  stat_mean_sd_text(y.pos = 350, size = 6)+
  labs(y = expression("Mean sediment yield in g/m"^-2))+
  labs(x = "Terrain age zone")+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", size = 6)+
  theme(text = element_text(size = 25))

#Boxplot mit T-test EROSION

#Ausreißer in RUSLE entfernen
#Finden Sie Q1, Q3 und den Interquartilsabstand für Werte in Spalte A.
Q1 <- quantile(rusle$log, .25, na.rm = TRUE)
Q3 <- quantile(rusle$log, .75, na.rm = TRUE)
IQR <- IQR(rusle$log, na.rm = TRUE)

# Behalten Sie nur Zeilen im Dataframe bei, deren Werte innerhalb von 1,5 * IQR von Q1 und Q3 liegen
no_outliers <- subset(rusle, rusle$log> (Q1 - 1.5*IQR) & rusle$log< (Q3 + 1.5*IQR))

#sortieren
rusle <- no_outliers[order(no_outliers$layer),]

#z-standardisieren
rusle$ZDN <- scale(rusle$DN)

#Histrogramm für Darstellung der Verteilung
hist(rusle$log)
hist(biomass$adj_agb_log)
shapiro.test(rusle$log)
biomass$adj_agb_log <- log10(biomass$adj_agb)

#Boxplots mit logarithmierten Werten
my_comparisons_rusle <- list(c("Zone 1","Zone 2"),c("Zone 2","Zone 3"),c("Zone 3","Zone 4"))

ggboxplot(rusle, x = "layer", y = "log", fill = "#CCCCCC",
          bxp.errorbar = TRUE, bxp.errorbar.width = 0.5, 
          xlab = "Sites", ylab = "Soil Erosion (log)")+
  stat_n_text(y.pos = -0.5, size = 6)+
  stat_median_iqr_text(y.pos = 0.075, size = 6)+
  stat_compare_means(comparisons = my_comparisons_rusle, method = "wilcox.test", size = 6)+
  theme(text = element_text(size = 25))

#Vergleich der Mediane AGB

ggplot(values, aes(y = agb_mean, group = zone, x = erosion_mean, label = zone_name, shape = zone_name))+
  geom_point(size = 5)+
  scale_shape_manual(values=c(15, 16, 17, 18))+
  geom_errorbar(aes(ymin=agb_mean-sd_agb, ymax=agb_mean+sd_agb))+
  geom_errorbarh(aes(xmin=erosion_mean-sd_erosion, xmax=erosion_mean+sd_erosion))+
  xlim(-10,1350)+
  ylim(-10,1350)+
  geom_point(shape=10, size=10, shape = "zone_name")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #geom_text(hjust=-0.3, vjust=0.5, size=10)+
  labs(x = expression("Mean sediment yield in g/m"^-2))+
  labs(y = expression("Mean AGB in g/m"^-2))+
  labs(shape='')+
  theme(text = element_text(size = 35))


sd(biomass_01$adj_agb)
sd(biomass_02$adj_agb)
sd(biomass_03$adj_agb)
sd(biomass_04$adj_agb)

#Lineare Regression
plot(biomass$agb_g_m2,biomass$E_median)

model <- lm(biomass$E_median~biomass$agb_g_m2)

abline(model)

summary(model)