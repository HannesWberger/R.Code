biomass <- read.csv("/Users/hanneswirnsberger/Documents/Master_Thesis/Data/daten_geprueft/Biomasse.csv",
                    header = TRUE, sep =";", dec = ",", na ="NA")
values <- read.csv("/Users/hanneswirnsberger/Documents/Master_Thesis/Finale Datensätze/Biomasse_Erosion.csv",
                   header = TRUE, sep =";", dec = ",", na ="NA")
erosion <- read.csv("/Users/hanneswirnsberger/Documents/Master_Thesis/Finale Datensätze/Erosionswannen_Werte.csv",
                    header = TRUE, sep =";", dec = ",", na ="NA")

my_comparisons <- list(c("1","2"),c("2","3"),c("3","4"))

#Boxplots AGB

ggboxplot(biomass, x = "site", y = "agb_g_m2", fill = "#CCCCCC", outlier.shape = 4,
          bxp.errorbar = TRUE, bxp.errorbar.width = 0.5, outlier.size = 4, 
          xlab = "Site", ylab = "AGB (g/m²)")+
  stat_n_text(y.pos = -100, size = 6)+
  stat_mean_sd_text(y.pos = 1900, size = 6)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", size = 6)+
  theme(text = element_text(size = 25))

#Boxplot Erosion

ggboxplot(erosion, x = "zone", y = "ssy", fill = "#CCCCCC", outlier.shape = NA,
          bxp.errorbar = TRUE, bxp.errorbar.width = 0.5, outlier.size = NA, 
          xlab = "Site", ylab = "AGB (g/m²)")+
  stat_n_text(y.pos = -100, size = 6)+
  stat_mean_sd_text(y.pos = 350, size = 6)+
  labs(y = expression("Mean sediment yield in g/m"^-2))+
  labs(x = "Terrain age zone")+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", size = 6)+
  theme(text = element_text(size = 25))

#Comparison AGB and Erosion

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
