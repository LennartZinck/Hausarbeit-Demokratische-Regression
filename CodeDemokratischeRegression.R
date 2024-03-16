library(haven)
library(tidyverse)
library(formattable)

#Word Values Survey Wichtigkeit von Demokratie
WVS_trends_3_0 <- readRDS("WVS_trends_3_0.rds")

WVS1 <- WVS_trends_3_0 %>%
  group_by(versn_w, e235) %>%
  filter(versn_w %in% c("WVS5", "WVS6", "WVS7"), e235 >= 1) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

legend_labels1 <- c("1 = Gar nicht wichtig", "10 = Äußerst wichtig")

Importance_Plot <- ggplot(data = WVS1, aes(x = e235, y = percentage)) +
  geom_bar(stat = "identity") +
  facet_wrap(~versn_w) +
  xlab("Grafik 1: Relevanz von Demokratie") +
  ylab("%-Anteil der Antworten")

#World Values Survey Demokratielevel im eigenen Land
WVS2 <- WVS_trends_3_0 %>%
  group_by(versn_w, e236) %>%
  filter(versn_w %in% c("WVS5", "WVS6", "WVS7"), e236 >= 1) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

Democraticness_Plot <- ggplot(data = WVS2, aes(x = e236, y = percentage)) +
  geom_bar(stat = "identity") +
  facet_wrap(~versn_w, scales = "free_y", ncol = 1)+
  xlab("Grafik 2: Demokratielevel im eigenen Land") +
  ylab("%-Anteil der Antworten")

ggsave("DemokratiewichtigkeitWVS.png", plot = Importance_Plot, width = 8, height = 6, dpi = 300)
ggsave("DemokratielevelWVS.png", plot = Democraticness_Plot, width = 8, height = 6, dpi = 300)


#Niedrigste Demokratiequalität im Zeitverlauf
Niedrige_Demokratie <- WVS_trends_3_0 %>%
  group_by(versn_w, cow_num, e236) %>%
  filter(versn_w %in% c("WVS5", "WVS6", "WVS7"), e236 >= 1) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(versn_w %in% c("WVS5", "WVS6", "WVS7"), e236 == 1) %>%
  filter(cow_num %in% c(2, 70, 100, 135, 140, 155, 160, 165, 210,
                        255, 352, 360, 365, 369, 600, 640, 663, 710,
                        713, 732, 740, 800, 820, 900)) %>%
  mutate(cow_num = case_when(
    cow_num == 2 ~ "United States of America",
    cow_num == 70 ~ "Mexico",
    cow_num == 100 ~ "Colombia",
    cow_num == 135 ~ "Peru",
    cow_num == 140 ~ "Brazil",
    cow_num == 155 ~ "Chile",
    cow_num == 160 ~ "Argentina",
    cow_num == 165 ~ "Uruguay",
    cow_num == 210 ~ "Netherlands",
    cow_num == 255 ~ "Germany",
    cow_num == 352 ~ "Cyprus",
    cow_num == 360 ~ "Romania",
    cow_num == 365 ~ "Russia",
    cow_num == 369 ~ "Ukraine",
    cow_num == 600 ~ "Morocco",
    cow_num == 640 ~ "Turkey",
    cow_num == 663 ~ "Jordan",
    cow_num == 710 ~ "China",
    cow_num == 713 ~ "Taiwan",
    cow_num == 732 ~ "South Korea",
    cow_num == 740 ~ "Japan",
    cow_num == 800 ~ "Thailand",
    cow_num == 820 ~ "Malaysia",
    cow_num == 900 ~ "Australia",
    TRUE ~ as.character(cow_num)
  ))%>%
  ggplot(aes(x = versn_w, y = percentage))+
  geom_hline(yintercept = 5.23, color ="Red")+
  geom_col()+
  facet_wrap(~cow_num, labeller = as_labeller(cow_num_labels)) +
  xlab("Grafik 2.1: Niedrige Demokratiequalität im Trend")+
  ylab("%-Anteil der Antworten")
ggsave("NiedrigeDemokratieQualitätWVS.png", plot = Niedrige_Demokratie, width = 8, height = 6, dpi = 300)



#Eurobarometer Mannheim Trendfile (1970 - 2002)
Trend7002 <- read_dta("EB_1970-2002_v2.dta")

#Eurobarometer Harmonisierter Datensatz (2004 - 2021)
Trend0421 <- read_dta("harmonised_EB_2004-2021_v3-0-0.dta")


#Line Plot 1 Zufriedenheit mit nationaler Demokratie EU-Kernländer
test2 <- Trend0421 %>% group_by(year, satisdms) %>% filter(!is.na(satisdms), country <= 14, year != 2008, year != 2009) %>% summarize(count = n()) %>% mutate(percentage = count / sum(count) * 100)
test2$satisdms <- factor(test2$satisdms, levels = c("1", "2", "3", "4"))

lineplot_EB <- ggplot(data = test2, aes (x = year, y = percentage, group = satisdms))+
  geom_line()+
  ylim(0, 100)+
  ylab("%-Anteil der Antworten")+
  geom_point(aes(shape = satisdms), size = 2)+
  scale_shape_manual(name = "Zufriedenheit mit der nat. Demokratie",
                     values = c("1" = 2, "2" = 17, "3" = 1, "4" = 19),
                     labels = c("1" = "Sehr Zufrieden", "2" = "Eher Zufrieden", "3" = "Eher Nicht Zufrieden", "4" = "Gar nicht Zufrieden"))


#Line Plot 2 Zufriedenheit mit der Nationalen Demokratie in EU-Kernländern
test3 <- Trend7002 %>% group_by(year, satisdmo) %>% filter(satisdmo <= 4, nation1 <= 14) %>% summarize(count = n()) %>% mutate(percentage = count / sum(count) * 100)
test3$satisdmo <- factor(test3$satisdmo, levels = c("1", "2", "3", "4"))

lineplot2 <- ggplot(data = test3, aes(x = year, y = percentage, group = satisdmo))+
  geom_line()+
  ylim(0, 100)+
  ylab("%-Anteil der Antworten")+
  geom_point(aes(shape = satisdmo), size = 2)+
  scale_shape_manual(name = "Zufriedenheit mit der nat. Demokratie",
                     values = c("1" = 2, "2" = 17, "3" = 1, "4" = 19),
                     labels = c("1" = "Sehr Zufrieden", "2" = "Eher Zufrieden", "3" = "Eher Nicht Zufrieden", "4" = "Gar nicht Zufrieden"))


#Zusammenfügen von Lineplot 1 und 2 Zufriedenheit mit der nationalen Demokratie in EU-Kernländern
test3 <- rename(test3, satisdms = satisdmo)
testfin <- merge(test2, test3, by = c("year", "satisdms", "count", "percentage"), all = T)

combined_plot <- ggplot(data = testfin, aes (x = year, y = percentage, group = satisdms))+
  geom_line()+
  ylim(0, 60)+
  ylab("%-Anteil der Antworten")+
  xlab("")+
  geom_point(aes(shape = satisdms), size = 2)+
  scale_shape_manual(name = "Grafik 3: Zufriedenheit mit der nat. Demokratie",
                     values = c("1" = 2, "2" = 17, "3" = 1, "4" = 19),
                     labels = c("1" = "Sehr Zufrieden", "2" = "Eher Zufrieden", "3" = "Eher Nicht Zufrieden", "4" = "Gar nicht Zufrieden"))+
  theme(legend.position = "top",  
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9))

ggsave("my_line_plot.png", plot = combined_plot, width = 8, height = 6, dpi = 300)


#Lineplot für die Zufriedenheit mit der Demokratie in der EU innerhalb der EU-Kernländer
test4 <- Trend0421 %>% group_by(year, satisdeu) %>% filter(!is.na(satisdeu), country <= 14, year != 2008, year != 2009) %>% summarize(count = n()) %>% mutate(percentage = count / sum(count) * 100)
test4$satisdeu <- factor(test4$satisdeu, levels = c("1", "2", "3", "4"))
test5 <- Trend7002 %>% group_by(year, satisdeu) %>% filter(satisdeu <= 4, nation1 <= 14) %>% summarize(count = n()) %>% mutate(percentage = count / sum(count) * 100)
test5$satisdeu <- factor(test5$satisdeu, levels = c("1", "2", "3", "4"))
testfin2 <- merge(test4, test5, by = c("year", "satisdeu", "count", "percentage"), all = T)

testfin2 <- testfin2 %>% filter(year != 2010)

combined_plot2 <- ggplot(data = testfin2, aes (x = year, y = percentage, group = satisdeu))+
  geom_line()+
  ylim(0, 60)+
  ylab("%-Anteil der Antworten")+
  xlab("")+
  geom_point(aes(shape = satisdeu), size = 2)+
  scale_shape_manual(name = "Grafik 4: Zufriedenheit Demokratie EU",
                     values = c("1" = 2, "2" = 17, "3" = 1, "4" = 19),
                     labels = c("1" = "Sehr Zufrieden", "2" = "Eher Zufrieden", "3" = "Eher Nicht Zufrieden", "4" = "Gar nicht Zufrieden"))+
  theme(legend.position = "top",  
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9))

ggsave("my_line_plot2.png", plot = combined_plot2, width = 8, height = 6, dpi = 300)

#Kontrolle der Zufriedenheit mit der nationalen Demokratie innerhalb der neuen EU-Mitgliedsstaaten
test2 <- Trend0421 %>% group_by(year, satisdms) %>% filter(!is.na(satisdms), country > 18 & country < 31) %>% summarize(count = n()) %>% mutate(percentage = count / sum(count) * 100)
test2$satisdms <- factor(test2$satisdms, levels = c("1", "2", "3", "4"))

lineplot_EBEast <- ggplot(data = test2, aes (x = year, y = percentage, group = satisdms))+
  geom_line()+
  ylim(0, 60)+
  ylab("%-Anteil der Antworten")+
  xlab("Jahr")+
  geom_point(aes(shape = satisdms), size = 2)+
  scale_shape_manual(name = "Grafik 5: Zufriedenheit mit der nat. Demokratie",
                     values = c("1" = 2, "2" = 17, "3" = 1, "4" = 19),
                     labels = c("1" = "Sehr Zufrieden", "2" = "Eher Zufrieden", "3" = "Eher Nicht Zufrieden", "4" = "Gar nicht Zufrieden"))+
  theme(legend.position = "top",  
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9))
ggsave("my_line_plot_OSt.png", plot = lineplot_EBEast, width = 8, height = 6, dpi = 300)

#Kontrolle der Zufriedenheit mit der EU Demokratie innerhalb der neuen EU-Mitgliedsstaaten
test4 <- Trend0421 %>% group_by(year, satisdeu) %>% filter(!is.na(satisdeu), country > 18 & country < 31, year != 2010) %>% summarize(count = n()) %>% mutate(percentage = count / sum(count) * 100)
test4$satisdeu <- factor(test4$satisdeu, levels = c("1", "2", "3", "4"))


lineplot_EBEast2 <- ggplot(data = test4, aes (x = year, y = percentage, group = satisdeu))+
  geom_line()+
  ylim(0, 60)+
  ylab("%-Anteil der Antworten")+
  xlab("")+
  geom_point(aes(shape = satisdeu), size = 2)+
  scale_shape_manual(name = "Grafik 6: Zufriedenheit Demokratie EU",
                     values = c("1" = 2, "2" = 17, "3" = 1, "4" = 19),
                     labels = c("1" = "Sehr Zufrieden", "2" = "Eher Zufrieden", "3" = "Eher Nicht Zufrieden", "4" = "Gar nicht Zufrieden"))+
  theme(legend.position = "top",  
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9))

ggsave("my_line_plot2_Ost.png", plot = lineplot_EBEast2, width = 8, height = 6, dpi = 300)



#Demokratiezufriedenheit und Efficacy der eigenen politischen Handlung in Deutschland (GLES 2009-2021)
GLES <- read_dta("ZA6835_v1-0-0.dta")
GLES2021 <- read_dta("ZA7701_v2-1-0.dta")


#Data Wrangling für die Variable Zufriedenheit mit der Demokratie
GLES2021$year <- 2021
testGLES2 <- GLES2021 %>% group_by(year, q119) %>% filter(q119 >= 1) %>% summarize(count = n()) %>% mutate(percentage = count / sum(count) * 100)
testGLES2$q119 <- factor(testGLES2$q119, levels = c("1", "2", "3", "4"))
testGLES2 <- rename(testGLES2, v70 = q119)

testGLES <- GLES %>% group_by(year, v70) %>% filter(v70 >= 1) %>% summarize(count = n()) %>% mutate(percentage = count / sum(count) * 100)
testGLES$v70 <- factor(testGLES$v70, levels = c("1", "2", "3", "4"))

#Merging den Trend-Datensatz (2009-2017) und die GLES Nachwahlstudie 2021
testGLESfin <- merge(testGLES, testGLES2, by = c("year", "v70", "count", "percentage"), all = T)

#Grafik für die Zufriedenheit mit der Demokratie
lineplotGLES <- ggplot(data = testGLESfin, aes (x = year, y = percentage, group = v70))+
  geom_line()+
  ylim(0, 75)+
  ylab("%-Anteil der Antworten")+
  xlab("")+
  geom_point(aes(shape = v70), size = 2)+
  scale_shape_manual(name = "Grafik 7: Zufriedenheit mit der Demokratie",
                     values = c("1" = 2, "2" = 17, "3" = 1, "4" = 19),
                     labels = c("1" = "Sehr Zufrieden", "2" = "Eher Zufrieden", "3" = "Eher Nicht Zufrieden", "4" = "Gar nicht Zufrieden"))+
  scale_x_continuous(breaks = seq(min(testGLESfin$year), max(testGLESfin$year), by = 1), labels = seq(min(testGLESfin$year), max(testGLESfin$year), by = 1))+
  theme(legend.position = "top",  
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9))


ggsave("DemokratiezufriedenheitGLES.png", plot = lineplotGLES, width = 8, height = 6, dpi = 300)


#Macht es einen Unterschied, wer an der Regierung beteiligt ist? 
table(GLES$v68)
table(GLES2021$q117)

testGLES2 <- GLES2021 %>% group_by(year, q117) %>% filter(q117 >= 1) %>% summarize(count = n()) %>% mutate(percentage = count / sum(count) * 100)
testGLES2$q117 <- factor(testGLES2$q117, levels = c("1", "2", "3", "4", "5"))
testGLES2 <- rename(testGLES2, v68 = q117)

testGLES <- GLES %>% group_by(year, v68) %>% filter(v68 >= 1) %>% summarize(count = n()) %>% mutate(percentage = count / sum(count) * 100)
testGLES$v68 <- factor(testGLES$v68, levels = c("1", "2", "3", "4", "5"))


testGLESfin <- merge(testGLES, testGLES2, by = c("year", "v68", "count", "percentage"), all = T)

lineplotGLES <- ggplot(data = testGLESfin, aes (x = year, y = percentage, group = v68))+
  geom_line()+
  ylim(0, 75)+
  ylab("%-Anteil der Antworten")+
  xlab("")+
  geom_point(aes(shape = v68), size = 2)+
  scale_shape_manual(name = "Grafik 8: Regierungsbeteiligung",
                     values = c("1" = 2, "2" = 17, "3" = 1, "4" = 19, "5" = 3),
                     labels = c("1" = "Keinen Unterschied", "2" = "Eher keinen Unterschied", "3" = "Teils/Teils", "4" = "Eher Unterschied", "5" = "Großer Unterschied"))+
  scale_x_continuous(breaks = seq(min(testGLESfin$year), max(testGLESfin$year), by = 1), labels = seq(min(testGLESfin$year), max(testGLESfin$year), by = 1))+
  theme(legend.position = "top",  
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9))


ggsave("UnterschiedRegierungsbeteiligungGLES.png", plot = lineplotGLES, width = 8, height = 6, dpi = 300)


#Macht die eigene Wahlentscheidung einen Unterschied?
table(GLES$v69)
table(GLES2021$q118)

testGLES2 <- GLES2021 %>% group_by(year, q118) %>% filter(q118 >= 1) %>% summarize(count = n()) %>% mutate(percentage = count / sum(count) * 100)
testGLES2$q118 <- factor(testGLES2$q118, levels = c("1", "2", "3", "4", "5"))
testGLES2 <- rename(testGLES2, v69 = q118)

testGLES <- GLES %>% group_by(year, v69) %>% filter(v69 >= 1) %>% summarize(count = n()) %>% mutate(percentage = count / sum(count) * 100)
testGLES$v69 <- factor(testGLES$v69, levels = c("1", "2", "3", "4", "5"))


testGLESfin <- merge(testGLES, testGLES2, by = c("year", "v69", "count", "percentage"), all = T)

lineplotGLES <- ggplot(data = testGLESfin, aes (x = year, y = percentage, group = v69))+
  geom_line()+
  ylim(0, 75)+
  ylab("%-Anteil der Antworten")+
  xlab("")+
  geom_point(aes(shape = v69), size = 2)+
  scale_shape_manual(name = "Grafik 9: Wahlentscheidung",
                     values = c("1" = 2, "2" = 17, "3" = 1, "4" = 19, "5" = 3),
                     labels = c("1" = "Keinen Unterschied", "2" = "Eher keinen Unterschied", "3" = "Teils/Teils", "4" = "Eher Unterschied", "5" = "Großer Unterschied"))+
  scale_x_continuous(breaks = seq(min(testGLESfin$year), max(testGLESfin$year), by = 1), labels = seq(min(testGLESfin$year), max(testGLESfin$year), by = 1))+
  theme(legend.position = "top",  
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9))


ggsave("UnterschiedDerWahlentscheidungGLES.png", plot = lineplotGLES, width = 8, height = 6, dpi = 300)



