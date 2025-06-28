rm(list=ls())
# Risultati prior ---------------------------------------------------------
library(tidyverse)
library(xtable)

setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Risultati/prior")

load("prior_1.1.RData")# n = 25, tau = c(0.4, 0.2, 0.4), m = c(4,4,4,3,3,3), delta = 0.55
load("prior_1.RData") # n = 25, tau = c(0.4, 0.2, 0.4), m = c(4,4,4,3,3,3), delta = 0.4
load("prior_1.2.RData") # n = 25, tau = c(0.4, 0.2, 0.4), m = c(4,4,4,3,3,3), delta = 0.7
load("prior_bc4.1.RData")
load("prior_bc4.RData")
load("prior_bc4.2.RData")


prior_1 <- cbind(prior_1, prior_bc4)
prior_1.1 <- cbind(prior_1.1, prior_bc4.1)
prior_1.2 <- cbind(prior_1.2, prior_bc4.2)


prior1 <- rbind(cbind(prior_1, "delta" = 0.4), 
                cbind(prior_1.1, "delta" = 0.55),
                cbind(prior_1.2, "delta" = 0.7))

load("prior_2.1.RData") #n = 50, delta = 0.55
load("prior_2.2.RData") #n = 50, delta = 0.7
load("prior_2.RData") # n = 50, delta = 0.4

load("prior_bc4.2.0.RData")
load("prior_bc4.2.1.RData")
load("prior_bc4.2.2.RData")

prior_2 <- cbind(prior_2, prior_bc4.2)
prior_2.1 <- cbind(prior_2.1, prior_bc4.2.1)
prior_2.2 <- cbind(prior_2.2, prior_bc4.2.2)

prior2 <- rbind(cbind(prior_2, "delta" = 0.4),
                cbind(prior_2.1, "delta" = 0.55), 
                cbind(prior_2.2, "delta" = 0.7))

load("prior_3.1.RData") # n = 100, delta = 0.55
load("prior_3.2.RData") # n = 100, delta = 0.7
load("prior_3.RData") #n = 100, delta = 0.4


load("prior_bc4.3.RData")
load("prior_bc4.3.1.RData")
load("prior_bc4.3.2.RData")

prior_3 <- cbind(prior_3, prior_bc4.3)
prior_3.1 <- cbind(prior_3.1, prior_bc4.3.1)
prior_3.2 <- cbind(prior_3.2, prior_bc4.3.2)

prior3 <- rbind(cbind(prior_3, "delta" = 0.4),
                cbind(prior_3.1, "delta" = 0.55),
                cbind(prior_3.2, "delta" = 0.7))


load("prior_4.1.RData") # n = 200, delta = 0.55
load("prior_4.2.RData") # n = 200, delta = 0.7
load("prior_4.RData") #n = 200, delta = 0.4
load("prior_bc4.4.RData")
load("prior_bc4.4.1.RData")
load("prior_bc4.4.2.RData")


prior_4 <- cbind(prior_4, prior_bc4.4)
prior_4.1 <- cbind(prior_4.1, prior_bc4.4.1)
prior_4.2 <- cbind(prior_4.2, prior_bc4.4.2)


prior4 <- rbind(cbind(prior_4, "delta" = 0.4), 
                cbind(prior_4.1, "delta" = 0.55),
                cbind(prior_4.2, "delta" = 0.7))


ris <- rbind(cbind(prior1, "n" = 25), cbind(prior2, "n" = 50),
             cbind(prior3, "n" = 100), cbind(prior4, "n" = 200))

str(ris)

names(ris) <- gsub("\\.\\.\\.", "=", names(ris))
names(ris)[2] <- "NMI.b=0.1"

ris$`ARI.c=4` = ris$`ARI.b=4`
ris$`NMI.c=4` = ris$`NMI.b=4`


ris_long <- ris %>%
  pivot_longer(
    cols = -c(n,delta),
    names_to = c(".value", "Metodo"), 
    names_pattern = "(ARI|NMI).(.+)" 
  ) %>%
  mutate(Metodo = str_to_upper(Metodo))

summary <- ris_long %>%
  group_by(n, Metodo, delta) %>%
  filter(startsWith(Metodo, "C") | startsWith(Metodo, "c"))%>%
  summarise(
    ARI_Media = mean(ARI, na.rm = TRUE),
    ARI_Mediana = median(ARI, na.rm = TRUE),
    ARI_DevStandard = sd(ARI, na.rm = TRUE),
    NMI_Media = mean(NMI, na.rm = TRUE),
    NMI_Mediana = median(NMI, na.rm = TRUE),
    NMI_DevStandard = sd(NMI, na.rm = TRUE),
    .groups = 'drop' # Rimuove il raggruppamento dopo summarise
  )
summary

digits_colonne <- c(0, 0, 0, 2, 6, 6, 6, 6, 6, 6)
tabella_sim <- xtable(summary, digits = digits_colonne)
print(tabella_sim, include.rownames = FALSE, type = "latex")


library(stringr)

ris_long <- ris_long %>%
  mutate(Metodo = str_replace(Metodo, "^B", "b")) %>%
  mutate(Metodo = str_replace(Metodo, "^C", "c")) 

summary_b <- ris_long %>%
  group_by(n, Metodo, delta) %>%
  filter(startsWith(Metodo, "b"))%>%
  summarise(
    ARI_Media = mean(ARI, na.rm = TRUE),
    ARI_Mediana = median(ARI, na.rm = TRUE),
    ARI_DevStandard = sd(ARI, na.rm = TRUE),
    NMI_Media = mean(NMI, na.rm = TRUE),
    NMI_Mediana = median(NMI, na.rm = TRUE),
    NMI_DevStandard = sd(NMI, na.rm = TRUE),
    .groups = 'drop' # Rimuove il raggruppamento dopo summarise
  )
summary_b

digits_colonne <- c(0, 0, 0, 2, 6, 6, 6, 6, 6, 6)
tabella_sim <- xtable(summary_b, digits = digits_colonne)
print(tabella_sim, include.rownames = FALSE, type = "latex")



summary_eb <- ris_long %>%
  group_by(n, Metodo, delta) %>%
  filter(startsWith(Metodo, "EB")| startsWith(Metodo, "TRUE"))%>%
  summarise(
    ARI_Media = mean(ARI, na.rm = TRUE),
    ARI_Mediana = median(ARI, na.rm = TRUE),
    ARI_DevStandard = sd(ARI, na.rm = TRUE),
    NMI_Media = mean(NMI, na.rm = TRUE),
    NMI_Mediana = median(NMI, na.rm = TRUE),
    NMI_DevStandard = sd(NMI, na.rm = TRUE),
    .groups = 'drop' # Rimuove il raggruppamento dopo summarise
  )%>%
  mutate(Metodo = str_replace(Metodo, "EB.STRENGHT", "strength"))

digits_colonne <- c(0, 0, 0, 2, 6, 6, 6, 6, 6, 6)
tabella_sim <- xtable(summary_eb, digits = digits_colonne)
print(tabella_sim, include.rownames = FALSE, type = "latex")


# ANALISI GRAFICA ---------------------------------------------------------

livelli_ordinati <- c("c=0.1", "c=1", "c=4","c=10","c=50","b=0.1","b=1","b=4",
                      "b=10","b=50","EB.STRENGTH=1","EB.STRENGTH=10","EB.STRENGTH=30","EB.STRENGTH=50",
                      "TRUE")

# ris_long$Metodo <- factor(ris_long$Metodo, levels = livelli_ordinati)
# levels(ris_long$Metodo)



#ARI per c al variare di n----------------------

library(RColorBrewer)
n <- 6
my_color <- brewer.pal(n = n, name = "Spectral")

setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Grafici/Prior")
ari_n25 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 25, startsWith(Metodo, "c") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))



boxplot_ari_n25 <- ggplot(ari_n25, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n25)
ggsave(boxplot_ari_n25, file = "c_ARI_25.pdf", width = 8, height = 6)

#n = 50--------------------------------------------

ari_n50 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 50, startsWith(Metodo, "c") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_ari_n50 <- ggplot(ari_n50, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n50)
ggsave(boxplot_ari_n50, file = "c_ARI_50.pdf", width = 8, height = 6)

#n = 100--------------------------------------------

ari_n100 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 100, startsWith(Metodo, "c") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_ari_n100 <- ggplot(ari_n100, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n100)
ggsave(boxplot_ari_n100, file = "c_ARI_100.pdf", width = 8, height = 6)


#n = 200--------------------------------------------

ari_n200 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 200, startsWith(Metodo, "c") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_ari_n200 <- ggplot(ari_n200, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n200)
ggsave(boxplot_ari_n200, file = "c_ARI_200.pdf", width = 8, height = 6)

#ARI per b al variare di n---------------------------

n <- 6
my_color <- brewer.pal(n = n, name = "PRGn")

#n = 100--------------------------------------------

ari_n25 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 25, startsWith(Metodo, "b") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_ari_n25 <- ggplot(ari_n25, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n25)
ggsave(boxplot_ari_n25, file = "b_ARI_25.pdf", width = 8, height = 6)


#n = 50----------------------------------

ari_n50 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 50, startsWith(Metodo, "b") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_ari_n50 <- ggplot(ari_n50, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n50)
ggsave(boxplot_ari_n50, file = "b_ARI_50.pdf", width = 8, height = 6)


#n = 100-------------------------------------

ari_n100 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 100, startsWith(Metodo, "b") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_ari_n100 <- ggplot(ari_n100, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n100)
ggsave(boxplot_ari_n100, file = "b_ARI_100.pdf", width = 8, height = 6)

#n = 200--------------------------------------

ari_n200 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 200, startsWith(Metodo, "b") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_ari_n200 <- ggplot(ari_n200, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n200)
ggsave(boxplot_ari_n200, file = "b_ARI_200.pdf", width = 8, height = 6)

#EMPIRICAL BAYES---------------------------------------------

#n = 100--------------------------------------------

n <- 6
my_color <- brewer.pal(n = n, name = "RdYlGn")

ris_long <- ris_long %>%
  mutate(Metodo = str_replace(Metodo, "EB.STRENGHT", "EB.STRENGTH"))


ari_n25 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 25, startsWith(Metodo, "EB") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati)) 


boxplot_ari_n25 <- ggplot(ari_n25, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n25)
ggsave(boxplot_ari_n25, file = "eb_ARI_25.pdf", width = 8, height = 6)


#n = 50----------------------------------

ari_n50 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 50, startsWith(Metodo, "EB") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_ari_n50 <- ggplot(ari_n50, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n50)
ggsave(boxplot_ari_n50, file = "eb_ARI_50.pdf", width = 8, height = 6)


#n = 100-------------------------------------

ari_n100 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 100, startsWith(Metodo, "EB") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_ari_n100 <- ggplot(ari_n100, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n100)
ggsave(boxplot_ari_n100, file = "eb_ARI_100.pdf", width = 8, height = 6)


#n = 200--------------------------------------

ari_n200 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 200, startsWith(Metodo, "EB") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_ari_n200 <- ggplot(ari_n200, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n200)
ggsave(boxplot_ari_n200, file = "eb_ARI_200.pdf", width = 8, height = 6)



#NMI per c al variare di n----------------------

n <- 6
my_color <- brewer.pal(n = n, name = "Spectral")


nmi_n25 <- ris_long %>%
  dplyr::select(delta, NMI, n, Metodo) %>%
  filter(n == 25, startsWith(Metodo, "c") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_nmi_n25 <- ggplot(nmi_n25, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmi_n25)
ggsave(boxplot_nmi_n25, file = "c_NMI_25.pdf", width = 8, height = 6)

#n = 50--------------------------------------------

nmi_n50 <- ris_long %>%
  dplyr::select(delta, NMI, n, Metodo) %>%
  filter(n == 50, startsWith(Metodo, "c") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_nmi_n50 <- ggplot(nmi_n50, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmi_n50)
ggsave(boxplot_nmi_n50, file = "c_NMI_50.pdf", width = 8, height = 6)

#n = 100--------------------------------------------

nmi_n100 <- ris_long %>%
  dplyr::select(delta, NMI, n, Metodo) %>%
  filter(n == 100, startsWith(Metodo, "c") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_nmi_n100 <- ggplot(nmi_n100, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmi_n100)
ggsave(boxplot_nmi_n100, file = "c_NMI_100.pdf", width = 8, height = 6)

#n = 200--------------------------------------------

nmi_n200 <- ris_long %>%
  dplyr::select(delta, NMI, n, Metodo) %>%
  filter(n == 200, startsWith(Metodo, "c") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_nmi_n200 <- ggplot(nmi_n200, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmi_n200)
ggsave(boxplot_nmi_n200, file = "c_NMI_200.pdf", width = 8, height = 6)


#ARI per b al variare di n---------------------------

n <- 6
my_color <- brewer.pal(n = n, name = "PRGn")

#n = 25--------------------------------------------



nmi_n25 <- ris_long %>%
  dplyr::select(delta, NMI, n, Metodo) %>%
  filter(n == 25, startsWith(Metodo, "b") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_nmi_n25 <- ggplot(nmi_n25, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmi_n25)
ggsave(boxplot_nmi_n25, file = "b_NMI_25.pdf", width = 8, height = 6)


#n = 50----------------------------------

nmi_n50 <- ris_long %>%
  dplyr::select(delta, NMI, n, Metodo) %>%
  filter(n == 50, startsWith(Metodo, "b") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_nmi_n50 <- ggplot(nmi_n50, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmi_n50)
ggsave(boxplot_nmi_n50, file = "b_NMI_50.pdf", width = 8, height = 6)

#n = 100-------------------------------------

nmi_n100 <- ris_long %>%
  dplyr::select(delta, NMI, n, Metodo) %>%
  filter(n == 100, startsWith(Metodo, "b") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_nmi_n100 <- ggplot(nmi_n100, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmi_n100)
ggsave(boxplot_nmi_n100, file = "b_NMI_100.pdf", width = 8, height = 6)


#n = 200--------------------------------------

nmi_n200 <- ris_long %>%
  dplyr::select(delta, NMI, n, Metodo) %>%
  filter(n == 200, startsWith(Metodo, "b") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_nmi_n200 <- ggplot(nmi_n200, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmi_n200)
ggsave(boxplot_nmi_n200, file = "b_NMI_200.pdf", width = 8, height = 6)


#EMPIRICAL BAYES---------------------------------------------

n <- 6
my_color <- brewer.pal(n = n, name = "RdYlGn")

#n = 100--------------------------------------------

nmi_n25 <- ris_long %>%
  dplyr::select(delta, NMI, n, Metodo) %>%
  filter(n == 25, startsWith(Metodo, "EB") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_nmi_n25 <- ggplot(nmi_n25, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmi_n25)
ggsave(boxplot_nmi_n25, file = "eb_NMI_25.pdf", width = 8, height = 6)

#n = 50----------------------------------

nmi_n50 <- ris_long %>%
  dplyr::select(delta, NMI, n, Metodo) %>%
  filter(n == 50, startsWith(Metodo, "EB") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_nmi_n50 <- ggplot(nmi_n50, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmi_n50)
ggsave(boxplot_nmi_n50, file = "eb_NMI_50.pdf", width = 8, height = 6)

#n = 100-------------------------------------

nmi_n100 <- ris_long %>%
  dplyr::select(delta, NMI, n, Metodo) %>%
  filter(n == 100, startsWith(Metodo, "EB") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_nmi_n100 <- ggplot(nmi_n100, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmi_n100)
ggsave(boxplot_nmi_n100, file = "eb_NMI_100.pdf", width = 8, height = 6)


#n = 200--------------------------------------

nmi_n200 <- ris_long %>%
  dplyr::select(delta, NMI, n, Metodo) %>%
  filter(n == 200, startsWith(Metodo, "EB") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_nmi_n200 <- ggplot(nmi_n200, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmi_n200)
ggsave(boxplot_nmi_n200, file = "eb_NMI_200.pdf", width = 8, height = 6)



# confronto b = 4 vs EB -------------------------


n <- 6
my_color <- brewer.pal(n = n, name = "PiYG")


ari_n25 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 25, Metodo == "b=4" | startsWith(Metodo, "EB") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_ari_n25 <- ggplot(ari_n25, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n25)
ggsave(boxplot_ari_n25, file = "confronto_ARI_25.pdf", width = 8, height = 6)


#n = 50----------------------------------

ari_n50 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 50, Metodo == "b=4" | startsWith(Metodo, "EB") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_ari_n50 <- ggplot(ari_n50, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n50)
ggsave(boxplot_ari_n50, file = "confronto_ARI_50.pdf", width = 8, height = 6)


#n = 100-------------------------------------

ari_n100 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 100, Metodo == "b=4" | startsWith(Metodo, "EB") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_ari_n100 <- ggplot(ari_n100, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n100)
ggsave(boxplot_ari_n100, file = "confronto_ARI_100.pdf", width = 8, height = 6)

#n = 200--------------------------------------

ari_n200 <- ris_long %>%
  dplyr::select(delta, ARI, n, Metodo) %>%
  filter(n == 200, Metodo == "b=4" | startsWith(Metodo, "EB") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = factor(Metodo, levels = livelli_ordinati))


boxplot_ari_n200 <- ggplot(ari_n200, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n200)
ggsave(boxplot_ari_n200, file = "confronto_ARI_200.pdf", width = 8, height = 6)

#eb n = 1000 -----------------------------------------



setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Risultati/prior")
load("prior_5.RData")
load("prior_5.1.RData")
load("prior_5.2.RData")

prior_5 <- rbind(cbind(prior_5, "delta" = 0.4),
                 cbind(prior_5.1, "delta" = 0.55),
                 cbind(prior_5.2, "delta" = 0.7))

names(prior_5) <- gsub("\\.\\.\\.", "=", names(prior_5))
names(prior_5)[2] <- "NMI.b=0.1"





prior_5_long <- prior_5 %>%
  pivot_longer(
    cols = -c(delta),
    names_to = c(".value", "Metodo"), 
    names_pattern = "(ARI|NMI).(.+)" 
  ) %>%
  mutate(Metodo = str_to_upper(Metodo))




summary_prior_5 <- prior_5_long %>%
  group_by( Metodo, delta) %>%
  filter(startsWith(Metodo, "EB")| startsWith(Metodo, "TRUE"))%>%
  summarise(
    ARI_Media = mean(ARI, na.rm = TRUE),
    ARI_Mediana = median(ARI, na.rm = TRUE),
    ARI_DevStandard = sd(ARI, na.rm = TRUE),
    NMI_Media = mean(NMI, na.rm = TRUE),
    NMI_Mediana = median(NMI, na.rm = TRUE),
    NMI_DevStandard = sd(NMI, na.rm = TRUE),
    .groups = 'drop' # Rimuove il raggruppamento dopo summarise
  )%>%
  mutate(Metodo = str_replace(Metodo, "EB.STRENGHT", "strength"))

summary_prior_5 %>%
  dplyr::select(delta, Metodo, ARI_Media, ARI_DevStandard)



digits_colonne <- c(0, 0,  2, 6, 6, 6, 6, 6, 6)
tabella_sim <- xtable(summary_prior_5, digits = digits_colonne)
print(tabella_sim, include.rownames = FALSE, type = "latex")



ari_n1000 <- prior_5_long %>%
  dplyr::select(delta, ARI, Metodo) %>%
  filter( startsWith(Metodo, "EB") | startsWith(Metodo, "TRUE"))%>%
  mutate(Metodo = str_replace(Metodo, "EB.STRENGHT", "EB.STRENGTH"))

boxplot_ari_n1000 <- ggplot(ari_n1000, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n1000)
ggsave(boxplot_ari_n1000, file = "eb_ari_n1000.pdf", width = 8, height = 6)




nmi_n1000 <- prior_5_long %>%
  dplyr::select(delta, NMI, Metodo) %>%
  filter( startsWith(Metodo, "EB") | startsWith(Metodo, "TRUE"))

boxplot_nmi_n1000 <- ggplot(nmi_n1000, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  scale_fill_manual(values = my_color)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmi_n1000)
ggsave(boxplot_nmi_n1000, file = "eb_nmi_n1000.pdf", width = 8, height = 6)






