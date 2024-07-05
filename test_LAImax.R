###### Test fonction LAImax ###### 

# la condition pour la lumière se fait avant de tirer une graine
# la banque de graines du sol n'est donc constitué que de graines qui ont assez de lumière pour être recrutées


library(ggplot2)
library(tidyverse)
library(FD)
library(ggpubr)
library(rcontroll)# package construit à partir de ma version du fichier rccp 
# !! penser à télécharger le fichier generate_parameters qui va avec ma version
library(SciViews)


# importer le fichier "datatest_LAImax_cor.csv" dans le dossier "simulations LAImax"
test_LAImax <- read.table(file.choose(), h = T, sep = ";", dec = "." )

summary(test_LAImax$MinLAImax)


##### 1) Graphiques préliminaires #####

# graphiques préliminaires pour voir le lien entre la valeur du LAI et les autres traits
ggplot(test_LAImax, aes(MinLAImax, s_wsg)) +
  geom_point(size = 1) +
  theme_bw() +
  xlab("MinLAImax") +
  ylab("Densité du bois (wsg en g/cm^3)") 

hist(log(test_LAImax$MinLAImax))


# pas de variabilité intraspécifique donc MinLAImax = MaxLAImax
ggplot(test_LAImax, aes(MinLAImax, MaxLAImax)) +
  geom_point(size = 1) +
  theme_bw() +
  xlab("MinLAImax") +
  ylab("MaxLAImax") 





##### 2) Simulations avec MinLAImax ###### 

#### Recrutement et LAImax activés ####

# forêt initiale (0-600 ans):
# avec pluie de graines :

sim_MinLAImax_v2 <- troll(
  name = "sim_MinLAImax_v2",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 0, Rrecruit = 1, distdisperse = 0, torus = 1, 
    MinLAImax = 1, MaxLAImax = 0
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE
)
save(sim_MinLAImax_v2, file = "D:/Mes Donnees/TROLL/simulations/sim_MinLAImax_v2.Rdata")

sim_MinLAImax_v2@inputs$species$simulation <- "MinLAImax_v2"
sim_MinLAImax_v2@species$simulation <- "MinLAImax_v2"


# calcul de la diversité fonctionnelle :

sim_MinLAImax_functional_v2 <- left_join(sim_MinLAImax_v2@inputs$species, 
                                              filter(sim_MinLAImax_v2@species, iter %in% c(seq(from = 0, to = 7199, by = 12))),
                                              by = c("simulation" = "simulation", "s_name" = "species")) %>% 
  group_by(simulation, s_name, iter) %>% 
  sample_n(1) %>% 
  group_by(simulation, iter) %>% 
  mutate(sum1 = ifelse(is.na(sum1), 0, sum1)) %>% 
  mutate(abundance = sum1/sum(sum1)) %>% 
  mutate(log_seedmass = log(s_seedmass)) %>%
  mutate(log_LMA = log(s_LMA)) %>%
  mutate(log_dbhmax = log(s_dbhmax)) %>%
  mutate(log_Nmass = log(s_Nmass)) %>%
  mutate(log_Pmass = log(s_Pmass)) %>%
  mutate(log_wsg = log(s_wsg)) %>%
  mutate(log_hmax = log(s_hmax)) %>%
  mutate(log_ah = log(s_ah)) %>%
  mutate(sim_iter = paste0(simulation, "-", iter)) %>%
  select(sim_iter, s_name, log_seedmass, log_LMA, log_dbhmax, log_Nmass,log_Pmass, log_wsg, log_hmax, log_ah,  abundance) %>% 
  filter(abundance > 0) %>% 
  split(.$sim_iter) %>% 
  lapply(function(tab) {
    tab <- as.data.frame(arrange(tab, s_name))
    print(tab$s_name)
    row.names(tab) <- tab$s_name
    dbFD(x = tab[c("log_seedmass", "log_LMA", "log_dbhmax", "log_Nmass","log_Pmass", "log_wsg", "log_hmax", "log_ah")], 
         a = reshape2::dcast(tab, 1 ~ s_name, value.var = "abundance")[-1], m = "max", calc.CWM = T, calc.FDiv = F)
  }) %>% 
  lapply(as.data.frame) %>% 
  bind_rows(.id = "sim_iter") %>%
  separate(sim_iter, c("simulation", "iter"), sep = "-")

save(sim_MinLAImax_functional_v2, file = "D:/Mes Donnees/TROLL/simulations/sim_MinLAImax_functional_v2.Rdata")

# forêt finale (600-1200 ans):
# sans pluie de graines :

sim_MinLAImax_seedrain0_v2 <- troll(
  name = "sim_MinLAImax_seedrain0_v2",
  global = update_parameters(sim_MinLAImax_v2, nbiter = 12 * 600, Cseedrain = 0, 
                             fecundity = 0, Rrecruit = 1, distdisperse = 0, torus = 1, 
                             MinLAImax = 1, MaxLAImax = 0),
  species = sim_MinLAImax_v2@inputs$species,
  climate = sim_MinLAImax_v2@inputs$climate,
  daily = sim_MinLAImax_v2@inputs$daily,
  forest = get_forest(sim_MinLAImax_v2),
  verbose = TRUE
)
save(sim_MinLAImax_seedrain0_v2, file = "D:/Mes Donnees/TROLL/simulations/sim_MinLAImaxseedrain0_v2.Rdata")

sim_MinLAImax_seedrain0_v2@inputs$species$simulation <- "MinLAImax_v2"
sim_MinLAImax_seedrain0_v2@species$simulation <- "MinLAImax_v2"

# calcul de diversité fonctionnelle :

sim_MinLAImax_seedrain0_functional_v2 <- left_join(sim_MinLAImax_seedrain0_v2@inputs$species, 
                                      filter(sim_MinLAImax_seedrain0_v2@species, iter %in% c(seq(from = 0, to = 7199, by = 12))),
                                      by = c("simulation" = "simulation", "s_name" = "species")) %>% 
  group_by(simulation, s_name, iter) %>% 
  sample_n(1) %>% 
  group_by(simulation, iter) %>% 
  mutate(sum1 = ifelse(is.na(sum1), 0, sum1)) %>% 
  mutate(abundance = sum1/sum(sum1)) %>% 
  mutate(log_seedmass = log(s_seedmass)) %>%
  mutate(sim_iter = paste0(simulation, "-", iter)) %>%
  select(sim_iter, s_name, log_seedmass,  abundance) %>% 
  filter(abundance > 0) %>% 
  split(.$sim_iter) %>% 
  lapply(function(tab) {
    tab <- as.data.frame(arrange(tab, s_name))
    print(tab$s_name)
    row.names(tab) <- tab$s_name
    dbFD(x = tab[c("log_seedmass", "abundance")], 
         a = reshape2::dcast(tab, 1 ~ s_name, value.var = "abundance")[-1], m = "max", calc.CWM = T, calc.FDiv = F)
  }) %>% 
  lapply(as.data.frame) %>% 
  bind_rows(.id = "sim_iter") %>%
  separate(sim_iter, c("simulation", "iter"), sep = "-")


save(sim_MinLAImax_seedrain0_functional_v2, file = "D:/Mes Donnees/TROLL/simulations/sim_MinLAImax_seedrain0_functional_v2.Rdata")

summary(sim_MinLAImax_seedrain0_functional_v2$iter)
sim_MinLAImax_seedrain0_functional_v2$iter <- as.factor(sim_MinLAImax_seedrain0_functional_v2$iter)
sim_MinLAImax_seedrain0_functional_v2 <- filter(sim_MinLAImax_seedrain0_functional_v2, sim_MinLAImax_seedrain0_functional_v2$iter == 7188)

# calcul indice de pielou :

pielou_sim_MinLAImax_v2 <- left_join(sim_MinLAImax_v2@inputs$species, 
                                           filter(sim_MinLAImax_v2@species, iter %in% c(seq(from = 0, to = 7199, by = 12))),
                                           by = c("simulation" = "simulation", "s_name" = "species")) %>% 
  group_by(simulation, s_name, iter) %>% 
  sample_n(1) %>% 
  group_by(simulation, iter) %>% 
  mutate(abundance = sum1/sum(sum1)) %>% 
  mutate(species = s_name) %>% 
  mutate(sim_iter = paste0(simulation, "-", iter)) %>%
  select(sim_iter, s_name,  abundance) %>% 
  split(.$sim_iter) %>% 
  lapply(function(tab) {
    tab <- as.data.frame(arrange(tab, s_name))
    print(tab$s_name)
    tab <- tab[, c("abundance")]
    tab_h <- diversity(tab)
    tab_s <- specnumber(tab)
    tab_j <- tab_h / ln(tab_s)
  }) %>% 
  lapply(as.data.frame) %>% 
  bind_rows(.id = "sim_iter") %>%
  separate(sim_iter, c("simulation", "iter"), sep = "-")

save(pielou_sim_MinLAImax_v2, file = "D:/Mes Donnees/TROLL/simulations/pielou_MinLAImax_v2.Rdata")

pielou_sim_MinLAImax_seedrain0_v2 <- left_join(sim_MinLAImax_seedrain0_v2@inputs$species, 
                                  filter(sim_MinLAImax_seedrain0_v2@species, iter %in% c(seq(from = 0, to = 7199, by = 12))),
                                  by = c("simulation" = "simulation", "s_name" = "species")) %>% 
  group_by(simulation, s_name, iter) %>% 
  sample_n(1) %>% 
  group_by(simulation, iter) %>% 
  mutate(abundance = sum1/sum(sum1)) %>% 
  mutate(species = s_name) %>% 
  mutate(sim_iter = paste0(simulation, "-", iter)) %>%
  select(sim_iter, s_name,  abundance) %>% 
  split(.$sim_iter) %>% 
  lapply(function(tab) {
    tab <- as.data.frame(arrange(tab, s_name))
    print(tab$s_name)
    tab <- tab[, c("abundance")]
    tab_h <- diversity(tab)
    tab_s <- specnumber(tab)
    tab_j <- tab_h / ln(tab_s)
  }) %>% 
  lapply(as.data.frame) %>% 
  bind_rows(.id = "sim_iter") %>%
  separate(sim_iter, c("simulation", "iter"), sep = "-")

save(pielou_sim_MinLAImax_seedrain0_v2, file = "D:/Mes Donnees/TROLL/simulations/pielou_MinLAImax_seedrain0_v2.Rdata")

pielou_sim_MinLAImax_seedrain0_v2$iter <- as.numeric(pielou_sim_MinLAImax_seedrain0_v2$iter)
pielou_sim_MinLAImax_v2$iter <- as.numeric(pielou_sim_MinLAImax_v2$iter)

pielou_sim_MinLAImax_seedrain0_v2$iter <- pielou_sim_MinLAImax_seedrain0_v2$iter + max(pielou_sim_MinLAImax_v2$iter)

data_pielou_sim_MinLAImax_v2 <- bind_rows(pielou_sim_MinLAImax_v2, pielou_sim_MinLAImax_seedrain0_v2)
data_pielou_sim_MinLAImax_v2$sim <- "MinLAImax_v2"
save(data_pielou_sim_MinLAImax_v2, file = "D:/Mes Donnees/TROLL/simulations/data_pielou_sim_MinLAImax_v2.Rdata")


sim_MinLAImax_seedrain0_functional_v2$iter <- as.numeric(sim_MinLAImax_seedrain0_functional_v2$iter)
sim_MinLAImax_functional_v2$iter <- as.numeric(sim_MinLAImax_functional_v2$iter)
sim_MinLAImax_seedrain0_functional_v2$iter <- sim_MinLAImax_seedrain0_functional_v2$iter + max(sim_MinLAImax_functional_v2$iter)


sim_func_MinLAImax_final_v2 <- rbind(sim_MinLAImax_functional_v2, sim_MinLAImax_seedrain0_functional_v2)
sim_func_MinLAImax_final_v2$sim <- "MinLAImax_v2"
save(sim_func_MinLAImax_final_v2, file = "D:/Mes Donnees/TROLL/simulations/sim_func_MinLAImax_final_v2.Rdata")


# on récupère les simulations où la fonction LAImax n'est pas activée
load("~/TROLL/simulations/Contrôle (C)/Diversité fonctionnelle/sim_00_functional_final.Rdata")
load("~/TROLL/simulations/Recrutement (R)/Diversité fonctionnelle/sim_02_func_final.Rdata")

sim_02_func_final$sim <- "Rrecruit"
final$sim <- "contrôle"


# mêmes graphiques que dans "graph_H1.R"

data_traj_div <- rbind(sim_02_func_final, sim_func_MinLAImax_final_v2)
data_traj_div <- rbind(data_traj_div, final)

data_traj_pielou <- rbind(data_pielou_sim00, data_pielou_sim_MinLAImax_v2)
data_traj_pielou <- rbind(data_traj_pielou, data_pielou_sim04)
data_traj_pielou$sim <- fct_inorder(data_traj_pielou$sim)



g1 <- data_traj_div %>% 
  group_by(sim, iter) %>% 
  summarise(l = min(nbsp), 
            m = quantile(nbsp, 0.5), 
            h = max(nbsp)) %>% 
  ggplot(aes(iter/12, m, col = sim, fill = sim)) +
  geom_ribbon(aes(ymin = l, ymax = h), col = NA, alpha = 0.2) +
  geom_line() +
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Temps (années)") + ylab("Richesse spécifique") +
  scale_color_discrete(labels = c("contrôle", "MinLAImax_v2", "recrutement")) +
  scale_fill_discrete(labels = c("contrôle", "MinLAImax_v2", "recrutement")) +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 15), legend.text = element_text(size = 15))
  
g2 <- data_traj_div %>% 
  filter(iter == 14376) %>% 
  ggplot(aes(sim, `X[[i]]`, col = sim, fill = sim)) +
  geom_boxplot(alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "none",legend.title = element_blank()) +
  xlab("Scénarios") + ylab("") +
  scale_color_discrete(labels = c("contrôle", "MinLAImax", "recrutement")) +
  scale_fill_discrete(labels = c("contrôle", "MinLAImax", "recrutement"))+
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 15), legend.text = element_text(size = 15))


nbsp <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
pielou <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
FRic <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
FEve <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))


figure <- ggarrange(nbsp, pielou, FRic, FEve,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "bottom")
figure

annotate_figure(figure, top = text_grob("Comparaison des scénarios pour la diversité de la forêt", face = "bold", size = 16))


# graphs différences entre les scénarios avec et sans LAImax avec uniquement recrutement activé : 

# on cherche quelles espèces sont présentes ou absentes dans les modèles selon si LAI est activé ou non
MinLAImax_active <- filter(sim_MinLAImax_seedrain0_v2@species, sim_MinLAImax_seedrain0_v2@species$iter==7199)
MinLAImax_active$abond_relative <- MinLAImax_active$sum1/sum(MinLAImax_active$sum1)
MinLAImax_active$presence <- ifelse(MinLAImax_active$abond_relative <= 0, "absence", "présence")
MinLAImax_active$num <- ifelse(MinLAImax_active$abond_relative <= 0, "0", "1")
MinLAImax_active$LAI <- "Avec LAI"

load("~/TROLL/simulations/stack sim02/stack_sim_02_seedrain0.Rdata")
data_test <- stack_sim_02_seedrain0@species
sim_02 <- filter(data_test, data_test$simulation == "sim_02_4")
MinLAImax_desactive <- filter(sim_02, sim_02$iter == 7199)
MinLAImax_desactive$abond_relative <- MinLAImax_desactive$sum1/sum(MinLAImax_desactive$sum1)
MinLAImax_desactive$presence <- ifelse(MinLAImax_desactive$abond_relative <= 0, "absence", "présence")
MinLAImax_desactive$num <- ifelse(MinLAImax_desactive$abond_relative <= 0, "0", "1")
MinLAImax_desactive$LAI <- "Sans LAI"

# pour les graphs avec et sans LAI :
MinLAImax_desactive <- MinLAImax_desactive[,-1]
test <- left_join(sim_MinLAImax_seedrain0_v2@inputs$species, test_LAImax)
test$species <- MinLAImax_desactive$species
test <- left_join(test, MinLAImax_desactive)

test2 <- left_join(sim_MinLAImax_seedrain0_v2@inputs$species, test_LAImax)
test2$species <- MinLAImax_active$species
test2 <- left_join(test2, MinLAImax_active)

test_LAI_sim_02 <- rbind(test, test2)

# pour les graphs de comparaison de présence/absence des espèces entre avec et sans LAI :
input_species <- left_join(sim_MinLAImax_seedrain0_v2@inputs$species, test_LAImax)
input_species$Recruit_abond_relative <- MinLAImax_desactive$abond_relative
input_species$Recruit_presence <- MinLAImax_desactive$presence
input_species$Recruit_presence <- as.factor(input_species$Recruit_presence)
input_species$Recruit_num <- MinLAImax_desactive$num
input_species$LAImax_abond_relative <- MinLAImax_active$abond_relative
input_species$LAImax_presence <- MinLAImax_active$presence
input_species$LAImax_presence <- as.factor(input_species$LAImax_presence)

input_species$LAImax_num <- MinLAImax_active$num
input_species$dif <- ifelse(MinLAImax_desactive$num >= 1 & MinLAImax_active$num <= 0, "loss_in_LAImax", 
                            ifelse(MinLAImax_desactive$num <= 0 & MinLAImax_active$num >= 1, "loss_in_Recruit", "no_dif"))
input_species$dif <- as.factor(input_species$dif)
save(input_species, file = "D:/Mes Donnees/TROLL/simulations/inputs_test_LAImax.Rdata")

summary(input_species)

ggplot(test_LAI_sim_02, aes(x = s_seedmass/1000, 
                          y = s_dbhmax*100, 
                          col = presence, 
                          size = abond_relative)) +
  geom_point() +
  scale_x_log10(labels = scales::comma) +
  theme_bw() +
  ylab("Dbhmax (en cm)") +
  xlab("Masse de la graine (g)") +
  labs(size = "Abondance relative de l'espèce \ndans la forêt finale (iter = 7199)", col = "Présence/absence de l'espèce") +
  facet_wrap(~LAI, nrow = 1, ncol = 2) +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  ggtitle("Masse de la graine ~ Dbhmax quand Rrecruit est activé (R), LAImax désactivé", "Richesse spécifique = 67 et 79")

mycols <- c("red", "cornflowerblue", "black")
ggplot(input_species, aes(x = s_seedmass/1000, 
                          y = s_dbhmax*100, 
                          col = dif)) +
  geom_point() +
  scale_x_log10(labels = scales::comma) +
  theme_bw() +
  scale_color_manual(values = mycols) +
  ylab("Dbhmax (en cm)") +
  xlab("Masse de la graine (g)") +
  labs(col = "Différence de \nprésence/absence") +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  ggtitle("Masse de la graine ~ Dbhmax avec la différence de présence/absence \nentre les scénarii Rrecruit et LAImax")


###

ggplot(test_LAI_sim_02, aes(x = MinLAImax, 
                          y = s_seedmass/1000, 
                          col = presence, 
                          size = abond_relative)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  theme_bw() +
  ylab("Masse de la graine (g)") +
  xlab("LAImax de l'espèce") +
  facet_wrap(~LAI, nrow = 1, ncol = 2) +
  labs(size = "Abondance relative de l'espèce \ndans la forêt finale (iter = 7199)", col = "Présence/absence de l'espèce") +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  ggtitle("LAImax ~ Masse de la graine quand Rrecruit est activé (R), LAImax désactivé", "Richesse spécifique = 63 et 79")

mycols <- c("red", "cornflowerblue", "black")
ggplot(input_species, aes(x = MinLAImax, 
                          y = s_seedmass/1000, 
                          col = dif)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  theme_bw() +
  scale_color_manual(values = mycols) +
  ylab("Masse de la graine (g)") +
  xlab("LAImax de l'espèce") +
  labs(col = "Différence de \nprésence/absence") +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  ggtitle("LAImax ~ Masse de la graine avec la différence de présence/absence \nentre les scénarii Rrecruit et LAImax")

###

ggplot(test_LAI_sim_02, aes(x = MinLAImax, 
                          y = s_dbhmax*100, 
                          col = presence, 
                          size = abond_relative)) +
  geom_point() +
  theme_bw() +
  ylab("Dbhmax (en cm)") +
  xlab("LAImax de l'espèce") +
  facet_wrap(~LAI, nrow = 1, ncol = 2) +
  labs(size = "Abondance relative de l'espèce \ndans la forêt finale (iter = 7199)", col = "Présence/absence de l'espèce") +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  ggtitle("LAImax ~ Dbhmax quand Rrecruit est activé, LAImax désactivé", "Richesse spécifique = 63 et 79")


mycols <- c("red", "cornflowerblue", "black")
ggplot(input_species, aes(x = MinLAImax, 
                          y = s_dbhmax*100, 
                          col = dif)) +
  geom_point() +
  theme_bw() +
  scale_color_manual(values = mycols) +
  ylab("Dbhmax (en cm)") +
  xlab("LAImax de l'espèce") +
  labs(col = "Différence de \nprésence/absence") +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  ggtitle("LAImax ~ Dbhmax avec la différence de présence/absence \nentre les scénarii Rrecruit et LAImax")



#### Fécondité, recrutement et LAImax activés ####

sim_04_MinLAImax <- troll(
  name = "sim_04_MinLAImax",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 1, Rrecruit = 1, distdisperse = 0, torus = 1, 
    MinLAImax = 1, MaxLAImax = 0
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE
)
save(sim_04_MinLAImax, file = "D:/Mes Donnees/TROLL/simulations/sim_04_MinLAImax.Rdata")

sim_04_MinLAImax_seedrain0 <- troll(
  name = "sim_04_MinLAImax_seedrain0",
  global = update_parameters(sim_04_MinLAImax, nbiter = 12 * 600, Cseedrain = 0, 
                             fecundity = 1, Rrecruit = 1, distdisperse = 0, torus = 1, 
                             MinLAImax = 1, MaxLAImax = 0),
  species = sim_04_MinLAImax@inputs$species,
  climate = sim_04_MinLAImax@inputs$climate,
  daily = sim_04_MinLAImax@inputs$daily,
  forest = get_forest(sim_04_MinLAImax),
  verbose = TRUE
)
save(sim_04_MinLAImax_seedrain0, file = "D:/Mes Donnees/TROLL/simulations/sim_04_MinLAImaxseedrain0.Rdata")


# calcul diversité fonctionnelle : 

sim_04_MinLAImax@inputs$species$simulation <- "F+R+LAImax"
sim_04_MinLAImax@species$simulation <- "F+R+LAImax"

sim_04_MinLAImax_functional <- left_join(sim_04_MinLAImax@inputs$species, 
                                         filter(sim_04_MinLAImax@species, iter %in% c(seq(from = 0, to = 7199, by = 12))),
                                         by = c("simulation" = "simulation", "s_name" = "species")) %>% 
  group_by(simulation, s_name, iter) %>% 
  sample_n(1) %>% 
  group_by(simulation, iter) %>% 
  mutate(sum1 = ifelse(is.na(sum1), 0, sum1)) %>% 
  mutate(abundance = sum1/sum(sum1)) %>% 
  mutate(log_seedmass = log(s_seedmass)) %>%
  mutate(log_LMA = log(s_LMA)) %>%
  mutate(log_dbhmax = log(s_dbhmax)) %>%
  mutate(log_Nmass = log(s_Nmass)) %>%
  mutate(log_Pmass = log(s_Pmass)) %>%
  mutate(log_wsg = log(s_wsg)) %>%
  mutate(log_hmax = log(s_hmax)) %>%
  mutate(log_ah = log(s_ah)) %>%
  mutate(sim_iter = paste0(simulation, "-", iter)) %>%
  select(sim_iter, s_name, log_seedmass, log_LMA, log_dbhmax, log_Nmass,log_Pmass, log_wsg, log_hmax, log_ah,  abundance) %>% 
  filter(abundance > 0) %>% 
  split(.$sim_iter) %>% 
  lapply(function(tab) {
    tab <- as.data.frame(arrange(tab, s_name))
    print(tab$s_name)
    row.names(tab) <- tab$s_name
    dbFD(x = tab[c("log_seedmass", "log_LMA", "log_dbhmax", "log_Nmass","log_Pmass", "log_wsg", "log_hmax", "log_ah")], 
         a = reshape2::dcast(tab, 1 ~ s_name, value.var = "abundance")[-1], m = "max", calc.CWM = T, calc.FDiv = F)
  }) %>% 
  lapply(as.data.frame) %>% 
  bind_rows(.id = "sim_iter") %>%
  separate(sim_iter, c("simulation", "iter"), sep = "-")

save(sim_04_MinLAImax_functional, file = "D:/Mes Donnees/TROLL/simulations/sim_04_MinLAImax_functional.Rdata")

sim_04_MinLAImax_seedrain0_functional <- left_join(sim_04_MinLAImax_seedrain0@inputs$species, 
                                         filter(sim_04_MinLAImax_seedrain0@species, iter %in% c(seq(from = 7199, to = 14398, by = 12))),
                                         by = c("simulation" = "simulation", "s_name" = "species")) %>% 
  group_by(simulation, s_name, iter) %>% 
  sample_n(1) %>% 
  group_by(simulation, iter) %>% 
  mutate(sum1 = ifelse(is.na(sum1), 0, sum1)) %>% 
  mutate(abundance = sum1/sum(sum1)) %>% 
  mutate(log_seedmass = log(s_seedmass)) %>%
  mutate(log_LMA = log(s_LMA)) %>%
  mutate(log_dbhmax = log(s_dbhmax)) %>%
  mutate(log_Nmass = log(s_Nmass)) %>%
  mutate(log_Pmass = log(s_Pmass)) %>%
  mutate(log_wsg = log(s_wsg)) %>%
  mutate(log_hmax = log(s_hmax)) %>%
  mutate(log_ah = log(s_ah)) %>%
  mutate(sim_iter = paste0(simulation, "-", iter)) %>%
  select(sim_iter, s_name, log_seedmass, log_LMA, log_dbhmax, log_Nmass,log_Pmass, log_wsg, log_hmax, log_ah,  abundance) %>% 
  filter(abundance > 0) %>% 
  split(.$sim_iter) %>% 
  lapply(function(tab) {
    tab <- as.data.frame(arrange(tab, s_name))
    print(tab$s_name)
    row.names(tab) <- tab$s_name
    dbFD(x = tab[c("log_seedmass", "log_LMA", "log_dbhmax", "log_Nmass","log_Pmass", "log_wsg", "log_hmax", "log_ah")], 
         a = reshape2::dcast(tab, 1 ~ s_name, value.var = "abundance")[-1], m = "max", calc.CWM = T, calc.FDiv = F)
  }) %>% 
  lapply(as.data.frame) %>% 
  bind_rows(.id = "sim_iter") %>%
  separate(sim_iter, c("simulation", "iter"), sep = "-")

save(sim_04_MinLAImax_seedrain0_functional, file = "D:/Mes Donnees/TROLL/simulations/sim_04_MinLAImax_seedrain0_functional.Rdata")

sim_04_MinLAImax_func_final <- rbind(sim_04_MinLAImax_functional, sim_04_MinLAImax_seedrain0_functional)
save(sim_04_MinLAImax_func_final, file = "D:/Mes Donnees/TROLL/simulations/sim_04_MinLAImax_func_final.Rdata")


# calcul indice de pielou :

sim_04_MinLAImax@inputs$species$simulation <- "F+R_LAImax"
sim_04_MinLAImax@species$simulation <- "F+R_LAImax"

pielou_sim_04_MinLAImax <- left_join(sim_04_MinLAImax@inputs$species, 
                                     filter(sim_04_MinLAImax@species, iter %in% c(seq(from = 0, to = 7199, by = 12))),
                                     by = c("simulation" = "simulation", "s_name" = "species")) %>% 
  group_by(simulation, s_name, iter) %>% 
  sample_n(1) %>% 
  group_by(simulation, iter) %>% 
  mutate(abundance = sum1/sum(sum1)) %>% 
  mutate(species = s_name) %>% 
  mutate(sim_iter = paste0(simulation, "-", iter)) %>%
  select(sim_iter, s_name,  abundance) %>% 
  split(.$sim_iter) %>% 
  lapply(function(tab) {
    tab <- as.data.frame(arrange(tab, s_name))
    print(tab$s_name)
    tab <- tab[, c("abundance")]
    tab_h <- diversity(tab)
    tab_s <- specnumber(tab)
    tab_j <- tab_h / ln(tab_s)
  }) %>% 
  lapply(as.data.frame) %>% 
  bind_rows(.id = "sim_iter") %>%
  separate(sim_iter, c("simulation", "iter"), sep = "-")

save(pielou_sim_04_MinLAImax, file = "D:/Mes Donnees/TROLL/simulations/pielou_sim_04_MinLAImax.Rdata")

sim_04_MinLAImax_seedrain0@inputs$species$simulation <- "F+R_LAImax"
sim_04_MinLAImax_seedrain0@species$simulation <- "F+R_LAImax"

pielou_sim_04_MinLAImax_seedrain0 <- left_join(sim_04_MinLAImax_seedrain0@inputs$species, 
                                     filter(sim_04_MinLAImax_seedrain0@species, iter %in% c(seq(from = 7200, to = 14398, by = 12))),
                                     by = c("simulation" = "simulation", "s_name" = "species")) %>% 
  group_by(simulation, s_name, iter) %>% 
  sample_n(1) %>% 
  group_by(simulation, iter) %>% 
  mutate(abundance = sum1/sum(sum1)) %>% 
  mutate(species = s_name) %>% 
  mutate(sim_iter = paste0(simulation, "-", iter)) %>%
  select(sim_iter, s_name,  abundance) %>% 
  split(.$sim_iter) %>% 
  lapply(function(tab) {
    tab <- as.data.frame(arrange(tab, s_name))
    print(tab$s_name)
    tab <- tab[, c("abundance")]
    tab_h <- diversity(tab)
    tab_s <- specnumber(tab)
    tab_j <- tab_h / ln(tab_s)
  }) %>% 
  lapply(as.data.frame) %>% 
  bind_rows(.id = "sim_iter") %>%
  separate(sim_iter, c("simulation", "iter"), sep = "-")

save(pielou_sim_04_MinLAImax_seedrain0, file = "D:/Mes Donnees/TROLL/simulations/pielou_sim_04_MinLAImax_seedrain0.Rdata")


pielou_sim_04_MinLAImax_seedrain0$iter <- as.numeric(pielou_sim_04_MinLAImax_seedrain0$iter)
pielou_sim_04_MinLAImax$iter <- as.numeric(pielou_sim_04_MinLAImax$iter)


data_pielou_sim_04_MinLAImax <- bind_rows(pielou_sim_04_MinLAImax, pielou_sim_04_MinLAImax_seedrain0)
data_pielou_sim_04_MinLAImax$sim <- "F+R+LAImax"
save(data_pielou_sim_04_MinLAImax, file = "D:/Mes Donnees/TROLL/simulations/data_pielou_sim_04_MinLAImax.Rdata")


# graphs différences entre les scénarios avec et sans LAImax quand fécondité et recrutement sont activés : 

# on cherche quelles espèces sont présentes ou absentes dans les modèles
MinLAImax_active <- filter(sim_04_MinLAImax_seedrain0@species, sim_04_MinLAImax_seedrain0@species$iter==7199)
MinLAImax_active$abond_relative <- MinLAImax_active$sum1/sum(MinLAImax_active$sum1)
MinLAImax_active$presence <- ifelse(MinLAImax_active$abond_relative <= 0, "absence", "présence")
MinLAImax_active$num <- ifelse(MinLAImax_active$abond_relative <= 0, "0", "1")
MinLAImax_active$LAI <- "Avec LAI"

data_test <- stack_sim_04_seedrain0@species
sim_04 <- filter(data_test, data_test$simulation == "sim_04_4")
MinLAImax_desactive <- filter(sim_04, sim_04$iter == 7199)
MinLAImax_desactive$abond_relative <- MinLAImax_desactive$sum1/sum(MinLAImax_desactive$sum1)
MinLAImax_desactive$presence <- ifelse(MinLAImax_desactive$abond_relative <= 0, "absence", "présence")
MinLAImax_desactive$presence <- as.factor(MinLAImax_desactive$presence)
MinLAImax_desactive$num <- ifelse(MinLAImax_desactive$abond_relative <= 0, "0", "1")
MinLAImax_desactive$LAI <- "Sans LAI"

input_species_sim04_LAImax <- left_join(sim_04_MinLAImax_seedrain0@inputs$species, test_LAImax)
input_species_sim04_LAImax$Recruit_abond_relative <- MinLAImax_desactive$abond_relative
input_species_sim04_LAImax$Recruit_presence <- MinLAImax_desactive$presence
input_species_sim04_LAImax$Recruit_presence <- as.factor(input_species_sim04_LAImax$Recruit_presence)
input_species_sim04_LAImax$Recruit_num <- MinLAImax_desactive$num
input_species_sim04_LAImax$LAImax_abond_relative <- MinLAImax_active$abond_relative
input_species_sim04_LAImax$LAImax_presence <- MinLAImax_active$presence
input_species_sim04_LAImax$LAImax_presence <- as.factor(input_species_sim04_LAImax$LAImax_presence)
input_species_sim04_LAImax$LAImax_num <- MinLAImax_active$num
input_species_sim04_LAImax$dif <- ifelse(MinLAImax_desactive$num >= 1 & MinLAImax_active$num <= 0, "loss_in_LAImax", 
                            ifelse(MinLAImax_desactive$num <= 0 & MinLAImax_active$num >= 1, "loss_in_Recruit", "no_dif"))
input_species_sim04_LAImax$dif <- as.factor(input_species_sim04_LAImax$dif)

save(input_species_sim04_LAImax, file = "D:/Mes Donnees/TROLL/simulations/inputs_test_LAImax_sim04.Rdata")

# richesse spé :
summary(input_species_sim04_LAImax)

MinLAImax_desactive <- MinLAImax_desactive[,-1]
test <- left_join(sim_04_MinLAImax_seedrain0@inputs$species, test_LAImax)
test$species <- MinLAImax_desactive$species
test <- left_join(test, MinLAImax_desactive)

test2 <- left_join(sim_04_MinLAImax_seedrain0@inputs$species, test_LAImax)
test2$species <- MinLAImax_active$species
test2 <- left_join(test2, MinLAImax_active)

test_LAI_sim_04 <- rbind(test, test2)


ggplot(test_LAI_sim_04, aes(x = s_seedmass/1000, 
                          y = s_dbhmax*100, 
                          col = presence, 
                          size = abond_relative)) +
  geom_point() +
  scale_x_log10(labels = scales::comma) +
  theme_bw() +
  ylab("Dbhmax (en cm)") +
  xlab("Masse de la graine (g)") +
  facet_wrap(~LAI, nrow = 2, ncol = 2) +
  labs(size = "Abondance relative de l'espèce \ndans la forêt finale (iter = 7199)", col = "Présence/absence de l'espèce") +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  ggtitle("Masse de la graine ~ Dbhmax quand Fécondité et Rrecruit sont activés", "Richesses spécifiques = 93 et 102")


mycols <- c("red", "cornflowerblue", "black")
ggplot(input_species_sim04_LAImax, aes(x = s_seedmass/1000, 
                          y = s_dbhmax*100, 
                          col = dif)) +
  geom_point() +
  scale_x_log10(labels = scales::comma) +
  theme_bw() +
  scale_color_manual(values = mycols) +
  ylab("Dbhmax (en cm)") +
  xlab("Masse de la graine (g)") +
  labs(col = "Différence de \nprésence/absence") +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  ggtitle("Masse de la graine ~ Dbhmax avec la différence de présence/absence \nentre les scénarii Fec+Rrecruit (F+R) et F+R+LAImax")


###

ggplot(test_LAI_sim_04, aes(x = MinLAImax, 
                          y = s_seedmass/1000, 
                          col = presence, 
                          size = abond_relative)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  theme_bw() +
  ylab("Masse de la graine (g)") +
  xlab("LAImax de l'espèce") +
  facet_wrap(~LAI, nrow = 2, ncol = 2) +
  labs(size = "Abondance relative de l'espèce \ndans la forêt finale (iter = 7199)", col = "Présence/absence de l'espèce") +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  ggtitle("LAImax ~ Masse de la graine quand Fécondité et Rrecruit sont activés", "Richesses spécifiques = 93 et 102")


mycols <- c("red", "cornflowerblue", "black")
ggplot(input_species_sim04_LAImax, aes(x = MinLAImax, 
                          y = s_seedmass/1000, 
                          col = dif)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  theme_bw() +
  scale_color_manual(values = mycols) +
  ylab("Masse de la graine (g)") +
  xlab("LAImax de l'espèce") +
  labs(col = "Différence de \nprésence/absence") +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  ggtitle("LAImax ~ Masse de la graine avec la différence de présence/absence \nentre les scénarii Fec+Rrecruit (F+R) et F+R+LAImax")

###

ggplot(test_LAI_sim_04, aes(x = MinLAImax, 
                          y = s_dbhmax*100, 
                          col = presence, 
                          size = abond_relative)) +
  geom_point() +
  theme_bw() +
  ylab("Dbhmax (en cm)") +
  xlab("LAImax de l'espèce") +
  labs(size = "Abondance relative de l'espèce \ndans la forêt finale (iter = 7199)", col = "Présence/absence de l'espèce") +
  facet_wrap(~LAI, nrow = 2, ncol = 2) +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  ggtitle("LAImax ~ Dbhmax quand Fécondité et Rrecruit sont activés", "Richesse spécifique = 93 et 102")

mycols <- c("red", "cornflowerblue", "black")
ggplot(input_species_sim04_LAImax, aes(x = MinLAImax, 
                          y = s_dbhmax*100, 
                          col = dif)) +
  geom_point() +
  theme_bw() +
  scale_color_manual(values = mycols) +
  ylab("Dbhmax (en cm)") +
  xlab("LAImax de l'espèce") +
  labs(col = "Différence de \nprésence/absence") +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  ggtitle("LAImax ~ Dbhmax avec la différence de présence/absence \nentre les scénarii Fec+Rrecruit (F+R) et F+R+LAImax")



##### 3) Distribution du LAI au sol #####

# ouvrir sim_02_recruit_0_LAI.txt dans le projet rcontroll dossier "sim_02_recruit" 
LAI_soil <- read.table(file.choose(), h = T, sep = "", dec = "." )

# on filtre uniquement pour h=0, c'est à dire la hauteur du sol 
LAI_soil <- filter(LAI_soil, LAI_soil$h == 0)

# on fait un histogramme des valeurs de LAI3D 
ggplot(LAI_soil, aes(LAI3D)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "yellow", col = "gold") +
  xlab("LAI3D") +
  ylab("Nombre de pixel") +
  theme_bw()+
  geom_vline(xintercept= 3.599, linetype="dashed", color = "black") + 
  geom_text(x=2.5, y=9000, label="Min LAImax", size = 3, color = "black") +
  geom_vline(xintercept= 4.995, linetype="dashed", color = "black") + 
  geom_text(x=6.2, y=9000, label="Max LAImax", size = 3, color = "black") +
  
  theme(
    strip.text.x = element_text(
      size = 12, color = "black"
    ), strip.background = element_rect(
      fill="grey91"), plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),  
    axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15) )+
  labs(title = "Distribution du LAI3D au sol")



##### 4) Comparaison de densité des tiges et diversité entre les scénarii #####


#### Recrutement est activé (avec et sans la fonction LAI) ####

load("~/TROLL/simulations/Contrôle (C)/Simulations/sim_00_ecosystem.Rdata")
test$sim <- "contrôle"
load("~/TROLL/simulations/Recrutement (R)/Simulations/sim_02_ecosystem_final.Rdata")
sim_02_ecosystem_final$sim <- "recrutement"
load("~/TROLL/simulations/simulations LAImax/Recrutement (R)/Simulations/sim_02_MinLAImax_v2.Rdata")
load("~/TROLL/simulations/simulations LAImax/Recrutement (R)/Simulations/sim_02_MinLAImaxseedrain0_v2.Rdata")

# pour mettre à jour le nom des itérations de 7200 à 14400 pour seedrain0 :
sim_MinLAImax_seedrain0_v2@ecosystem$iter <- sim_MinLAImax_seedrain0_v2@ecosystem$iter + max(sim_MinLAImax_v2@ecosystem$iter)
sim_02_LAI_ecosystem_final <- rbind(sim_MinLAImax_v2@ecosystem, sim_MinLAImax_seedrain0_v2@ecosystem)
sim_02_LAI_ecosystem_final$sim <- "recrutement + LAI"
# on rajoute une colonne "simulation" à ce tableau pour avoir le même nb de colonne que les autres tableaux
simulation <- "sim_LAI"
sim_02_LAI_ecosystem_final <- cbind(simulation, sim_02_LAI_ecosystem_final)

# graphs des trajectoires au cours des itérations et des distributions dans la forêt finale :
# structure de la forêt :
data_traj <- rbind(test, sim_02_ecosystem_final)
data_traj <- rbind(data_traj, sim_02_LAI_ecosystem_final)

mycols <- c("brown1", "turquoise2", "gold")
g1 <- data_traj %>% 
  group_by(sim, iter) %>% 
  summarise(l = min(agb), 
            m = quantile(agb, 0.5), 
            h = max(agb)) %>% 
  ggplot(aes(iter/12, m, col = sim, fill = sim)) +
  geom_ribbon(aes(ymin = l, ymax = h), col = NA, alpha = 0.2) +
  geom_line() +
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  xlab("Temps (années)") + ylab(expression(Biomasse ~ aérienne ~ (AGB ~ Kg ~ ha^{
    -1
  })))+
  scale_color_manual(values= mycols, labels = c("contrôle", "recrutement", "recrutement + LAI")) +
  scale_fill_manual(values= mycols, labels = c("contrôle", "recrutement", "recrutement + LAI"))

g2 <- data_traj %>% 
  filter(iter == 14398) %>% 
  ggplot(aes(sim, agb, col = sim, fill = sim)) +
  geom_boxplot(alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "none",legend.title = element_blank(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  xlab("Scénarios") + ylab("") +
  scale_color_manual(values= mycols, labels = c("contrôle", "recrutement", "recrutement + LAI")) +
  scale_fill_manual(values= mycols, labels = c("contrôle", "recrutement", "recrutement + LAI"))


agb <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
sum1 <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
sum10 <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
sum30 <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))

figure <- ggarrange(agb, sum1, sum10, sum30,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "bottom")
figure

annotate_figure(figure, top = text_grob("Comparaison des scénarios recrutement avec et sans LAI pour la structure de la forêt", face = "bold", size = 16))


# diversité spécifique de la forêt :

# on charge les données : 
load("~/TROLL/simulations/Contrôle (C)/Diversité spécifique/Indice d'équitabilité de Piélou/data_final_pielou_sim00.Rdata")
load("~/TROLL/simulations/Contrôle (C)/Diversité fonctionnelle/sim_00_functional_final.Rdata")
final$sim <- "contrôle"

load("~/TROLL/simulations/Recrutement (R)/Diversité spécifique/Indice d'équitabilité de Piélou/data_final_pielou_sim02.Rdata")
load("~/TROLL/simulations/Recrutement (R)/Diversité fonctionnelle/sim_02_func_final.Rdata")
sim_02_func_final$sim <- "recrutement"

load("~/TROLL/simulations/simulations LAImax/Recrutement (R)/Indice d'équitabilité de Piélou/data_pielou_sim_MinLAImax_v2.Rdata")
load("~/TROLL/simulations/simulations LAImax/Recrutement (R)/Richesse fonctionnelle/sim_func_MinLAImax_final_v2.Rdata")

data_pielou <- rbind(data_pielou_sim00, data_pielou_sim02)
data_pielou <- rbind(data_pielou, data_pielou_sim_MinLAImax_v2)
data_pielou$sim <- fct_inorder(data_pielou$sim)

data_nbsp <- rbind(final, sim_02_func_final)
data_nbsp <- rbind(data_nbsp, sim_func_MinLAImax_final_v2)
data_nbsp$sim <- fct_inorder(data_nbsp$sim)


mycols <- c("brown1", "turquoise2", "gold")
g1 <- data_nbsp %>% 
  group_by(sim, iter) %>% 
  summarise(l = min(nbsp), 
            m = quantile(nbsp, 0.5), 
            h = max(nbsp)) %>% 
  ggplot(aes(iter/12, m, col = sim, fill = sim)) +
  geom_ribbon(aes(ymin = l, ymax = h), col = NA, alpha = 0.2) +
  geom_line() +
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  xlab("Temps (années)") + ylab("Richesse spécifique")+
  scale_color_manual(values= mycols, labels = c("contrôle", "recrutement", "recrutement + LAI")) +
  scale_fill_manual(values= mycols, labels = c("contrôle", "recrutement", "recrutement + LAI"))

g2 <- data_nbsp %>% 
  filter(iter == 14376) %>% 
  ggplot(aes(sim, nbsp, col = sim, fill = sim)) +
  geom_boxplot(alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "none",legend.title = element_blank(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  xlab("Scénarios") + ylab("") +
  scale_color_manual(values= mycols, labels = c("contrôle", "recrutement", "recrutement + LAI")) +
  scale_fill_manual(values= mycols, labels = c("contrôle", "recrutement", "recrutement + LAI"))


pielou <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
nbsp <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))

figure <- ggarrange(nbsp, pielou,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1, 
                    common.legend = TRUE, legend = "bottom")
figure

annotate_figure(figure, top = text_grob("Comparaison des scénarios recrutement avec et sans LAI pour la diversité spécifique de la forêt", face = "bold", size = 16))



#### Fécondité et Recrutement sont activés (avec et sans la fonction LAI) ####

load("~/TROLL/simulations/Contrôle (C)/Simulations/sim_00_ecosystem.Rdata")
test$sim <- "contrôle"
load("~/TROLL/simulations/Recrutement (R)/Simulations/sim_04_ecosystem_final.Rdata")
sim_04_ecosystem_final$sim <- "recrutement"
load("~/TROLL/simulations/simulations LAImax/Recrutement (R)/Simulations/sim_04_MinLAImax.Rdata")
load("~/TROLL/simulations/simulations LAImax/Recrutement (R)/Simulations/sim_04_MinLAImaxseedrain0.Rdata")

# pour mettre à jour le nom des itérations de 7200 à 14400 pour seedrain0 :
sim_04_MinLAImax_seedrain0@ecosystem$iter <- sim_04_MinLAImax_seedrain0@ecosystem$iter + max(sim_04_MinLAImax@ecosystem$iter)
sim_04_LAI_ecosystem_final <- rbind(sim_04_MinLAImax@ecosystem, sim_04_MinLAImax_seedrain0@ecosystem)
sim_04_LAI_ecosystem_final$sim <- "Fec + recrutement + LAI"
# on rajoute une colonne "simulation" à ce tableau pour avoir le même nb de colonne que les autres tableaux
simulation <- "sim_LAI"
sim_04_LAI_ecosystem_final <- cbind(simulation, sim_04_LAI_ecosystem_final)

# graphs des trajectoires au cours des itérations et des distributions dans la forêt finale :
# structure de la forêt :
data_traj <- rbind(test, sim_04_ecosystem_final)
data_traj <- rbind(data_traj, sim_04_LAI_ecosystem_final)
data_traj$sim <- fct_inorder(data_traj$sim)

mycols <- c("brown1", "darkorchid2", "gold")
g1 <- data_traj %>% 
  group_by(sim, iter) %>% 
  summarise(l = min(agb), 
            m = quantile(agb, 0.5), 
            h = max(agb)) %>% 
  ggplot(aes(iter/12, m, col = sim, fill = sim)) +
  geom_ribbon(aes(ymin = l, ymax = h), col = NA, alpha = 0.2) +
  geom_line() +
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  xlab("Temps (années)") + ylab(expression(Biomasse ~ aérienne ~ (AGB ~ Kg ~ ha^{
    -1
  })))+
  scale_color_manual(values= mycols, labels = c("contrôle", "recrutement", "recrutement + LAI")) +
  scale_fill_manual(values= mycols, labels = c("contrôle", "recrutement", "recrutement + LAI"))

g2 <- data_traj %>% 
  filter(iter == 14398) %>% 
  ggplot(aes(sim, agb, col = sim, fill = sim)) +
  geom_boxplot(alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "none",legend.title = element_blank(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  xlab("Scénarios") + ylab("") +
  scale_color_manual(values= mycols, labels = c("contrôle", "Fécondité + recrutement", "Fec + recrutement + LAI")) +
  scale_fill_manual(values= mycols, labels = c("contrôle", "Fécondité + recrutement", "Fec + recrutement + LAI"))


agb <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
sum1 <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
sum10 <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
sum30 <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))

figure <- ggarrange(agb, sum1, sum10, sum30,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "bottom")
figure

annotate_figure(figure, top = text_grob("Comparaison des scénarios fécondité + recrutement avec et sans LAI pour la structure de la forêt", face = "bold", size = 16))


# diversité spécifique de la forêt :

# on charge les données : 
load("~/TROLL/simulations/Contrôle (C)/Diversité spécifique/Indice d'équitabilité de Piélou/data_final_pielou_sim00.Rdata")
load("~/TROLL/simulations/Contrôle (C)/Diversité fonctionnelle/sim_00_functional_final.Rdata")
final$sim <- "contrôle"

load("~/TROLL/simulations/Fécondité + Recrutement (F+R)/Diversité spécifique/Indice d'équitabilité de Piélou/data_final_pielou_sim04.Rdata")
load("~/TROLL/simulations/Fécondité + Recrutement (F+R)/Diversité fonctionnelle/sim_04_func_final.Rdata")
sim_04_func_final$sim <- "Fécondité + recrutement"

load("~/TROLL/simulations/simulations LAImax/Fécondité + Recrutement (F+R)/Indice d'équitabilité de Pielou/data_pielou_sim_04_MinLAImax.Rdata")
load("~/TROLL/simulations/simulations LAImax/Fécondité + Recrutement (F+R)/Diversité fonctionnelle/sim_04_MinLAImax_functional_final.Rdata")

data_pielou <- rbind(data_pielou_sim00, data_pielou_sim04)
data_pielou <- rbind(data_pielou, data_pielou_sim_04_MinLAImax)
data_pielou$sim <- fct_inorder(data_pielou$sim)
data_pielou$iter <- as.numeric(data_pielou$iter)

data_nbsp <- rbind(final, sim_04_func_final)
sim_04_MinLAImax_func_final$sim <- "F+R+LAImax"
data_nbsp <- rbind(data_nbsp, sim_04_MinLAImax_func_final)
data_nbsp$sim <- fct_inorder(data_nbsp$sim)
data_nbsp$iter <- as.numeric(data_nbsp$iter)


mycols <- c("brown1", "darkorchid2", "gold")
g1 <- data_pielou %>% 
  group_by(sim, iter) %>% 
  summarise(l = min(`X[[i]]`), 
            m = quantile(`X[[i]]`, 0.5), 
            h = max(`X[[i]]`)) %>% 
  ggplot(aes(iter/12, m, col = sim, fill = sim)) +
  geom_ribbon(aes(ymin = l, ymax = h), col = NA, alpha = 0.2) +
  geom_line() +
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  xlab("Temps (années)") + ylab("Indice d'équitabilité de Piélou")+
  scale_color_manual(values= mycols, labels = c("contrôle", "recrutement", "recrutement + LAI")) +
  scale_fill_manual(values= mycols, labels = c("contrôle", "recrutement", "recrutement + LAI"))

sim_04_MinLAImax_func_final$iter <- as.numeric(sim_04_MinLAImax_func_final$iter)
max(sim_04_MinLAImax_func_final$iter)

data_test <- filter(data_pielou, data_pielou$iter == 14376)
data_test2 <- filter(data_pielou, data_pielou$iter == 14387)

data <- rbind(data_test, data_test2)

g2 <- data %>% 
  ggplot(aes(sim, `X[[i]]`, col = sim, fill = sim)) +
  geom_boxplot(alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "none",legend.title = element_blank(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  xlab("Scénarios") + ylab("") +
  scale_color_manual(values= mycols, labels = c("contrôle", "recrutement", "recrutement + LAI")) +
  scale_fill_manual(values= mycols, labels = c("contrôle", "recrutement", "recrutement + LAI"))


pielou <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
nbsp <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))

figure <- ggarrange(nbsp, pielou,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1, 
                    common.legend = TRUE, legend = "bottom")
figure

annotate_figure(figure, top = text_grob("Comparaison des scénarios Fécondité + recrutement (F+R) avec et sans LAI pour la diversité spécifique de la forêt", face = "bold", size = 16))

