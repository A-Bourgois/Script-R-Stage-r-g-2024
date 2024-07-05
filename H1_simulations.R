############## Simulations #############

# Quel est le rôle de la taille des graines dans les processus de régénération, 
# quels sont leurs effets sur la communauté végétale ?


####### I) Pré-requis #######

# téléchargement des packages
library(ggplot2)
library(tidyverse)
library(FD)
library(ggpubr)
library(rcontroll) # package construit à partir de ma version du fichier rccp 
# !! penser à télécharger le fichier generate_parameters qui va avec ma version
library(SciViews)



# téléchargement des fichiers d'entrée : 
species_smass <- read.table(file.choose(), h = T, sep = "", dec = "." ) # fichier de Bruno
data("TROLLv3_species")
data("TROLLv3_climatedaytime12")
data("TROLLv3_daytimevar")
data("TROLLv3_output")

####### II) Simulations avec un seul paramètre #######

# Simulation test :
# avec le modèle initial (version de Maréchaux & Chave, 2017) :

sim_test <- troll(
  name = "sim_test",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 600
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE
)
sim_test
save(sim_test, file = "D:/Mes Donnees/TROLL/simulations/sim_test.Rdata")

sim_test_nul <- troll(
  name = "sim_test_nul",
  global = update_parameters(sim_test, nbiter = 12 * 600, Cseedrain = 50000),
  species = sim_test@inputs$species,
  climate = sim_test@inputs$climate,
  daily = sim_test@inputs$daily,
  forest = get_forest(sim_test),
  verbose = TRUE
)
sim_test_nul
save(sim_test_nul, file = "D:/Mes Donnees/TROLL/simulations/sim_test_nul.Rdata")

sim_test_bis <- troll(
  name = "sim_test_bis",
  global = update_parameters(sim_test, nbiter = 12 * 600, Cseedrain = 0),
  species = sim_test@inputs$species,
  climate = sim_test@inputs$climate,
  daily = sim_test@inputs$daily,
  forest = get_forest(sim_test),
  verbose = TRUE
)
sim_test_bis
save(sim_test, file = "D:/Mes Donnees/TROLL/simulations/sim_test_bis.Rdata")

# mise à jour des valeurs des itérations pour seedrain0
sim_test_nul@ecosystem$iter <- sim_test_nul@ecosystem$iter + max(sim_test@ecosystem$iter)
sim_test_bis@ecosystem$iter <- sim_test_bis@ecosystem$iter + max(sim_test@ecosystem$iter)


# Créer les graphiques

my3cols <- c("#FC4E07", "cornflowerblue","gold1" )

a <- list(
  "initial forest" = sim_test@ecosystem,
  "seed rain = 50000" = sim_test_nul@ecosystem,
  "seed rain = 0" = sim_test_bis@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, agb, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_manual(values = my3cols) 

b <- list(
  "initial forest" = sim_test@ecosystem,
  "seed rain = 50000" = sim_test_nul@ecosystem,
  "seed rain = 0" = sim_test_bis@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum1, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 1cm of dbh") +
  scale_color_manual(values = my3cols) 

c <- list(
  "initial forest" = sim_test@ecosystem,
  "seed rain = 50000" = sim_test_nul@ecosystem,
  "seed rain = 0" = sim_test_bis@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum10, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 10cm of dbh") +
  scale_color_manual(values = my3cols) 

d <- list(
  "initial forest" = sim_test@ecosystem,
  "seed rain = 50000" = sim_test_nul@ecosystem,
  "seed rain = 0" = sim_test_bis@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum30, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 30cm of dbh") +
  scale_color_manual(values = my3cols) 

# Combiner les graphiques sur une page
figure <- ggarrange(a, b, c, d,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "bottom")
figure

annotate_figure(figure,
                top = text_grob("Simulation test avec variation de la seed rain", face = "bold", size = 16))


###### 1) Contrôle (C) ######

##### A) Simulations #####

# simulation forêt initiale (0-600 ans) :
# forte pluie de graines pour construire une forêt mature à partir d'un sol nu
sim_00 <- troll(
  name = "sim_00",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 600, nbs0 = 2250, fecundity = 0, Rrecruit = 0, distdisperse = 0, torus = 1
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE
)
sim_00
save(sim_00, file = "D:/Mes Donnees/TROLL/simulations/sim_00.Rdata")

# graphique de vérification de la simulation :
g <- sim_00@ecosystem %>% 
  gather(variable, value, -iter) %>% 
  filter(variable %in% c("sum1", "sum10", "agb")) %>% 
  ggplot(aes(iter/12, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") + 
  theme_bw() +
  labs(title = "sim_00 (fecundity = 0, Rrecruit = 0, distdisperse = 0, torus = 1)")
g
ggsave("D:/Mes Donnees/TROLL/simulations/sim_00_t.png", g, width = 10, height = 10, dpi = 300, bg = "white")

# simulation suite de la forêt (600-1200 ans) avec la fonction get_forest :
# on fait varier l'intensité de la pluie de graines
sim_00_nul <- troll(
  name = "sim_00_null_rain50000",
  global = update_parameters(sim_00, nbiter = 12 * 600, Cseedrain = 50000),
  species = sim_00@inputs$species,
  climate = sim_00@inputs$climate,
  daily = sim_00@inputs$daily,
  forest = get_forest(sim_00),
  verbose = T
)
save(sim_00_nul, file = "D:/Mes Donnees/TROLL/simulations/sim_00_seedrain50000.Rdata")

sim_00_bis <- troll(
  name = "sim_00_null_rain17",
  global = update_parameters(sim_00, nbiter = 12 * 600, Cseedrain = 17),
  species = sim_00@inputs$species,
  climate = sim_00@inputs$climate,
  daily = sim_00@inputs$daily,
  forest = get_forest(sim_00),
  verbose = T
)
save(sim_00_bis, file = "D:/Mes Donnees/TROLL/simulations/sim_00_seedrain17.Rdata")

sim_00_tris <- troll(
  name = "sim_00_rain0",
  global = update_parameters(sim_00, nbiter = 12 * 600, fecundity = 0, Rrecruit = 0, distdisperse = 0, 
                             torus = 1, Cseedrain = 0),
  species = sim_00@inputs$species,
  climate = sim_00@inputs$climate,
  daily = sim_00@inputs$daily,
  forest = get_forest(sim_00),
  verbose = T
)
save(sim_00_tris, file = "D:/Mes Donnees/TROLL/simulations/sim_00_seedrain0.Rdata")

## Graphiques de validation de la simulation :

sim_00_nul@ecosystem$iter <- sim_00_nul@ecosystem$iter + max(sim_00@ecosystem$iter)
sim_00_tris@ecosystem$iter <- sim_00_tris@ecosystem$iter + max(sim_00@ecosystem$iter)

# Créer les graphiques

my3cols <- c("#FC4E07", "cornflowerblue","gold1" )

a <- list(
  "initial forest" = sim_00@ecosystem,
  "seed rain = 50000" = sim_00_nul@ecosystem,
  "seed rain = 0" = sim_00_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, agb, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_manual(values = my3cols) 

b <- list(
  "initial forest" = sim_00@ecosystem,
  "seed rain = 50000" = sim_00_nul@ecosystem,
  "seed rain = 0" = sim_00_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum1, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("sum1") +
  scale_color_manual(values = my3cols) 

c <- list(
  "initial forest" = sim_00@ecosystem,
  "seed rain = 50000" = sim_00_nul@ecosystem,
  "seed rain = 0" = sim_00_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum10, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("sum10") +
  scale_color_manual(values = my3cols) 

d <- list(
  "initial forest" = sim_00@ecosystem,
  "seed rain = 50000" = sim_00_nul@ecosystem,
  "seed rain = 0" = sim_00_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum30, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("sum30") +
  scale_color_manual(values = my3cols) 

# Combiner les graphiques sur une page
figure_00 <- ggarrange(a, b, c, d,
                       labels = c("A", "B", "C", "D"),
                       ncol = 2, nrow = 2, 
                       common.legend = TRUE, legend = "bottom")


annotate_figure(figure_00,
                top = text_grob("Simulation 00 avec variation de la seed rain", face = "bold", size = 16))


##### B) Test perte de diversité (stabilisation ?) #####

# on teste de faire davantage d'itérations pour voir la perte de diversité :
# +600 ans après tris 
sim_00_quad <- troll(
  name = "sim_00_rain0_suite",
  global = update_parameters(sim_00, nbiter = 12 * 600, fecundity = 0, Rrecruit = 0, distdisperse = 0, torus = 1, Cseedrain = 0),
  species = sim_00_tris@inputs$species,
  climate = sim_00_tris@inputs$climate,
  daily = sim_00_tris@inputs$daily,
  forest = get_forest(sim_00_tris),
  verbose = T
)
save(sim_00_quad, file = "D:/Mes Donnees/TROLL/simulations/sim_00_seedrain0_suite.Rdata")

# aussi sim_00_cinq et sim_00_six avec à chaque fois +600 ans
load("~/TROLL/simulations/Test perte RS/Simulations/sim_00_seedrain0_suite_bis.Rdata")
load("~/TROLL/simulations/Test perte RS/Simulations/sim_00_seedrain0_suite_tris.Rdata")

# calcul de la richesse spécifique pour sim_00 seedrain 0 dans la forêt finale : 
R_00 <- filter(sim_00_tris@forest, sim_00_tris@forest$iter==7200)
R_00 <- unique(R_00$s_name)
length(R_00)
# richesse spé : 110

# calcul de la richesse spécifique pour chaque itération :
# sim_00
data_test <- sim_00@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1 + data_test$sum10 + data_test$sum30

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_00@ecosystem <- left_join(sim_00@ecosystem, mydata)

ggplot(sim_00@ecosystem, aes(iter / 12, r_sp)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Richesse spécifique")

# sim_00_nul
data_test <- sim_00_nul@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1 + data_test$sum10 + data_test$sum30

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_00_nul@ecosystem <- left_join(sim_00_nul@ecosystem, mydata)

# sim_00_tris
data_test <- sim_00_tris@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1 + data_test$sum10 + data_test$sum30

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata_tris <- data.frame(r_sp = test, iter)

sim_00_tris@ecosystem <- left_join(sim_00_tris@ecosystem, mydata)

ggplot(mydata_tris, aes(iter / 12, r_sp)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Richesse spécifique")


# sim_00_quad
data_test <- sim_00_quad@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata_quad <- data.frame(r_sp = test, iter)

sim_00_quad@ecosystem <- left_join(sim_00_quad@ecosystem, mydata)

ggplot(mydata_quad, aes(iter / 12, r_sp)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Richesse spécifique")

# sim_00_cinq
data_test <- sim_00_cinq@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata_cinq <- data.frame(r_sp = test, iter)

sim_00_cinq@ecosystem <- left_join(sim_00_cinq@ecosystem, mydata)

ggplot(mydata_cinq, aes(iter / 12, r_sp)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Richesse spécifique")

# sim_00_six
data_test <- sim_00_six@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata_six <- data.frame(r_sp = test, iter)

sim_00_six@ecosystem <- left_join(sim_00_six@ecosystem, mydata)

ggplot(test, aes(iter / 12, r_sp)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Richesse spécifique")

# faire le graph pour toutes les itérations : 

mydata_tris$iter <- mydata_tris$iter + max(sim_00@ecosystem$iter)
mydata_quad$iter <- mydata_quad$iter + max(mydata_tris$iter)
mydata_cinq$iter <- mydata_cinq$iter + max(mydata_quad$iter)
sim_00_six@ecosystem$iter <- sim_00_six@ecosystem$iter + max(mydata_cinq$iter)


my3cols <- c("cornflowerblue", "#FC4E07" )

x <- list(
  "forêt initiale avec \n pluie de graines" = sim_00@ecosystem,
  "forêt finale sans \n pluie de graines" = mydata_tris,
  "forêt finale sans \n pluie de graines" = mydata_quad, 
  "forêt finale sans \n pluie de graines" = mydata_cinq 
) %>%
  bind_rows(.id = "Simulations") %>%
  ggplot(aes(iter / 12, r_sp, col = Simulations)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Temps (années)") +
  ylab("Richesse spécifique") +
  scale_color_manual(values = my3cols)+
  theme(legend.position = "bottom")+
  labs(title = "Variation de la richesse spécifique au cours du temps dans le scénario contrôle")
  

##### C) Test augmentation taille de parcelle #####

# on fait varier la taille de la parcelle pour sim_00 (contrôle) : 

#### 6,25 hectares ####
# avec pluie de graines
load("~/TROLL/simulations/H1_simulations/sim_00_fec_recruit_disperse.Rdata")

# calcul pour 6,25 hectares (250x250) (simulation déjà faite) :
data_test <- sim_00@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_00@ecosystem <- left_join(sim_00@ecosystem, mydata)

# sans pluie de graines
load("~/TROLL/simulations/H1_simulations/Contrôle (C)/sim_00_seedrain0.Rdata")

data_test <- sim_00_tris@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_00_tris@ecosystem <- left_join(sim_00_tris@ecosystem, mydata)

ggplot(sim_00_tris@ecosystem, aes(iter / 12, r_sp)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Richesse spécifique")

# simulation en augmentant la taille de parcelle :

#### 25 hectares ####
# simulations :
sim_00_x2 <- troll(
  name = "sim_02_500x500",
  global = generate_parameters(
    cols = 500, rows = 500,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 0, Rrecruit = 0, distdisperse = 0, torus = 0, MinLAImax = 0, MaxLAImax = 0
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)

save(sim_00_x2, file = "D:/Mes Donnees/TROLL/simulations/sim_00_x2.Rdata")

sim_00_x2_seedrain0 <- troll(
  name = "sim_00x2_seedrain0",
  global = update_parameters(sim_00_x2, nbiter = 12 * 600, Cseedrain = 0),
  species = sim_00_x2@inputs$species,
  climate = sim_00_x2@inputs$climate,
  daily = sim_00_x2@inputs$daily,
  forest = get_forest(sim_00_x2),
  verbose = T
)
save(sim_00_x2_seedrain0, file = "D:/Mes Donnees/TROLL/simulations/sim_00_x2_seedrain0.Rdata")

# calcul richesse spécifique :
data_test <- sim_00_x2@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_00_x2@ecosystem <- left_join(sim_00_x2@ecosystem, mydata)

sim_00_x2_seedrain0@species$iter <- sim_00_x2_seedrain0@species$iter + max(sim_00_x2@species$iter)

sim_00_x2_seedrain0@ecosystem <- sim_00_x2_seedrain0@ecosystem[,-14]

data_test <- sim_00_x2_seedrain0@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(7200:14399)

mydata <- data.frame(r_sp = test, iter)

sim_00_x2_seedrain0@ecosystem <- left_join(sim_00_x2_seedrain0@ecosystem, mydata)

ggplot(sim_00_x2_seedrain0@ecosystem, aes(iter / 12, r_sp)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Richesse spécifique")

#### 100 hectares ####
# simulations :
sim_00_x4 <- troll(
  name = "sim_00_1000x1000",
  global = generate_parameters(
    cols = 1000, rows = 1000,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 0, Rrecruit = 0, distdisperse = 0, torus = 0, MinLAImax = 0, MaxLAImax = 0
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)

save(sim_00_x4, file = "D:/Mes Donnees/TROLL/simulations/sim_00_x4.Rdata")

sim_00_x4_seedrain0 <- troll(
  name = "sim_00x4_seedrain0",
  global = update_parameters(sim_00_x4, nbiter = 12 * 600, Cseedrain = 0),
  species = sim_00_x4@inputs$species,
  climate = sim_00_x4@inputs$climate,
  daily = sim_00_x4@inputs$daily,
  forest = get_forest(sim_00_x4),
  verbose = T
)
save(sim_00_x4_seedrain0, file = "D:/Mes Donnees/TROLL/simulations/sim_00_x4_seedrain0.Rdata")

# calcul richesse spécifique :
data_test <- sim_00_x4@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_00_x4@ecosystem <- left_join(sim_00_x4@ecosystem, mydata)

sim_00_x4_seedrain0@species$iter <- sim_00_x4_seedrain0@species$iter + max(sim_00_x4@species$iter)

data_test <- sim_00_x4_seedrain0@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(7199:14398)

mydata <- data.frame(r_sp = test, iter)

sim_00_x4_seedrain0@ecosystem <- left_join(sim_00_x4_seedrain0@ecosystem, mydata)

ggplot(sim_00_x4_seedrain0@ecosystem, aes(iter / 12, r_sp)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Richesse spécifique")

# on fait le graph pour voir l'évolution de la perte de RS selon la taille de parcelle :
sim_00_tris@ecosystem$iter <- sim_00_tris@ecosystem$iter + max(sim_00@ecosystem$iter)
sim_00_x2_seedrain0@ecosystem$iter <- sim_00_x2_seedrain0@ecosystem$iter + max(sim_00_x2@ecosystem$iter)
sim_00_x4_seedrain0@ecosystem$iter <- sim_00_x4_seedrain0@ecosystem$iter + max(sim_00_x4@ecosystem$iter)

my3cols <- c("red4", "red2","lightsalmon","red4" , "red2","lightsalmon")

a <- list(
  "6,25ha" = sim_00_tris@ecosystem,
  "25ha" = sim_00_x2_seedrain0@ecosystem,
  "100ha" = sim_00_x4_seedrain0@ecosystem, 
  "6,25ha" = sim_00@ecosystem,
  "25ha" = sim_00_x2@ecosystem,
  "100ha" = sim_00_x4@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, r_sp, col = simulation)) +
  geom_point(size = 2) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression("Richesse spécifique")) +
  scale_color_manual(values = my3cols) +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 15), legend.text = element_text(size = 15))+
  ggtitle("Richesse spécifique dans des parcelles de tailles différentes pour le scénario C")


##### D) Réplicats #####

# Réplicats : avec la fonction stack

# en faisant une boucle pour définir les paramètres : 
# ! ici que de 1 à 9 car on récupère la première simulation qu'on a faite 

#### Forêt initiale (0-600 ans) #####

simulation <- paste0("sim_00_",1:4)
year <- 600
parameters <- data.frame()

for (sim in simulation) { 
  par_00 <- generate_parameters(cols = 250, rows = 250, nbiter = 12 * year, fecundity = 0, Rrecruit = 0, distdisperse = 0, torus = 1, NONRANDOM = 2, nbs0 = 2250)
  par_00$simulation <- sim
  
  parameters <- rbind(parameters, par_00)
  
}

stack_sim00 <- stack(
  name = "stack_sim00",
  simulations = simulation,
  global = parameters,
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = T,
  cores = 4,
  overwrite = F,
  path = "D:/Mes Programmes/rcontroll"
)


simulation <- paste0("sim_00_",5:9)
year <- 600
parameters <- data.frame()

for (sim in simulation) { 
  par_00 <- generate_parameters(cols = 250, rows = 250, nbiter = 12 * year, fecundity = 0, Rrecruit = 0, distdisperse = 0, torus = 1, NONRANDOM = 2, nbs0 = 2250)
  par_00$simulation <- sim
  
  parameters <- rbind(parameters, par_00)
  
}

save(stack_sim00, file = "D:/Mes Donnees/TROLL/simulations/stack_sim00.Rdata")

stack_sim00_bis <- stack(
  name = "stack_sim00_bis",
  simulations = simulation,
  global = parameters,
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = T,
  cores = 5,
  overwrite = F,
  path = "D:/Mes Programmes/rcontroll"
)
save(stack_sim00_bis, file = "D:/Mes Donnees/TROLL/simulations/stack_sim00_bis.Rdata")


#### Forêt finale (600-1200 ans) ####

simulation <- paste0("sim_00_",1:4)

stack_sim00_seedrain0 <- stack(
  name = "stack_sim00_seedrain0",
  simulations = simulation,
  global = update_parameters(stack_sim00  , nbiter = 12 * 600, fecundity = 0, Rrecruit = 0, distdisperse = 0, torus = 1, Cseedrain = 0, nbs0 = 2250)  ,
  species =  stack_sim00@inputs$species,
  climate = stack_sim00@inputs$climate,
  daily = stack_sim00@inputs$daily,
  forest = get_forest(stack_sim00),
  verbose = T,
  cores = 4, 
  overwrite = F,
  path = "D:/Mes Programmes/rcontroll")

save(stack_sim00_seedrain0, file = "D:/Mes Donnees/TROLL/simulations/stack_sim00_seedrain0.Rdata")


simulation <- paste0("sim_00_",5:9)

stack_sim00_tris_seedrain0 <- stack(
  name = "stack_sim00_tris_seedrain0",
  simulations = simulation,
  global = update_parameters(stack_sim00_bis  , nbiter = 12 * 600, fecundity = 0, Rrecruit = 0, distdisperse = 0, torus = 1, Cseedrain = 0, nbs0 = 2250)  ,
  species =  stack_sim00_bis@inputs$species,
  climate = stack_sim00_bis@inputs$climate,
  daily = stack_sim00_bis@inputs$daily,
  forest = get_forest(stack_sim00_bis),
  verbose = T,
  cores = 5, 
  overwrite = T,
  path = "D:/Mes Programmes/rcontroll/stack")

save(stack_sim00_tris_seedrain0, file = "D:/Mes Donnees/TROLL/simulations/stack_sim00_tris_seedrain0.Rdata")


#### Graphiques ####

# 1) Regrouper les tableaux de données pour la mise en place de la forêt

# on donne un nom de simulation à la première simulation faite précédemment 
simulation <- "sim_00_10"
sim_00@ecosystem <- cbind(simulation, sim_00@ecosystem)

# on regroupe tous les réplicats sur sim_00
sim_00_ecosystem <- bind_rows(stack_sim00@ecosystem, stack_sim00_bis@ecosystem, sim_00@ecosystem)
save(sim_00_ecosystem, file = "D:/Mes Donnees/TROLL/simulations/sim_00_ecosystem.Rdata")

sim_00@forest <- cbind(simulation, sim_00@forest)
sim_00_forest <- bind_rows(stack_sim00@forest, stack_sim00_bis@forest, sim_00@forest)
save(sim_00_forest, file = "D:/Mes Donnees/TROLL/simulations/sim_00_forest.Rdata")

# 2) Regrouper les tableaux de données pour la coupure de la pluie de graine
simulation <- "sim_00_10"
sim_00_tris@ecosystem <- cbind(simulation, sim_00_tris@ecosystem)
sim_00_seedrain0_ecosystem <- bind_rows(stack_sim00_seedrain0@ecosystem, stack_sim00_tris_seedrain0@ecosystem, sim_00_tris@ecosystem)
save(sim_00_seedrain0_ecosystem, file = "D:/Mes Donnees/TROLL/simulations/sim_00_seedrain0_ecosystem.Rdata")

sim_00_tris@forest <- cbind(simulation, sim_00_tris@forest)
sim_00_seedrain0_forest <- bind_rows(stack_sim00_seedrain0@forest, stack_sim00_tris_seedrain0@forest, sim_00_tris@forest)
save(sim_00_seedrain0_forest, file = "D:/Mes Donnees/TROLL/simulations/sim_00_seedrain0_forest.Rdata")

sim_00_seedrain0_ecosystem$iter <- sim_00_seedrain0_ecosystem$iter + max(sim_00_ecosystem$iter)
test <- bind_rows(sim_00_ecosystem, sim_00_seedrain0_ecosystem)

# 3) Calcul de la diversité fonctionnelle + RS : 

length(seq(from = 0, to = 7199, by = 120))

sim_00_tris@inputs$species$simulation <- "sim_00_10"
sim_00@inputs$species$simulation <- "sim_00_10"

sim_00_tris_seedrain0_functional <- left_join(stack_sim00_tris_seedrain0@inputs$species, 
                               filter(stack_sim00_tris_seedrain0@species, iter %in% c(seq(from = 0, to = 7199, by = 12))),
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

save(sim_00_tris_seedrain0_functional, file = "D:/Mes Donnees/TROLL/simulations/sim_00_tris_seedrain0_functional.Rdata")

# pour seedrain = 50 000
sim_00_tris_functional$iter <- as.numeric(sim_00_tris_functional$iter)
sim_00_bis_functional$iter <- as.numeric(sim_00_bis_functional$iter)
sim_00_functional$iter <- as.numeric(sim_00_functional$iter)

# on regroupe les data :
sim_00_func <- bind_rows(sim_00_functional, sim_00_bis_functional, sim_00_tris_functional)
save(sim_00_func, file = "D:/Mes Donnees/TROLL/simulations/sim_00_functional_global.Rdata")

# pour seedrain = 0
sim_00_seedrain0_tris_functional$iter <- as.numeric(sim_00_seedrain0_tris_functional$iter)
sim_00_seedrain0_bis_functional$iter <- as.numeric(sim_00_seedrain0_bis_functional$iter)
sim_00_seedrain0_functional$iter <- as.numeric(sim_00_seedrain0_functional$iter)

sim_00_seedrain0_func$iter <- as.numeric(sim_00_seedrain0_func$iter)
sim_00_func$iter <- as.numeric(sim_00_func$iter)

# on regroupe les data :
sim_00_seedrain0_func <- bind_rows(sim_00_seedrain0_functional, sim_00_tris_seedrain0_functional, sim_00_seedrain0_tris_functional)
save(sim_00_seedrain0_func, file = "D:/Mes Donnees/TROLL/simulations/sim_00_seedrain0_functional_global.Rdata")

# on regroupe seedrain 50 000 et 0 :
sim_00_seedrain0_func$iter <- sim_00_seedrain0_func$iter + max(sim_00_func$iter)
sim_00_func_final <- bind_rows(sim_00_func, sim_00_seedrain0_func)

# 4) Faire les graphiques :

# on fait des graphs en séparant chaque réplicat (ici nommés "simulation") :

my3cols <- c("#FC4E07", "cornflowerblue", "mediumaquamarine","gold1", "orchid", "red3", "green3", "violetred2", "dodgerblue","coral2"  )

a <- 
  ggplot(test, aes(iter / 12, agb, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_manual(values = my3cols) 
a

b <-  ggplot(test, aes(iter / 12, sum1, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 1cm of dbh") +
  scale_color_manual(values = my3cols) 

c <-  ggplot(test, aes(iter / 12, sum10, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 10cm of dbh") +
  scale_color_manual(values = my3cols) 

d <- ggplot(test, aes(iter / 12, sum30, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 30cm of dbh") +
  scale_color_manual(values = my3cols) 

e <- ggplot(final, aes(iter / 12, nbsp, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Specific richness") +
  scale_color_manual(values = my3cols) 
e

f <- ggplot(final, aes(iter / 12, FRic, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Functional richness") +
  scale_color_manual(values = my3cols) 
f

g <- ggplot(final, aes(iter / 12, FEve, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Functional eveness") +
  scale_color_manual(values = my3cols) 
g

h <- ggplot(final, aes(iter / 12, RaoQ, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Indice de Rao") +
  scale_color_manual(values = my3cols) 
h


# Combiner les graphiques sur une page
figure <- ggarrange(a, b, c, d, e, f, g , h,
                       labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                       ncol = 2, nrow = 4, 
                       common.legend = TRUE, legend = "bottom")
figure

annotate_figure(figure, top = text_grob("Variation des paramètres de l'écosystème entre les réplicas de la simulation 0", face = "bold", size = 16))

?annotate_figure


##### surement à supprimer #####
# calcul des moyennes des réplicas par traits 

sim_00_1 <- filter(test, test$simulation=="sim_00_1")
names(sim_00_1) = c("simulation_1", "iter", "sum1_1", "sum10_1", "sum30_1", "ba_1", "ba10_1", "agb_1", "gpp_1", "npp_1", "rday_1", "rnight_1", "rstem_1", "litterfall_1")
sim_00_2 <- filter(test, test$simulation=="sim_00_2")
names(sim_00_2) = c("simulation_2", "iter", "sum1_2", "sum10_2", "sum30_2", "ba_2", "ba10_2", "agb_2", "gpp_2", "npp_2", "rday_2", "rnight_2", "rstem_2", "litterfall_2")
sim_00_3 <- filter(test, test$simulation=="sim_00_3")
names(sim_00_3) = c("simulation_3", "iter", "sum1_3", "sum10_3", "sum30_3", "ba_3", "ba10_3", "agb_3", "gpp_3", "npp_3", "rday_3", "rnight_3", "rstem_3", "litterfall_3")
sim_00_4 <- filter(test, test$simulation=="sim_00_4")
names(sim_00_4) = c("simulation_4", "iter", "sum1_4", "sum10_4", "sum30_4", "ba_4", "ba10_4", "agb_4", "gpp_4", "npp_4", "rday_4", "rnight_4", "rstem_4", "litterfall_4")
sim_00_5 <- filter(test, test$simulation=="sim_00_5")
names(sim_00_5) = c("simulation_5", "iter", "sum1_5", "sum10_5", "sum30_5", "ba_5", "ba10_5", "agb_5", "gpp_5", "npp_5", "rday_5", "rnight_5", "rstem_5", "litterfall_5")
sim_00_6 <- filter(test, test$simulation=="sim_00_6")
names(sim_00_6) = c("simulation_6", "iter", "sum1_6", "sum10_6", "sum30_6", "ba_6", "ba10_6", "agb_6", "gpp_6", "npp_6", "rday_6", "rnight_6", "rstem_6", "litterfall_6")
sim_00_7 <- filter(test, test$simulation=="sim_00_7")
names(sim_00_7) = c("simulation_7", "iter", "sum1_7", "sum10_7", "sum30_7", "ba_7", "ba10_7", "agb_7", "gpp_7", "npp_7", "rday_7", "rnight_7", "rstem_7", "litterfall_7")
sim_00_8 <- filter(test, test$simulation=="sim_00_8")
names(sim_00_8) = c("simulation_8", "iter", "sum1_8", "sum10_8", "sum30_8", "ba_8", "ba10_8", "agb_8", "gpp_8", "npp_8", "rday_8", "rnight_8", "rstem_8", "litterfall_8")
sim_00_9 <- filter(test, test$simulation=="sim_00_9")
names(sim_00_9) = c("simulation_9", "iter", "sum1_9", "sum10_9", "sum30_9", "ba_9", "ba10_9", "agb_9", "gpp_9", "npp_9", "rday_9", "rnight_9", "rstem_9", "litterfall_9")
sim_00_10 <- filter(test, test$simulation=="sim_00_10")
names(sim_00_10) = c("simulation_10", "iter", "sum1_10", "sum10_10", "sum30_10", "ba_10", "ba10_10", "agb_10", "gpp_10", "npp_10", "rday_10", "rnight_10", "rstem_10", "litterfall_10")


data <- left_join(sim_00_1, sim_00_2,
          by = c("iter" = "iter"), relationship = "many-to-many")

data <- left_join(data, sim_00_3,
                  by = c("iter" = "iter"), relationship = "many-to-many")
data <- left_join(data, sim_00_4,
                  by = c("iter" = "iter"), relationship = "many-to-many")

data <- left_join(data, sim_00_5,
                  by = c("iter" = "iter"), relationship = "many-to-many")

data <- left_join(data, sim_00_6,
                  by = c("iter" = "iter"), relationship = "many-to-many")

data <- left_join(data, sim_00_7,
                  by = c("iter" = "iter"), relationship = "many-to-many")

data <- left_join(data, sim_00_8,
                  by = c("iter" = "iter"), relationship = "many-to-many")

data <- left_join(data, sim_00_9,
                  by = c("iter" = "iter"), relationship = "many-to-many")


data <- left_join(data, sim_00_10,
                  by = c("iter" = "iter"), relationship = "many-to-many")


write.csv2(data, "D:/Mes Donnees/TROLL/simulations/data_test.csv")

data_test <- read.table(file.choose(), h = T, sep = ";", dec = "," ) # mean_sim00

data_test <- bind_rows(test, data_test)

data_test$simulation <- fct_inorder(data_test$simulation)


#

my3cols <- c("grey", "grey", "grey","grey", "grey", "grey", "grey", "grey", "grey","grey" , "green3" )

a <- 
  ggplot(data_test, aes(iter / 12, agb, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_manual(values = my3cols) 
a

b <-  ggplot(data_test, aes(iter / 12, sum1, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 1cm of dbh") +
  scale_color_manual(values = my3cols) 

c <-  ggplot(data_test, aes(iter / 12, sum10, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 10cm of dbh") +
  scale_color_manual(values = my3cols) 

d <- ggplot(data_test, aes(iter / 12, sum30, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 30cm of dbh") +
  scale_color_manual(values = my3cols) 

e <- ggplot(final, aes(iter / 12, nbsp, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Specific richness") +
  scale_color_manual(values = my3cols) 
e

f <- ggplot(final, aes(iter / 12, FRic, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Functional richness") +
  scale_color_manual(values = my3cols) 
f

g <- ggplot(final, aes(iter / 12, FEve, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Functional eveness") +
  scale_color_manual(values = my3cols) 
g

h <- ggplot(final, aes(iter / 12, RaoQ, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Indice de Rao") +
  scale_color_manual(values = my3cols) 
h



###### 2) Fécondité (F) ######

##### A) Simulation unique #####

# forêt initiale (0-600 ans) :
sim_01 <- troll(
  name = "sim_01_fecundity",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 1, Rrecruit = 0, distdisperse = 0, torus = 1
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)
sim_01
save(sim_01, file = "D:/Mes Donnees/TROLL/simulations/sim_01_fecundity.Rdata")

g <- sim_01@ecosystem %>% 
  gather(variable, value, -iter) %>% 
  filter(variable %in% c("sum1", "sum10", "agb")) %>% 
  ggplot(aes(iter/12, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") + 
  theme_bw()+
  labs(title = "sim_01 (fecundity = 1, Rrecruit = 0, distdisperse = 0, torus = 1)")
g
ggsave("D:/Mes Donnees/TROLL/simulations/sim_01_t.png", g, width = 10, height = 10, dpi = 300, bg = "white")

# forêt finale (600-1200 ans) :
# en faisant varier l'intensité de la pluie de graines :
sim_01_nul <- troll(
  name = "sim_01_null_rain50000",
  global = update_parameters(sim_01, nbiter = 12 * 600, Cseedrain = 50000),
  species = sim_01@inputs$species,
  climate = sim_01@inputs$climate,
  daily = sim_01@inputs$daily,
  forest = get_forest(sim_01),
  verbose = T
)
save(sim_01_nul, file = "D:/Mes Donnees/TROLL/simulations/sim_01_seedrain50000.Rdata")

sim_01_bis <- troll(
  name = "sim_01_null_rain17",
  global = update_parameters(sim_01, nbiter = 12 * 600, Cseedrain = 17),
  species = sim_01@inputs$species,
  climate = sim_01@inputs$climate,
  daily = sim_01@inputs$daily,
  forest = get_forest(sim_01),
  verbose = T
)
save(sim_01_bis, file = "D:/Mes Donnees/TROLL/simulations/sim_01_seedrain11.Rdata")

sim_01_tris <- troll(
  name = "sim_01_rain0",
  global = update_parameters(sim_01, nbiter = 12 * 600, Cseedrain = 0),
  species = sim_01@inputs$species,
  climate = sim_01@inputs$climate,
  daily = sim_01@inputs$daily,
  forest = get_forest(sim_01),
  verbose = T
)
save(sim_01_tris, file = "D:/Mes Donnees/TROLL/simulations/sim_01_seedrain0.Rdata")


## Graphique validation de la simulation : 

# mise à jour du nom des itérations pour seedrain0
sim_01_nul@ecosystem$iter <- sim_01_nul@ecosystem$iter + max(sim_01@ecosystem$iter)
sim_01_bis@ecosystem$iter <- sim_01_bis@ecosystem$iter + max(sim_01@ecosystem$iter)
sim_01_tris@ecosystem$iter <- sim_01_tris@ecosystem$iter + max(sim_01@ecosystem$iter)

my3cols <- c("#FC4E07", "cornflowerblue","gold1" )

a <- list(
  "initial forest" = sim_01@ecosystem,
  "seed rain = 50000" = sim_01_nul@ecosystem,
  "seed rain = 0" = sim_01_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, agb, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_manual(values = my3cols) 

b <- list(
  "initial forest" = sim_01@ecosystem,
  "seed rain = 50000" = sim_01_nul@ecosystem,
  "seed rain = 0" = sim_01_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum1, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 1cm of dbh") +
  scale_color_manual(values = my3cols) 

c <- list(
  "initial forest" = sim_01@ecosystem,
  "seed rain = 50000" = sim_01_nul@ecosystem,
  "seed rain = 0" = sim_01_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum10, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 10cm of dbh") +
  scale_color_manual(values = my3cols) 

d <- list(
  "initial forest" = sim_01@ecosystem,
  "seed rain = 50000" = sim_01_nul@ecosystem,
  "seed rain = 0" = sim_01_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum30, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 30cm of dbh") +
  scale_color_manual(values = my3cols) 

# Combiner les graphiques sur une page
figure_01 <- ggarrange(a, b, c, d,
                       labels = c("A", "B", "C", "D"),
                       ncol = 2, nrow = 2, 
                       common.legend = TRUE, legend = "bottom")

annotate_figure(figure_01,
                top = text_grob("Simulation 01 avec variation de la seed rain", face = "bold", size = 16))


## Richesse spécifique :

R_01 <- filter(sim_01_tris@forest, sim_01_tris@forest$iter==7200)
R_01 <- unique(R_01$s_name)
length(R_01)
# richesse spé : 78

# calcul de la richesse spécifique pour chaque itération
# sim_01
data_test <- sim_01@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1 + data_test$sum10 + data_test$sum30

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_01@ecosystem <- left_join(sim_01@ecosystem, mydata)

# sim_01_nul
data_test <- sim_01_nul@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1 + data_test$sum10 + data_test$sum30

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_01_nul@ecosystem <- left_join(sim_01_nul@ecosystem, mydata)

# sim_01_tris
data_test <- sim_01_tris@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1 + data_test$sum10 + data_test$sum30

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_01_tris@ecosystem <- left_join(sim_01_tris@ecosystem, mydata)

# faire le graph : 

sim_01_nul@ecosystem$iter <- sim_01_nul@ecosystem$iter + max(sim_01@ecosystem$iter)
sim_01_tris@ecosystem$iter <- sim_01_tris@ecosystem$iter + max(sim_01@ecosystem$iter)

my3cols <- c("#FC4E07", "cornflowerblue","gold1" )

x <- list(
  "initial forest" = sim_01@ecosystem,
  "seed rain = 50000" = sim_01_nul@ecosystem,
  "seed rain = 0" = sim_01_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, r_sp, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Richesse spécifique") +
  scale_color_manual(values = my3cols)+
  labs(title = "Variation de la richesse spécifique au cours du temps dans la simulation 1")

x


##### B) Réplicats #####

#### Forêt initiale (0-600 ans) ####
# avec pluie de graines (50 000)

simulation <- paste0("sim_01_",1:6)
year <- 600
parameters <- data.frame()

for (sim in simulation) { 
  par_00 <- generate_parameters (cols = 250, rows = 250, nbiter = 12 * year, fecundity = 1, Rrecruit = 0, distdisperse = 0, torus = 1, nbs0 = 2250, NONRANDOM = 2)
  par_00$simulation <- sim
  
  parameters <- rbind(parameters, par_00)
  
}

stack_sim01 <- stack(
  name = "stack_sim01",
  simulations = simulation,
  global = parameters,
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = T,
  cores = 6,
  overwrite = T,
  path = "D:/Mes Programmes/rcontroll"
)

save(stack_sim01, file = "D:/Mes Donnees/TROLL/simulations/stack_sim_01.Rdata")


simulation <- paste0("si_01_",7:9)
year <- 600
parameters <- data.frame()

for (sim in simulation) { 
  par_01 <- generate_parameters(cols = 250, rows = 250, nbiter = 12 * year, fecundity = 1, Rrecruit = 0, distdisperse = 0, torus = 1, NONRANDOM = 2, nbs0 = 2250)
  par_01$simulation <- sim
  
  parameters <- rbind(parameters, par_01)
  
}

stack_sim01_bis <- stack(
  name = "stack_sim01_bis",
  simulations = simulation,
  global = parameters,
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = T,
  cores = 4,
  overwrite = T,
  path = "D:/Mes Programmes/rcontroll"
)

save(stack_sim01_bis, file = "D:/Mes Donnees/TROLL/simulations/stack_sim_01_bis.Rdata")


#### Forêt finale (600-1200 ans) ####
# sans pluie de graines (0)

simulation <- paste0("sim_01_",1:6)

stack_sim_01_seedrain0 <- stack(
  name = "stack_sim_01_seedrain0",
  simulations = simulation,
  global = update_parameters(stack_sim01, nbiter = 12 * 600, fecundity = 1, Rrecruit = 0, distdisperse = 0, torus = 1, Cseedrain = 0, nbs0 = 2250), 
  species =  stack_sim01@inputs$species,
  climate = stack_sim01@inputs$climate,
  daily = stack_sim01@inputs$daily,
  forest = get_forest(stack_sim01),
  verbose = T,
  cores = 6, 
  overwrite = T,
  path = "D:/Mes Programmes/rcontroll")
save(stack_sim_01_seedrain0, file = "D:/Mes Donnees/TROLL/simulations/stack_sim_01_seedrain0.Rdata")

simulation <- paste0("si_01_",7:9)

stack_sim_01_bis_seedrain0 <- stack(
  name = "stack_sim_01_bis_seedrain0",
  simulations = simulation,
  global = update_parameters(stack_sim01_bis, nbiter = 12 * 600, fecundity = 1, Rrecruit = 0, distdisperse = 0, torus = 1, Cseedrain = 0, nbs0 = 2250), 
  species =  stack_sim01_bis@inputs$species,
  climate = stack_sim01_bis@inputs$climate,
  daily = stack_sim01_bis@inputs$daily,
  forest = get_forest(stack_sim01_bis),
  verbose = T,
  cores = 6, 
  overwrite = T,
  path = "D:/Mes Programmes/rcontroll")

save(stack_sim_01_bis_seedrain0, file = "D:/Mes Donnees/TROLL/simulations/stack_sim_01_bis_seedrain0.Rdata")


#### Diversité fonctionnelle #### 

# télécharger les Rdata directement, je n'ai pas remis les codes, c'est le même que pour sim_00 en modifiant les données
sim_01_func <- bind_rows(sim_01_functional, sim_01_bis_functional)
sim_01_seedrain0_func <- bind_rows(sim_01_seedrain0_functional, sim_01_bis_seedrain0_functional)

sim_01_func$iter <- as.numeric(sim_01_func$iter)
sim_01_seedrain0_func$iter <- as.numeric(sim_01_seedrain0_func$iter)

sim_01_seedrain0_func$iter <- sim_01_seedrain0_func$iter + max(sim_01_func$iter)
sim_01_func_final <- bind_rows(sim_01_func, sim_01_seedrain0_func)
save(sim_01_func_final, file = "D:/Mes Donnees/TROLL/simulations/sim_01_func_final.Rdata")

# on combine pour avoir un fichier ecosystem final avec tous les réplicats pour avec et sans seedrain
sim_01_ecosystem <- bind_rows(stack_sim01@ecosystem, stack_sim01_bis@ecosystem)
sim_01_seedrain0_ecosystem <- bind_rows(stack_sim_01_seedrain0@ecosystem, stack_sim_01_bis_seedrain0@ecosystem)

sim_01_seedrain0_ecosystem$iter <- sim_01_seedrain0_ecosystem$iter + max(sim_01_ecosystem$iter)
sim_01_ecosystem_final <- bind_rows(sim_01_ecosystem, sim_01_seedrain0_ecosystem)
save(sim_01_ecosystem_final, file = "D:/Mes Donnees/TROLL/simulations/sim_01_ecosystem_final.Rdata")

#### Graphiques des différents réplicats ####

my3cols <- c("#FC4E07", "cornflowerblue", "mediumaquamarine","gold1", "orchid", "red3", "green3", "violetred2", "dodgerblue","coral2"  )

a <- 
  ggplot(sim_01_ecosystem_final, aes(iter / 12, agb, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Biomasse ~ aérienne ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_manual(values = my3cols) 
a

b <-  ggplot(sim_01_ecosystem_final, aes(iter / 12, sum1, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Nombre d'individus > 1cm de dbh") +
  scale_color_manual(values = my3cols) 

c <-  ggplot(sim_01_ecosystem_final, aes(iter / 12, sum10, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Nombre d'individus > 10cm de dbh") +
  scale_color_manual(values = my3cols) 

d <- ggplot(sim_01_ecosystem_final, aes(iter / 12, sum30, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Nombre d'individus > 30cm de dbh") +
  scale_color_manual(values = my3cols) 

e <- ggplot(sim_01_func_final, aes(iter / 12, nbsp, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Richesse spécifique") +
  scale_color_manual(values = my3cols) 
e

sim_00_functional$iter <- as.numeric(sim_00_functional$iter)

f <- ggplot(sim_01_func_final, aes(iter / 12, FRic, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Richesse fonctionnelle") +
  scale_color_manual(values = my3cols) 
f

g <- ggplot(sim_01_func_final, aes(iter / 12, FEve, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Régularité fonctionnelle") +
  scale_color_manual(values = my3cols) 
g

h <- ggplot(sim_01_func_final, aes(iter / 12, FDis, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Dissimilarité fonctionnelle") +
  scale_color_manual(values = my3cols) 
h

figure <- ggarrange(a, b, c, d, e, f, g , h,
                    labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                    ncol = 2, nrow = 4, 
                    common.legend = TRUE, legend = "bottom")

annotate_figure(figure, top = text_grob("Variation des paramètres de l'écosystème entre les réplicas de la simulation 1", face = "bold", size = 16))


###### 3) Recrutement (R) ######

##### A) Simulation unique #####

sim_02 <- troll(
  name = "sim_02_recruit",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 0, Rrecruit = 1, distdisperse = 0, torus = 1
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)
sim_02
save(sim_02, file = "D:/Mes Donnees/TROLL/simulations/sim_02_recruit.Rdata")

g <- sim_02@ecosystem %>% 
  gather(variable, value, -iter) %>% 
  filter(variable %in% c("sum1", "sum10", "agb")) %>% 
  ggplot(aes(iter/12, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") + 
  theme_bw()+
  labs(title = "sim_02 (fecundity = 0, Rrecruit = 1, distdisperse = 0, torus = 1)")
g
ggsave("D:/Mes Donnees/TROLL/simulations/sim_02_t.png", g, width = 10, height = 10, dpi = 300, bg = "white")

sim_02_nul <- troll(
  name = "sim_02_rain50000",
  global = update_parameters(sim_02, nbiter = 12 * 600, Cseedrain = 50000),
  species = sim_02@inputs$species,
  climate = sim_02@inputs$climate,
  daily = sim_02@inputs$daily,
  forest = get_forest(sim_02),
  verbose = T
)
save(sim_02_nul, file = "D:/Mes Donnees/TROLL/simulations/sim_02_seedrain50000.Rdata")

sim_02_bis <- troll(
  name = "sim_02_rain17",
  global = update_parameters(sim_02, nbiter = 12 * 600, Cseedrain = 17),
  species = sim_02@inputs$species,
  climate = sim_02@inputs$climate,
  daily = sim_02@inputs$daily,
  forest = get_forest(sim_02),
  verbose = T
)
save(sim_02_bis, file = "D:/Mes Donnees/TROLL/simulations/sim_02_seedrain17.Rdata")

sim_02_tris <- troll(
  name = "sim_02_rain0",
  global = update_parameters(sim_02, nbiter = 12 * 600, Cseedrain = 0),
  species = sim_02@inputs$species,
  climate = sim_02@inputs$climate,
  daily = sim_02@inputs$daily,
  forest = get_forest(sim_02),
  verbose = T
)
save(sim_02_tris, file = "D:/Mes Donnees/TROLL/simulations/sim_02_seedrain0.Rdata")

# richesse spécifique :
R_02 <- filter(sim_02_tris@forest, sim_02_tris@forest$iter==7200)
R_02 <- unique(R_02$s_name)
length(R_02)
# richesse spé : 67


# graphique validation simulation :

sim_02_nul@ecosystem$iter <- sim_02_nul@ecosystem$iter + max(sim_02@ecosystem$iter)
sim_02_tris@ecosystem$iter <- sim_02_tris@ecosystem$iter + max(sim_02@ecosystem$iter)

my3cols <- c("#FC4E07", "cornflowerblue","gold1" )

a <- list(
  "initial forest" = sim_02@ecosystem,
  "seed rain = 50000" = sim_02_nul@ecosystem,
  "seed rain = 0" = sim_02_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, agb, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_manual(values = my3cols) 

b <- list(
  "initial forest" = sim_02@ecosystem,
  "seed rain = 50000" = sim_02_nul@ecosystem,
  "seed rain = 0" = sim_02_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum1, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 1cm of dbh") +
  scale_color_manual(values = my3cols) 

c <- list(
  "initial forest" = sim_02@ecosystem,
  "seed rain = 50000" = sim_02_nul@ecosystem,
  "seed rain = 0" = sim_02_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum10, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 10cm of dbh") +
  scale_color_manual(values = my3cols) 

d <- list(
  "initial forest" = sim_02@ecosystem,
  "seed rain = 50000" = sim_02_nul@ecosystem,
  "seed rain = 0" = sim_02_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum30, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 30cm of dbh") +
  scale_color_manual(values = my3cols) 

# Combiner les graphiques sur une page
figure_02 <- ggarrange(a, b, c, d,
                       labels = c("A", "B", "C", "D"),
                       ncol = 2, nrow = 2, 
                       common.legend = TRUE, legend = "bottom")
figure_02

annotate_figure(figure_02,
                top = text_grob("Simulation 02 avec variation de la seed rain", face = "bold", size = 16))


##### B) Replicats #####

simulation <- paste0("sim_02_",1:6)
year <- 600
parameters <- data.frame()

for (sim in simulation) { 
  par_00 <- generate_parameters (cols = 250, rows = 250, nbiter = 12 * year, fecundity = 0, Rrecruit = 1, distdisperse = 0, torus = 1, nbs0 = 2250, NONRANDOM = 2)
  par_00$simulation <- sim
  
  parameters <- rbind(parameters, par_00)
  
}

stack_sim02 <- stack(
  name = "stack_sim02",
  simulations = simulation,
  global = parameters,
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = T,
  cores = 6,
  overwrite = T,
  path = "D:/Mes Programmes/rcontroll"
)

save(stack_sim02, file = "D:/Mes Donnees/TROLL/simulations/stack_sim_04.Rdata")


stack_sim_02_seedrain0 <- stack(
  name = "stack_sim_02_seedrain0",
  simulations = simulation,
  global = update_parameters(stack_sim02, nbiter = 12 * 600, fecundity = 0, Rrecruit = 1, distdisperse = 0, torus = 1, Cseedrain = 0, nbs0 = 2250), 
  species =  stack_sim02@inputs$species,
  climate = stack_sim02@inputs$climate,
  daily = stack_sim02@inputs$daily,
  forest = get_forest(stack_sim02),
  verbose = T,
  cores = 6, 
  overwrite = T,
  path = "D:/Mes Programmes/rcontroll")

save(stack_sim_02_seedrain0, file = "D:/Mes Donnees/TROLL/simulations/stack_sim_02_sedrain0.Rdata")


# calcul diversité fonctionnelle : 

# même code pour calculer pour les autres stacks, juste changer les données d'entrée

sim_02_bis_seedrain0_functional <- left_join(stack_sim_02_bis_seedrain0@inputs$species, 
                                    filter(stack_sim_02_bis_seedrain0@species, iter %in% c(seq(from = 0, to = 7199, by = 12))),
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

save(sim_02_bis_seedrain0_functional, file = "D:/Mes Donnees/TROLL/simulations/sim_02_bis_seedrain0_functional.Rdata")

# Récupérer les Rdata enregistrés
# avec pluie de graines :
sim_02_func <- bind_rows(sim_02_functional, sim_02_bis_functional)
# sans pluie de graines :
sim_02_seedrain0_func <- bind_rows(sim_02_seedrain0_functional, sim_02_bis_seedrain0_functional)

sim_02_func$iter <- as.numeric(sim_02_func$iter)
sim_02_seedrain0_func$iter <- as.numeric(sim_02_seedrain0_func$iter)

# on fait un seul data global avec et sans pluie de graines :
sim_02_seedrain0_func$iter <- sim_02_seedrain0_func$iter + max(sim_02_func$iter)
sim_02_func_final <- bind_rows(sim_02_func, sim_02_seedrain0_func)
save(sim_02_func_final, file = "D:/Mes Donnees/TROLL/simulations/sim_02_func_final.Rdata")

# on fait un seul data global de ecosystem avec et sans pluie de graines :

sim_02_ecosystem <- bind_rows(stack_sim02@ecosystem, stack_sim02_bis@ecosystem)
sim_02_seedrain0_ecosystem <- bind_rows(stack_sim_02_seedrain0@ecosystem, stack_sim_02_bis_seedrain0@ecosystem)

sim_02_seedrain0_ecosystem$iter <- sim_02_seedrain0_ecosystem$iter + max(sim_02_ecosystem$iter)
sim_02_ecosystem_final <- bind_rows(sim_02_ecosystem, sim_02_seedrain0_ecosystem)
save(sim_02_ecosystem_final, file = "D:/Mes Donnees/TROLL/simulations/sim_02_ecosystem_final.Rdata")


## Graphique des réplicats

my3cols <- c("#FC4E07", "cornflowerblue", "mediumaquamarine","gold1", "orchid", "red3", "green3", "violetred2", "dodgerblue","coral2"  )

a <- 
  ggplot(sim_02_ecosystem_final, aes(iter / 12, agb, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_manual(values = my3cols) 
a

b <-  ggplot(sim_02_ecosystem_final, aes(iter / 12, sum1, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 1cm of dbh") +
  scale_color_manual(values = my3cols) 

c <-  ggplot(sim_02_ecosystem_final, aes(iter / 12, sum10, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 10cm of dbh") +
  scale_color_manual(values = my3cols) 

d <- ggplot(sim_02_ecosystem_final, aes(iter / 12, sum30, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 30cm of dbh") +
  scale_color_manual(values = my3cols) 

e <- ggplot(sim_02_func_final, aes(iter / 12, nbsp, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Specific richness") +
  scale_color_manual(values = my3cols) 
e

f <- ggplot(sim_02_func_final, aes(iter / 12, FRic, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Functional richness") +
  scale_color_manual(values = my3cols) 
f

g <- ggplot(sim_02_func_final, aes(iter / 12, FEve, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Functional eveness") +
  scale_color_manual(values = my3cols) 
g

h <- ggplot(sim_02_func_final, aes(iter / 12, RaoQ, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Indice de Rao") +
  scale_color_manual(values = my3cols) 
h

figure <- ggarrange(a, b, c, d, e, f, g , h,
                    labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                    ncol = 2, nrow = 4, 
                    common.legend = TRUE, legend = "bottom")

annotate_figure(figure, top = text_grob("Variation des paramètres de l'écosystème entre les réplicas de la simulation 2", face = "bold", size = 16))



###### 4) Dispersion (D) ######

##### Simulation unique #####

sim_03 <- troll(
  name = "test",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 0, Rrecruit = 0, distdisperse = 1, torus = 1
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)
sim_03
save(sim_03, file = "D:/Mes Donnees/TROLL/simulations/sim_03_disperse.Rdata")

g <- sim_03@ecosystem %>% 
  gather(variable, value, -iter) %>% 
  filter(variable %in% c("sum1", "sum10", "agb")) %>% 
  ggplot(aes(iter/12, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") + 
  theme_bw()+
  labs(title = "sim_03 (fecundity = 0, Rrecruit = 0, distdisperse = 1, torus = 1)")
g
ggsave("D:/Mes Donnees/TROLL/simulations/sim_03_t.png", g, width = 10, height = 10, dpi = 300, bg = "white")

sim_03_nul <- troll(
  name = "sim_03_rain50000",
  global = update_parameters(sim_03, nbiter = 12 * 600, Cseedrain = 50000),
  species = sim_03@inputs$species,
  climate = sim_03@inputs$climate,
  daily = sim_03@inputs$daily,
  forest = get_forest(sim_03),
  verbose = T
)
save(sim_03_nul, file = "D:/Mes Donnees/TROLL/simulations/sim_03_seedrain50000.Rdata")

sim_03_bis <- troll(
  name = "sim_03_rain17",
  global = update_parameters(sim_03, nbiter = 12 * 600, Cseedrain = 17),
  species = sim_03@inputs$species,
  climate = sim_03@inputs$climate,
  daily = sim_03@inputs$daily,
  forest = get_forest(sim_03),
  verbose = T
)
save(sim_03_bis, file = "D:/Mes Donnees/TROLL/simulations/sim_03_seedrain17.Rdata")

sim_03_tris <- troll(
  name = "sim_03_rain0",
  global = update_parameters(sim_03, nbiter = 12 * 600, Cseedrain = 0),
  species = sim_03@inputs$species,
  climate = sim_03@inputs$climate,
  daily = sim_03@inputs$daily,
  forest = get_forest(sim_03),
  verbose = T
)
save(sim_03_tris, file = "D:/Mes Donnees/TROLL/simulations/sim_03_seedrain0.Rdata")

# richesse spécifique :
R_03 <- filter(sim_03_tris@forest, sim_03_tris@forest$iter==7200)
R_03 <- unique(R_03$s_name)
length(R_03)
# richesse spé : 117


# Graphique de validation de la simulation :

sim_03_nul@ecosystem$iter <- sim_03_nul@ecosystem$iter + max(sim_03@ecosystem$iter)
sim_03_tris@ecosystem$iter <- sim_03_tris@ecosystem$iter + max(sim_03@ecosystem$iter)

my3cols <- c("#FC4E07", "cornflowerblue","gold1" )

a <- list(
  "initial forest" = sim_03@ecosystem,
  "seed rain = 50000" = sim_03_nul@ecosystem,
  "seed rain = 0" = sim_03_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, agb, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_manual(values = my3cols) 

b <- list(
  "initial forest" = sim_03@ecosystem,
  "seed rain = 50000" = sim_03_nul@ecosystem,
  "seed rain = 0" = sim_03_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum1, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 1cm of dbh") +
  scale_color_manual(values = my3cols) 

c <- list(
  "initial forest" = sim_03@ecosystem,
  "seed rain = 50000" = sim_03_nul@ecosystem,
  "seed rain = 0" = sim_03_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum10, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 10cm of dbh") +
  scale_color_manual(values = my3cols) 

d <- list(
  "initial forest" = sim_03@ecosystem,
  "seed rain = 50000" = sim_03_nul@ecosystem,
  "seed rain = 0" = sim_03_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum30, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 30cm of dbh") +
  scale_color_manual(values = my3cols) 

# Combiner les graphiques sur une page
figure_03 <- ggarrange(a, b, c, d,
                       labels = c("A", "B", "C", "D"),
                       ncol = 2, nrow = 2, 
                       common.legend = TRUE, legend = "bottom")
figure_03

annotate_figure(figure_03,
                top = text_grob("Simulation 03 avec variation de la seed rain", face = "bold", size = 16))

# pas de réplicats ici car on a pas vu d'effet de la dispersion sur notre communauté 
# hypothèse : parcelle trop petite + torus donc les graines sont le tour de la parcelle jusqu'à tombé à un endroit 
# (ce qui peut s'apparenté finalement à une forme de dispersion aléatoire)
# à tester avec des parcelles beaucoup plus grandes ou en restreignant fictivement la distance de dispersion en fonction de la taille des graines


####### III) Simulations avec plusieurs paramètres #######


###### 5) Fécondité + recrutement (F+R) ######

##### A) Simulation unique #####

sim_04 <- troll(
  name = "sim_04_fecundity_recruit",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 1, Rrecruit = 1, distdisperse = 0, torus = 1
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)
sim_04
save(sim_04, file = "D:/Mes Donnees/TROLL/simulations/sim_04_fecundity_recruit.Rdata")

g <- sim_04@ecosystem %>% 
  gather(variable, value, -iter) %>% 
  filter(variable %in% c("sum1", "sum10", "agb")) %>% 
  ggplot(aes(iter/12, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") + 
  theme_bw()+
  labs(title = "sim_04 (fecundity = 1, Rrecruit = 1, distdisperse = 0, torus = 1)")
g
ggsave("D:/Mes Donnees/TROLL/simulations/sim_04_t.png", g, width = 10, height = 10, dpi = 300, bg = "white")

sim_04_nul <- troll(
  name = "sim_04_rain50000",
  global = update_parameters(sim_04, nbiter = 12 * 600, Cseedrain = 50000),
  species = sim_04@inputs$species,
  climate = sim_04@inputs$climate,
  daily = sim_04@inputs$daily,
  forest = get_forest(sim_04),
  verbose = T
)
save(sim_04_nul, file = "D:/Mes Donnees/TROLL/simulations/sim_04_seedrain50000.Rdata")

sim_04_bis <- troll(
  name = "sim_04_rain17",
  global = update_parameters(sim_04, nbiter = 12 * 600, Cseedrain = 17),
  species = sim_04@inputs$species,
  climate = sim_04@inputs$climate,
  daily = sim_04@inputs$daily,
  forest = get_forest(sim_04),
  verbose = T
)
save(sim_04_bis, file = "D:/Mes Donnees/TROLL/simulations/sim_04_seedrain17.Rdata")

sim_04_tris <- troll(
  name = "sim_04_rain0",
  global = update_parameters(sim_04, nbiter = 12 * 600, Cseedrain = 0),
  species = sim_04@inputs$species,
  climate = sim_04@inputs$climate,
  daily = sim_04@inputs$daily,
  forest = get_forest(sim_04),
  verbose = T
)
save(sim_04_tris, file = "D:/Mes Donnees/TROLL/simulations/sim_04_seedrain0.Rdata")


##### B) Réplicats #####

# avec pluie de graines (50 000)
simulation <- paste0("sim_04_",1:6)
year <- 600
parameters <- data.frame()

for (sim in simulation) { 
  par_00 <- generate_parameters (cols = 250, rows = 250, nbiter = 12 * year, fecundity = 1, Rrecruit = 1, distdisperse = 0, torus = 1, nbs0 = 2250, NONRANDOM = 2)
  par_00$simulation <- sim
  
  parameters <- rbind(parameters, par_00)
  
}

stack_sim04 <- stack(
  name = "stack_sim04",
  simulations = simulation,
  global = parameters,
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = T,
  cores = 6,
  overwrite = T,
  path = "D:/Mes Programmes/rcontroll"
)

save(stack_sim04, file = "D:/Mes Donnees/TROLL/simulations/stack_sim_04.Rdata")


simulation <- paste0("sim_04_",7:10)
year <- 600
parameters <- data.frame()

for (sim in simulation) { 
  par_01 <- generate_parameters(cols = 250, rows = 250, nbiter = 12 * year, fecundity = 1, Rrecruit = 1, distdisperse = 0, torus = 1, NONRANDOM = 2, nbs0 = 2250)
  par_01$simulation <- sim
  
  parameters <- rbind(parameters, par_01)
  
}

stack_sim04_bis <- stack(
  name = "stack_sim04_bis",
  simulations = simulation,
  global = parameters,
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = T,
  cores = 4,
  overwrite = T,
  path = "D:/Mes Programmes/rcontroll"
)

save(stack_sim04_bis, file = "D:/Mes Donnees/TROLL/simulations/stack_sim_04_bis.Rdata")

# sans pluie de graines (0)
simulation <- paste0("sim_04_",1:6)
stack_sim_04_seedrain0 <- stack(
  name = "stack_sim_04_seedrain0",
  simulations = simulation,
  global = update_parameters(stack_sim04, nbiter = 12 * 600, fecundity = 1, Rrecruit = 1, distdisperse = 0, torus = 1, Cseedrain = 0, nbs0 = 2250), 
  species =  stack_sim04@inputs$species,
  climate = stack_sim04@inputs$climate,
  daily = stack_sim04@inputs$daily,
  forest = get_forest(stack_sim04),
  verbose = T,
  cores = 6, 
  overwrite = T,
  path = "D:/Mes Programmes/rcontroll")

save(stack_sim_04_seedrain0, file = "D:/Mes Donnees/TROLL/simulations/stack_sim_04_seedrain0.Rdata")

simulation <- paste0("sim_04_",7:10)
stack_sim_04_bis_seedrain0 <- stack(
  name = "stack_sim_04_bis_seedrain0",
  simulations = simulation,
  global = update_parameters(stack_sim04_bis, nbiter = 12 * 600, fecundity = 1, Rrecruit = 1, distdisperse = 0, torus = 1, Cseedrain = 0, nbs0 = 2250), 
  species =  stack_sim04_bis@inputs$species,
  climate = stack_sim04_bis@inputs$climate,
  daily = stack_sim04_bis@inputs$daily,
  forest = get_forest(stack_sim04_bis),
  verbose = T,
  cores = 4, 
  overwrite = T,
  path = "D:/Mes Programmes/rcontroll")

save(stack_sim_04_bis_seedrain0, file = "D:/Mes Donnees/TROLL/simulations/stack_sim_04_bis_seedrain0.Rdata")


# richesse spécifique :
R_04 <- filter(sim_04_tris@forest, sim_04_tris@forest$iter==7200)
R_04 <- unique(R_04$s_name)
length(R_04)
# richesse spé : 93

# diversité fonctionnelle : 
sim_04_functional <- left_join(stack_sim04@inputs$species, 
                                             filter(stack_sim04@species, iter %in% c(seq(from = 0, to = 7199, by = 12))),
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

save(sim_04_functional, file = "D:/Mes Donnees/TROLL/simulations/sim_04_functional.Rdata")

sim_04_bis_seedrain0_functional <- left_join(stack_sim_04_bis_seedrain0@inputs$species, 
                               filter(stack_sim_04_bis_seedrain0@species, iter %in% c(seq(from = 0, to = 7199, by = 12))),
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

save(sim_04_bis_seedrain0_functional, file = "D:/Mes Donnees/TROLL/simulations/sim_04_bis_seedrain0_functional.Rdata")

# pas les codes pour la diversité fonctionnelle de tous les stacks
# téléchargé les Rdata que j'ai enregistré


# avec pluie de graines :
sim_04_func <- bind_rows(sim_04_functional, sim_04_bis_functional)
# sans pluie de graines :
sim_04_seedrain0_func <- bind_rows(sim_04_seedrain0_functional, sim_04_bis_seedrain0_functional)

# data global : 
sim_04_func$iter <- as.numeric(sim_04_func$iter)
sim_04_seedrain0_func$iter <- as.numeric(sim_04_seedrain0_func$iter)

sim_04_seedrain0_func$iter <- sim_04_seedrain0_func$iter + max(sim_04_func$iter)
sim_04_func_final <- bind_rows(sim_04_func, sim_04_seedrain0_func)
save(sim_04_func_final, file = "D:/Mes Donnees/TROLL/simulations/sim_04_func_final.Rdata")

# data global ecosystem :
sim_04_ecosystem <- bind_rows(stack_sim04@ecosystem, stack_sim04_bis@ecosystem)
sim_04_seedrain0_ecosystem <- bind_rows(stack_sim_04_seedrain0@ecosystem, stack_sim_04_bis_seedrain0@ecosystem)

sim_04_seedrain0_ecosystem$iter <- sim_04_seedrain0_ecosystem$iter + max(sim_04_ecosystem$iter)
sim_04_ecosystem_final <- bind_rows(sim_04_ecosystem, sim_04_seedrain0_ecosystem)
save(sim_04_ecosystem_final, file = "D:/Mes Donnees/TROLL/simulations/sim_04_ecosystem_final.Rdata")

#
# graphique de validation des réplicats :

my3cols <- c("#FC4E07", "cornflowerblue", "mediumaquamarine","gold1", "orchid", "red3", "green3", "violetred2", "dodgerblue","coral2"  )

a <- 
  ggplot(sim_04_ecosystem_final, aes(iter / 12, agb, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_manual(values = my3cols) 
a

b <-  ggplot(sim_04_ecosystem_final, aes(iter / 12, sum1, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 1cm of dbh") +
  scale_color_manual(values = my3cols) 

c <-  ggplot(sim_04_ecosystem_final, aes(iter / 12, sum10, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 10cm of dbh") +
  scale_color_manual(values = my3cols) 

d <- ggplot(sim_04_ecosystem_final, aes(iter / 12, sum30, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 30cm of dbh") +
  scale_color_manual(values = my3cols) 

e <- ggplot(sim_04_func_final, aes(iter / 12, nbsp, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Specific richness") +
  scale_color_manual(values = my3cols) 
e

f <- ggplot(sim_04_func_final, aes(iter / 12, FRic, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Functional richness") +
  scale_color_manual(values = my3cols) 
f

g <- ggplot(sim_04_func_final, aes(iter / 12, FEve, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Functional eveness") +
  scale_color_manual(values = my3cols) 
g

h <- ggplot(sim_04_func_final, aes(iter / 12, RaoQ, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  xlab("Time (years)") +
  ylab("Indice de Rao") +
  scale_color_manual(values = my3cols) 
h

figure <- ggarrange(a, b, c, d, e, f, g , h,
                    labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                    ncol = 2, nrow = 4, 
                    common.legend = TRUE, legend = "bottom")

annotate_figure(figure, top = text_grob("Variation des paramètres de l'écosystème entre les réplicas de la simulation 4", face = "bold", size = 16))



###### 6) Fécondité + dispersion (F+D) ######

##### Simulation unique ######

sim_05 <- troll(
  name = "sim_05_fecundity_disperse",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 1, Rrecruit = 0, distdisperse = 1, torus = 1
  ),
  species = species_smass,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)
sim_05
save(sim_05, file = "D:/Mes Donnees/TROLL/simulations/sim_05_fecundity_disperse.Rdata")

g <- sim_05@ecosystem %>% 
  gather(variable, value, -iter) %>% 
  filter(variable %in% c("sum1", "sum10", "agb")) %>% 
  ggplot(aes(iter/12, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") + 
  theme_bw()+
  labs(title = "sim_05 (fecundity = 1, Rrecruit = 0, distdisperse = 1, torus = 1)")
g
ggsave("D:/Mes Donnees/TROLL/simulations/sim_05_t.png", g, width = 10, height = 10, dpi = 300, bg = "white")

sim_05_nul <- troll(
  name = "sim_05_rain50000",
  global = update_parameters(sim_05, nbiter = 12 * 600, Cseedrain = 50000),
  species = sim_05@inputs$species,
  climate = sim_05@inputs$climate,
  daily = sim_05@inputs$daily,
  forest = get_forest(sim_05),
  verbose = T
)
save(sim_05_nul, file = "D:/Mes Donnees/TROLL/simulations/sim_05_seedrain50000.Rdata")

sim_05_bis <- troll(
  name = "sim_05_rain17",
  global = update_parameters(sim_05, nbiter = 12 * 600, Cseedrain = 17),
  species = sim_05@inputs$species,
  climate = sim_05@inputs$climate,
  daily = sim_05@inputs$daily,
  forest = get_forest(sim_05),
  verbose = T
)
save(sim_05_bis, file = "D:/Mes Donnees/TROLL/simulations/sim_05_seedrain17.Rdata")

sim_05_tris <- troll(
  name = "sim_05_rain0",
  global = update_parameters(sim_05, nbiter = 12 * 600, Cseedrain = 0),
  species = sim_05@inputs$species,
  climate = sim_05@inputs$climate,
  daily = sim_05@inputs$daily,
  forest = get_forest(sim_05),
  verbose = T
)
save(sim_05_tris, file = "D:/Mes Donnees/TROLL/simulations/sim_05_seedrain0.Rdata")

# richesse spécifique :
R_05 <- filter(sim_05_tris@forest, sim_05_tris@forest$iter==7200)
R_05 <- unique(R_05$s_name)
length(R_05)
# richesse spé : 79

sim_05_nul@ecosystem$iter <- sim_05_nul@ecosystem$iter + max(sim_05@ecosystem$iter)
sim_05_tris@ecosystem$iter <- sim_05_tris@ecosystem$iter + max(sim_05@ecosystem$iter)

my3cols <- c("#FC4E07", "cornflowerblue","gold1" )

a <- list(
  "initial forest" = sim_05@ecosystem,
  "seed rain = 50000" = sim_05_nul@ecosystem,
  "seed rain = 0" = sim_05_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, agb, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_manual(values = my3cols) 

b <- list(
  "initial forest" = sim_05@ecosystem,
  "seed rain = 50000" = sim_05_nul@ecosystem,
  "seed rain = 0" = sim_05_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum1, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 1cm of dbh") +
  scale_color_manual(values = my3cols) 

c <- list(
  "initial forest" = sim_05@ecosystem,
  "seed rain = 50000" = sim_05_nul@ecosystem,
  "seed rain = 0" = sim_05_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum10, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 10cm of dbh") +
  scale_color_manual(values = my3cols) 

d <- list(
  "initial forest" = sim_05@ecosystem,
  "seed rain = 50000" = sim_05_nul@ecosystem,
  "seed rain = 0" = sim_05_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum30, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 30cm of dbh") +
  scale_color_manual(values = my3cols) 

# Combiner les graphiques sur une page
figure_05 <- ggarrange(a, b, c, d,
                       labels = c("A", "B", "C", "D"),
                       ncol = 2, nrow = 2, 
                       common.legend = TRUE, legend = "bottom")
figure_05

annotate_figure(figure_05,
                top = text_grob("Simulation 05 avec variation de la seed rain", face = "bold", size = 16))

# pas de réplicats


###### 7) Recrutement + dispersion (R+D) ######

##### Simulation unique #####

sim_06 <- troll(
  name = "sim_06_recruit_disperse",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 0, Rrecruit = 1, distdisperse = 1, torus = 1
  ),
  species = species_smass,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)
sim_06
save(sim_06, file = "D:/Mes Donnees/TROLL/simulations/sim_06_recruit_disperse.Rdata")

g <- sim_06@ecosystem %>% 
  gather(variable, value, -iter) %>% 
  filter(variable %in% c("sum1", "sum10", "agb")) %>% 
  ggplot(aes(iter/12, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") + 
  theme_bw()+
  labs(title = "sim_06 (fecundity = 0, Rrecruit = 1, distdisperse = 1, torus = 1)")
g
ggsave("D:/Mes Donnees/TROLL/simulations/sim_06_t.png", g, width = 10, height = 10, dpi = 300, bg = "white")

sim_06_nul <- troll(
  name = "sim_06_rain50000",
  global = update_parameters(sim_06, nbiter = 12 * 600, Cseedrain = 50000),
  species = sim_06@inputs$species,
  climate = sim_06@inputs$climate,
  daily = sim_06@inputs$daily,
  forest = get_forest(sim_06),
  verbose = T
)
save(sim_06_nul, file = "D:/Mes Donnees/TROLL/simulations/sim_06_seedrain50000.Rdata")

sim_06_bis <- troll(
  name = "sim_06_rain17",
  global = update_parameters(sim_06, nbiter = 12 * 600, Cseedrain = 17),
  species = sim_06@inputs$species,
  climate = sim_06@inputs$climate,
  daily = sim_06@inputs$daily,
  forest = get_forest(sim_06),
  verbose = T
)
save(sim_06_bis, file = "D:/Mes Donnees/TROLL/simulations/sim_06_seedrain17.Rdata")

sim_06_tris <- troll(
  name = "sim_06_rain0",
  global = update_parameters(sim_06, nbiter = 12 * 600, Cseedrain = 0),
  species = sim_06@inputs$species,
  climate = sim_06@inputs$climate,
  daily = sim_06@inputs$daily,
  forest = get_forest(sim_06),
  verbose = T
)
save(sim_06_tris, file = "D:/Mes Donnees/TROLL/simulations/sim_06_seedrain0.Rdata")

# richesse spécifique :
R_06 <- filter(sim_06_tris@forest, sim_06_tris@forest$iter==7200)
R_06 <- unique(R_06$s_name)
length(R_06)
# richesse spé : 81

sim_06_nul@ecosystem$iter <- sim_06_nul@ecosystem$iter + max(sim_06@ecosystem$iter)
sim_06_tris@ecosystem$iter <- sim_06_tris@ecosystem$iter + max(sim_06@ecosystem$iter)

my3cols <- c("#FC4E07", "cornflowerblue","gold1" )

a <- list(
  "initial forest" = sim_06@ecosystem,
  "seed rain = 50000" = sim_06_nul@ecosystem,
  "seed rain = 0" = sim_06_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, agb, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_manual(values = my3cols) 

b <- list(
  "initial forest" = sim_06@ecosystem,
  "seed rain = 50000" = sim_06_nul@ecosystem,
  "seed rain = 0" = sim_06_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum1, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 1cm of dbh") +
  scale_color_manual(values = my3cols) 

c <- list(
  "initial forest" = sim_06@ecosystem,
  "seed rain = 50000" = sim_06_nul@ecosystem,
  "seed rain = 0" = sim_06_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum10, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 10cm of dbh") +
  scale_color_manual(values = my3cols) 

d <- list(
  "initial forest" = sim_06@ecosystem,
  "seed rain = 50000" = sim_06_nul@ecosystem,
  "seed rain = 0" = sim_06_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum30, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 30cm of dbh") +
  scale_color_manual(values = my3cols) 

# Combiner les graphiques sur une page
figure_06 <- ggarrange(a, b, c, d,
                       labels = c("A", "B", "C", "D"),
                       ncol = 2, nrow = 2, 
                       common.legend = TRUE, legend = "bottom")
figure_06

annotate_figure(figure_06,
                top = text_grob("Simulation 06 avec variation de la seed rain", face = "bold", size = 16))

# pas de réplicats


###### 8) Fécondité + recrutement + dispersion (F+R+D) ######

##### Simulation unique  #####

sim_07 <- troll(
  name = "sim_07_fec_recruit_disperse",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 1, Rrecruit = 1, distdisperse = 1, torus = 1
  ),
  species = species_smass,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)
sim_07
save(sim_07, file = "D:/Mes Donnees/TROLL/simulations/sim_07_fec_recruit_disperse.Rdata")

g <- sim_07@ecosystem %>% 
  gather(variable, value, -iter) %>% 
  filter(variable %in% c("sum1", "sum10", "agb")) %>% 
  ggplot(aes(iter/12, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") + 
  theme_bw()+
  labs(title = "sim_07 (fecundity = 1, Rrecruit = 1, distdisperse = 1, torus = 1)")
g
ggsave("D:/Mes Donnees/TROLL/simulations/sim_07_t.png", g, width = 10, height = 10, dpi = 300, bg = "white")

sim_07_nul <- troll(
  name = "sim_07_rain50000",
  global = update_parameters(sim_07, nbiter = 12 * 600, Cseedrain = 50000),
  species = sim_07@inputs$species,
  climate = sim_07@inputs$climate,
  daily = sim_07@inputs$daily,
  forest = get_forest(sim_07),
  verbose = T
)
save(sim_07_nul, file = "D:/Mes Donnees/TROLL/simulations/sim_07_seedrain50000.Rdata")

sim_07_bis <- troll(
  name = "sim_07_rain17",
  global = update_parameters(sim_07, nbiter = 12 * 600, Cseedrain = 17),
  species = sim_07@inputs$species,
  climate = sim_07@inputs$climate,
  daily = sim_07@inputs$daily,
  forest = get_forest(sim_07),
  verbose = T
)
save(sim_07_bis, file = "D:/Mes Donnees/TROLL/simulations/sim_07_seedrain17.Rdata")

sim_07_tris <- troll(
  name = "sim_07_rain0",
  global = update_parameters(sim_07, nbiter = 12 * 600, Cseedrain = 0),
  species = sim_07@inputs$species,
  climate = sim_07@inputs$climate,
  daily = sim_07@inputs$daily,
  forest = get_forest(sim_07),
  verbose = T
)
save(sim_07_tris, file = "D:/Mes Donnees/TROLL/simulations/sim_07_seedrain0.Rdata")

# richesse spécifique :
R_07 <- filter(sim_07_tris@forest, sim_07_tris@forest$iter==7200)
R_07 <- unique(R_07$s_name)
length(R_07)
# richesse spé : 96

sim_07_nul@ecosystem$iter <- sim_07_nul@ecosystem$iter + max(sim_07@ecosystem$iter)
sim_07_tris@ecosystem$iter <- sim_07_tris@ecosystem$iter + max(sim_07@ecosystem$iter)

my3cols <- c("#FC4E07", "cornflowerblue","gold1" )

a <- list(
  "initial forest" = sim_07@ecosystem,
  "seed rain = 50000" = sim_07_nul@ecosystem,
  "seed rain = 0" = sim_07_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, agb, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_manual(values = my3cols) 

b <- list(
  "initial forest" = sim_07@ecosystem,
  "seed rain = 50000" = sim_07_nul@ecosystem,
  "seed rain = 0" = sim_07_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum1, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 1cm of dbh") +
  scale_color_manual(values = my3cols) 

c <- list(
  "initial forest" = sim_07@ecosystem,
  "seed rain = 50000" = sim_07_nul@ecosystem,
  "seed rain = 0" = sim_07_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum10, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 10cm of dbh") +
  scale_color_manual(values = my3cols) 

d <- list(
  "initial forest" = sim_07@ecosystem,
  "seed rain = 50000" = sim_07_nul@ecosystem,
  "seed rain = 0" = sim_07_tris@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum30, col = simulation)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 30cm of dbh") +
  scale_color_manual(values = my3cols) 

# Combiner les graphiques sur une page
figure_07 <- ggarrange(a, b, c, d,
                       labels = c("A", "B", "C", "D"),
                       ncol = 2, nrow = 2, 
                       common.legend = TRUE, legend = "bottom")
figure_07

annotate_figure(figure_07,
                top = text_grob("Simulation 07 avec variation de la seed rain", face = "bold", size = 16))


##### Test taille de parcelle #####

#### 6,25 hectares ####
# on télécharge la simulation précédente à 6,25 hectares
load("~/TROLL/simulations/H1_simulations/sim_07_fec_recruit_disperse.Rdata")
load("~/TROLL/simulations/H1_simulations/sim_07_seedrain0.Rdata")

# calcul de la richesse spécifique :
# avec pluie de graines :
data_test <- sim_07@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_07@ecosystem <- left_join(sim_07@ecosystem, mydata)

# sans pluie de graines :
data_test <- sim_07_tris@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_07_tris@ecosystem <- left_join(sim_07_tris@ecosystem, mydata)

ggplot(sim_07_tris@ecosystem, aes(iter / 12, r_sp)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Richesse spécifique")


# on fait des simulations en augmentant la taille de la parcelle : 

#### 25 hectares ####

sim_07_x2 <- troll(
  name = "sim_07_500x500",
  global = generate_parameters(
    cols = 500, rows = 500,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 1, Rrecruit = 1, distdisperse = 1, torus = 1
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)

save(sim_07_x2, file = "D:/Mes Donnees/TROLL/simulations/sim_07_x2.Rdata")

sim_07_x2_seedrain0 <- troll(
  name = "sim_07x2_seedrain0",
  global = update_parameters(sim_07_x2, nbiter = 12 * 600, Cseedrain = 0),
  species = sim_07_x2@inputs$species,
  climate = sim_07_x2@inputs$climate,
  daily = sim_07_x2@inputs$daily,
  forest = get_forest(sim_07_x2),
  verbose = T
)
save(sim_07_x2_seedrain0, file = "D:/Mes Donnees/TROLL/simulations/sim_07_x2_seedrain0.Rdata")

# calcul de la richesse spécifique :

data_test <- sim_07_x2@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_07_x2@ecosystem <- left_join(sim_07_x2@ecosystem, mydata)

data_test <- sim_07_x2_seedrain0@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_07_x2_seedrain0@ecosystem <- left_join(sim_07_x2_seedrain0@ecosystem, mydata)

ggplot(sim_07_x2_seedrain0@ecosystem, aes(iter / 12, r_sp)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Richesse spécifique")


#### 100 hectares ####

sim_07_x4 <- troll(
  name = "sim_07_1000x1000",
  global = generate_parameters(
    cols = 1000, rows = 1000,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 1, Rrecruit = 1, distdisperse = 1, torus = 1
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)

save(sim_07_x4, file = "D:/Mes Donnees/TROLL/simulations/sim_07_x4.Rdata")

sim_07_x4_seedrain0 <- troll(
  name = "sim_07x4_seedrain0",
  global = update_parameters(sim_07_x4, nbiter = 12 * 600, Cseedrain = 0),
  species = sim_07_x4@inputs$species,
  climate = sim_07_x4@inputs$climate,
  daily = sim_07_x4@inputs$daily,
  forest = get_forest(sim_07_x4),
  verbose = T
)
save(sim_07_x4_seedrain0, file = "D:/Mes Donnees/TROLL/simulations/sim_07_x4_seedrain0.Rdata")

# calcul de la richesse spécifique :
data_test <- sim_07_x4@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_07_x4@ecosystem <- left_join(sim_07_x4@ecosystem, mydata)


data_test <- sim_07_x4_seedrain0@species
data_test$iter <- as.factor(data_test$iter)
data_test$abond <- data_test$sum1

data_test <- data_test[-which(data_test$abond == 0), ]
which(data_test$abond == 0)

test <- tapply(data_test$species,data_test$iter, length)
test
iter <- c(0:7199)

mydata <- data.frame(r_sp = test, iter)

sim_07_x4_seedrain0@ecosystem <- left_join(sim_07_x4_seedrain0@ecosystem, mydata)

ggplot(sim_07_x4_seedrain0@ecosystem, aes(iter / 12, r_sp)) +
  geom_point(size = 1) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Richesse spécifique")


# on fait un graphique global avec les 3 tailles de parcelles :

# mise à jour des noms des itérations :
sim_07_tris@ecosystem$iter <- sim_07_tris@ecosystem$iter + max(sim_07@ecosystem$iter)
sim_07_x2_seedrain0@ecosystem$iter <- sim_07_x2_seedrain0@ecosystem$iter + max(sim_07_x2@ecosystem$iter)
sim_07_x4_seedrain0@ecosystem$iter <- sim_07_x4_seedrain0@ecosystem$iter + max(sim_07_x4@ecosystem$iter)

# graphique :
my3cols <- c("red4","springgreen4","red2", "green3","lightsalmon", "palegreen")

a <- list(
  "6,25ha F+R+D" = sim_07_tris@ecosystem,
  "25ha F+R+D" = sim_07_x2_seedrain0@ecosystem,
  "100ha F+R+D" = sim_07_x4_seedrain0@ecosystem, 
  "6,25ha F+R+D" = sim_07@ecosystem,
  "25ha F+R+D" = sim_07_x2@ecosystem,
  "100ha F+R+D" = sim_07_x4@ecosystem, 
  "6,25ha C" = sim_00_tris@ecosystem,
  "25ha C" = sim_00_x2_seedrain0@ecosystem,
  "100ha C" = sim_00_x4_seedrain0@ecosystem, 
  "6,25ha C" = sim_00@ecosystem,
  "25ha C" = sim_00_x2@ecosystem,
  "100ha C" = sim_00_x4@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, r_sp, col = simulation)) +
  geom_point(size = 2) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression("Richesse spécifique")) +
  scale_color_manual(values = my3cols) +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 15), legend.text = element_text(size = 15))+
  ggtitle("Richesse spécifique dans des parcelles de tailles différentes pour les scénarios F+R+D et C")



####### IV) Test réplicats #######

# Faire des réplicats en faisant varier l'origine de la graine de départ de la simulation

# NONRANDOM = 1
sim_07_B <- troll(
  name = "sim_07_replic_testB",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 1, Rrecruit = 1, distdisperse = 1, torus = 1
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)
sim_07_B
save(sim_07_B, file = "D:/Mes Donnees/TROLL/simulations/sim_07_B.Rdata")

# NONRANDOM = 0 (donc fixe la graine aléatoirement (voir en détail dans le fichier Rcpp))
sim_07_C <- troll(
  name = "sim_07_replic_testC",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 1, Rrecruit = 1, distdisperse = 1, torus = 1, NONRANDOM = 0
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)
sim_07_C
save(sim_07_C, file = "D:/Mes Donnees/TROLL/simulations/sim_07_C.Rdata")

sim_07_D <- troll(
  name = "sim_07_replic_testD",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 600, fecundity = 1, Rrecruit = 1, distdisperse = 1, torus = 1, NONRANDOM = 0
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)
sim_07_D
save(sim_07_D, file = "D:/Mes Donnees/TROLL/simulations/sim_07_D.Rdata")

# Comparaison des simulations selon si la graine de déârt est fixée ou non

my3cols <- c("#FC4E07", "cornflowerblue","gold1", "orchid2" )

a <- list(
  "fixed 1" = sim_07@ecosystem,
  "fixed 2" = sim_07_B@ecosystem,
  "random 1" = sim_07_C@ecosystem,
  "random 2" = sim_07_D@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, agb, col = simulation)) +
  geom_point(size = 1, aes(shape = simulation)) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_manual(values = my3cols) 
a

b <- list(
  "fixed 1" = sim_07@ecosystem,
  "fixed 2" = sim_07_B@ecosystem,
  "random 1" = sim_07_C@ecosystem,
  "random 2" = sim_07_D@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum1, col = simulation)) +
  geom_point(size = 1, aes(shape = simulation)) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 1cm of dbh") +
  scale_color_manual(values = my3cols) 

c <- list(
  "fixed 1" = sim_07@ecosystem,
  "fixed 2" = sim_07_B@ecosystem,
  "random 1" = sim_07_C@ecosystem,
  "random 2" = sim_07_D@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum10, col = simulation)) +
  geom_point(size = 1, aes(shape = simulation)) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 10cm of dbh") +
  scale_color_manual(values = my3cols) 

d <- list(
  "fixed 1" = sim_07@ecosystem,
  "fixed 2" = sim_07_B@ecosystem,
  "random 1" = sim_07_C@ecosystem,
  "random 2" = sim_07_D@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, sum30, col = simulation)) +
  geom_point(size = 1, aes(shape = simulation)) +
  geom_line()+
  theme_bw() +
  xlab("Time (years)") +
  ylab("Number of individuals > 30cm of dbh") +
  scale_color_manual(values = my3cols) 

# Combiner les graphiques sur une page
figure <- ggarrange(a, b, c, d,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "bottom")
figure

annotate_figure(figure,
                top = text_grob("Comparaison de la simulation 07 avec différentes ''graines de départ''", face = "bold", size = 16))



####### V) Comparaison des scénarios #######

# analyse préliminaire pour voir s'il y avait des différences 
# entre les scénarios à partir des simulations uniques

##### Strcuture de la forêt ######

# on donne un nom de simulation 

data1 <- sim_00@ecosystem
data1$simulation <- "sim_00"
data2 <- sim_01@ecosystem
data2$simulation <- "sim_01"
data3 <- sim_02@ecosystem
data3$simulation <- "sim_02"
data4 <- sim_03@ecosystem
data4$simulation <- "sim_03"
data5 <- sim_04@ecosystem
data5$simulation <- "sim_04"
data6 <- sim_05@ecosystem
data6$simulation <- "sim_05"
data7 <- sim_06@ecosystem
data7$simulation <- "sim_06"
data8 <- sim_07@ecosystem
data8$simulation <- "sim_07"

# on combine tous les scénarios dans un même data frame
data <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8)

# graphique :
data %>% 
  gather(variable, value, -iter, -simulation) %>% 
  filter(variable %in% c("sum1", "sum10", "agb")) %>% 
  ggplot(aes(iter/12, value, col = simulation)) +
  geom_line() +
  geom_point(size = 0.5) +
  facet_wrap(~variable, scales = "free_y") + 
  theme_bw()+
  labs(title = "comparaison des simulations")



# Compraison des scénarios  

simulations <- c("initial", "fecundity", "recruitment", "dispersal", "fec + recruit", "fec + disp", "recruit + disp", "fec + recruit + disp")
sum_30 <- c(104.16, 121.76, 104.16, 104.96, 103.68, 112.80, 109.92, 112.80)
data <- data.frame(simulations, sum_30)
data$simulations <- fct_inorder(data$simulations)

ggplot(data, aes(x = simulations, y = sum_30, fill = simulations))+
  geom_col()+
  theme(axis.text.x = element_text(angle=45, size = 15, colour = "black"), axis.text.y = element_text(size = 15),  legend.position = "none"
        , axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20) )+
  ylab("Nombre d'individus dbh > 30cm") +
  xlab("Simulations") +
  labs(title = "NB ind > 30cm pour chaque scénarios de simulations réalisés", size = 35)


#### Richesse spécifique ####

simulations <- c("initial", "fecundity", "recruitment", "dispersal", "fec + recruit", "fec + disp", "recruit + disp", "fec + recruit + disp")
richesse_specifique <- c(110, 78, 67, 117, 93, 79, 81, 96)
data <- data.frame(simulations, richesse_specifique)
data$simulations <- fct_inorder(data$simulations)

ggplot(data, aes(x = simulations, y = richesse_specifique, fill = simulations))+
  geom_col()+
  theme(axis.text.x = element_text(angle=45, size = 15, colour = "black"), axis.text.y = element_text(size = 15),  legend.position = "none"
        , axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20) )+
  ylab("Richesse spécifique") +
  xlab("Simulations") +
  labs(title = "Richesse spécifique pour chaque scénarios de simulations réalisés", size = 35)


######## VI) Test torus ########

sim_tor <- troll(
  name = "sim_tor",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 100, fecundity = 1, Rrecruit = 1, distdisperse = 1, torus = 1
  ),
  species = species_smass,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)

sim_tor2 <- troll(
  name = "sim_tor2",
  global = generate_parameters(
    cols = 250, rows = 250,
    iterperyear = 12, nbiter = 12 * 100, fecundity = 1, Rrecruit = 1, distdisperse = 1, torus = 1, Cseedrain = 1000
  ),
  species = species_smass,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE, 
  path =getwd()
)


sim_tor@ecosystem %>% 
  gather(variable, value, -iter) %>% 
  filter(variable %in% c("sum1", "sum10", "agb")) %>% 
  ggplot(aes(iter/12, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") + 
  theme_bw()

sim_tor2@ecosystem %>% 
  gather(variable, value, -iter) %>% 
  filter(variable %in% c("sum1", "sum10", "agb")) %>% 
  ggplot(aes(iter/12, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") + 
  theme_bw()


######## VII) Analyse des fonctions ######## 

# calcul de la valeur de la fécondité, recrutement et distance de dispersion 
# à partir des équations implémentées dans le modèle 

s_seedmass <- sim_07@inputs$species$s_seedmass
s_wsg <- sim_07@inputs$species$s_wsg
s_dbhmax <- sim_07@inputs$species$s_dbhmax
s_LMA <- sim_07@inputs$species$s_LMA

s_recruit_rate <- exp(4.33 + (1.428*(log10(sim_07@inputs$species$s_seedmass/1000))) + (0.098*sim_07@inputs$species$s_wsg) + (-0.000013*sim_07@inputs$species$s_LMA) + (-2.421*(log10(sim_07@inputs$species$s_dbhmax*1000))))/(1+(exp(4.33 + (1.428*log10(sim_07@inputs$species$s_seedmass/1000)) + (0.098*sim_07@inputs$species$s_wsg) + (-0.000013*sim_07@inputs$species$s_LMA) + (-2.421*(log10(sim_07@inputs$species$s_dbhmax*1000))))))

s_fecundity <- (exp(-4.234 + (-1.223*log10(sim_07@inputs$species$s_seedmass/1000)) + (0.108*sim_07@inputs$species$s_wsg) + (-0.0005*sim_07@inputs$species$s_LMA) + (-0.0564*log10(sim_07@inputs$species$s_dbhmax*1000))))
nbs <- s_fecundity*4710000*(s_dbhmax*0.5)^2
median(nbs) # 2254.707 donc on va prendre nbs0 = 2250 dans le modèle


s_DistSynd <- sim_07@inputs$species$s_DispSynd
s_DistSynd[s_DistSynd == "1"] <- "-0.72" 
s_DistSynd[s_DistSynd == "2"] <- "-1.03" 
s_DistSynd[s_DistSynd == "3"] <- "-1.43" 
b = as.numeric(s_DistSynd)
s_maxDD <- 2.19 + b + 0.76 + (-0.32*log10(s_seedmass))

s_ah <- sim_07@inputs$species$s_ah
s_hmax <- sim_07@inputs$species$s_hmax
height <- s_hmax * ((s_dbhmax*0.5)/((s_dbhmax*0.5)*s_ah))
s_ds <- 10^(s_maxDD + 0.60*log10(height))

s_DistSynd2 <- sim_07@inputs$species$s_DispSynd
s_DistSynd2[s_DistSynd2 == "0"] <- "animal" 
s_DistSynd2[s_DistSynd2 == "1"] <- "ant" 
s_DistSynd2[s_DistSynd2 == "2"] <- "wind" 
s_DistSynd2[s_DistSynd2 == "3"] <- "ballistic"
dispersal_syndrome <- as.character(s_DispSynd2)

# je ne sais plus quel code fonctionne donc à tester
dispersal_syndrome <- ifelse(sim_07@inputs$species$s_DispSynd <= 0, "animal",
                ifelse(sim_07@inputs$species$s_DispSynd <= 1, "ant",
                       ifelse(sim_07@inputs$species$s_DispSynd <= 2, "wind",
                              ifelse(sim_07@inputs$species$s_DispSynd <= 3, "ballistic"))))

# on créer un data frame en regroupant toutes les données calculées
data2 <- data.frame(s_recruit_rate, s_fecundity, s_seedmass, s_dbhmax, s_wsg, height, s_LMA, s_maxDD, dispersal_syndrome, s_ds)


plot(s_seedmass, s_dbhmax)


G1<- ggplot(data, aes(x = s_seedmass, y = s_fecundity, col = s_seedmass)) +
  geom_point()
ggsave("s_fecundity-s_seedmass.png", G1, width = 10, height = 10, dpi = 300, bg = "white")

G2<- ggplot(data, aes(x = s_seedmass, y = s_recruit_rate, col = class)) +
  geom_point()
ggsave("s_recruit_rate-s_seedmass.png", G2, width = 10, height = 10, dpi = 300, bg = "white")

ggplot(data, aes(x = s_recruit_rate*100, 
                      y = s_fecundity*4710000*(s_dbhmax*0.5)^2, 
                      col = s_seedmass)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  theme_bw() +
  scale_color_viridis_c("seed mass",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  ylab("Number of seed produce per species") +
  xlab("Percentage of recruitment") +
  ggtitle("titre", "sous-titre")


ggplot(data, aes(x = s_seedmass, 
                      y = s_fecundity*4710000*(s_dbhmax*0.5)^2, 
                      col = s_seedmass)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  scale_x_log10() +
  theme_bw() +
  scale_color_viridis_c("seed mass",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  ylab("number of seed produce per species") +
  xlab("seed mass (g)") +
  ggtitle("nombre de graines produites selon la masse des graines", "s_seedmass ~ s_fecundity*4710000*(s_dbhmax*0.5)^2")


ggplot(data2, aes(x = s_LMA, 
                 y = s_fecundity*4710000*(s_dbhmax*0.5)^2, 
                 col = s_seedmass)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  scale_x_log10() +
  theme_bw() +
  scale_color_viridis_c("seed mass (g)",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  ylab("Number of seed produce per species") +
  xlab("Leaf mass per area (LMA) (g/m^2)") +
  ggtitle("nombre de graines produites par espèce selon le LMA", "s_LMA ~ s_fecundity*4710000*(s_dbhmax*0.5)^2")


ggplot(data2, aes(x = s_seedmass, 
                       y = s_fecundity, 
                       col = s_seedmass)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  scale_x_log10() +
  theme_bw() +
  scale_color_viridis_c("seed mass",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  ylab("fecundity per species") +
  xlab("seed mass (g)") +
  ggtitle("fécondité des espèces selon la masse des graines", "s_seedmass ~ s_fecundity")

ggplot(data2, aes(x = s_LMA, 
                  y = s_fecundity, 
                  col = s_seedmass)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  scale_x_log10() +
  theme_bw() +
  scale_color_viridis_c("seed mass",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  ylab("Fecundity per species") +
  xlab("Leaf mass per area (LMA) (g/m^2)") +
  ggtitle("fécondité des espèces selon le LMA", "s_LMA ~ s_fecundity")


ggplot(data, aes(x = s_seedmass, 
                       y = s_recruit_rate*100, 
                       col = s_seedmass)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  scale_color_viridis_c("seed mass",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  ylab("percentage of recruitment (%)") +
  xlab("seed mass (g)") +
  ggtitle("pourcentage de recrutement par espèce selon la masse des graines", "s_seedmass ~ s_recruit_rate*100")

ggplot(data2, aes(x = s_LMA, 
                 y = s_recruit_rate*100, 
                 col = s_seedmass)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  scale_color_viridis_c("seed mass",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  ylab("Percentage of recruitment (%)") +
  xlab("Leaf mass per area (LMA) (g/m^2)") +
  ggtitle("pourcentage de recrutement par espèce selon le LMA", "s_LMA ~ s_recruit_rate*100")


ggplot(data2, aes(x = s_seedmass, 
                       y = s_maxDD, 
                       col = s_seedmass)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  scale_color_viridis_c("seed mass",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  ylab("distance de dispersion maximale (m)") +
  xlab("seed mass (g)") +
  ggtitle("distance de dispersion maximale par espèce selon la masse des graines", "s_seedmass ~ s_maxDD")


ggplot(data2, aes(x = s_LMA, 
                  y = s_maxDD, 
                  col = s_seedmass)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  scale_color_viridis_c("seed mass (g)",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  ylab("Distance de dispersion maximale (m)") +
  xlab("Leaf mass per area (LMA) (g/m^2)") +
  ggtitle("distance de dispersion maximale par espèce selon le LMA", "s_LMA ~ s_maxDD")


ggplot(data2, aes(x = s_LMA, 
                  y = s_maxDD, 
                  col = dispersal_syndrome)) +
  geom_point()+
  scale_x_log10() +
  theme_bw() +
  ylab("distance de dispersion maximale (m)") +
  xlab("Leaf mass per area (LMA) (g/m^2)") +
  ggtitle("distance de dispersion maximale par espèce selon le LMA", "s_LMA ~ s_maxDD colored by dispersal syndrome")

ggplot(data2, aes(x = s_dbhmax, 
                  y = s_maxDD, 
                  col = dispersal_syndrome)) +
  geom_point()+
  scale_x_log10() +
  theme_bw() +
  ylab("Distance de dispersion maximale (m)") +
  xlab("Maximum diameter at breast height per species (cm)") +
  ggtitle("distance de dispersion maximale par espèce selon le dbh maximum", "s_dbhmax ~ s_maxDD colored by dispersal syndrome")


ggplot(data2, aes(x = s_seedmass, 
                       y = 10^(s_maxDD + 0.60*log10(height)), 
                       col = s_seedmass)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  scale_x_log10() +
  theme_bw() +
  scale_color_viridis_c("seed mass",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  ylab("distance de dispersion maximale (t_ds) (m)") +
  xlab("seed mass (g)") +
  ggtitle("distance de dispersion maximale par espèce selon la masse des graines", "s_seedmass ~ 10^(s_maxDD + 0.60*log10(height))")


ggplot(data2, aes(x = s_LMA, 
                  y = 10^(s_maxDD + 0.60*log10(height)), 
                  col = s_seedmass)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  scale_x_log10() +
  theme_bw() +
  scale_color_viridis_c("seed mass (g)",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  ylab("Distance de dispersion maximale (t_ds) (m)") +
  xlab("Leaf mass per area (LMA) (g/m^2)") +
  ggtitle("distance de dispersion maximale par espèce selon le LMA", "s_LMA ~ 10^(s_maxDD + 0.60*log10(height))")


ggplot(data2, aes(x = s_seedmass, 
                  y = 10^(s_maxDD + 0.60*log10(height)), 
                  col = dispersal_syndrome)) +
  geom_point()+
  scale_y_log10(labels = scales::comma) +
  scale_x_log10() +
  theme_bw() +
  ylab("distance de dispersion maximale (t_ds) (m)") +
  xlab("seed mass (g)") +
  ggtitle("distance de dispersion maximale par espèce selon la masse des graines", "s_seedmass ~ 10^(s_maxDD + 0.60*height) colored by dispersal syndrome")

ggplot(data2, aes(x = height/100, 
                  y = 10^(s_maxDD + 0.60*log10(height)), 
                  col = s_seedmass)) +
  geom_point()+
  scale_y_log10(labels = scales::comma) +
  scale_x_log10() +
  scale_color_viridis_c("seed mass (g)",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  theme_bw() +
  ylab("Dispersal distance maximum (t_ds) (m)") +
  xlab("Height of mature tree per species (m)") +
  ggtitle("distance de dispersion maximale par espèce selon la hauteur des arbres matures", "height ~ 10^(s_maxDD + 0.60*log10(height))")


#### Graphiques ####
ggplot(data2, aes(x = s_recruit_rate*100, 
                 y = s_fecundity*4710000*(s_dbhmax*0.5)^2, 
                 col = s_seedmass, 
                 size = s_dbhmax)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  theme_bw() +
  geom_hline(yintercept=2250, linetype="dashed", color = "black") + 
  scale_color_viridis_c("Masse de la \n graine (en g)",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  ylab("Nombre de graines produites par an") +
  xlab("Pourcentage de recrutement de la graine") +
  labs(size = "Dbhmax (en cm)") +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  ggtitle("Pourcentage de recrutement selon le nombre de graines produites par espèce", "s_recruit_rate*100 ~ s_fecundity*4710000*(s_dbhmax*0.5)^2")


ggplot(data2, aes(x = s_recruit_rate*100, 
                  y = s_fecundity*4710000*(s_dbhmax*0.5)^2, 
                  col = s_seedmass, 
                  size = s_wsg)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  theme_bw() +
  geom_hline(yintercept=2250, linetype="dashed", color = "black") + 
  scale_color_viridis_c("seed mass",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  ylab("Number of seed produce per species") +
  xlab("Percentage of recruitment") +
  ggtitle("Pourcentage de recrutement selon le nombre de graines produites par espèce", "s_recruit_rate*100 ~ s_fecundity*4710000*(s_dbhmax*0.5)^2")


ggplot(data2, aes(x = 10^(s_maxDD + 0.60*log10(height)), 
                  y = s_fecundity*4710000*(s_dbhmax*0.5)^2, 
                  col = s_seedmass)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  scale_x_log10() +
  geom_hline(yintercept=2250, linetype="dashed", color = "black") +
  geom_vline(xintercept = 40, linetype="dashed", color = "black") +
  scale_color_viridis_c("seed mass",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  theme_bw() +
  ylab("Number of seed produce per species") +
  xlab("Dispersal distance maximum (t_ds) (m)") +
  ggtitle("Distance de dispersion maximale selon le nombre de graines produites par espèce", "10^(s_maxDD + 0.60*log10(height)) ~ s_fecundity*4710000*(s_dbhmax*0.5)^2")


ggplot(data2, aes(x = 10^(s_maxDD + 0.60*log10(height)), 
                  y = s_fecundity*4710000*(s_dbhmax*0.5)^2, 
                  col = dispersal_syndrome)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  scale_x_log10() +
  geom_hline(yintercept=2250, linetype="dashed", color = "black") +
  geom_vline(xintercept = 40, linetype="dashed", color = "black") +
  theme_bw() +
  ylab("Number of seed produce per species") +
  xlab("Dispersal distance maximum (t_ds) (m)") +
  ggtitle("Distance de dispersion maximale selon le nombre de graines produites par espèce", "10^(s_maxDD + 0.60*log10(height)) ~ s_fecundity*4710000*(s_dbhmax*0.5)^2")

ggplot(data2, aes(x = s_recruit_rate*100, 
                  y = 10^(s_maxDD + 0.60*log10(height)), 
                  col = s_seedmass)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  geom_hline(yintercept = 40, linetype="dashed", color = "black") +
  theme_bw() +
  scale_color_viridis_c("seed mass",trans = "log", breaks=c(1, 10, 100, 1000, 10000), labels = scales::comma)+
  ylab("Dispersal distance maximum (t_ds) (m)") +
  xlab("Percentage of recruitment") +
  ggtitle("Pourcentage de recrutement selon la distance de dispersion par espèce", "s_recruit_rate*100 ~ 10^(s_maxDD + 0.60*log10(height))")

ggplot(data2, aes(x = s_recruit_rate*100, 
                  y = 10^(s_maxDD + 0.60*log10(height)), 
                  col = dispersal_syndrome)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  theme_bw() +
  geom_hline(yintercept = 40, linetype="dashed", color = "black") +
  ylab("Dispersal distance maximum (t_ds) (m)") +
  xlab("Percentage of recruitment") +
  ggtitle("Pourcentage de recrutement selon la distance de dispersion par espèce", "s_recruit_rate*100 ~ 10^(s_maxDD + 0.60*log10(height))")

ggplot(sim_07@inputs$species, aes(x = s_dbhmax*100, 
                  y = s_seedmass/1000)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  theme_bw() +
  ylab("Masse des graines (en g)") +
  xlab("Dbh max (en cm)") +
  ggtitle("Dbh maximum selon la masse des graines")




####### VIII) Tests comparaison moyenne ########

# Comparaison entre les simulations 

##### Structure #####

#### avant coupure de la pluie de graines ####

# on construit le data frame
simulation <- "sim_00_10"
sim_00@ecosystem <- cbind(simulation, sim_00@ecosystem)
data0 <- rbind(stack_sim00@ecosystem, stack_sim00_bis@ecosystem, sim_00@ecosystem)
data0 <- filter(data0, data0$iter==7199)
data0$sim <- "sim_00"

jsp <- filter(stack_sim01@ecosystem, stack_sim01@ecosystem$iter==7199)
jsp2 <- filter(stack_sim01_bis@ecosystem, stack_sim01_bis@ecosystem$iter==7199)
data1 <- rbind(jsp, jsp2)
data1$sim <- "sim_01"


jsp <- filter(stack_sim02@ecosystem, stack_sim02@ecosystem$iter==7199)
jsp2 <- filter(stack_sim02_bis@ecosystem, stack_sim02_bis@ecosystem$iter==7199)
data2 <- rbind(jsp, jsp2)
data2$sim <- "sim_02"

jsp <- filter(stack_sim04@ecosystem, stack_sim04@ecosystem$iter==7199)
jsp2 <- filter(stack_sim04_bis@ecosystem, stack_sim04_bis@ecosystem$iter==7199)
data4 <- rbind(jsp, jsp2)
data4$sim <- "sim_04"

data <- rbind(data0, data1)
data <- rbind(data, data2)
data <- rbind(data, data4)


# on fait les tests statistiques :
# !! pour le test de Dunn significativité à p-value/2

# Pour sum1 :

# H0 : le nombre d'individu dont le dbh est >1cm ne diffère pas entre les simulations
# H1 : le nombre d'individu dont le dbh est >1cm diffère entre les simulations

data$sim <- as.factor(data$sim)
kruskal.test(data$sum1 ~ data$sim)
# Kruskal-Wallis chi-squared = 17.824, df = 3, p-value = 0.0004781
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour sum1

library(dunn.test)

dunn.test(data$sum1, data$sim)
# Comparison of x by group                            
# (No adjustment)                                
# Col Mean-|
#  Row Mean |     sim_00     sim_01     sim_02
#
#   sim_01  |   2.065748
#           |    0.0194*
#   sim_02  |   0.038254  -2.027493
#           |    0.4847    0.0213*
#   sim_04  |   3.557678   1.491929   3.519423
#           |    0.0002*     0.0607    0.0002*


# Pour sum10 :

# H0 : le nombre d'individu dont le dbh est >10cm ne diffère pas entre les simulations
# H1 : le nombre d'individu dont le dbh est >10cm diffère entre les simulations

kruskal.test(data$sum10 ~ data$sim)
# Kruskal-Wallis chi-squared = 36.154, df = 3, p-value = 6.947e-08
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour sum10

dunn.test(data$sum10, data$sim)

# Comparison of x by group                            
# (No adjustment)                                
#  Col Mean-|
#  Row Mean |     sim_00     sim_01     sim_02
# 
#    sim_01 |   3.768255
#           |    0.0001*
#    sim_02 |  -1.931948  -5.700203
#           |     0.0267    0.0000*
#    sim_04 |   1.912819  -1.855435   3.844767
#           |    0.0279     0.0318    0.0001*

# Pour sum30 :

# H0 : le nombre d'individu dont le dbh est >30cm ne diffère pas entre les simulations
# H1 : le nombre d'individu dont le dbh est >30cm diffère entre les simulations

kruskal.test(data$sum30 ~ data$sim)
# Kruskal-Wallis chi-squared = 30.181, df = 3, p-value = 1.264e-06
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour sum30

dunn.test(data$sum30, data$sim)

# Comparison of x by group                            
# (No adjustment)                                
#  Col Mean-|
#  Row Mean |     sim_00     sim_01     sim_02
#  
#    sim_01 |  -3.482148
#           |    0.0002*
#    sim_02 |   0.918368    4.400517
#           |     0.1792     0.0000*
#    sim_04 |  -3.252556   0.229592  -4.170925
#           |    0.0006*     0.4092    0.0000*


# Pour agb :

# H0 : la biomasse aérienne ne diffère pas entre les simulations
# H1 : la biomasse aérienne diffère entre les simulations

kruskal.test(data$agb ~ data$sim)
# Kruskal-Wallis chi-squared = 34.18, df = 3, p-value = 1.816e-07
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour l'agb

dunn.test(data$agb, data$sim)

# Comparison of x by group                            
# (No adjustment)                                
#  Col Mean-|
#  Row Mean |     sim_00     sim_01     sim_02
#  
#    sim_01 |  -3.634187
#           |    0.0001*
#    sim_02 |   2.027493    5.661681
#           |    0.0213*     0.0000*
#    sim_04 |  -1.453674    2.180512  -3.481168
#           |    0.0730     0.0146*    0.0002*


#### après coupure de la pluie de graines ####

# on construit le data frame

simulation <- "sim_00_10"
sim_00_tris@ecosystem <- cbind(simulation, sim_00_tris@ecosystem)
data0 <- rbind(stack_sim00_seedrain0@ecosystem, stack_sim00_tris_seedrain0@ecosystem, sim_00_tris@ecosystem)
data0 <- filter(data0, data0$iter==7199)
data0$sim <- "sim_00"

jsp <- filter(stack_sim_01_seedrain0@ecosystem, stack_sim_01_seedrain0@ecosystem$iter==7199)
jsp2 <- filter(stack_sim_01_bis_seedrain0@ecosystem, stack_sim_01_bis_seedrain0@ecosystem$iter==7199)
data1 <- rbind(jsp, jsp2)
data1$sim <- "sim_01"


jsp <- filter(stack_sim_02_seedrain0@ecosystem, stack_sim_02_seedrain0@ecosystem$iter==7199)
jsp2 <- filter(stack_sim_02_bis_seedrain0@ecosystem, stack_sim_02_bis_seedrain0@ecosystem$iter==7199)
data2 <- rbind(jsp, jsp2)
data2$sim <- "sim_02"

jsp <- filter(stack_sim_04_seedrain0@ecosystem, stack_sim_04_seedrain0@ecosystem$iter==7199)
jsp2 <- filter(stack_sim_04_bis_seedrain0@ecosystem, stack_sim_04_bis_seedrain0@ecosystem$iter==7199)
data4 <- rbind(jsp, jsp2)
data4$sim <- "sim_04"

data <- rbind(data0, data1)
data <- rbind(data, data2)
data <- rbind(data, data4)

# on fait les tests statistiques

# Pour sum1 :

# H0 : le nombre d'individu dont le dbh est >1cm ne diffère pas entre les simulations
# H1 : le nombre d'individu dont le dbh est >1cm diffère entre les simulations

data$sim <- as.factor(data$sim)
kruskal.test(data$sum1 ~ data$sim)
# Kruskal-Wallis chi-squared = 18.957, df = 3, p-value = 0.000279
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour sum1

library(dunn.test)

dunn.test(data$sum1, data$sim)
# Comparison of x by group                            
# (No adjustment)                                
# Col Mean-|
#  Row Mean |     sim_00     sim_01     sim_02
#  
#   sim_01  |   3.901969
#           |    0.0000*
#   sim_02  |   0.803346   -3.098622
#           |    0.2109     0.0010*
#   sim_04  |   2.716076   -1.185892   1.912730
#           |    0.0033*     0.1178     0.0279


# Pour sum10 :

# H0 : le nombre d'individu dont le dbh est >10cm ne diffère pas entre les simulations
# H1 : le nombre d'individu dont le dbh est >10cm diffère entre les simulations

kruskal.test(data$sum10 ~ data$sim)
# Kruskal-Wallis chi-squared = 36.16, df = 3, p-value = 6.929e-08
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour sum10

dunn.test(data$sum10, data$sim)

# Comparison of x by group                            
# (No adjustment)                                
#  Col Mean-|
#  Row Mean |     sim_00     sim_01     sim_02
#  
#    sim_01 |   3.882842
#           |    0.0001*
#    sim_02 |  -1.797966  -5.680808
#           |     0.0361    0.0000*
#    sim_04 |   1.970112  -1.912730   3.768078
#           |    0.0244*     0.0279    0.0001*

# Pour sum30 :

# H0 : le nombre d'individu dont le dbh est >30cm ne diffère pas entre les simulations
# H1 : le nombre d'individu dont le dbh est >30cm diffère entre les simulations

kruskal.test(data$sum30 ~ data$sim)
# Kruskal-Wallis chi-squared = 29.68, df = 3, p-value = 1.611e-06
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour sum30

dunn.test(data$sum30, data$sim)

# Comparison of x by group                            
# (No adjustment)                                
#  Col Mean-|
#  Row Mean |     sim_00     sim_01     sim_02
#  
#    sim_01 |  -4.659027
#           |    0.0000*
#    sim_02 | -0.985379     3.673648
#           |     0.1622     0.0001*
#    sim_04 |  -3.807583    0.851444  -2.822203
#           |    0.0001*     0.1973    0.0024*


# Pour agb :

# H0 : la biomasse aérienne ne diffère pas entre les simulations
# H1 : la biomasse aérienne diffère entre les simulations

kruskal.test(data$agb ~ data$sim)
# Kruskal-Wallis chi-squared = 36.44, df = 3, p-value = 6.043e-08
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour l'agb

dunn.test(data$agb, data$sim, method = "bonferroni")

# Comparison of x by group                            
# (No adjustment)                                
#  Col Mean-|
#  Row Mean |     sim_00     sim_01     sim_02
#  
#    sim_01 |  -3.844587
#           |    0.0001*
#    sim_02 |   1.874475    5.719063
#           |    0.0304     0.0000*
#    sim_04 |  -1.931857    1.912730  -3.806332
#           |    0.0267      0.0279     0.0001*



##### Diversité #####

#### avant coupure de la pluie de graines ####

data0 <- filter(sim_00_func, sim_00_func$iter==7188)
data0$sim <- "sim_00"

jsp <- filter(sim_01_functional, sim_01_functional$iter==7188)
jsp2 <- filter(sim_01_bis_functional, sim_01_bis_functional$iter==7188)
data1 <- rbind(jsp, jsp2)
data1$sim <- "sim_01"


jsp <- filter(sim_02_functional, sim_02_functional$iter==7188)
jsp2 <- filter(sim_02_bis_functional, sim_02_bis_functional$iter==7188)
data2 <- rbind(jsp, jsp2)
data2$sim <- "sim_02"

jsp <- filter(sim_04_functional, sim_04_functional$iter==7188)
jsp2 <- filter(sim_04_bis_functional, sim_04_bis_functional$iter==7188)
data4 <- rbind(jsp, jsp2)
data4$sim <- "sim_04"

data <- rbind(data0, data1)
data <- rbind(data, data2)
data <- rbind(data, data4)

# Pour la richesse spécifique :

# H0 : la richesse spécifique ne diffère pas entre les simulations
# H1 : la richesse spécifique diffère entre les simulations

data$sim <- as.factor(data$sim)
kruskal.test(data$nbsp ~ data$sim)
# Kruskal-Wallis chi-squared = 32.593, df = 3, p-value = 3.924e-07
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour la richesse spécifique

library(dunn.test)

dunn.test(data$nbsp, data$sim)
# Comparison of x by group                            
# (No adjustment)                                
# Col Mean-|
#  Row Mean |     sim_00     sim_01     sim_02
#  
#   sim_01  |   -0.470076
#           |    0.3192
#   sim_02  |   4.528082   4.998158
#           |    0.0000*    0.0000*
#   sim_04  |   2.542249   3.012326   -1.985832
#           |    0.0055*     0.0013*    0.0235*


# à refaire pour shannon et pielou : valeurs à retrouver dans mon rapport


# Pour la richesse fonctionnelle :

# H0 : la richesse fonctionnelle ne diffère pas entre les simulations
# H1 : la richesse fonctionnelle diffère entre les simulations

kruskal.test(data$FRic ~ data$sim)
# Kruskal-Wallis chi-squared = 32.999, df = 3, p-value = 3.223e-07
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour la richesse fonctionnelle

dunn.test(data$FRic, data$sim)

# Comparison of x by group                            
# (No adjustment)                                
#  Col Mean-|
#  Row Mean |     sim_00     sim_01     sim_02
#  
#    sim_01 |  -0.267782
#           |    0.3944
#    sim_02 |   4.647934   4.915716
#           |    0.0000*    0.0000*
#    sim_04 |   2.735204   3.002986   -1.912730
#           |    0.0031*    0.0013*     0.0279

# Pour la régularité fonctionnelle :

# H0 : la régularité fonctionnelle ne diffère pas entre les simulations
# H1 : la régularité fonctionnelle diffère entre les simulations

kruskal.test(data$FEve ~ data$sim)
# Kruskal-Wallis chi-squared = 6.9293, df = 3, p-value = 0.07419
# p-value > 0.05 donc on ne rejette pas H0
# la régularité fonctionnelle ne diffère pas entre les simulations

# valeur pour la dissimilarité fonctionnelle dans mon rapport


#### après coupure de la pluie de graines ####

data0 <- filter(final, final$iter==14376)
data0$sim <- "sim_00"

data1 <- filter(sim_01_func_final, sim_01_func_final$iter==14376)
data1$sim <- "sim_01"

data2 <- filter(sim_02_func_final, sim_02_func_final$iter==14376)
data2$sim <- "sim_02"

data4 <- filter(sim_04_func_final, sim_04_func_final$iter==14376)
data4$sim <- "sim_04"

data <- rbind(data0, data1)
data <- rbind(data, data2)
data <- rbind(data, data4)

# Pour la richesse spécifique :

# H0 : la richesse spécifique ne diffère pas entre les simulations
# H1 : la richesse spécifique diffère entre les simulations

data$sim <- as.factor(data$sim)
kruskal.test(data$nbsp ~ data$sim)
# Kruskal-Wallis chi-squared = 33.297, df = 3, p-value = 2.789e-07
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour la richesse spécifique

library(dunn.test)

dunn.test(data$nbsp, data$sim)
# Comparison of x by group                            
# (No adjustment)                                
# Col Mean-|
#  Row Mean |     sim_00     sim_01     sim_02
#  
#   sim_01  |   4.530624
#           |    0.0000*
#   sim_02  |   5.047863   0.517238
#           |    0.0000*    0.3025
#   sim_04  |   1.915697   -2.614927  -3.132165
#           |    0.0277     0.0045*     0.0009*


# Pour la richesse fonctionnelle :

# H0 : la richesse fonctionnelle ne diffère pas entre les simulations
# H1 : la richesse fonctionnelle diffère entre les simulations

kruskal.test(data$FRic ~ data$sim)
# Kruskal-Wallis chi-squared = 33.758, df = 3, p-value = 2.228e-07
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour la richesse fonctionnelle

dunn.test(data$FRic, data$sim)

# Comparison of x by group                            
# (No adjustment)                                
#  Col Mean-|
#  Row Mean |     sim_00     sim_01     sim_02
#  
#    sim_01 |   3.404659
#           |    0.0003*
#    sim_02 |   5.661681   2.257021
#           |    0.0000*    0.0120*
#    sim_04 |   2.104003  -1.300656   -3.557678
#           |    0.0177*    0.0967     0.0002*


# Pour la régularité fonctionnelle :

# H0 : la régularité fonctionnelle ne diffère pas entre les simulations
# H1 : la régularité fonctionnelle diffère entre les simulations

kruskal.test(data$FEve ~ data$sim)
# Kruskal-Wallis chi-squared = 15.414, df = 3, p-value = 0.001495
# p-value > 0.05 donc on ne rejette pas H0
# la régularité fonctionnelle  diffère entre les simulations

dunn.test(data$FEve, data$sim)
# Comparison of x by group                            
# (No adjustment)                                
#  Col Mean-|
#  Row Mean |     sim_00     sim_01     sim_02
#  
#    sim_01 |   2.123130
#           |    0.0169*
#    sim_02 |   3.156004   1.032874
#           |    0.0008*    0.1508
#    sim_04 |  -0.076509  -2.199639   -3.232513
#           |    0.4695     0.0139*     0.0006*


# Pour la dissimilarité foonctionnelle :

# H0 : FDis ne diffère pas entre les simulations
# H1 : FDis diffère entre les simulations

kruskal.test(data$FDis ~ data$sim)
# Kruskal-Wallis chi-squared = 34.158, df = 3, p-value = 1.835e-07
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour l'indice de Rao

dunn.test(data$FDis, data$sim)

# Comparison of x by group                            
# (No adjustment)                                
#  Col Mean-|
#  Row Mean |     sim_00     sim_01     sim_02
#  
#    sim_01 |  -3.175132
#           |    0.0007*
#    sim_02 |  -0.612073    2.563058
#           |    0.2702      0.0052*
#    sim_04 |   2.563058    5.738190   3.175132
#           |    0.0052*     0.0000*    0.0007*

# faire de même pour les indices de Shannon et de Pielou (à retrouver dans le fichier graph_H1.R)


##### Différence entre avant et après coupure de la pluie de graines #####
# pas utilisé au final 
# ! abandon de l'indice de Rao car calcul par le package pas clair
# on a utilisé la dissimilarité fonctionnelle à la place

# différence de diversité entre avant et après coupure de la pluie de graines 
# avant - après : perte 

# Calcul : 

# sim 0 : 
# après : 
data0 <- filter(sim_00_seedrain0_func, sim_00_seedrain0_func$iter==7188)
# avant : 
data5 <- filter(sim_00_func, sim_00_func$iter==7188)
# différence
data0$dif_nbsp <- data5$nbsp - data0$nbsp
data0$dif_FRic <- data5$FRic - data0$FRic
data0$dif_FEve <- data5$FEve - data0$FEve
data0$dif_RaoQ <- data5$RaoQ - data0$RaoQ
data0$sim <- "sim_00"

# sim 1 :
# après :
jsp <- filter(sim_01_seedrain0_functional, sim_01_seedrain0_functional$iter==7188)
jsp2 <- filter(sim_01_bis_seedrain0_functional, sim_01_bis_seedrain0_functional$iter==7188)
data1 <- rbind(jsp, jsp2)
# avant : 
jsp <- filter(sim_01_functional, sim_01_functional$iter==7188)
jsp2 <- filter(sim_01_bis_functional, sim_01_bis_functional$iter==7188)
data6 <- rbind(jsp, jsp2)
# différence
data1$dif_nbsp <- data6$nbsp - data1$nbsp
data1$dif_FRic <- data6$FRic - data1$FRic
data1$dif_FEve <- data6$FEve - data1$FEve
data1$dif_RaoQ <- data6$RaoQ - data1$RaoQ
data1$sim <- "sim_01"

# sim 2 : 
# après : 
jsp <- filter(sim_02_seedrain0_functional, sim_02_seedrain0_functional$iter==7188)
jsp2 <- filter(sim_02_bis_seedrain0_functional, sim_02_bis_seedrain0_functional$iter==7188)
data2 <- rbind(jsp, jsp2)
#avant : 
jsp <- filter(sim_02_functional, sim_02_functional$iter==7188)
jsp2 <- filter(sim_02_bis_functional, sim_02_bis_functional$iter==7188)
data7 <- rbind(jsp, jsp2)
# différence
data2$dif_nbsp <- data7$nbsp - data2$nbsp
data2$dif_FRic <- data7$FRic - data2$FRic
data2$dif_FEve <- data7$FEve - data2$FEve
data2$dif_RaoQ <- data7$RaoQ - data2$RaoQ
data2$sim <- "sim_02"

# sim 4 : 
#après :
jsp <- filter(sim_04_seedrain0_functional, sim_04_seedrain0_functional$iter==7188)
jsp2 <- filter(sim_04_bis_seedrain0_functional, sim_04_bis_seedrain0_functional$iter==7188)
data4 <- rbind(jsp, jsp2)
# avant :
jsp <- filter(sim_04_functional, sim_04_functional$iter==7188)
jsp2 <- filter(sim_04_bis_functional, sim_04_bis_functional$iter==7188)
data8 <- rbind(jsp, jsp2)
# différence
data4$dif_nbsp <- data8$nbsp - data4$nbsp
data4$dif_FRic <- data8$FRic - data4$FRic
data4$dif_FEve <- data8$FEve - data4$FEve
data4$dif_RaoQ <- data8$RaoQ - data4$RaoQ
data4$sim <- "sim_04"

#data final :
data <- rbind(data0, data1)
data <- rbind(data, data2)
data <- rbind(data, data4)


#graphs Box plot : 

nbsp <- ggplot(data, aes(x = sim, y = dif_nbsp, fill = sim))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size = 10, colour = "black"), axis.text.y = element_text(size = 10),  legend.position = "none"
        , axis.title.x = element_blank(), axis.title.y = element_text(size = 15) )+
  ylab("Perte de richesse spécifique") +
  xlab("Simulations")

FRic <- ggplot(data, aes(x = sim, y = dif_FRic, fill = sim))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size = 10, colour = "black"), axis.text.y = element_text(size = 10),  legend.position = "none"
        , axis.title.x = element_blank(), axis.title.y = element_text(size = 15) )+
  ylab("Perte de richesse fonctionnelle") +
  xlab("Simulations")


FEve <- ggplot(data, aes(x = sim, y = dif_FEve, fill = sim))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size = 10, colour = "black"), axis.text.y = element_text(size = 10),  legend.position = "none"
        , axis.title.x = element_blank(), axis.title.y = element_text(size = 15) )+
  ylab("Perte de régularité fonctionnelle") +
  xlab("Simulations")

RaoQ <- ggplot(data, aes(x = sim, y = dif_RaoQ, fill = sim))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size = 10, colour = "black"), axis.text.y = element_text(size = 10),  legend.position = "none"
        , axis.title.x = element_blank(), axis.title.y = element_text(size = 15) )+
  ylab("Perte de diversité fonctionnelle \n (indice de Rao)") +
  xlab("Simulations")


figure <- ggarrange(nbsp, FRic, FEve, RaoQ,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "none")
figure

annotate_figure(figure,
                top = text_grob("Comparaison des différences de variables de diversité de la forêt entre les scénarios \n avant et après coupure de la pluie de graines", face = "bold", size = 16))


# différence de structure entre avant et après coupure de la pluie de graines 
# avant - après : perte 

# Calcul : 

# sim 0 : 
# après :
simulation <- "sim_00_10"
sim_00_tris@ecosystem <- cbind(simulation, sim_00_tris@ecosystem)
data0 <- rbind(stack_sim00_seedrain0@ecosystem, stack_sim00_tris_seedrain0@ecosystem, sim_00_tris@ecosystem)
data0 <- filter(data0, data0$iter==7199)
# avant :
simulation <- "sim_00_10"
sim_00@ecosystem <- cbind(simulation, sim_00@ecosystem)
data5 <- rbind(stack_sim00@ecosystem, stack_sim00_bis@ecosystem, sim_00@ecosystem)
data5 <- filter(data5, data5$iter==7199)
# dif : 
data0$dif_agb <- data5$agb - data0$agb
data0$dif_sum1 <- data5$sum1 - data0$sum1
data0$dif_sum10 <- data5$sum10 - data0$sum10
data0$dif_sum30 <- data5$sum30 - data0$sum30
data0$sim <- "sim_00"

# sim 1 : 
# après : 
jsp <- filter(stack_sim_01_seedrain0@ecosystem, stack_sim_01_seedrain0@ecosystem$iter==7199)
jsp2 <- filter(stack_sim_01_bis_seedrain0@ecosystem, stack_sim_01_bis_seedrain0@ecosystem$iter==7199)
data1 <- rbind(jsp, jsp2)
# avant : 
jsp <- filter(stack_sim01@ecosystem, stack_sim01@ecosystem$iter==7199)
jsp2 <- filter(stack_sim01_bis@ecosystem, stack_sim01_bis@ecosystem$iter==7199)
data6 <- rbind(jsp, jsp2)
#dif : 
data1$dif_agb <- data6$agb - data1$agb
data1$dif_sum1 <- data6$sum1 - data1$sum1
data1$dif_sum10 <- data6$sum10 - data1$sum10
data1$dif_sum30 <- data6$sum30 - data1$sum30
data1$sim <- "sim_01"

data <- rbind(data0, data1)
save(data, file = "D:/Mes Donnees/TROLL/simulations/data_comp_structure.Rdata")

# sim 2 : 
# après : 
jsp <- filter(stack_sim_02_seedrain0@ecosystem, stack_sim_02_seedrain0@ecosystem$iter==7199)
jsp2 <- filter(stack_sim_02_bis_seedrain0@ecosystem, stack_sim_02_bis_seedrain0@ecosystem$iter==7199)
data2 <- rbind(jsp, jsp2)
# avant : 
jsp <- filter(stack_sim02@ecosystem, stack_sim02@ecosystem$iter==7199)
jsp2 <- filter(stack_sim02_bis@ecosystem, stack_sim02_bis@ecosystem$iter==7199)
data7 <- rbind(jsp, jsp2)
#dif : 
data2$dif_agb <- data7$agb - data2$agb
data2$dif_sum1 <- data7$sum1 - data2$sum1
data2$dif_sum10 <- data7$sum10 - data2$sum10
data2$dif_sum30 <- data7$sum30 - data2$sum30
data2$sim <- "sim_02"

# sim 4 : 
# après : 
jsp <- filter(stack_sim_04_seedrain0@ecosystem, stack_sim_04_seedrain0@ecosystem$iter==7199)
jsp2 <- filter(stack_sim_04_bis_seedrain0@ecosystem, stack_sim_04_bis_seedrain0@ecosystem$iter==7199)
data4 <- rbind(jsp, jsp2)
# avant : 
jsp <- filter(stack_sim04@ecosystem, stack_sim04@ecosystem$iter==7199)
jsp2 <- filter(stack_sim04_bis@ecosystem, stack_sim04_bis@ecosystem$iter==7199)
data8 <- rbind(jsp, jsp2)
#dif : 
data4$dif_agb <- data8$agb - data4$agb
data4$dif_sum1 <- data8$sum1 - data4$sum1
data4$dif_sum10 <- data8$sum10 - data4$sum10
data4$dif_sum30 <- data8$sum30 - data4$sum30
data4$sim <- "sim_04"

data <- rbind(data, data2)
data <- rbind(data, data4)

save(data, file = "D:/Mes Donnees/TROLL/simulations/data_comp_structure.Rdata")

# graphs : 

sum1 <- ggplot(data, aes(x = sim, y = dif_sum1, fill = sim))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size = 10, colour = "black"), axis.text.y = element_text(size = 10),  legend.position = "none"
        , axis.title.x = element_blank(), axis.title.y = element_text(size = 15) )+
  ylab("Nombre d'individus > 1cm de dbh") 


sum10 <- ggplot(data, aes(x = sim, y = dif_sum10, fill = sim))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size = 10, colour = "black"), axis.text.y = element_text(size = 10),  legend.position = "none"
        , axis.title.x = element_blank(), axis.title.y = element_text(size = 15) )+
  ylab("Nombre d'individus > 10cm de dbh") 


sum30 <- ggplot(data, aes(x = sim, y = dif_sum30, fill = sim))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size = 10, colour = "black"), axis.text.y = element_text(size = 10),  legend.position = "none"
        , axis.title.x = element_blank(), axis.title.y = element_text(size = 15) )+
  ylab("Nombre d'individus > 30cm de dbh") +
  xlab("Simulations")

agb <- ggplot(data, aes(x = sim, y = dif_agb, fill = sim))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size = 10, colour = "black"), axis.text.y = element_text(size = 10),  legend.position = "none"
        , axis.title.x = element_blank(), axis.title.y = element_text(size = 15) )+
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  xlab("Simulations")

figure <- ggarrange(sum1, sum10, sum30, agb,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "none")
figure

annotate_figure(figure,
                top = text_grob("Comparaison des différences de variables de structure de la forêt entre les scénarios \n avant et après coupure de la pluie de graines", face = "bold", size = 16))


kruskal.test(data$dif_agb ~ data$sim)
# Kruskal-Wallis chi-squared = 31.111, df = 3, p-value = 8.056e-07
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour l'indice de Rao

dunn.test(data$dif_agb, data$sim)


##### Community Weighted Mean (CWM) ######

# H0 : la seedmass moyenne ne diffère pas entre les simulations
# H1 : la seedmass moyenne diffère entre les simulations

kruskal.test(data_traj_div$CWM.seedmass ~ data_traj_div$sim)
#  Kruskal-Wallis chi-squared = 39924, df = 3, p-value < 2.2e-16
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour la seedmass moyenne

dunn.test(data_traj_div$CWM.seedmass, data_traj_div$sim)

kruskal.test(data_traj_div$CWM.dbhmax ~ data_traj_div$sim)
#  Kruskal-Wallis chi-squared = 43560, df = 3, p-value < 2.2e-16
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour le dbhmax moyen

dunn.test(data_traj_div$CWM.dbhmax, data_traj_div$sim)

kruskal.test(data_traj_div$CWM.LMA ~ data_traj_div$sim)
#  Kruskal-Wallis chi-squared = 29318, df = 3, p-value < 2.2e-16
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour le LMA moyen

dunn.test(data_traj_div$CWM.LMA, data_traj_div$sim)


kruskal.test(data_traj_div$CWM.log_wsg ~ data_traj_div$sim)
#  Kruskal-Wallis chi-squared = 15484, df = 3, p-value < 2.2e-16
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour le wsg moyen

dunn.test(data_traj_div$CWM.log_wsg, data_traj_div$sim)


###### Autres ######


# indice de Shannon 
# c'est une mesure de la stabilité des communautés
# prend en compte lors de son calcul la richesse et l’abondance relative des espèces 
# H vaudra 0 quand l’échantillon ne contient qu’une seule espèce et augmente lorsque le nombre d’espèce augmente. 
# Plus l’indice H est élevé, plus la diversité est grande
shannon <- vegan::diversity(sim_00@forest$s_name)

# indice d’équitabilité de Piélou
# c'est une mesure de la répartition des individus au sein des espèce. 
# Il s’agit d’un paramètre plus rigoureux pour comparer des dominances potentielles entre sites puisqu’il est indépendant de la richesse spécifique.
# varie donc entre 0 et 1
# 0 => dominance d’une des espèces 
# 1 => équirépartition des individus entre les différentes espèces
pielou <- shannon / ln(r_spe)


######## IX) Histogramme de distribution de traits #########

#### Dbh max ####

sp <- ggplot(sim_00@inputs$species, aes(s_dbhmax*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "grey", col = "black") +
  xlab("Dbhmax en cm") +
  ylab("Nombre d'espèces") +
  labs(title = "Distribution du dbhmax selon les espèces implémentées \n dans le modèle")+
  expand_limits(x=c(0,150)) +
  theme(
    plot.title = element_text(hjust = 0.5))
sp

data0 <- left_join(sim_00_tris@forest, sim_00_tris@inputs$species)
data0 <- filter(data0, data0$iter==7200)

ind0 <- ggplot(data0, aes(dbhmax*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "blue", col = "darkblue") +
  xlab("Dbhmax en cm") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du dbhmax selon les arbres présents \n dans le modèle pour la simulation 0")+
  expand_limits(x=c(0,150)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind0

data1 <- left_join(sim_01_tris@forest, sim_01_tris@inputs$species)
data1 <- filter(data1, data1$iter==7200)

ind1 <- ggplot(data1, aes(dbhmax*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "blue", col = "darkblue") +
  xlab("Dbhmax en cm") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du dbhmax selon les arbres présents \n dans le modèle pour la simulation 1")+
  expand_limits(x=c(0,150)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind1

data2 <- left_join(sim_02_tris@forest, sim_02_tris@inputs$species)
data2 <- filter(data2, data2$iter==7200)

ind2 <- ggplot(data2, aes(dbhmax*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "blue", col = "darkblue") +
  xlab("Dbhmax en cm") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du dbhmax selon les arbres présents \n dans le modèle pour la simulation 2")+
  expand_limits(x=c(0,150)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind2

data3 <- left_join(sim_03_tris@forest, sim_03_tris@inputs$species)
data3 <- filter(data3, data3$iter==7200)

ind3 <- ggplot(data3, aes(dbhmax*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "blue", col = "darkblue") +
  xlab("Dbhmax en cm") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du dbhmax selon les arbres présents \n dans le modèle pour la simulation 3")+
  expand_limits(x=c(0,150)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind3

data4 <- left_join(sim_04_tris@forest, sim_04_tris@inputs$species)
data4 <- filter(data4, data4$iter==7200)

ind4 <- ggplot(data4, aes(dbhmax*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "blue", col = "darkblue") +
  xlab("Dbhmax en cm") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du dbhmax selon les arbres présents \n dans le modèle pour la simulation 4")+
  expand_limits(x=c(0,150)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind4

data5 <- left_join(sim_05_tris@forest, sim_05_tris@inputs$species)
data5 <- filter(data5, data5$iter==7200)

ind5 <- ggplot(data5, aes(dbhmax*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "blue", col = "darkblue") +
  xlab("Dbhmax en cm") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du dbhmax selon les arbres présents \n dans le modèle pour la simulation 5")+
  expand_limits(x=c(0,150)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind5

data6 <- left_join(sim_06_tris@forest, sim_06_tris@inputs$species)
data6 <- filter(data6, data6$iter==7200)

ind6 <- ggplot(data6, aes(dbhmax*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "blue", col = "darkblue") +
  xlab("Dbhmax en cm") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du dbhmax selon les arbres présents \n dans le modèle pour la simulation 6")+
  expand_limits(x=c(0,150)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind6

data7 <- left_join(sim_07_tris@forest, sim_07_tris@inputs$species)
data7 <- filter(data7, data7$iter==7200)

ind7 <- ggplot(data7, aes(dbhmax*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "blue", col = "darkblue") +
  xlab("Dbhmax en cm") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du dbhmax selon les arbres présents \n dans le modèle pour la simulation 7")+
  expand_limits(x=c(0,150)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind7

figure <- ggarrange(sp, ind0, ind1, ind2, ind3, ind4, ind5, ind6, ind7,
                    labels = c("A", "B", "C" , "D" , "E" , "F" , "G" , "H", "I"),
                    ncol = 1, nrow = 9, 
                    common.legend = TRUE, legend = "bottom")
figure

#### Masse des graines ####

sp <- ggplot(sim_00@inputs$species, aes(s_seedmass/1000)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "grey", col = "black") +
  xlab("Masse des graines en g") +
  ylab("Nombre d'espèces") +
  labs(title = "Distribution de la masse des graines selon les espèces implémentées \n dans le modèle")+
  expand_limits(x=c(0,27.5)) +
  theme(
    plot.title = element_text(hjust = 0.5))
sp

ind0 <- ggplot(data0, aes(s_seedmass/1000)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "red", col = "darkred") +
  xlab("Masse des graines en g") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution de la masse des graines selon les arbres présents \n dans le modèle pour la simulation 0")+
  expand_limits(x=c(0,27.5)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind0

ind1 <- ggplot(data1, aes(s_seedmass/1000)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "red", col = "darkred") +
  xlab("Masse des graines en g") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution de la masse des graines selon les arbres présents \n dans le modèle pour la simulation 1")+
  expand_limits(x=c(0,27.5)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind1

ind2 <- ggplot(data2, aes(s_seedmass/1000)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "red", col = "darkred") +
  xlab("Masse des graines en g") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution de la masse des graines selon les arbres présents \n dans le modèle pour la simulation 2")+
  expand_limits(x=c(0,27.5)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind2

ind3 <- ggplot(data3, aes(s_seedmass/1000)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "red", col = "darkred") +
  xlab("Masse des graines en g") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution de la masse des graines selon les arbres présents \n dans le modèle pour la simulation 3")+
  expand_limits(x=c(0,27.5)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind3

ind4 <- ggplot(data4, aes(s_seedmass/1000)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "red", col = "darkred") +
  xlab("Masse des graines en g") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution de la masse des graines selon les arbres présents \n dans le modèle pour la simulation 4")+
  expand_limits(x=c(0,27.5)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind4

ind5 <- ggplot(data5, aes(s_seedmass/1000)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "red", col = "darkred") +
  xlab("Masse des graines en g") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution de la masse des graines selon les arbres présents \n dans le modèle pour la simulation 5")+
  expand_limits(x=c(0,27.5)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind5

ind6 <- ggplot(data6, aes(s_seedmass/1000)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "red", col = "darkred") +
  xlab("Masse des graines en g") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution de la masse des graines selon les arbres présents \n dans le modèle pour la simulation 6")+
  expand_limits(x=c(0,27.5)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind6

ind7 <- ggplot(data7, aes(s_seedmass/1000)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "red", col = "darkred") +
  xlab("Masse des graines en g") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution de la masse des graines selon les arbres présents \n dans le modèle pour la simulation 7")+
  expand_limits(x=c(0,27.5)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind7

figure <- ggarrange(sp, ind0, ind1, ind2, ind3, ind4, ind5, ind6, ind7,
                    labels = c("A", "B", "C" , "D" , "E" , "F" , "G" , "H", "I"),
                    ncol = 1, nrow = 9, 
                    common.legend = TRUE, legend = "bottom")
figure


#### LMA ####

sp <- ggplot(sim_00@inputs$species, aes(s_LMA*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "grey", col = "black") +
  xlab("LMA en g.m^3") +
  ylab("Nombre d'espèces") +
  labs(title = "Distribution du LMA selon les espèces implémentées \n dans le modèle")+
  expand_limits(x=c(0,20000)) +
  theme(
    plot.title = element_text(hjust = 0.5))
sp

data0 <- left_join(sim_00_tris@forest, sim_00_tris@inputs$species)
ind0 <- ggplot(data0, aes(LMA*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "mediumaquamarine", col = "seagreen") +
  xlab("LMA en g.m^3") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du LMA selon les arbres présents \n dans le modèle pour la simulation 0")+
  expand_limits(x=c(0,20000)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind0

data1 <- left_join(sim_01_tris@forest, sim_01_tris@inputs$species)
ind1 <- ggplot(data1, aes(LMA*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "mediumaquamarine", col = "seagreen") +
  xlab("LMA en g.m^3") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du LMA selon les arbres présents \n dans le modèle pour la simulation 1")+
  expand_limits(x=c(0,20000)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind1

data2 <- left_join(sim_02_tris@forest, sim_02_tris@inputs$species)
ind2 <- ggplot(data2, aes(LMA*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "mediumaquamarine", col = "seagreen") +
  xlab("LMA en g.m^3") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du LMA selon les arbres présents \n dans le modèle pour la simulation 2")+
  expand_limits(x=c(0,20000)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind2

data3 <- left_join(sim_03_tris@forest, sim_03_tris@inputs$species)
ind3 <- ggplot(data3, aes(LMA*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "mediumaquamarine", col = "seagreen") +
  xlab("LMA en g.m^3") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du LMA selon les arbres présents \n dans le modèle pour la simulation 3")+
  expand_limits(x=c(0,20000)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind3

data4 <- left_join(sim_04_tris@forest, sim_04_tris@inputs$species)
ind4 <- ggplot(data4, aes(LMA*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "mediumaquamarine", col = "seagreen") +
  xlab("LMA en g.m^3") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du LMA selon les arbres présents \n dans le modèle pour la simulation 4")+
  expand_limits(x=c(0,20000)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind4

data5 <- left_join(sim_05_tris@forest, sim_05_tris@inputs$species)
ind5 <- ggplot(data5, aes(LMA*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "mediumaquamarine", col = "seagreen") +
  xlab("LMA en g.m^3") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du LMA selon les arbres présents \n dans le modèle pour la simulation 5")+
  expand_limits(x=c(0,20000)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind5

data6 <- left_join(sim_06_tris@forest, sim_06_tris@inputs$species)
ind6 <- ggplot(data6, aes(LMA*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "mediumaquamarine", col = "seagreen") +
  xlab("LMA en g.m^3") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du LMA selon les arbres présents \n dans le modèle pour la simulation 6")+
  expand_limits(x=c(0,20000)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind6

data7 <- left_join(sim_07_tris@forest, sim_07_tris@inputs$species)
ind7 <- ggplot(data7, aes(LMA*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "mediumaquamarine", col = "seagreen") +
  xlab("LMA en g.m^3") +
  ylab("Nombre d'individus") +
  labs(title = "Distribution du LMA selon les arbres présents \n dans le modèle pour la simulation 7")+
  expand_limits(x=c(0,20000)) +
  theme(
    plot.title = element_text(hjust = 0.5))
ind7

figure <- ggarrange(sp, ind0, ind1, ind2, ind3, ind4, ind5, ind6, ind7,
                    labels = c("A", "B", "C" , "D" , "E" , "F" , "G" , "H", "I"),
                    ncol = 1, nrow = 9, 
                    common.legend = TRUE, legend = "bottom")
figure


#### Densité du bois #### 

# à faire 




## Histogramme des tailles de graines avant et après coupure pluie de graine

# sim_00 : 
data0_A <- left_join(sim_00@forest, sim_00@inputs$species)
data0_A <- filter(data0_A, data0_A$iter==7200)
data0_B <- left_join(sim_00_tris@forest, sim_00_tris@inputs$species)
data0_B <- filter(data0, data0$iter==7200)
data0_A$stade <- "Avant coupure de la pluie de graines"
data0_B$stade <- "Après coupure de la pluie de graines"
data <- rbind(data0_A, data0_B)
data$stade <- fct_inorder(data$stade)

ggplot(data, aes(s_seedmass/1000)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "red", col = "darkred") +
  xlab("Masse des graines en g") +
  ylab("Nombre d'individus") +
  scale_x_log10(labels = scales::comma) +
  facet_wrap(~stade, nrow = 2)+
  theme_bw()+
  theme(
    strip.text.x = element_text(
      size = 12, color = "black"
    ), strip.background = element_rect(
      fill="grey91"), plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),  
    axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12) )+
  labs(title = "Distribution de la masse des graines selon les arbres présents \n dans le modèle pour la simulation 0")
  
  

# sim_01 : 
data1_A <- left_join(sim_01@forest, sim_01@inputs$species)
data1_A <- filter(data1_A, data1_A$iter==7200)
data1_B <- filter(data1, data1$iter==7200)
data1_A$stade <- "Avant coupure de la pluie de graines"
data1_B$stade <- "Après coupure de la pluie de graines"
data <- rbind(data1_A, data1_B)
data$stade <- fct_inorder(data$stade)

ggplot(data, aes(s_seedmass/1000)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "red", col = "darkred") +
  xlab("Masse des graines en g") +
  ylab("Nombre d'individus") +
  scale_x_log10(labels = scales::comma) +
  facet_wrap(~stade, nrow = 2)+
  theme_bw()+
  theme(
    strip.text.x = element_text(
      size = 12, color = "black"
    ), strip.background = element_rect(
      fill="grey91"), plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),  
    axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12) )+
  labs(title = "Distribution de la masse des graines selon les arbres présents \n dans le modèle pour la simulation 1")

# sim_02 :
data2_A <- left_join(sim_02@forest, sim_02@inputs$species)
data2_A <- filter(data2_A, data2_A$iter==7200)
data2_B <- filter(data2, data2$iter==7200)
data2_A$stade <- "Avant coupure de la pluie de graines"
data2_B$stade <- "Après coupure de la pluie de graines"
data <- rbind(data2_A, data2_B)
data$stade <- fct_inorder(data$stade)

ggplot(data, aes(s_seedmass/1000)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "red", col = "darkred") +
  xlab("Masse des graines en g") +
  ylab("Nombre d'individus") +
  scale_x_log10(labels = scales::comma) +
  facet_wrap(~stade, nrow = 2)+
  theme_bw()+
  theme(
    strip.text.x = element_text(
      size = 12, color = "black"
    ), strip.background = element_rect(
      fill="grey91"), plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),  
    axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12) )+
  labs(title = "Distribution de la masse des graines selon les arbres présents \n dans le modèle pour la simulation 2")

# sim_04 :
data4_A <- left_join(sim_04@forest, sim_04@inputs$species)
data4_A <- filter(data4_A, data4_A$iter==7200)
data4_B <- filter(data4, data4$iter==7200)
data4_A$stade <- "Avant coupure de la pluie de graines"
data4_B$stade <- "Après coupure de la pluie de graines"
data <- rbind(data4_A, data4_B)
data$stade <- fct_inorder(data$stade)

ggplot(data, aes(s_seedmass/1000)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "red", col = "darkred") +
  xlab("Masse des graines en g") +
  ylab("Nombre d'individus") +
  scale_x_log10(labels = scales::comma) +
  facet_wrap(~stade, nrow = 2)+
  theme_bw()+
  theme(
    strip.text.x = element_text(
      size = 12, color = "black"
    ), strip.background = element_rect(
      fill="grey91"), plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),  
    axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12) )+
  labs(title = "Distribution de la masse des graines selon les arbres présents \n dans le modèle pour la simulation 4")


# histogramme des dbhmax avant et après coupure pluie de graine

# sim_00 : 
data0_A <- left_join(sim_00@forest, sim_00@inputs$species)
data0_A <- filter(data0_A, data0_A$iter==7200)
data0_B <- left_join(sim_00_tris@forest, sim_00_tris@inputs$species)
data0_B <- filter(data0_B, data0_B$iter==7200)
data0_A$stade <- "Avant coupure de la pluie de graines"
data0_B$stade <- "Après coupure de la pluie de graines"
data <- rbind(data0_A, data0_B)
data$stade <- fct_inorder(data$stade)

ggplot(data, aes(s_dbhmax*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "blue", col = "darkblue") +
  xlab("Dbh max en cm") +
  ylab("Nombre d'individus") +
  facet_wrap(~stade, nrow = 2)+
  scale_x_log10(labels = scales::comma) +
  theme_bw()+
  theme(
    strip.text.x = element_text(
      size = 12, color = "black"
    ), strip.background = element_rect(
      fill="grey91"), plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),  
    axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12) )+
  labs(title = "Distribution du dbh max selon les arbres présents \n dans le modèle pour la simulation 0")



# sim_01 : 
data1_A <- left_join(sim_01@forest, sim_01@inputs$species)
data1_A <- filter(data1_A, data1_A$iter==7200)
data1_B <- left_join(sim_01_tris@forest, sim_01_tris@inputs$species)
data1_B <- filter(data1_B, data1_B$iter==7200)
data1_A$stade <- "Avant coupure de la pluie de graines"
data1_B$stade <- "Après coupure de la pluie de graines"
data <- rbind(data1_A, data1_B)
data$stade <- fct_inorder(data$stade)

ggplot(data, aes(s_dbhmax*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "blue", col = "darkblue") +
  xlab("Dbh max en cm") +
  ylab("Nombre d'individus") +
  facet_wrap(~stade, nrow = 2)+
  scale_x_log10(labels = scales::comma) +
  theme_bw()+
  theme(
    strip.text.x = element_text(
      size = 12, color = "black"
    ), strip.background = element_rect(
      fill="grey91"), plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),  
    axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12) )+
  labs(title = "Distribution du dbh max selon les arbres présents \n dans le modèle pour la simulation 1")

# sim_02 :
data2_A <- left_join(sim_02@forest, sim_02@inputs$species)
data2_A <- filter(data2_A, data2_A$iter==7200)
data2_B <- left_join(sim_02_tris@forest, sim_02_tris@inputs$species)
data2_B <- filter(data2_B, data2_B$iter==7200)
data2_A$stade <- "Avant coupure de la pluie de graines"
data2_B$stade <- "Après coupure de la pluie de graines"
data <- rbind(data2_A, data2_B)
data$stade <- fct_inorder(data$stade)

ggplot(data, aes(s_dbhmax*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "blue", col = "darkblue") +
  xlab("Dbh max en cm") +
  ylab("Nombre d'individus") +
  facet_wrap(~stade, nrow = 2)+
  scale_x_log10(labels = scales::comma) +
  theme_bw()+
  theme(
    strip.text.x = element_text(
      size = 12, color = "black"
    ), strip.background = element_rect(
      fill="grey91"), plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),  
    axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12) )+
  labs(title = "Distribution du dbh max selon les arbres présents \n dans le modèle pour la simulation 2")

# sim_04 :
data4_A <- left_join(sim_04@forest, sim_04@inputs$species)
data4_A <- filter(data4_A, data4_A$iter==7200)
data4_B <- left_join(sim_04_tris@forest, sim_04_tris@inputs$species)
data4_B <- filter(data4_B, data4_B$iter==7200)
data4_A$stade <- "Avant coupure de la pluie de graines"
data4_B$stade <- "Après coupure de la pluie de graines"
data <- rbind(data4_A, data4_B)
data$stade <- fct_inorder(data$stade)

ggplot(data, aes(s_dbhmax*100)) +
  geom_histogram(position = "dodge", alpha=0.5, fill = "blue", col = "darkblue") +
  xlab("Dbh max en cm") +
  ylab("Nombre d'individus") +
  facet_wrap(~stade, nrow = 2)+
  scale_x_log10(labels = scales::comma) +
  theme_bw()+
  theme(
    strip.text.x = element_text(
      size = 12, color = "black"
    ), strip.background = element_rect(
      fill="grey91"), plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),  
    axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12) )+
  labs(title = "Distribution du dbh max selon les arbres présents \n dans le modèle pour la simulation 4")



