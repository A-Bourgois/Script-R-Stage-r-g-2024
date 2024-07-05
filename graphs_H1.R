####### Graphiques ########


##### 1) Structure de la forêt #####

# on récupère les données de l'écosystème pour les 4 simulations avant et après pluie de graines 

# pour la simulation 0 : 
load("~/TROLL/simulations/sim_00_ecosystem.Rdata")
# on ajoute une colonne pour indiquer la simulation 
test$sim <- "sim_00"

# pour la simulation 1 : 
load("~/TROLL/simulations/sim_01_ecosystem_final.Rdata")
sim_01_ecosystem_final$sim <- "sim_01"

# pour la simulation 2 : 
load("~/TROLL/simulations/sim_02_ecosystem_final.Rdata")
sim_02_ecosystem_final$sim <- "sim_02"

# pour la simulation 4 : 
load("~/TROLL/simulations/sim_04_ecosystem_final.Rdata")
sim_04_ecosystem_final$sim <- "sim_04"

# on fait un tableau combiné pour les 4 simulations
data_traj <- rbind(test, sim_01_ecosystem_final)
data_traj <- rbind(data_traj, sim_02_ecosystem_final)
data_traj <- rbind(data_traj, sim_04_ecosystem_final)

# graphiques trajectoires au cours du temps + distribution dans la forêt finale

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
  theme(legend.position = "none", legend.title = element_blank()) +
  xlab("Temps (années)") + ylab(expression(Biomasse ~ aérienne ~ (AGB ~ Kg ~ ha^{
    -1
  }))) +
  scale_color_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement")) +
  scale_fill_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement"))

g2 <- data_traj %>% 
  filter(iter == 7199) %>% 
  ggplot(aes(sim, agb, col = sim, fill = sim)) +
  geom_boxplot(alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "none",legend.title = element_blank()) +
  xlab("Scénarios") + ylab("") +
  scale_color_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement"))+
  scale_fill_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement"))

agb <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))

# refaire tourner le code en remplcant agb par sum1
sum1 <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))

# refaire tourner le code en remplcant agb par sum10
sum10 <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))

# refaire tourner le code en remplcant agb par sum30
sum30 <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))

# combiner les 4 graphiques en un seul
figure <- ggarrange(agb, sum1, sum10, sum30,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "bottom")
figure

annotate_figure(figure, top = text_grob("Comparaison des scénarios pour la structure de la forêt", face = "bold", size = 16))


##### 2) Diversité de la communauté ##### 

# on télécharge les data de diversité fonctionnelle

load("~/TROLL/simulations/sim_00_functional_final.Rdata")
final$sim <- "sim_00"

load("~/TROLL/simulations/sim_01_func_final.Rdata")
sim_01_func_final$sim <- "sim_01"

load("~/TROLL/simulations/sim_02_func_final.Rdata")
sim_02_func_final$sim <- "sim_02"

load("~/TROLL/simulations/sim_04_func_final.Rdata")
sim_04_func_final$sim <- "sim_04"

# on les combine en un seul tableau 
data_traj_div <- rbind(final, sim_01_func_final)
data_traj_div <- rbind(data_traj_div, sim_02_func_final)
data_traj_div <- rbind(data_traj_div, sim_04_func_final)

# graphiques :
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
  theme(legend.position = "none", legend.title = element_blank()) +
  xlab("Temps (années)") + ylab("Richesse spécifique") +
  scale_color_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement")) +
  scale_fill_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement"))

g2 <- data_traj_div %>% 
  filter(iter == 14376) %>% 
  ggplot(aes(sim, nbsp, col = sim, fill = sim)) +
  geom_boxplot(alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "none",legend.title = element_blank()) +
  xlab("Scénarios") + ylab("") +
  scale_color_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement"))+
  scale_fill_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement"))


nbsp <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))

# de même, il faut remplacer nbsp par les autres indices de diversité dans le code précédent 

FRic <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
FEve <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
FDis <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))

figure <- ggarrange(nbsp, FRic, FEve, FDis,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "bottom")
figure

annotate_figure(figure, top = text_grob("Comparaison des scénarios pour la diversité de la forêt", face = "bold", size = 16))

##### 3) Community Weighted Mean (CWM) #####

# comme on avait mis en log pour le calcul, il faut utiliser la fonction inverse donc exp
data_traj_div$CWM.seedmass <- exp(data_traj_div$CWM.log_seedmass)
data_traj_div$CWM.dbhmax <- exp(data_traj_div$CWM.log_dbhmax)
data_traj_div$CWM.LMA <- exp(data_traj_div$CWM.log_LMA)
data_traj_div$CWM.wsg <- exp(data_traj_div$CWM.log_wsg)

g1 <- data_traj_div %>% 
  group_by(sim, iter) %>% 
  summarise(l = min(CWM.wsg), 
            m = quantile(CWM.wsg, 0.5), 
            h = max(CWM.wsg)) %>% 
  ggplot(aes(iter/12, m, col = sim, fill = sim)) +
  geom_ribbon(aes(ymin = l, ymax = h), col = NA, alpha = 0.2) +
  geom_line() +
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  theme(legend.position = "none", legend.title = element_blank()) +
  xlab("Temps (années)") + ylab(expression(CWM ~ Densité ~ du ~ bois ~ (wsg ~ g ~ cm^{
    -3
  }))) +
  scale_color_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement")) +
  scale_fill_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement"))

g2 <- data_traj_div %>% 
  filter(iter == 14376) %>% 
  ggplot(aes(sim, CWM.wsg, col = sim, fill = sim)) +
  geom_boxplot(alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "none",legend.title = element_blank()) +
  xlab("Scénarios") + ylab("") +
  scale_color_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement"))+
  scale_fill_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement"))

# !! pour dbhmax il faut faire *100 dans le code des graphs pour mettre en centimètre
# !! pour seedmass il faut faire /1000 pour mettre en gramme

# pareil ici il faut run les lignes de code précédentes avec le bon indice de diversité avant de run celles-ci
dbhmax <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
seedmass <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
LMA <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
wsg <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))

figure <- ggarrange(seedmass, dbhmax, LMA, wsg,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "bottom")
figure

annotate_figure(figure, top = text_grob("Comparaison des scénarios pour les CWM de différents traits fonctionnels", face = "bold", size = 16))



####### ACP : Analyses en Composantes Principales ########

# data : forest

# trier les données des forêts :
# on ne garde que la dernière itération

# simulation 00
data2 <- left_join(sim_00_tris@forest, sim_00_tris@inputs$species)
data2 <- filter(data2, data2$iter==7200)
simulation <- "sim_00_10"
data2 <- cbind(simulation, data2)

data0 <- left_join(stack_sim00_seedrain0@forest, stack_sim00_seedrain0@inputs$species)
data0 <- filter(data0, data0$iter==7200)

data1 <- left_join(stack_sim00_tris_seedrain0@forest, stack_sim00_tris_seedrain0@inputs$species)
data1 <- filter(data1, data1$iter==7200)

data_00 <- rbind(data0, data1, data2)
data_00$sim <- "sim_00"
save(data_00, file = "D:/Mes Donnees/TROLL/simulations/sim_00_forest_final.Rdata")


# simulation 01
data0 <- left_join(stack_sim_01_seedrain0@forest, stack_sim_01_seedrain0@inputs$species)
data0 <- filter(data0, data0$iter==7200)

data1 <- left_join(stack_sim_01_bis_seedrain0@forest, stack_sim_01_bis_seedrain0@inputs$species)
data1 <- filter(data1, data1$iter==7200)

data_01 <- rbind(data0, data1)
data_01$sim <- "sim_01"
save(data_01, file = "D:/Mes Donnees/TROLL/simulations/sim_01_forest_final.Rdata")


# simulation 02
data0 <- left_join(stack_sim_02_seedrain0@forest, stack_sim_02_seedrain0@inputs$species)
data0 <- filter(data0, data0$iter==7200)

data1 <- left_join(stack_sim_02_bis_seedrain0@forest, stack_sim_02_bis_seedrain0@inputs$species)
data1 <- filter(data1, data1$iter==7200)

data_02 <- rbind(data0, data1)
data_02$sim <- "sim_02"
save(data_02, file = "D:/Mes Donnees/TROLL/simulations/sim_02_forest_final.Rdata")


# simulation 04
data0 <- left_join(stack_sim_04_seedrain0@forest, stack_sim_04_seedrain0@inputs$species)
data0 <- filter(data0, data0$iter==7200)

data1 <- left_join(stack_sim_04_bis_seedrain0@forest, stack_sim_04_bis_seedrain0@inputs$species)
data1 <- filter(data1, data1$iter==7200)

data_04 <- rbind(data0, data1)
data_04$sim <- "sim_04"
save(data_04, file = "D:/Mes Donnees/TROLL/simulations/sim_04_forest_final.Rdata")


# on combine le tout :

data <- rbind(data_00, data_01, data_02, data_04)
data$sim <- as.factor(data$sim)
save(data, file = "D:/Mes Donnees/TROLL/simulations/sim_forest_final.Rdata")

# on défini les traits fonctionnels qu'on veut pour l'ACP
Pmass <- data$Pmass
Nmass <- data$Nmass
seedmass <- data$s_seedmass
LMA <- data$LMA
ah <- data$ah
hmax <- data$hmax
wsg <- data$wsg
dbhmax <- data$dbhmax
sim <- data$sim
simulation <- data$simulation


# on créer le jeu de données pour l'ACP

acp <- data_frame(Pmass, Nmass, seedmass, LMA, ah, hmax, wsg, dbhmax, sim, simulation)
  
acp <- acp %>% 
  group_by(Pmass, Nmass, seedmass, LMA, ah, hmax, wsg, dbhmax, sim) %>%
  summarise(abond = n()/10) # /10 pour avoir le nb moyen par simulation

acp <- acp %>% 
  group_by(sim) %>% 
  mutate(abond_relative = abond/sum(abond)) %>%  # pour faire une abondance relative par simulation (toutes les sim ont pas le même max d'abond) 
  ungroup()


# on fait l'ACP

autoplot(princomp(select(acp, -sim, -abond, -abond_relative), cor = TRUE),
         data = acp,
         alpha = 1, loadings = T, loadings.label = T, x = 1, y = 2) +
  coord_equal() +
  geom_hline(aes(yintercept = 0), col = 'black', linetype = "dotted") +
  geom_vline(aes(xintercept = 0), col = 'black', linetype = "dotted") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, colour = "black"), axis.text.y = element_text(size = 12, colour = "black")
        , axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15))

sim_bis <- c("contrôle", "fécondité", "recrutement", "fécondité + recrutement")
names(sim_bis) <- c("contrôle", "fécondité", "recrutement", "fécondité + recrutement")

autoplot(princomp(select(acp, -sim), cor = TRUE),
         data = acp, col = "abond_relative", size = "abond_relative",
         alpha = 1, loadings = F, loadings.label = F, x = 1, y = 3) +
  coord_equal() +
  geom_hline(aes(yintercept = 0), col = 'black', linetype = "dotted") +
  geom_vline(aes(xintercept = 0), col = 'black', linetype = "dotted") +
  theme_bw() +
  scale_color_viridis_c() +
  scale_size_continuous(range = c(2, 8)) +
  facet_wrap(~sim, nrow = 2, ncol = 2, labeller = labeller(sim = sim_bis)) +
  theme(legend.position = "bottom", axis.text.x = element_text(size = 20, colour = "black"), axis.text.y = element_text(size = 20, colour = "black")
        , axis.title.y = element_text(size = 24), axis.title.x = element_text(size = 24), legend.title = element_text(size = 10), legend.text = element_text(size = 10))



##### PCoA #####

# richness = sim

data2 <- filter(sim_00_tris@species, sim_00_tris@species$iter==7199)
simulation <- "sim_00_10"
data2 <- cbind(simulation, data2)
save(data, file = "D:/Mes Donnees/TROLL/simulations/sim_00_species_final.txt")


data1 <- filter(stack_sim00_seedrain0@species, stack_sim00_seedrain0@species$iter==7199)
data0 <- filter(stack_sim00_tris_seedrain0@species, stack_sim00_tris_seedrain0@species$iter==7199)
data <- rbind(data1, data0, data2)
save(data, file = "D:/Mes Donnees/TROLL/simulations/sim_01_species_final.csv")

data1 <- filter(stack_sim_01_seedrain0@species, stack_sim_01_seedrain0@species$iter==7199)
data2 <- filter(stack_sim_01_bis_seedrain0@species, stack_sim_01_bis_seedrain0@species$iter==7199)
data <- rbind(data1, data2)
save(data, file = "D:/Mes Donnees/TROLL/simulations/sim_01_species_final.csv")

data1 <- filter(stack_sim_02_seedrain0@species, stack_sim_02_seedrain0@species$iter==7199)
data2 <- filter(stack_sim_02_bis_seedrain0@species, stack_sim_02_bis_seedrain0@species$iter==7199)
data <- rbind(data1, data2)
save(data, file = "D:/Mes Donnees/TROLL/simulations/sim_02_species_final.csv")

data1 <- filter(stack_sim_04_seedrain0@species, stack_sim_04_seedrain0@species$iter==7199)
data2 <- filter(stack_sim_04_bis_seedrain0@species, stack_sim_04_bis_seedrain0@species$iter==7199)
data <- rbind(data1, data2)
save(data, file = "D:/Mes Donnees/TROLL/simulations/sim_04_species_final.Rdata")


data <- read.table(file.choose(), h = T, sep = ";", dec = "." )
nmds <- data[,-(1:2)]
sim <- data$sim
sim <- as.factor(sim)

library(vegan)

# Calcul Bray Curtis (en version metrique pour �tre utilis� dans PCoA). Notes : la plupart des indices non-m�triques utilises en ecologie peuvent le devenir par une transformation racine carr�e (cf CM). Toutefois il est important de verifier a quel point cette transformation peut distordre l'information initiale
# Dissimilarit� de Bray Curtis
bcu=vegdist(nmds,"bray")

# Distance de Bray Curtis
bc=sqrt(vegdist(nmds,"bray"))

# Comparaison des 2 matrices afin d'evaluer si la transformation racine carree a engendre une distorsion des donnees pour rendre la matrice metrique
plot(bc,bcu,xlim=c(0,1), ylim=c(0,1))
abline(0,1)

# Verifier si propriete euclidienne respectee, car sinon la PCoA peut generer des valeurs propres negatives sur les derniers axes (probleme s'ils doivent etre retenus)
is.euclid(bc)


# PCoA
pcbc=dudi.pco(bc, scannf = FALSE, nf = 3)

# Pourcentage associ� aux axes
pourc=round((pcbc$eig/sum(pcbc$eig))*100,2)
pourc
cumsum(pourc)

# Projections des individus
s.label(pcbc$li, sub="Bray curtis")
# Calcul des distances euclidiennes entre objets sur le 1er plan factoriel
de=dist(pcbc$li[,1:2])

# Verifier que les individus sont bien representes (projetes) � partir de la valeur de leur contribution relative

cont=inertia.dudi(pcbc, row.inertia = TRUE)
cos2=abs(cont$row.rel)/10000
par(mfrow = c(1,2))
plot(pcbc$li[,1], cos2[,1])
plot(pcbc$li[,2], cos2[,2])
# Les objets au centre de chaque axe ont des contributions relatives faibles


par(mfrow = c(1,3))
s.class(pcbc$li, sim, col=c("brown1","darkolivegreen3", "turquoise3", "darkorchid2"), clabel = NULL, addaxes = T, cellipse = 3)



#### Calcul indices de Shannon et Piélou #### 

library(vegan)


simulation <- "sim_00_10"

sim_00_tris@species <- cbind(simulation, sim_00_tris@species)
sim_00_tris@inputs$species <- cbind(simulation, sim_00_tris@inputs$species)

pielou_stack_04_bis_seedrain0 <- left_join(stack_sim_04_bis_seedrain0@inputs$species, 
                                              filter(stack_sim_04_bis_seedrain0@species, iter %in% c(seq(from = 0, to = 7199, by = 12))),
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


save(pielou_stack_04_bis_seedrain0, file = "D:/Mes Donnees/TROLL/simulations/pielou_stack_04_bis_seedrain0.Rdata")


shannon_sim0 <- bind_rows(shannon_stack_00, shannon_stack_00_bis, shannon_sim_00_10_seedrain50000)

data_shannon_sim04$sim <- "sim_04"    

pielou_stack_04$iter <- as.numeric(pielou_stack_04$iter)
pielou_stack_04_bis$iter <- as.numeric(pielou_stack_04_bis$iter)
pielou_stack_04_seedrain0$iter <- as.numeric(pielou_stack_04_seedrain0$iter)
pielou_stack_04_bis_seedrain0$iter <- as.numeric(pielou_stack_04_bis_seedrain0$iter)

pielou_sim04_seedrain0 <- bind_rows(pielou_stack_04_seedrain0, pielou_stack_04_bis_seedrain0)

pielou_sim04_seedrain0$iter <- pielou_sim04_seedrain0$iter + max(pielou_stack_04$iter)

data_pielou_sim04 <- bind_rows(pielou_stack_04, pielou_stack_04_bis, pielou_sim04_seedrain0)
data_pielou_sim04$sim <- "sim_04"

save(data_pielou_sim04, file = "D:/Mes Donnees/TROLL/simulations/data_final_pielou_sim04.Rdata")

## beaucoup de bazarre
# télécharger plutôt les fichiers que j'ai enregistré 



###### Graphiques trajectoires et distribution pour Shannon et Pielou ######

load("~/TROLL/simulations/functional/sim_00_functional_final.Rdata")
final$sim <- "sim_00"
final$shannon <- data_shannon_sim00$`X[[i]]`
final$pielou <- data_pielou_sim00$`X[[i]]`

load("~/TROLL/simulations/functional/sim_01_func_final.Rdata")
sim_01_func_final$sim <- "sim_01"
sim_01_func_final$shannon <- data_shannon_sim01$`X[[i]]`
sim_01_func_final$pielou <- data_pielou_sim01$`X[[i]]`

load("~/TROLL/simulations/functional/sim_02_func_final.Rdata")
sim_02_func_final$sim <- "sim_02"
sim_02_func_final$shannon <- data_shannon_sim02$`X[[i]]`
sim_02_func_final$pielou <- data_pielou_sim02$`X[[i]]`


load("~/TROLL/simulations/functional/sim_04_func_final.Rdata")
sim_04_func_final$sim <- "sim_04"
sim_04_func_final$shannon <- data_shannon_sim04$`X[[i]]`
sim_04_func_final$pielou <- data_pielou_sim04$`X[[i]]`


data_traj_div <- rbind(data_pielou_sim00, data_pielou_sim01)
data_traj_div <- rbind(data_traj_div, data_pielou_sim02)
data_traj_div <- rbind(data_traj_div, data_pielou_sim04)

data_pielou_sim04$`X[[i]]`

g1 <- data_traj_div %>% 
  group_by(sim, iter) %>% 
  summarise(l = min(pielou), 
            m = quantile(pielou, 0.5), 
            h = max(pielou)) %>% 
  ggplot(aes(iter/12, m, col = sim, fill = sim)) +
  geom_ribbon(aes(ymin = l, ymax = h), col = NA, alpha = 0.2) +
  geom_line() +
  geom_vline(xintercept = 600, linetype="dashed", color = "black") +
  theme_bw() +
  theme(legend.position = "none", legend.title = element_blank()) +
  xlab("Temps (années)") + ylab("Indice d’équitabilitéde Piélou") +
  scale_color_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement")) +
  scale_fill_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement"))

g2 <- data_traj_div %>% 
  filter(iter == 14376) %>% 
  ggplot(aes(sim, `X[[i]]`, col = sim, fill = sim)) +
  geom_boxplot(alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "none",legend.title = element_blank()) +
  theme(axis.text.x = element_text(size = 15, colour = "black"), axis.text.y = element_text(size = 15, colour = "black")
        , axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), legend.title = element_text(size = 15), legend.text = element_text(size = 15)) +
  xlab("Scénarios") + ylab("Indice d'équitabilité de Pielou") +
  scale_color_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement"))+
  scale_fill_discrete(labels = c("contrôle", "fécondité", "recrutement", "fécondité + recrutement"))


nbsp <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
FRic <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
FEve <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
FDis <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
shannon <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))
pielou <- cowplot::plot_grid(g1, g2, rel_widths = c(2,1))


figure <- ggarrange(nbsp, shannon, pielou, FRic, FEve, FDis,
                    labels = c("A", "B", "C", "D", "E", "F"),
                    ncol = 2, nrow = 3, 
                    common.legend = TRUE, legend = "bottom")
figure

annotate_figure(figure, top = text_grob("Comparaison des scénarios pour la diversité de la forêt", face = "bold", size = 16))

##### Tests de Dunn #####

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


data_traj_div$sim <- as.factor(data_traj_div$sim)
kruskal.test(data_traj_div$shannon ~ data_traj_div$sim)
# Kruskal-Wallis chi-squared = 25855, df = 3, p-value < 2.2e-16
# p-value < 0.05 donc on rejette H0
# au moins une simulation diffère des autres pour l'indice de shannon

library(dunn.test)

dunn.test(data_traj_div$shannon, data_traj_div$sim)

kruskal.test(data_traj_div$pielou ~ data_traj_div$sim)
# Kruskal-Wallis chi-squared = 31412, df = 3, p-value < 2.2e-16

dunn.test(data_traj_div$pielou, data_traj_div$sim)


