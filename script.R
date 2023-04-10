#########################################################################################################################################
######                                 Script for the statistical analysis and graph generation of:                             #########
######  Diniz, U.M., Aguiar, L.M.S. (2023) Spatiotemporal trends in floral visitation and interaction networks reveal shifting  #########
######             niches for bats in a Neotropical savanna. (Under review, Journal of Animal Ecology, v. 10.04.2023)           #########           
######                                         R Version used: 4.1.0 ("Camp Potanezen")                                         #########
#########################################################################################################################################


################################################################################
#### PREPARATIONS ##############################################################
################################################################################

### Set wroking directory using (depends on which folder the script and data are found in your computer)
setwd("C:/...")

### Loading necessary packages
require(gdata)
require(circular)
require(bipartite)
require(ggplot2)
require(vegan)
require(ggfortify)


################################################################################
### 1. VISUALIZING  PLANT AND BAT PHENOLOGY (VERTICAL GRAPHS) ##################
################################################################################

### Loading necessary data
plantphen <- read.xls("plant_phenology.xlsx", h=T)  #Phenological data of plant species. Months are letter-coded (A-January...)
str(plantphen)
batphen <- read.xls("bat_phenology.xlsx", h=T)  #Phenological data of bats. Months are letter-coded (A-January...)
str(batphen)

### Vertical graphs 

# Plants
plantphen$species  <- factor(plantphen$species ,levels = c("PsiRob", "MimSet", "MerTom", "LamTer", "JusSp", "IpoPro",
                                                           "HipGla", "PseTom", "PseLon", "LueGra", "LafPac", "HymSti", 
                                                           "HymCou", "CarBra", "BauRuf", "BauHol", "BauGoy"))

tiff("plantphenology.tiff", width = 11, height = 10, units = "cm", res = 600)   
ggplot(plantphen, aes(x=month, y=species, size = abundance)) +
  geom_point(alpha=0.7, color = "gray50") + scale_size(range = c(0, 10), name="Abundance") +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0),
        axis.title.y = element_text(size=0),
        axis.title.x = element_text(size=0))
dev.off()

### Bats
batphen$species  <- factor(batphen$species ,levels = c("StuLil", "PlaLin", "MicSch", "DerCin", "DerAnd", "CarPer", "ArtPla", "ArtLit",
                                                       "LonDek", "GloSor", "AnoGeo", "AnoCau"))

tiff("batphenology.tiff", width = 11, height = 10, units = "cm", res = 600) 
ggplot(batphen, aes(x=month, y=species, size = abundance)) +
  geom_point(alpha=0.7, color = "gray50") + scale_size(range = c(0, 8), name="Abundance") +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0),
        axis.title.y = element_text(size=0),
        axis.title.x = element_text(size=0))
dev.off()


################################################################################
### 2. VISUALIZING  PLANT AND BAT PHENOLOGY (CIRCULAR GRAPHS) ##################
################################################################################

### loading general (pooled) phenological data. Months are number-coded (1-January...)
phenology_pool <- read.xls("phenology_general.xlsx", h=T)
str(phenology_pool)

### Plants (chiropterophilous)
tiff("phenchiro.tiff", width = 10, height = 10, units = "cm", res = 600) 
ggplot(phenology_pool, aes(x = month, y = plant_chiro)) +
  geom_col(color="black", fill="salmon") +
  coord_polar() + scale_x_continuous(breaks = seq(0, 12, by =1), 
                                     minor_breaks = seq(0, 12, by = 1)) + 
  theme_linedraw()+
  theme(axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0), 
        panel.border = element_rect(colour = "white", fill=NA, size=0), 
        axis.text.y= element_text(color="black", size=12),
        axis.text.x= element_blank(), legend.position = "none")
dev.off()

### Plants (other syndromes)
tiff("phenothers.tiff", width = 10, height = 10, units = "cm", res = 600) 
ggplot(phenology_pool, aes(x = month, y = plant_other)) +
  geom_col(color="black", fill="mediumpurple1") +
  coord_polar() + scale_x_continuous(breaks = seq(0, 12, by =1), 
                                     minor_breaks = seq(0, 12, by = 1)) + 
  theme_linedraw()+
  theme(axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0), 
        panel.border = element_rect(colour = "white", fill=NA, size=0), 
        axis.text.y= element_text(color="black", size=12),
        axis.text.x= element_blank(), legend.position = "none")
dev.off()

### Bats (nectarivorous)
tiff("phennect.tiff", width = 10, height = 10, units = "cm", res = 600) 
ggplot(phenology_pool, aes(x = month, y = bat_nectar)) +
  geom_col(color="black", fill="salmon") +
  coord_polar() + scale_x_continuous(breaks = seq(0, 12, by =1), 
                                     minor_breaks = seq(0, 12, by = 1)) + 
  theme_linedraw()+
  theme(axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0), 
        panel.border = element_rect(colour = "white", fill=NA, size=0), 
        axis.text.y= element_text(color="black", size=12),
        axis.text.x= element_blank(), legend.position = "none")
dev.off()

### Bats (other guilds)
tiff("phenotherbats.tiff", width = 10, height = 10, units = "cm", res = 600) 
ggplot(phenology_pool, aes(x = month, y = bat_other)) +
  geom_col(color="black", fill="mediumpurple1") +
  coord_polar() + scale_x_continuous(breaks = seq(0, 12, by =1), 
                                     minor_breaks = seq(0, 12, by = 1)) + 
  theme_linedraw()+
  theme(axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0), 
        panel.border = element_rect(colour = "white", fill=NA, size=0), 
        axis.text.y= element_text(color="black", size=12),
        axis.text.x= element_blank(), legend.position = "none")
dev.off()


################################################################################
### 3. CIRCULAR ANALYSIS OF PLANT AND BAT ASSEMBLAGES ##########################
################################################################################

### Chiropterophilous plants
# Transforming linear data into circular data
int_chiro <- rep(phenology_pool$angle, phenology_pool$plant_chiro) 
int.rad_chiro <- rad(int_chiro)
int.circ_chiro <- as.circular(int.rad_chiro)
# Analyzing data
rho.circular(int.circ_chiro)
rayleigh.test(int.circ_chiro)
deg(mean(int.circ_chiro))+360
deg(sd.circular(int.circ_chiro))
# Plotting vector
plot.circular(cbind(int.circ_chiro), rotation ="clock", bins = 80, zero=pi/2, stack=T,units = "rads")
arrows.circular(mean(int.circ_chiro), rho.circular(int.circ_chiro), zero=pi/2, rotation = "clock")

### Plants of other syndromes
int_other <- rep(phenology_pool$angle, phenology_pool$plant_other)
int.rad_other <- rad(int_other)
int.circ_other <- as.circular(int.rad_other)
rho.circular(int.circ_other)
rayleigh.test(int.circ_other)
deg(mean(int.circ_other))+360
deg(sd.circular(int.circ_other))
deg(sd.circular(int.circ_chiro))
plot.circular(cbind(int.circ_other), rotation ="clock", bins = 80, zero=pi/2, stack=T,units = "rads")
arrows.circular(mean(int.circ_other), rho.circular(int.circ_other), zero=pi/2, rotation = "clock")


### Nectarivorous bats
int_nect <- rep(phenology_pool$angle, phenology_pool$bat_nectar)
int.rad_nect <- rad(int_nect)
int.circ_nect <- as.circular(int.rad_nect)
rho.circular(int.circ_nect)
rayleigh.test(int.circ_nect)
deg(mean(int.circ_nect))+360
deg(sd.circular(int.circ_nect))
plot.circular(cbind(int.circ_nect), rotation ="clock", bins = 80, zero=pi/2, stack=T,units = "rads")
arrows.circular(mean(int.circ_nect), rho.circular(int.circ_nect), zero=pi/2, rotation = "clock")

### Other bats
int_otherbats <- rep(phenology_pool$angle, phenology_pool$bat_other)
int.rad_otherbats <- rad(int_otherbats)
int.circ_otherbats <- as.circular(int.rad_otherbats)
rho.circular(int.circ_otherbats)
rayleigh.test(int.circ_otherbats)
deg(mean(int.circ_otherbats))+360
deg(sd.circular(int.circ_otherbats))
plot.circular(cbind(int.circ_otherbats), rotation ="clock", bins = 80, zero=pi/2, stack=T,units = "rads")
arrows.circular(mean(int.circ_otherbats), rho.circular(int.circ_otherbats), zero=pi/2, rotation = "clock")


################################################################################
### 4.SPATIAL DISTRIBUTION OF BAT AND PLANT ASSEMBLAGES ########################
################################################################################

### Bats
pcabats<-read.xls("pca_bats.xlsx",h=T)
str(pcabats)
pca.bats <- pcabats[,3:14]
pca.bats <- rda(pcabats[,3:14], scale=T)
pca.bats
plot(pca.bats)
scalebats <- scale(pcabats[,3:14])
pca_res <- prcomp(scalebats, center = T, scale. = T)

tiff("PCAbats.tiff", width = 15, height = 10, units = "cm", res = 600)  
autoplot(pca_res, label = T, loadings = T, loadings.label = T)+
  theme_classic() +
  theme(axis.title.x = element_text(color="black", face ="bold", size =11),
        axis.title.y = element_text(color="black", face ="bold", size =11), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5), 
        axis.text= element_text(color="black", size=10),
        legend.position ="none")

dev.off()

#### plants
pcaplants<-read.xls("pca_plants.xlsx",h=T)
str(pcaplants)
pca.plants <- rda(pcaplants[,3:19], scale=T)
pca.plants
plot(pca.plants)
scaleplants <- scale(pcaplants[,3:19])
pcap_res <- prcomp(scaleplants, center = T, scale. = T)

tiff("PCAplants.tiff", width = 15, height = 10, units = "cm", res = 600)  
autoplot(pcap_res, label = T, loadings = F, loadings.label = T)+
  theme_classic() +
  theme(axis.title.x = element_text(color="black", face ="bold", size =11),
        axis.title.y = element_text(color="black", face ="bold", size =11), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5), 
        axis.text= element_text(color="black", size=10),
        legend.position ="none")

dev.off()


################################################################################
### 5. SPATIAL NETWORKS, METRICS, AND SPATIAL VARIATION OF RON AND POG #########
################################################################################

### Loading networks and calculating metrics
network_savanna <- read.delim("savanna_net.txt", row.names=1)
networklevel(network_savanna, index="H2")
computeModules(network_savanna, method="Beckett")@likelihood

network_edge <-read.delim("edge_net.txt", row.names = 1)
networklevel(network_edge, index="H2")
computeModules(network_edge, method="Beckett")@likelihood

network_forest<-read.delim("forest_net.txt", row.names = 1)
networklevel(network_forest, index="H2")
computeModules(network_forest, method="Beckett")@likelihood

### Plotting networks
tiff("spatial_networks.tiff", width = 15, height = 10, units = "cm", res = 600)
par(mfrow=c(3,1))
plotweb(network_savanna, col.interaction = "grey90", bor.col.interaction = "grey50", labsize = 0.5,
        bor.col.high = "white", bor.col.low = "white", method = "normal")

plotweb(network_edge, col.interaction = "grey90", bor.col.interaction = "grey50", labsize = 0.5,
        bor.col.high = "white", bor.col.low = "white",ybig = 1,method = "normal")

plotweb(network_forest,col.interaction = "grey90", bor.col.interaction = "grey50", labsize = 0.5,
        bor.col.high = "white", bor.col.low = "white",ybig = 1,method = "normal")
dev.off()

### Plotting POG and RON according to habitats
spatial_trends <- read.xls("spatial_means.xlsx", h=T) #loading averaged POG and RON data
str(spatial_trends)
ggplot(spatial_trends, aes(x=order, y=mean, group=variable, color=variable)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, lwd=1)+
  geom_line(lwd=1.2) + geom_point(cex =5)+
  theme_classic()+
  scale_color_manual(values=c("mediumpurple1", "salmon"))+
  theme(axis.title.x = element_text(color="black", face ="bold", size =11),
        axis.title.y = element_text(color="black", face ="bold", size =11), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5), 
        axis.text= element_text(color="black", size=15),
        legend.position ="none")

### ANOVAs RON & POG
spatial <- read.xls("spatial_raw.xlsx", h=T) #loading raw POG and RON data

aov_ron <- aov(spatial$ron~spatial$type)
summary(aov_ron)
TukeyHSD(aov_ron)

aov_pog <- aov(spatial$pog~spatial$type)
summary(aov_pog)
TukeyHSD(aov_pog)


################################################################################
### 6. TEMPORAL NETWORKS, METRICS, AND TEMPORAL VARIATION OF RON AND POG #######
################################################################################

### Loading networks and calculating metrics
network_dry <- read.delim("dry.txt", row.names=1)
networklevel(network_dry, index="H2")
computeModules(network_dry, method="Beckett")@likelihood

network_drywet <- read.delim("dry-wet.txt", row.names=1)
networklevel(network_drywet, index="H2")
computeModules(network_drywet, method="Beckett")@likelihood

network_wet <- read.delim("wet.txt", row.names=1)
networklevel(network_wet, index="H2")
computeModules(network_wet, method="Beckett")@likelihood

network_wetdry <- read.delim("wet-dry.txt", row.names=1)
networklevel(network_wetdry, index="H2")
computeModules(network_wetdry, method="Beckett")@likelihood

### Plotting networks

tiff("temporal_networks.tiff", width = 15, height = 10, units = "cm", res = 600)
par(mfrow=c(4,1))
plotweb(network_dry,col.interaction = "grey90", bor.col.interaction = "grey50", labsize = 0.5,
        bor.col.high = "white", bor.col.low = "white",ybig = 1, method="normal")
plotweb(network_drywet,col.interaction = "grey90", bor.col.interaction = "grey50", labsize = 0.5,
        bor.col.high = "white", bor.col.low = "white",ybig = 1, method = "normal")
plotweb(network_wetdry,col.interaction = "grey90", bor.col.interaction = "grey50", labsize = 0.5,
        bor.col.high = "white", bor.col.low = "white",ybig = 1, method = "normal")
plotweb(network_wet,col.interaction = "grey90", bor.col.interaction = "grey50", labsize = 0.5,
        bor.col.high = "white", bor.col.low = "white",ybig = 1, method="normal")
dev.off()
?plotweb

### Plotting POG and RON according to seasons
temporal_means <- read.xls("temporal_means.xlsx", h=T) # loading averaged POG and RON data

tiff("temporal_rontiff", width = 10, height = 10, units = "cm", res = 600) 
ggplot(temporal_means, aes(x = month, y = ron)) +
  geom_col(color="black", fill="salmon") +
  coord_polar() + scale_x_continuous(breaks = seq(0, 12, by =1), 
                                     minor_breaks = seq(0, 12, by = 1)) + 
  theme_linedraw()+
  theme(axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0), 
        panel.border = element_rect(colour = "white", fill=NA, size=0), 
        axis.text.y= element_text(color="black", size=12),
        axis.text.x= element_blank(), legend.position = "none")
dev.off()

tiff("temporal_pog.tiff", width = 10, height = 10, units = "cm", res = 600) 
ggplot(temporal_means, aes(x = month, y = pog)) +
  geom_col(color="black", fill="mediumpurple1") +
  coord_polar() + scale_x_continuous(breaks = seq(0, 12, by =1), 
                                     minor_breaks = seq(0, 12, by = 1)) + 
  theme_linedraw()+
  theme(axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0), 
        panel.border = element_rect(colour = "white", fill=NA, size=0), 
        axis.text.y= element_text(color="black", size=12),
        axis.text.x= element_blank(), legend.position = "none")
dev.off()

### Circular analysis of RON and POG

# RON
int_ron <- rep(temporal_means$angle, temporal_means$ron_transf)
int.rad_ron <- rad(int_ron)
int.circ_ron <- as.circular(int.rad_ron)
rho.circular(int.circ_ron)
rayleigh.test(int.circ_ron)
deg(mean(int.circ_ron))+360
deg(sd.circular(int.circ_ron))
par(mfrow=c(1,1))
plot.circular(cbind(int.circ_ron), rotation ="clock", bins = 80, zero=pi/2, stack=T,units = "rads")
arrows.circular(mean(int.circ_ron), rho.circular(int.circ_ron), zero=pi/2, rotation = "clock")

# POG
int_pog <- rep(temporal_means$angle, temporal_means$pog_transf)
int_pog <- rad(int_pog)
int_pog <- as.circular(int_pog)
rho.circular(int_pog)
rayleigh.test(int_pog)
deg(mean(int_pog))
deg(sd.circular(int_pog))
plot.circular(cbind(int_pog), rotation ="clock", bins = 80, zero=pi/2, stack=T,units = "rads")
arrows.circular(mean(int_pog), rho.circular(int_pog), zero=pi/2, rotation = "clock")