# instaleer de propere lib, want er is geen CRAN repo.
library(devtools)
devtools::install_github("gfinak/cytoRSuite")

# nu unnen we beginnen
library(cytoRSuite)
library(ggcyto)
library(gridExtra)

# cytoRSuite heeft voorbeeld data die laden we
data(Activation, package = "cytoRSuite")
flow_set <- Activation

# Voeg sample names in
pData(flow_set)$Samples <- c("Control","Activated")
channel_info <- colnames(flow_set)
colnames(flow_set)

# maak de GS
gating_set <- GatingSet(flow_set)

# Data compenseren met de compensatie matrix
# Use spillover matrix attached to samples
spill <- flow_set[[1]]@description$SPILL
gating_set <- compensate(gating_set, spill)

# Refer to ?computeSpillover and ?editSpillover to modify spillover matrix

# we moeten Transformeren na compensatie
# Get a list of the fluorescent channels
channels <- getChannels(flow_set)

trans <- estimateLogicle(gating_set[[2]], channels)
gating_set <- transform(gating_set, trans)

### Nu kunnen we gaan gaten ###
# je kan punten plaatsen door te left-clicken.
# Als je klaar bent right-click en sluit de window.
if (interactive()) {
  drawGate(gating_set, # de gating_set
           parent = "root", # waarop we gaan gaten (root is de raw data)
           alias = "cells", # hoe de gate gaat heeten
           channels = c("FSC-A", "SSC-A"), # Welke channels we willen gebuiken`
           type = "polygon", # Welke type gate, voor meer zie: ?`drawGate,GatingSet-method`
           gtfile = "./dataset/flowcyto/Example_gatingTemplate.csv" # Waar we de gating data saven
           )
}

if (interactive()) {
  drawGate(gating_set, # de gating_set
           parent = "cells", # we kunnen gates nemen als de parent
           alias = "single cells", 
           channels = c("FSC-A", "FSC-H"),
           type = "polygon",
           gtfile = "./dataset/flowcyto/Example_gatingTemplate.csv" # Meerdere gates kunnen in dezelfde template file
           )
}

# E.Z.V we kunnen ook de data uit een gating template toepassen op data

gs <- GatingSet(flow_set)
gs <- compensate(gs, spill)
gs <- transform(gs, trans)

# Haal de Example-gatingTemplate.csv op
path <- system.file("extdata", "Example-gatingTemplate.csv", package = "cytoRSuite")
gt <- gatingTemplate(path)

# pas de template toe
gating(gt, gs)

# Check wat we hebben gedaan
getNodes(gs)
plot(gs) # we kunnen de gating stappen plotten

# nu de data visualiseren
autoplot(gs[[2]], bins = 255) # dit is een makkelijke manier om alle te plotten

# handmatig kan ook
ggcyto(gs[[2]], aes("FSC-A", "SSC-A"), subset = "root") +
  geom_point(aes(alpha = 0.5)) + # als je stippen wilt
  geom_gate("Cells") + # de gate zelf
  geom_stats() # percentage van de gate

ggcyto(gs[[2]], aes("FSC-A", "FSC-H"), subset = "Cells") +
  geom_hex(bins = 255) +
  geom_gate("Single Cells") # de gate zelf
  