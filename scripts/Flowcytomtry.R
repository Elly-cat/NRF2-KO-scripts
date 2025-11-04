library(CytoPipeline)

# raw data
# Halen we uit de CytoPipeline package
# output files
workDir <- suppressMessages(base::tempdir())

jsonDir <- rawDataDir

# main parameters : sample files and output files

experimentName <- "OMIP021_PeacoQC"
sampleFiles <- file.path(rawDataDir, list.files(rawDataDir,
                                                pattern = "Donor"))
# creation on CytoPipeline object,
# using json file as input
pipL_flowAI <-
  CytoPipeline(file.path(jsonDir, "OMIP021_flowAI_pipeline.json"),
               experimentName = "OMIP021_flowAI",
sampleFiles = sampleFiles)

execute(pipL_flowAI, path = workDir)

# plot work flow graph - flowAI - scale transformList
plotCytoPipelineProcessingQueue(
  pipL_flowAI,
whichQueue = "scale transform",
path = workDir)

# Check welke objecten gemaakt zijn en waar ze uit bestaan
getCytoPipelineObjectInfos(pipL_flowAI, 
                           path = workDir,
whichQueue = "scale transform")

getCytoPipelineObjectInfos(pipL_flowAI, 
                           path = workDir,
whichQueue = "pre-processing",
sampleFile = sampleFiles(pipL_flowAI)[1])

## Vistueel met de data werken via shinny app ###
# handig voor begrijpen wat de pipeline heeft gedaan
# uncomment om daar te kijken

library(CytoPipelineGUI)
if (interactive())
  CytoPipelineCheckApp(dir = workDir) 

if  (interactive())
  ScaleTransformApp(dir = workDir)


### Vistualiseren van de cytometrie data ###
# de data zit in een flow frame (ff) die moeten we extahere als volgt
# single flow frame
# raw reads
ff_raw <- getCytoPipelineFlowFrame(
  pipL_flowAI,
whichQueue = "pre-processing",
sampleFile = 1,
objectName = "flowframe_read_obj", #dit komt uit `getCytoPipelineObjectInfos`
path = workDir
)

# geen douplets
ff_de_douplet <- getCytoPipelineFlowFrame(
  pipL_flowAI,
whichQueue = "pre-processing",
sampleFile = 1,
objectName = "remove_doublets_obj", #dit komt uit `getCytoPipelineObjectInfos`
path = workDir
)

# plot de data
library(gridExtra)
library(plotly)
gg_raw <- ggplotEvents(ff_raw, yChannel = "FSC-H", xChannel = "FSC-A") + 
  ggtitle("raw")
gg_de_douplet <- ggplotEvents(ff_de_douplet, yChannel = "FSC-H", xChannel = "FSC-A") +
  ggtitle("de douplet")
gg_grid <- grid.arrange(gg_raw, gg_de_douplet, ncol = 2)
ggplotly(gg_raw)
