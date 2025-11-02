library(CytoPipeline)

# raw data
# Halen we uit de CytoPipeline package
rawDataDir <- system.file("extdata", package = "CytoPipeline")
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

### Vistueel met de data werken via shinny app ###
# handig voor begrijpen wat de pipeline heeft gedaan
library(CytoPipelineGUI)

if (interactive())
  CytoPipelineCheckApp(dir = workDir) 

if  (interactive())
  ScaleTransformApp(dir = workDir)
