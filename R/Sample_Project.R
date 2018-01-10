sample_project <- function(){

  # Copy sample run file
  SrcDFile = system.file("extdata", "Sample_Project.zip", package = "rAnalysis4CC")
  DstDFile = file.path(getwd(), "Sample_Project.zip")
  if(!file.exists(DstDFile)) file.copy(SrcDFile, DstDFile)
  unzip ("Sample_Project.zip", exdir = ".")
  unlink("Sample_Project.zip")


}
