#'Renders .rmd file to display all model inputs
#'
#'
#'@export

create_documentation <- function(outFile="documentation.html",outDir=getwd()){

  rmdFile <- system.file("rmd",file = "create_documetation.Rmd",package="mshydradata")
  rmarkdown::render(rmdFile,output_file=outFile,output_dir = outDir)
}
