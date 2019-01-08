#'Renders .rmd file to display all model inputs
#'
#'
#'@export

create_documentation <- function(outFile="shit.html",outDir=getwd()){

  rmdFile <- system.file("rmd",file = "main_produceInputFigures.Rmd",package="mshydradata")
  rmarkdown::render(rmdFile,output_file=outFile,output_dir = outDir)
}
