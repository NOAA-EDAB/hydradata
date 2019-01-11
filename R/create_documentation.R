#'Renders .rmd file to display all model data inputs
#'
#'Figures and text are created to document the data used in the particular run of the model
#'
#'@param outFile Name of the html file to be output (Default = documentation.html)
#'@param outDir Path to the location of where the html should be saved
#'
#'@section :Dependencies
#'An rmd file is rendered (using rmarkdown) to produce the html file.
#'
#'@export

create_documentation <- function(outFile="documentation.html",outDir=getwd()){
  main_plot_inputs(outPath=outDir)
  rmdFile <- system.file("rmd",file = "create_documentation.Rmd",package="mshydradata")
  rmarkdown::render(rmdFile,params=list(dir=outDir),output_file=outFile,output_dir = outDir)

}
