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

create_documentation <- function(outFile="documentation.html",outDir=getwd(),data=hydraData){
  #data <-  hydraData
  # calls all of the plotting routines
  main_plot_inputs(outDir,data) # path to where pngs will be saved
  # locates the path to the rmd file
  rmdFile <- system.file("rmd",file = "create_documentation.Rmd",package="mshydradata")
  # makes the documentation
  rmarkdown::render(rmdFile,params=list(dir=outDir,data=data),output_file=outFile,output_dir = outDir)

}
