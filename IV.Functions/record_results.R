box::use(magrittr[...])
box::use(arrow[...])


#' Write results
#' Function to conveniently write results of various iterations to disk
#' Files are exported as parquet files and can be opened with the open_dataset() function from arrow.
#'
#' @param x tabular data to be written to disk
#' @param country country of the analysis
#' @param iter iteration number
#' @param path path to write dataset to
#' @param analysis_name name of the analysis
#'
#' @return
#' @export
#'
#' @examples
save <- function(x, path, analysis_name){
  #generate folder name for file
  folder_path <- stringr::str_c(path, "/vac_strategy=", 
                               analysis_name)
  #create folder to store file
  if(!(dir.exists(folder_path))){
    dir.create(folder_path, recursive = TRUE)
  }
  
  #create path of final file
  final_path <- stringr::str_c(folder_path, "/",
                               "part-0.parquet")
  write_parquet(x, 
                sink = final_path,
                version = "2.6")
}
