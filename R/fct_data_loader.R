#' preload_data
#'
#' @param App_settings to access App_settings object
#' @param skipRows numeric, rows to skip when importing data
#'
#' @description A function to create the Raw_mouse_data and preload the 
#'     uploaded files inside of it, decomposing the metadata file and the 
#'     data files.
#' 
#' @return No returned value to the user. Internal function
#'
#' @noRd

preload_data <- function(App_settings, skipRows = 4){
  myMice <- NULL
  for(i in seq_len(length.out = length(App_settings$dataList$name))) {
    myMice[[i]] <- Raw_mouse_data$new()
    myMice[[i]]$add(App_settings$dataList$datapath[i], App_settings$metadata$datapath, skipRows)
  }
  App_settings$env1 <- pryr::where("myMice") #store env of myMice inside App_settings
}

#' load_data
#'
#' @param env App_settings environment
#' @description creates Clean_mouse_data object and stores into myCleanMice list
#' @export
#'
load_data <- function(env){
    App_settings <- env
    myMice2 <- App_settings$env1$myMice
    myCleanMice <- list()
    for (i in seq_len(length.out = length(App_settings$dataList$name))){
      myCleanMice[[i]] <- Clean_mouse_data$new()
      myCleanMice[[i]]$compile(myMice2[[i]])
      myCleanMice[[i]]$addData(myMice2[[i]], App_settings)
    }
    env$env2 <- pryr::where("myCleanMice")
    App_settings$setListMice(env)
      Custom_tables <- Custom_tables$new()
    Custom_tables$compile(App_settings$env2)
    App_settings$env3 <- pryr::where("Custom_tables")
      Annotate <- Annotate$new()
    App_settings$env4 <- pryr::where("Annotate")
}

#' upload_subsetting
#'
#' @param funEnv connects to App_settings object
#' @param session session object
#'
#' @examples upload_subsetting(App_settings)
upload_subsetting <- function(funEnv, session, input_result){
  App_settings <- funEnv
  #import data
  listMice <- App_settings$listMice[,2]
  metadata <- App_settings$env2$Annotate$metaTable
  #update subsetting fields
  updateSelectInput(session, "idSubsetList", choice = c("choose" = "", listMice), selected = NULL)
  listSex <- unique(metadata$Sex)
  updateSelectInput(session, "sexSubsetList", choices = c("choose" = "", listSex), selected = NULL)
  listGenotype <- unique(metadata$Genotype)
  updateSelectInput(session, "geneSubsetList", choices = c("choose" = "", listGenotype), selected = NULL)
  listCabinet <- unique(metadata$Cabinet)     #using unique instead of levels because Cabinet is not considered factor
  updateSelectInput(session, "cabSubsetList", choices = c("choose" = "", listCabinet), selected = NULL)
  #update subsetting slider
  max1 <- round((max(metadata$Datapoints)/1440), 3)
  updateSliderInput(session, "timeSubset", max = max1, value = c(0, max1))
}