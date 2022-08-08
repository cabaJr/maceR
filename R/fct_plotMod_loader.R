#' plotMod_loader 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
load_plots <- function(env, plot_list, session){
  Annotate <- env$env4$Annotate
  plot_list_static <- isolate(plot_list)

    acto_choices <- Annotate$output_list_acto

  acto_selected <- acto_choices[acto_choices$handler  %in% plot_list_static, ]
  if(is.null(acto_selected) == FALSE){
    for(i in seq_len(nrow(acto_selected[, 1]))){
      title = unlist(acto_selected[i, 4])
      pos <- unlist(acto_selected[i, 2])
      module_id <- paste("box_plot_ui_", pos, sep = "")
      plots_out <- mod_plots_server(id ="plots_ui_1", #check this, might be the cause for same title
                                    env = env,
                                    acto_selected = acto_selected,
                                    title = title,
                                    module_id = module_id,
                                    count = i,
                                    pos = pos,
                                    session = session)
    }
  }
}

#' download_obj
#'
#'@description Function to download data tables and plots 
#'
#' @param title Title of the document to download
#' @param location path to object location
#' @param format extension of the file to output ("csv", "png", "2.csv")
#' @param ... any additional to pass to download_obj
#'
#' @return a file to be saved locally
#' @export
#'
#' @examples download_obj(title = "Plot_1", location = Histo_plot, format = "csv")
download_obj <- function(title, location, format, ...){
  extension <- paste(".", format, sep = "")
  object <- downloadHandler(
          filename = function(){paste(title, Sys.Date(), extension, sep = "")},
          content = function(file){#browser()
            switch(format,
                   "csv" = {write.csv(location, file, quote = FALSE, row.names = FALSE)},
                   "2.csv" = {write.csv2(location, file, quote = FALSE, row.names = FALSE)},
                   "png" = {png(file, width = 1820, height = 787, units = "px")
                             print(plot_location)
                             dev.off()
                             }
                   )
            }
        )
  
  ## not functional version of the download switch 
  # switch(format,
  #        "csv" = {
  #          object <- downloadHandler( 
  #            filename = function(){paste(title, Sys.Date(), extension, sep = "")},
  #            content = function(file){write.csv(location, file, quote = FALSE, row.names = FALSE)
  #            }
  #          )
  #        },
  #        "png" = {object <- downloadHandler( 
  #          filename = function(){paste(title, Sys.Date(), extension, sep = "")},
  #          content = function(file){
  #            png(file, width = 1820, height = 787, units = "px")
  #             print(plot_location)
  #             dev.off()
  #          }
  #        )
  #        }
  #        )

  

  return(object)
}
