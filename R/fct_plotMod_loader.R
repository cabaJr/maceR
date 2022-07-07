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
      plots_out <- mod_plots_server(id = "plots_ui_1",
                                    env = env,
                                    acto_selected = acto_selected,
                                    title = title,
                                    module_id = module_id,
                                    count = i,
                                    pos = pos,
                                    session = session)
      # return(plots_out)
    }
  }
  # return(plots_out)
}