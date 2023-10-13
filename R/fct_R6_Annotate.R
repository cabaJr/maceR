#' Annotate 
#'
#' @description R6 class object used for generating and storing plots and other 
#'     processed data. It is accessed internally to display data. Each top level 
#'     list contains a different type of plot. Sublists are organised to store 
#'     different plot versions.
#'     Functions for plotting (i.e. plot_actogram, plot_DAct) get data from 
#'     fct_R6_Custom_tables object and generate a plot that is saved into the 
#'     relative list. 
#'     There should be a method to pass plots to an external container based on
#'     the handler.
#'
#' @return An Annotate object
#'
#' @noRd
Annotate <- R6::R6Class("Annotate",
                    list(
#' @field Actograms stores actograms generated via plot_actogram(), using ggetho 
#' function
#' @field DPActograms stores Double plotted actograms generated via 
#' plot_actogram(), using ggetho function
#' @field DAct_plots stores line plots summarizing total daily activity
#' @field period_plots stores periodogram power plot generated via 
#' plot_periodogram(), using ggetho::ggperio
#' @field period_plots_box stores histogram plots of period length. Generated
#' via plot_periodogram_hist()
#' @field avg_day_plots list containing average circadian day line plots
#' @field output_list_acto table with combination of output handler, 
#' destination, location, title, data, and file label
#' @field actTable Table containing data assembled using showData()
#' @field metaTable Table containing data assembled using showMeta()
#' @field cacheKeys list of cacheKeys to compare rendering of new plots and 
#' avoid if it already present. Currently not in use
                      Actograms = list(acto1 = list(),
                                          acto2 = list(),
                                          acto3 = list(),
                                          acto4 = list()
                    ),
                    DPActograms = list(DPacto1 = list(),
                                       DPacto2 = list(),
                                       DPacto3 = list(),
                                       DPacto4 = list(),
                                       DPacto5 = list()
                    ),
                    DAct_plots = list(DAct1 = list(),
                                      DAct2 = list(),
                                      DAct3 = list(),
                                      DAct4 = list(),
                                      DAct5 = list(),
                                      DAct6 = list()
                    ),
                    period_plots = list(Per1 = list(),
                                        Per2 = list(),
                                        Per3 = list(),
                                        Per4 = list(),
                                        Per5 = list()
                    ),
                    period_plots_box = list(Per1_box = list(),
                                        Per2_box = list(),
                                        Per3_box = list(),
                                        Per4_box = list(),
                                        Per5_box = list()
                    ),
                    avg_day_plots = list(AvgDay1 = list()
                      
                    ),
                    output_list_acto = dplyr::tibble(
                      # unique keyword that identifies the plots that have been requested from the user in the Analysis tab (see mod_analysis)
                      "handler" = c("total", "sex", "genotype", "cabinet",
                                    "DAtotal", "DAsex", "DAgenotype", "DAcabinet",
                                    "~gen", "~sex", "individual", "gen~sex", "indiv+sex~gen", "indiv+cab~gen",
                                    "Pertotal", "Perfaceted", "Persex", "Pergenotype", "Percabinet",
                                    "Pertotal_hist", "Perfaceted_hist", "Persex_hist", "Pergenotype_hist", "Percabinet_hist",
                                    "individualAvgD", "sexAvgD", "genotypeAvgD"),
                      #list of placeholders in the mod_plots file where the boxes are created
                      "destination" = c("acto1", "acto2", "acto3", "acto4",
                                        "DPacto1", "DPacto2", "DPacto3", "DPacto4",
                                        "DAct1", "DAct2", "DAct3", "DAct4", "DAct5", "DAct6",
                                        "Per1", "Per2", "Per3", "Per4", "Per5",
                                        "Per1_boxplot", "Per2_boxplot", "Per3_boxplot", "Per4_boxplot", "Per5_boxplot",
                                        "AvgDay1", "AvgDay2", "AvgDay3"),
                      ## list containing the location of each plot in Annotate object 
                      "location" = list(
                                     ## Actograms
                                     "Annotate$Actograms$acto1[[1]]", "Annotate$Actograms$acto2[[1]]",
                                     "Annotate$Actograms$acto3[[1]]", "Annotate$Actograms$acto4[[1]]",
                                     ## DPActograms
                                     "Annotate$DPActograms$DPacto1[[1]]", "Annotate$DPActograms$DPacto2[[1]]", 
                                     "Annotate$DPActograms$DPacto3[[1]]", "Annotate$DPActograms$DPacto4[[1]]", 
                                     ## Daily activity
                                     "Annotate$DAct_plots$DAct1", "Annotate$DAct_plots$DAct2", 
                                     "Annotate$DAct_plots$DAct3", "Annotate$DAct_plots$DAct4", 
                                     "Annotate$DAct_plots$DAct5", "Annotate$DAct_plots$DAct6", 
                                     ## Periodograms
                                     "Annotate$period_plots$Per1", "Annotate$period_plots$Per2", 
                                     "Annotate$period_plots$Per3", "Annotate$period_plots$Per4", 
                                     "Annotate$period_plots$Per5",
                                     ## Period boxplots
                                     "Annotate$period_plots_box$Per1_box", "Annotate$period_plots_box$Per2_box", 
                                     "Annotate$period_plots_box$Per3_box", "Annotate$period_plots_box$Per4_box", 
                                     "Annotate$period_plots_box$Per5_box",
                                     ## Average daily activity
                                     "Annotate$avg_day_plots[1][[1]]", "Annotate$avg_day_plots[2][[1]]",
                                     "Annotate$avg_day_plots[3][[1]]"
                                     ),
                      # Title to be assigned to each box
                      "title" = c(
                                  ## Actograms
                                  "Actogram - all animals", "Actogram - split by sex",
                                  "Actogram - split by genotype", "Actogram - split by cabinet",
                                  ## DPActograms
                                  "Double plotted actogram - all animals", "Double plotted actogram - split by sex",
                                  "Double plotted actogram - split by genotype", "Double plotted actogram - split by cabinet",
                                  ## Daily Activity
                                  "Sum of daily activity - Genotype", "Sum of daily activity - Sex", 
                                  "Sum of daily activity - Genotype ~ id", "Sum of daily activity - Genotype ~ sex", 
                                  "Sum of daily activity - Sex ~ Genotype", "Sum of daily activity - Cabinet ~ Genotype", 
                                  ## Periodograms
                                  "Periodogram - cumulative", "Periodogram - individual", 
                                  "Periodogram - by sex", "Periodogram - by genotype", 
                                  "Periodogram - by cabinet",
                                  ## Periodogram boxplot
                                  "Period boxplot - cumulative", "Period boxplot - individual", 
                                  "Period boxplot - by sex", "Period boxplot - by genotype", 
                                  "Period boxplot - by cabinet",
                                  ## Average day of activity
                                  "Average daily Activity - Individual", "Average daily Activity - Sex",
                                  "Average daily Activity - Genotype"),
                      #Location of each table containing data
                      "data" = c("Custom_tables$locomotor_act[[2]]", "Custom_tables$locomotor_act[[3]]", "Custom_tables$locomotor_act[[4]]", "Custom_tables$locomotor_act[[2]]",
                                 "Custom_tables$locomotor_act[[2]]", "Custom_tables$locomotor_act[[3]]", "Custom_tables$locomotor_act[[4]]", "Custom_tables$locomotor_act[[2]]",
                                 "Custom_tables$daily_act[[2]]", "Custom_tables$daily_act[[2]]", "Custom_tables$daily_act[[2]]",
                                 "Custom_tables$daily_act[[2]]", "Custom_tables$daily_act[[2]]", "Custom_tables$daily_act[[2]]",
                                 #changed to 2 to get only the first peak. table [[1]] contains all peaks
                                 "Custom_tables$periodograms[[2]]", "Custom_tables$periodograms[[2]]", "Custom_tables$periodograms[[2]]", 
                                 "Custom_tables$periodograms[[2]]", "Custom_tables$periodograms[[2]]",
                                 "Custom_tables$periodograms[[2]]", "Custom_tables$periodograms[[2]]", "Custom_tables$periodograms[[2]]",
                                 "Custom_tables$periodograms[[2]]", "Custom_tables$periodograms[[2]]",
                                 "Custom_tables$average_day[[2]]",  "Custom_tables$average_day[[2]]",
                                 "Custom_tables$average_day[[2]]"),
                      #partial label to be assigned to the saved file
                      "file_label" = c("Counts_table_id", "Counts_table_id_sex", "Counts_table_id_gen", "Counts_table_id",
                                       "Counts_table_id", "Counts_table_id_sex", "Counts_table_id_gen", "Counts_table_id",
                                       "Total_daily_act", "Total_daily_act", "Total_daily_act",
                                       "Total_daily_act", "Total_daily_act", "Total_daily_act",
                                       "Period_peaks", "Period_peaks", "Period_peaks", 
                                       "Period_peaks", "Period_peaks",
                                       "Period_peaks_boxplot", "Period_peaks_boxplot", "Period_peaks_boxplot", 
                                       "Period_peaks_boxplot", "Period_peaks_boxplot",
                                       "Average_day", "Average_day",
                                       "Average_day")
                    ),
                    actTable = NULL,
                    metaTable = NULL,
                    cacheKeys = dplyr::tibble("table" = seq(1:8), #table to store keys of hashed tables when plotting
                                       "key" = 0), #to be implemented
#' showMeta
#' @description function to create a table that arranges all the available 
#' metadata in a table, and saves it in metaTable var
#' @param env App_settings environment to access myCleanMice object
#'
#' @return no return
#' 
                    showMeta = function(env){
                      # browser()
                      myCleanMice <- env$myCleanMice
                      d1 <- dplyr::tibble(
                        "id" = as.character(),
                        "Sex" = as.character(),
                        "Genotype" = as.character(),
                        "Cabinet" = as.character(),
                        "Light_On" = as.character(),
                        "Datapoints" = as.character()
                      )
                      for(h in seq_len(length(myCleanMice))){
                        d2 <- dplyr::tibble(
                          "id" = myCleanMice[[h]]$id,
                          "Sex" = myCleanMice[[h]]$sex,
                          "Genotype" = myCleanMice[[h]]$genotype,
                          "Cabinet" = myCleanMice[[h]]$cabinet,
                          "Light_On" = myCleanMice[[h]]$lightOn,
                          "Datapoints" = myCleanMice[[h]]$length
                        )
                        d1 <- rbind(d1, d2)
                      }
                      self$metaTable <- d1
                    },

#' showdata
#'
#' @description function to create a table that arranges activity data in a 
#' table, and saves it in actTable var
#' @param env App_settings environment to access myCleanMice object
#' @param id mouse Id value from the list of Ids or "All" to display all data 
#' @param miceList list of all available mouse Ids
#' @return no return
#'
                    showData = function(env, id, miceList){
                      myCleanMice <- env$myCleanMice
                        d2 <- dplyr::tibble(
                          "id" = as.character(),
                          "Sex" = as.character(),
                          "Genotype" = as.character(),
                          "Real time" = as.character(),
                          "Time point" = as.character(),
                          "Days" = as.character(),
                          "Activity" = as.character()
                        )
                        if (id == "All"){
                        for (h in seq_len(length(myCleanMice))){
                          d1 <- dplyr::tibble(
                            "id" = myCleanMice[[h]]$id,
                            "Sex" = myCleanMice[[h]]$sex,
                            "Genotype" = myCleanMice[[h]]$genotype,
                            "Real time" = myCleanMice[[h]]$realTime,
                            "Time point" = myCleanMice[[h]]$timepoint,
                            "Days" = round(((myCleanMice[[h]]$timepoint)/86400), 2),
                            "Activity" = myCleanMice[[h]]$countsMinute
                          )
                          d2 <- rbind(d2, d1)
                        }

                      }else if(id == ""){

                      }else{
                        id <- id
                        miceList <- miceList
                        y <- which(miceList$id == id)
                        h <- as.numeric(miceList[y, 1])
                        d2 <- dplyr::tibble(
                          "id" = myCleanMice[[h]]$id,
                          "Sex" = myCleanMice[[h]]$sex,
                          "Genotype" = myCleanMice[[h]]$genotype,
                          "Real time" = myCleanMice[[h]]$realTime,
                          "Time point" = myCleanMice[[h]]$timepoint,
                          "Days" = round(((myCleanMice[[h]]$timepoint)/86400), 2),
                          "Activity" = myCleanMice[[h]]$countsMinute
                        )
                      }
                      self$actTable <- d2
                    },
#                     
#' plot_actogram
#'
#' @description function to generate actogram plots arranged in different 
#' configurations and store them in Actograms list
#' @param env App_settings environment to access Custom_tables object
#' @param type plot type to be rendered (list available in output_list_acto$handler[1:4])
#'
                    # add one option that increases summary time window when the data are longer than x
                    # also explore the function of time_wrap
                    # check if you implemented a solution that was available using time_offset

                    plot_actogram = function(env, type){   #access data to env2 (where custom_tables object is stored). env2 should contain myCleanMice objects
                      data <- env$env2$Custom_tables$locomotor_act[[1]]
                      # len <- length(env$App_settings$dataList$name)
                      # Llpha <- (0.4 / len)
                      LDcond <- env$LDcondition
                      if(type == "total"){
                        plot <- ggetho::ggetho(data, mapping = ggplot2::aes(x = t, y = id, z = Activity), summary_time_window = 600)+
                          ggetho::stat_bar_tile_etho()+
                          LDcond$SLLD+
                          LDcond$SLDD+
                          ggplot2::ggtitle("Full length actogram ")
                        self$Actograms$acto1[[1]] <- plot
                      }else if(type == "sex"){
                        # plot <- ggetho::ggetho(data, mapping = ggplot2::aes(x = t, y = id, z = Activity), summary_time_window = 180)+
                        #   ggetho::stat_bar_tile_etho()+
                        #   LDcond$SLLD+
                        #   LDcond$SLDD+
                        #   ggplot2::ggtitle("Full length actogram ")
                        plot <- ggetho::ggetho(data, ggplot2::aes(x = t, y = id, z = Activity), summary_time_window = 180) +
                          LDcond$SLLD+
                          LDcond$SLDD+
                          ggetho::stat_bar_tile_etho()+
                          ggplot2::facet_grid(sex ~ ., space = "free", scales = "free_y")+
                          ggplot2::ylab("")+
                          ggplot2::ggtitle("Actogram", subtitle = "Splitted by sex")
                        self$Actograms$acto2[[1]] <- plot
                      }else if(type == "genotype"){
                        plot <- ggetho::ggetho(data, ggplot2::aes(x = t, y = id, z = Activity), summary_time_window = 180) +
                          ggetho::stat_bar_tile_etho()+
                          ggplot2::facet_grid(Genotype ~ ., space = "free", scales = "free_y")+
                          ggplot2::ylab("")+
                          LDcond$SLLD+
                          LDcond$SLDD+
                          ggplot2::ggtitle("Actogram", subtitle = "Splitted by genotype")
                        self$Actograms$acto3[[1]] <- plot
                      }else if(type == "cabinet"){
                        plot <- ggetho::ggetho(data, ggplot2::aes(x = t, y = id, z = Activity), summary_time_window = 180) +
                          LDcond$SLLD+
                          LDcond$SLDD+
                          ggetho::stat_bar_tile_etho()+
                          ggplot2::facet_grid(Cabinet ~ ., space = "free", scales = "free_y")+
                          ggplot2::ylab("")+
                          ggplot2::ggtitle("Actogram", subtitle = "Splitted by cabinet")
                        self$Actograms$acto4[[1]] <- plot
                      }
                      # self$cacheKeys[1,2] <- data.table::copy(x$env2$Custom_tables$cacheKeys[1,2])
                    },


#' plot_DPactogram
#'
#' @description function to generate double plotted actogram plots arranged in 
#' different configurations and store them in DPActograms list
#' @param env App_settings environment to access Custom_tables object
#' @param type plot type to be rendered (list available in output_list_acto$handler[4:8])
#' 
                    plot_DPactogram = function(env, type){ #"total", "sex", "genotype", "cabinet", "individual"
                      data <- env$env2$Custom_tables$locomotor_act[[1]]
                      # len <- as.numeric(length(env$App_settings$dataList$name))
                      # lenD <-  env$Custom_tables$metadata$Data_length[1]/1440
                      # Llpha <- (0.4 / (len*lenD))
                      LDcond <- env$LDcondition
                      # browser()
                      if(type == "DAtotal"){
                        plot <- ggetho::ggetho(data, ggplot2::aes(x = t, z = Activity), multiplot = 2, summary_time_window = 120)+
                          LDcond$DPLD+
                          LDcond$DPDD1+
                          LDcond$DPDD2+
                          # stat_ld_annotations(height = 1, alpha = Llpha, outline = NA, period = hours(24), l_duration = hours(12), phase = 0, ld_colours = c(NA, "black"))+
                          ggetho::stat_bar_tile_etho()+
                          ggplot2::facet_wrap(~id+sex+Genotype, ncol = 4, labeller = ggplot2::label_wrap_gen(multi_line=FALSE))+
                          ggplot2::ylab("")+
                         ggplot2::ggtitle("Double plotted actogram", subtitle = "splitted by ID")
                        self$DPActograms$DPacto1[[1]] <- plot
                      }else if(type == "DAsex"){
                        plot <- ggetho::ggetho(data, ggplot2::aes(x = t, z=Activity),
                                       summary_time_window = 120,
                                       multiplot = 2) +
                          LDcond$DPLD+
                          LDcond$DPDD1+
                          LDcond$DPDD2+
                          # stat_ld_annotations(height = 1, alpha = Llpha, outline = NA, period = hours(24), l_duration = hours(12), phase = 0, ld_colours = c(NA, "black"))+
                          ggetho::stat_bar_tile_etho()+
                          ggplot2::facet_grid(sex~Genotype)+
                          ggplot2::ylab("")+
                         ggplot2::ggtitle("Double plotted actogram", subtitle = "averaged by sex vs genotype, time window per each bin is 5'")
                        self$DPActograms$DPacto2[[1]] <- plot
                      }else if(type == "DAgenotype"){
                        plot <- ggetho::ggetho(data,ggplot2::aes(x = t, z=Activity),
                                       summary_time_window = 120,
                                       multiplot = 2)+
                          LDcond$DPLD+
                          LDcond$DPDD1+
                          LDcond$DPDD2+
                          # stat_ld_annotations(height = 1, alpha = Llpha, outline = NA, period = hours(24), l_duration = hours(12), phase = 0, ld_colours = c(NA, "black"))+
                          ggetho::stat_bar_tile_etho()+
                          ggplot2::facet_grid(Cabinet~Genotype)+
                          ggplot2::ylab("")+
                         ggplot2::ggtitle("Double plotted actogram", subtitle = "averaged by cabinet vs genotype, time window per each bin is 5'")
                        self$DPActograms$DPacto3[[1]] <- plot
                      }else if(type == "DAcabinet"){
                        plot <- ggetho::ggetho(data,ggplot2::aes(x = t, z=Activity),
                                       summary_time_window = 120,
                                       multiplot = 2)+
                          LDcond$DPLD+
                          LDcond$DPDD1+
                          LDcond$DPDD2+
                          # stat_ld_annotations(height = 1, alpha = Llpha, outline = NA, period = hours(24), l_duration = hours(12), phase = 0, ld_colours = c(NA, "black"))+
                          ggetho::stat_bar_tile_etho()+
                          ggplot2::facet_grid(Cabinet~sex)+
                          ggplot2::ylab("")+
                         ggplot2::ggtitle("Double plotted actogram", subtitle = "averaged by cabinet vs sex, time window per each bin is 5'")
                        self$DPActograms$DPacto4[[1]] <- plot
                      }else if(type == "individual"){

                      }
                    },

#' plot_DAct
#'
#' @description function to generate sum of daily activity line plots arranged 
#' in different configurations and store them in DAct list
#' @param env App_settings environment to access Custom_tables object
#' @param type plot type to be rendered (list available in output_list_acto$handler[9:14])
#' @param error "Sem" or "SD"
#'

                    plot_DAct = function(env, type, error = "Sem"){
                      activity <- env$env3$Custom_tables$daily_act[[1]] #get activity file from Custom_tables

                      if (type == "~gen"){
                        G_eff <- activity %>%
                          dplyr::group_by(Genotype, Day) %>%
                          dplyr::summarise(Activity = mean(Activity))
                        std_gen <- activity %>%
                          dplyr::group_by(Genotype, Day) %>%
                          dplyr::summarise(n = dplyr::n(),
                                           std = sd(Activity),
                                           sem = sd(Activity)/sqrt(n))
                        
                        ## create table with valuer calculated (maybe not necessary to reorganise table?)
                        G_eff <- dplyr::tibble(Genotype = G_eff$Genotype,
                                        Day = G_eff$Day,
                                        Activity = G_eff$Activity,
                                        Std = std_gen$std,
                                        Sem = std_gen$sem,
                                        n = std_gen$n)
                        
                        ## decide error to apply based on user choice
                        # create a new table 'G_eff_use' in which on column is renamed error
                        switch(error,
                               "Sem" = {
                                 G_eff_use <- G_eff
                                 colnames(G_eff_use) <- c('Genotype', 'Day', 'Activity', 'Std', 'error', 'n')
                               },
                               "SD" = {
                                 G_eff_use <- G_eff
                                 colnames(G_eff_use) <- c('Genotype', 'Day', 'Activity', 'error', 'Sem', 'n')
                               })
                        plot <- ggplot2::ggplot(G_eff_use)+
                         ggplot2::geom_line(
                           ggplot2::aes(
                             Day, Activity, colour = Genotype), size = 1)+
                         ggplot2::geom_errorbar(
                           ggplot2::aes(
                             x = Day, ymin=Activity-error, ymax=Activity+error, colour=Genotype), width=.2,
                                        position=ggplot2::position_dodge(0.05))+
                          ggplot2::theme(axis.text = ggplot2::element_text(size = 15))+
                          ggplot2::theme(axis.title = ggplot2::element_text(size = 15))+
                          ggplot2::ggtitle("Mean activity across genotype")
                        self$DAct_plots[1][[1]] <- plot
                      }else if (type == "~sex"){
                        S_eff <- activity %>%
                          dplyr::group_by(Sex, Day) %>%
                          dplyr::summarise(Activity = mean(Activity))
                        std_sex <- activity %>% 
                          dplyr::group_by(Sex, Day)%>% 
                          dplyr::summarise(n = dplyr::n(),
                                           std = sd(Activity),
                                           sem = sd(Activity)/sqrt(n))
                       ## create table with valuer calculated (maybe not necessary to reorganise table?)
                        S_eff <- dplyr::tibble(Sex = S_eff$Sex,
                                               Day = S_eff$Day,
                                               Activity = S_eff$Activity,
                                               Std = std_sex$std,
                                               Sem = std_sex$sem,
                                               n = std_sex$n)
                        
                        ## decide error to apply based on user choice
                        # create a new table 'S_eff_use' in which on column is renamed error
                        switch(error,
                               "Sem" = {
                                 S_eff_use <- S_eff
                                 colnames(S_eff_use) <- c('Sex', 'Day', 'Activity', 'Std', 'error', 'n')
                               },
                               "SD" = {
                                 S_eff_use <- S_eff
                                 colnames(S_eff_use) <- c('Sex', 'Day', 'Activity', 'error', 'Sem', 'n')
                               })

                        plot <- ggplot2::ggplot(S_eff_use)+
                         ggplot2::geom_line(
                           ggplot2::aes(
                             Day, Activity, colour = Sex), size = 1)+
                         ggplot2::geom_errorbar(
                           ggplot2::aes(
                             x = Day, ymin=Activity-error, ymax=Activity+error, colour=Sex), width=.2,
                                        position=ggplot2::position_dodge(0.05))+
                          ggplot2::theme(axis.text = ggplot2::element_text(size = 15))+
                          ggplot2::theme(axis.title = ggplot2::element_text(size = 15))+
                          ggplot2::ggtitle("Mean activity across sex")
                        self$DAct_plots[2][[1]] <- plot
                      }else if (type == "individual"){
                        G_eff <- activity %>%
                          dplyr::group_by(Genotype, Day) %>%
                          dplyr::summarise(Activity = mean(Activity))
                        std_gen <- activity %>%
                          dplyr::group_by(Genotype, Day) %>%
                          dplyr::summarise(n = dplyr::n(),
                                           std = sd(Activity),
                                           sem = sd(Activity)/sqrt(n))
                        
                        ## create table with valuer calculated (maybe not necessary to reorganise table?)
                        G_eff <- dplyr::tibble(Genotype = G_eff$Genotype,
                                               Day = G_eff$Day,
                                               Activity = G_eff$Activity,
                                               Std = std_gen$std,
                                               Sem = std_gen$sem,
                                               n = std_gen$n)
                        
                        ## decide error to apply based on user choice
                        # create a new table 'G_eff_use' in which on column is renamed error
                        switch(error,
                               "Sem" = {
                                 G_eff_use <- G_eff
                                 colnames(G_eff_use) <- c('Genotype', 'Day', 'Activity', 'Std', 'error', 'n')
                               },
                               "SD" = {
                                 G_eff_use <- G_eff
                                 colnames(G_eff_use) <- c('Genotype', 'Day', 'Activity', 'error', 'Sem', 'n')
                               })

                        plot <- ggplot2::ggplot()+
                         ggplot2::geom_line(
                           data = activity,ggplot2::aes(
                             Day, Activity, colour = id))+
                         ggplot2::geom_line(
                           data = G_eff_use,ggplot2::aes(Day, Activity), size = 1)+
                         ggplot2::geom_point(
                           data = G_eff_use,ggplot2::aes(Day, Activity), size = 2)+
                         ggplot2::geom_errorbar(
                           data = G_eff_use,
                                   ggplot2::aes(
                                     x = Day, ymin=Activity-error, ymax=Activity+error), width=.2, position=ggplot2::position_dodge(0.05))+
                          ggplot2::facet_wrap(~Genotype, ncol = 2)+
                          ggplot2::theme(axis.text = ggplot2::element_text(size = 15))+
                          ggplot2::theme(axis.title = ggplot2::element_text(size = 15))+
                         ggplot2::ggtitle("Activity compared between genotypes", subtitle =  "mean activity for both genotypes in black")
                        self$DAct_plots[3][[1]] <- plot
                      }else if (type == "gen~sex"){
                        GS_eff <- activity %>%
                          dplyr::group_by(Genotype, Sex, Day) %>%
                          dplyr::summarise(Activity = mean(Activity))
                        std_gs <- activity %>% 
                          dplyr::group_by(Genotype, Sex, Day) %>% 
                          dplyr::summarise(n = dplyr::n(),
                                           std = sd(Activity),
                                           sem = sd(Activity)/sqrt(n))
                        GS_eff <-dplyr::tibble(Genotype = GS_eff$Genotype,
                                         Sex = GS_eff$Sex,
                                         Day = GS_eff$Day,
                                         Activity = GS_eff$Activity,
                                         Std = std_gs$std,
                                         Sem = std_gs$sem,
                                         n = std_gs$n)
                        
                        ## decide error to apply based on user choice
                        # create a new table 'G_eff_use' in which on column is renamed error
                        switch(error,
                               "Sem" = {
                                 GS_eff_use <- GS_eff
                                 colnames(GS_eff_use) <- c('Genotype', 'Sex', 'Day', 'Activity', 'Std', 'error', 'n')
                               },
                               "SD" = {
                                 GS_eff_use <- GS_eff
                                 colnames(GS_eff_use) <- c('Genotype', 'Sex', 'Day', 'Activity', 'error', 'Sem', 'n')
                               })

                        plot <- ggplot2::ggplot()+
                         ggplot2::geom_line(
                           data = GS_eff_use,ggplot2::aes(
                             Day, Activity, colour = Sex), size = 1)+
                         ggplot2::geom_point(
                           data = GS_eff_use,ggplot2::aes(
                             Day, Activity, colour = Sex), size = 2)+
                         ggplot2::geom_errorbar(
                           data = GS_eff_use,ggplot2::aes(
                             x = Day, ymin=Activity-error, ymax=Activity+error, colour = Sex), width=.2, position=ggplot2::position_dodge(0.05))+
                          ggplot2::facet_wrap(~Genotype)+
                          ggplot2::theme(axis.text = ggplot2::element_text(size = 15))+
                          ggplot2::theme(axis.title = ggplot2::element_text(size = 15))+
                         ggplot2::ggtitle("Mean activity for Male and Females across genotypes")
                        self$DAct_plots[4][[1]] <- plot
                      }else if (type == "indiv+sex~gen"){
                        GS_eff <- activity %>%
                          dplyr::group_by(Genotype, Sex, Day) %>%
                          dplyr::summarise(Activity = mean(Activity))
                        std_gs <- activity %>% 
                          dplyr::group_by(Genotype, Sex, Day) %>% 
                          dplyr::summarise(n = dplyr::n(),
                                           std = sd(Activity),
                                           sem = sd(Activity)/sqrt(n))
                        GS_eff <-dplyr::tibble(Genotype = GS_eff$Genotype,
                                               Sex = GS_eff$Sex,
                                               Day = GS_eff$Day,
                                               Activity = GS_eff$Activity,
                                               Std = std_gs$std,
                                               Sem = std_gs$sem,
                                               n = std_gs$n)
                        
                        ## decide error to apply based on user choice
                        # create a new table 'G_eff_use' in which on column is renamed error
                        switch(error,
                               "Sem" = {
                                 GS_eff_use <- GS_eff
                                 colnames(GS_eff_use) <- c('Genotype', 'Sex', 'Day', 'Activity', 'Std', 'error', 'n')
                               },
                               "SD" = {
                                 GS_eff_use <- GS_eff
                                 colnames(GS_eff_use) <- c('Genotype', 'Sex', 'Day', 'Activity', 'error', 'Sem', 'n')
                               })
                        
                        plot <- ggplot2::ggplot()+
                         ggplot2::geom_line(
                           data = activity,ggplot2::aes(
                             Day, Activity, group = id, colour = Genotype))+
                         ggplot2::geom_line(
                           data = GS_eff_use, ggplot2::aes(
                             Day, Activity, colour = Genotype), linetype = "dashed", size = 1.5)+
                         ggplot2::geom_errorbar(
                           data = GS_eff_use, ggplot2::aes(
                             x = Day, ymin=Activity-error, ymax=Activity+error, colour = Genotype), width=.2, position=ggplot2::position_dodge(0.05))+
                          ggplot2::facet_wrap(~Sex)+
                          ggplot2::theme(axis.text = ggplot2::element_text(size = 15))+
                          ggplot2::theme(axis.title = ggplot2::element_text(size = 15))+
                         ggplot2::ggtitle("Activity divided across sex and genotype", subtitle =  "mean activity as dashed lines")
                        self$DAct_plots[5][[1]] <- plot
                      }else if (type == "indiv+cab~gen"){
                        G_eff <- activity %>%
                          dplyr::group_by(Genotype, Day) %>%
                          dplyr::summarise(Activity = mean(Activity))
                        std_gen <- activity %>%
                          dplyr::group_by(Genotype, Day) %>%
                          dplyr::summarise(n = dplyr::n(),
                                           std = sd(Activity),
                                           sem = sd(Activity)/sqrt(n))
                        
                        ## create table with valuer calculated (maybe not necessary to reorganise table?)
                        G_eff <- dplyr::tibble(Genotype = G_eff$Genotype,
                                               Day = G_eff$Day,
                                               Activity = G_eff$Activity,
                                               Std = std_gen$std,
                                               Sem = std_gen$sem,
                                               n = std_gen$n)
                        
                        ## decide error to apply based on user choice
                        # create a new table 'G_eff_use' in which on column is renamed error
                        switch(error,
                               "Sem" = {
                                 G_eff_use <- G_eff
                                 colnames(G_eff_use) <- c('Genotype', 'Day', 'Activity', 'Std', 'error', 'n')
                               },
                               "SD" = {
                                 G_eff_use <- G_eff
                                 colnames(G_eff_use) <- c('Genotype', 'Day', 'Activity', 'error', 'Sem', 'n')
                               })

                        plot <- ggplot2::ggplot()+
                         ggplot2::geom_line(data = activity,ggplot2::aes(Day, Activity, group = id, colour = Genotype))+
                         ggplot2::geom_line(data = G_eff_use,ggplot2::aes(Day, Activity, colour = Genotype), size = 1.5)+
                         ggplot2::geom_point(data = G_eff_use,ggplot2::aes(Day, Activity, colour = Genotype), size = 2)+
                         ggplot2::geom_errorbar(data = G_eff_use,ggplot2::aes(x = Day, ymin=Activity-error, ymax=Activity+error, colour = Genotype), width=.2, position=ggplot2::position_dodge(0.05))+
                          ggplot2::facet_wrap(~Cabinet)+
                          ggplot2::theme(axis.text = ggplot2::element_text(size = 15))+
                          ggplot2::theme(axis.title = ggplot2::element_text(size = 15))+
                         ggplot2::ggtitle("Activity compared between cabinets", subtitle =  "Activity grouped by genotypes and mean for M / F")
                        self$DAct_plots[6][[1]] <- plot
                      }
                    },

#' plot_periodogram
#' 
#' @description function to generate periodograms arranged in different
#' configurations and store them in period_plots list
#' @param funEnv App_settings environment to access Custom_tables object
#' @param plotType plot type to be rendered (list available in output_list_acto$handler[15:19])
#'

                    plot_periodogram = function(funEnv, plotType){
                      data <- funEnv$env3$Custom_tables$periodograms[[1]]
                      if ("Pertotal" %in% plotType){
                        plot <- ggetho::ggperio(data, mapping = ggplot2::aes(y = power, peak = peak))+
                         ggplot2::geom_line(ggplot2::aes(group = id, colour = Genotype))+
                         ggplot2::geom_line(ggplot2::aes(y = signif_threshold), colour = "red", alpha = 0.4)+
                          ggetho::geom_peak()
                        self$period_plots[1][[1]] <- plot
                      }
                      if ("Perfaceted" %in% plotType){
                        plot <- ggetho::ggperio(data, mapping = ggplot2::aes(y = power, peak = peak))+
                         ggplot2::geom_line(ggplot2::aes(group = id, colour = Genotype))+
                         ggplot2::geom_line(ggplot2::aes(y = signif_threshold), colour = "red", alpha = 0.4)+
                          ggetho::geom_peak()+
                          ggplot2::facet_wrap(~id+sex+Genotype, ncol = 6, labeller = ggplot2::label_wrap_gen(multi_line=FALSE))
                        self$period_plots[2][[1]] <- plot
                      }
                      if ("Persex" %in% plotType){
                        plot <- ggetho::ggperio(data, mapping = ggplot2::aes(y = power, peak = peak))+
                         ggplot2::geom_line(ggplot2::aes(group = id, colour = sex))+
                         ggplot2::geom_line(ggplot2::aes(y = signif_threshold), colour = "red", alpha = 0.4)+
                          ggetho::geom_peak()+
                          ggplot2::facet_wrap(sex ~ Genotype, ncol = 6, labeller = ggplot2::label_wrap_gen(multi_line=FALSE))
                        self$period_plots[3][[1]] <- plot
                      }
                      if ("Pergenotype" %in% plotType){
                        plot <- ggetho::ggperio(data, mapping = ggplot2::aes(y = power, peak = peak))+
                         ggplot2::geom_line(ggplot2::aes(group = id, colour = sex))+
                         ggplot2::geom_line(ggplot2::aes(y = signif_threshold), colour = "red", alpha = 0.4)+
                          ggetho::geom_peak()+
                          ggplot2::facet_wrap(Genotype ~ ., ncol = 6, labeller = ggplot2::label_wrap_gen(multi_line=FALSE))
                        self$period_plots[4][[1]] <- plot
                      }
                      if ("Percabinet" %in% plotType){
                        plot <- ggetho::ggperio(data, mapping = ggplot2::aes(y = power, peak = peak))+
                         ggplot2::geom_line(ggplot2::aes(group = id, colour = Genotype))+
                         ggplot2::geom_line(ggplot2::aes(y = signif_threshold), colour = "red", alpha = 0.4)+
                          ggetho::geom_peak()+
                          ggplot2::facet_wrap(Cabinet ~ ., ncol = 6, labeller = ggplot2::label_wrap_gen(multi_line=FALSE))
                        self$period_plots[5][[1]] <- plot
                      }
                    },

#' plot_periodogram_hist
#' 
#' @description function to generate histograms of period length data in different
#' configurations and store them in period_plots_box list
#' @param funEnv App_settings environment to access Custom_tables object
#' @param plotType plot type to be rendered (list available in output_list_acto$handler[20:24])
#' @param periodRange vector containing period range selected 
#'

                    plot_periodogram_hist = function(funEnv, plotType, periodRange){
                      data <- funEnv$env3$Custom_tables$periodograms[[2]]
                      # browser()
                      if ("Pertotal_hist" %in% plotType){
                        # plot <- 
                        # self$period_plots[1][[2]] <- plot
                      }
                      if ("Perfaceted_hist" %in% plotType){
                        # plot <- 
                        # self$period_plots[2][[2]] <- plot
                      }
                      if ("Persex_hist" %in% plotType){
                        plot <- ggplot2::ggplot(data, ggplot2::aes(sex, period, fill = Genotype)) + 
                          ggplot2::geom_boxplot() +
                          ggplot2::geom_jitter(ggplot2::aes(size = power-signif_threshold), alpha=.4)+
                          ggplot2::ylim(periodRange[1], periodRange[2])
                        self$period_plots_box[3][[1]] <- plot
                      }
                      if ("Pergenotype_hist" %in% plotType){
                        plot <- ggplot2::ggplot(data, ggplot2::aes(Genotype, period, fill = Genotype)) + 
                          ggplot2::geom_boxplot() +
                          ggplot2::geom_jitter(ggplot2::aes(size = power-signif_threshold), alpha=.4)+
                          ggplot2::ylim(periodRange[1], periodRange[2])
                        self$period_plots_box[4][[1]] <- plot
                      }
                      if ("Percabinet_hist" %in% plotType){
                        plot <- ggplot2::ggplot(data, ggplot2::aes(Genotype, period, fill = as.factor(Cabinet))) + 
                          ggplot2::geom_boxplot() +
                          ggplot2::geom_jitter(ggplot2::aes(size = power-signif_threshold), alpha=.4)+
                          ggplot2::ylim(periodRange[1], periodRange[2])
                        self$period_plots_box[5][[1]] <- plot
                      }
                    },
                  
#' plot_avg_day
#'
#' @description function to generate average daily activity line plots arranged 
#' in different configurations and store them in avg_day_plots list
#' @param funenv App_settings environment to access Custom_tables object
#' @param type plot type to be rendered (list available in output_list_acto$handler[25:27])
#'
                    plot_avg_day = function(funEnv, plotType, error = "Sem"){
                      activity <- funEnv$env3$Custom_tables$average_day[[1]] #get activity file from Custom_tables
                      if ("individualAvgD" %in% plotType){
                      # generate plot
                      plot <- ggplot2::ggplot()+
                        ggplot2::geom_line(
                        data = activity,ggplot2::aes(
                        CT, activity, colour = mouse))+
                        ggplot2::scale_x_continuous(breaks = seq(0,24, by = 2))+
                        ggplot2::theme(axis.text = ggplot2::element_text(size = 15))+
                        ggplot2::theme(axis.title = ggplot2::element_text(size = 15))+
                        ggplot2::ggtitle("Individual average activity", subtitle =  "")
                      # store plot in Annotate
                      self$avg_day_plots[1][[1]] <- plot
                      }
                      if ("sexAvgD" %in% plotType){
                        # group data by sex
                        S_eff <- activity %>%
                          dplyr::group_by(sex, CT) %>%
                          dplyr::summarise(Activity = mean(activity))
                        # compute SD and Sem of data
                        std_sex <- activity %>% 
                          dplyr::group_by(sex, CT)%>% 
                          dplyr::summarise(n = dplyr::n(),
                                           std = sd(activity),
                                           sem = sd(activity)/sqrt(n))
                        ## create table with values calculated (maybe not necessary to reorganise table?)
                        S_eff <- dplyr::tibble(Sex = S_eff$sex,
                                               CT = S_eff$CT,
                                               Activity = S_eff$Activity,
                                               Std = std_sex$std,
                                               Sem = std_sex$sem,
                                               n = std_sex$n)
                        
                        ## decide error to apply based on user choice
                        # create a new table 'S_eff_use' in which on column is renamed error
                        switch(error,
                               "Sem" = {
                                 S_eff_use <- S_eff
                                 colnames(S_eff_use) <- c('Sex', 'CT', 'Activity', 'Std', 'error', 'n')
                               },
                               "SD" = {
                                 S_eff_use <- S_eff
                                 colnames(S_eff_use) <- c('Sex', 'CT', 'Activity', 'error', 'Sem', 'n')
                               })
                        
                        # generate plot
                        plot <- ggplot2::ggplot(S_eff_use)+
                          ggplot2::geom_line(
                            ggplot2::aes(
                              CT, Activity, colour = Sex), size = 1)+
                          ggplot2::geom_errorbar(
                            ggplot2::aes(
                              x = CT, ymin=Activity-error, ymax=Activity+error, colour=Sex), width=.2,
                            position=ggplot2::position_dodge(0.05))+
                          ggplot2::scale_x_continuous(breaks = seq(0,24, by = 2))+
                          ggplot2::theme(axis.text = ggplot2::element_text(size = 15))+
                          ggplot2::theme(axis.title = ggplot2::element_text(size = 15))+
                          ggplot2::ggtitle("Average day - grouped by sex", subtitle =  "")  
                        # store plot in Annotate
                        self$avg_day_plots[2][[1]] <- plot
                      }
                      if ("genotypeAvgD" %in% plotType){
                        # group data by genotype
                        G_eff <- activity %>%
                          dplyr::group_by(genotype, CT) %>%
                          dplyr::summarise(Activity = mean(activity))
                        # compute SD and Sem of data
                        std_gen <- activity %>%
                          dplyr::group_by(genotype, CT) %>%
                          dplyr::summarise(n = dplyr::n(),
                                           std = sd(activity),
                                           sem = sd(activity)/sqrt(n))
                        
                        ## create table with values calculated (maybe not necessary to reorganise table?)
                        G_eff <- dplyr::tibble(Genotype = G_eff$genotype,
                                               CT = G_eff$CT,
                                               Activity = G_eff$Activity,
                                               Std = std_gen$std,
                                               Sem = std_gen$sem,
                                               n = std_gen$n)
                        
                        ## decide error to apply based on user choice
                        # create a new table 'G_eff_use' in which on column is renamed error
                        switch(error,
                               "Sem" = {
                                 G_eff_use <- G_eff
                                 colnames(G_eff_use) <- c('Genotype', 'CT', 'Activity', 'Std', 'error', 'n')
                               },
                               "SD" = {
                                 G_eff_use <- G_eff
                                 colnames(G_eff_use) <- c('Genotype', 'CT', 'Activity', 'error', 'Sem', 'n')
                               })
                        # generate plot
                        plot <- ggplot2::ggplot(G_eff_use)+
                          ggplot2::geom_line(
                            ggplot2::aes(
                              CT, Activity, colour = Genotype), size = 1)+
                          ggplot2::geom_errorbar(
                            ggplot2::aes(
                              x = CT, ymin=Activity-error, ymax=Activity+error, colour=Genotype), width=.2,
                            position=ggplot2::position_dodge(0.05))+
                          ggplot2::scale_x_continuous(breaks = seq(0,24, by = 2))+
                          ggplot2::theme(axis.text = ggplot2::element_text(size = 15))+
                          ggplot2::theme(axis.title = ggplot2::element_text(size = 15))+
                          ggplot2::ggtitle("Average day - grouped by Genotype", subtitle =  "")    
                        # store plot in Annotate
                        self$avg_day_plots[3][[1]] <- plot
                      }
                    }
                    ###
                    )
)
