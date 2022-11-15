#' Custom_tables 
#'
#' @title Custom_tables
#'
#' @description A R6 object that contains methods to analyse the uploaded data.
#'     The secondary data that are generated are stored inside the object.
#' 
#' @return The return value, if any, from executing the function.


# Custom tables class initializer
##### CUSTOM TABLES ###########
Custom_tables <- R6::R6Class("Custom_tables",
                         list(
#' @field metadata list of metadata
#' @field locomotor_act list containing tables related to actograms
#' @field daily_act list containing tables related to sum of daily activity
#' @field average_day list containing tables related to average circadian day
#' @field periodograms list containing tables related to periodograms
#' @field cacheKeys table storing the cacheKeys to avoid reloading the same plots
                           metadata = NULL,
                           locomotor_act = list(),
                           daily_act = list(),
                           average_day = list(),
                           periodograms = list(),
                           cacheKeys = dplyr::tibble("table" = seq(1:8), #table to store keys of hashed tables
                                              "key" = 0),
#' compile
#'
#' @param env App_settings object to access the env containing myCleanMice obj
#' 
#' @details method to generate a metadata table, integrating the metadata file
#'     uploaded from the user with computing of each raw data file in the 
#'     dataset.
#'
#' @return Metadata table to be stored inside the Custom_tables R6 object.
#'
                           compile = function(env){ #requires myClean mice to exist, therefore


                             idList <- c(0,0)
                             sexList <- c(0,0)
                             geneList <- c(0,0)
                             cabinetList <- c(0,0)
                             lightOnList <- c(0,0)
                             lengthList <- c(0,0)
                             myCleanMice <-  env$myCleanMice

                             for (i in seq_len(length(myCleanMice))){
                               idList[i] <- as.character(myCleanMice[[i]]$id)
                               sexList[i] <- as.character(myCleanMice[[i]]$sex)
                               geneList[i] <- as.character(myCleanMice[[i]]$genotype)
                               cabinetList[i] <- myCleanMice[[i]]$cabinet
                               lightOnList[i] <- myCleanMice[[i]]$lightOn
                               lengthList[i] <- (myCleanMice[[i]]$length)
                             }
                             metadataExtr <- dplyr::tibble(
                               "id" = idList,
                               "sex" = sexList,
                               "Genotype" = geneList,
                               "Cabinet" = cabinetList,
                               "Light_Onset" = lightOnList,
                               "Data_lenght" = lengthList
                             )
                             self$metadata <- metadataExtr
                             # browser()
                           },

#' AvgDay
#' @description function to create tibble with mouse activity daily grouped in 
#'     15 minutes bins
#'
#' @param per_len customizable period length value, used to normalise Circadian day
#' @param env App_settings environment
#' @param subsetVal Yes/No value
#'
#' @return A table with the sum 
#'
                           AvgDay = function(env, per_len, subsetVal){ #handle NA values to avoid dropping of values
                             d6 <- NULL
                             #browser()
                             #switch to allow for data subsetting
                             switch(subsetVal,
                                    "Yes" = {
                                      # subset_input_check(idlist = App_settings$subsetting$miceListFiltered)
                                      idlist = env$subsetting$miceListFiltered
                                      if (is.na(idlist) || is.null(idlist)){
                                        shinyjs::alert("You need to select at least one animal in the subsetting options")
                                      }
                                      req(!is.null(idlist))
                                      filteredMice <- env$subsetting$miceListFiltered$pos
                                      range = c((env$subsetting$timespan[1]*1440),(env$subsetting$timespan[2]*1440))
                                    },
                                    "No" = {
                                      filteredMice <- seq_len(length(env$env2$myCleanMice))
                                      range = c(0,max(env$env2$Annotate$metaTable$Datapoints))
                                    })
                             myCleanMice <- env$env3$myCleanMice[filteredMice]
                             for (i in seq_len(length(myCleanMice))){
                               #get light length
                               light_len <- env$LDparams$light
                               # discard half of light_len to align data
                               discard_first <- (light_len/2)*60
                               allData <- myCleanMice[[i]]
                               
                               #if condition to handle time subsetting
                               if(subsetVal == "Yes"){
                                 range1 <- which(allData$timepoint >= range[1]*60)
                                 range2 <- which(allData$timepoint <= range[2]*60)
                                 range3 <- intersect(range1, range2)
                                 allData$countsMinute <- allData$countsMinute[range3]    #filter based on timepoint
                               }
                               data <- allData$countsMinute
                               data <- data[-(1:discard_first), drop = FALSE]
                               id <- as.factor(myCleanMice[[i]]$id)
                               sex <- as.character(myCleanMice[[i]]$sex)
                               genotype <- as.character(myCleanMice[[i]]$genotype)
                               # split into daily chunks and divide into columns
                               # add option to split data based on animal period length
                               d1 <- split(data, ceiling(seq_along(data)/per_len))
                               # elongate last chunk to 1440
                               ## register the length of missing part and return a 
                               ## message if too short, maybe discard data
                               toAdd <- replicate((per_len-length(d1[[length(d1)]])), 0)
                               d1[[length(d1)]] <- append(d1[[length(d1)]], toAdd)
                               ## compute mean across days
                               d2 <- rowMeans(do.call(cbind, d1), na.rm = TRUE)
                               # reduce and get the sum of 15 minutes chunks
                               d3 <- split(d2, ceiling(seq_along(d2)/15))
                               # from list of vectors of length 1 get the atomic values into a vector
                               d4 <- unlist(lapply(d3, sum), recursive = TRUE, use.names = FALSE)
                               d5 <- data.frame(seq(0, 23.75, by = 0.25), id,  d4, sex, genotype)
                               d6 <- rbind(d6, d5)
                             }
                             d7 <- dplyr::tibble("CT" = d6[,1],
                                          "mouse" = d6$id,
                                          "activity" = d6$d4,
                                          "sex" = d6$sex,
                                          "genotype" = d6$genotype)
                             self$average_day[[1]] <- d7
                             
                           },
#                            
#' CheckIf
#'
#' @param funEnv env containing App_settings
#' @param subsetPlot Yes/No value
#'
                           checkIf = function(funEnv, subsetPlot){#to add option to check for presence of different tables
                             
                             # if(is.null(self$locomotor_act[[1]]) == TRUE){ #add conditions to see if values changed and it's necessary to recalculate the table
                             # if(funEnv$env2$Annotate$cacheKeys[1, 2] != self$cacheKeys[1, 2]){
                             self$behavrTable(funEnv, subsetPlot)
                             # }else{}
                           },
#                            
#' behavrTable
#'
#' @param x App_settings environment
#' @param subsetVal Yes/No value if to subset dataset or not
#'
                           behavrTable = function(x, subsetVal){
                             d2 <- NULL
                             myCleanMice <- x$env2$myCleanMice
                             #add a vector containing the myCleanMice numbers in the list:
                             # if no subsetting list all --> toLoad <- seq_len(length(myCleanMice))
                             # if subsetting (subsetPlot == "Yes") --> toLoad <-  listMicefiltered[,1]
                             # if (subsetVal == "Yes"){
                             #   validate(
                             #     need(is.null(x$subsetting$miceListFiltered) == FALSE,
                             #          message = showModal(modalDialog("You need to select a group of mice", title = "Data", easyClose = TRUE))
                             #     )
                             #   )
                             # }
                             # range = c((x$subsetting$timespan[1]*1440),(x$subsetting$timespan[2]*1440))
                             switch(subsetVal,
                                    "Yes" = {
                                      toLoad <- x$subsetting$miceListFiltered$pos
                                      range = c((x$subsetting$timespan[1]*1440),(x$subsetting$timespan[2]*1440))
                                    },
                                    "No" = {
                                      toLoad <- seq_len(length(myCleanMice))
                                      range = c(0,max(x$env2$Annotate$metaTable$Datapoints))
                                    })
                             for (i in toLoad){
                               range1 <- which(myCleanMice[[i]]$timepoint >= range[1]*60)
                               range2 <- which(myCleanMice[[i]]$timepoint <= range[2]*60)
                               range3 <- intersect(range1, range2)
                               data <- myCleanMice[[i]]$countsMinute[range3]    #filter based on timepoint
                               timepoint <- myCleanMice[[i]]$timepoint[range3]  #filter based on timepoint
                               realtime <- myCleanMice[[i]]$realTime[range3]    #filter based on timepoint
                               id <- as.character(myCleanMice[[i]]$id)
                               sex <- as.character(myCleanMice[[i]]$sex)
                               genotype <- as.character(myCleanMice[[i]]$genotype)
                               d1 <- dplyr::tibble("id" = id,
                                            "t" = timepoint,
                                            "Activity" = data,
                                            "Sex" = sex,
                                            "Genotype" = genotype)
                               d2 <- rbind(d2, d1)
                             }
                             d2
                             metadata <- self$metadata
                             data.table::setDT(d2, key = "id")
                             data.table::setDT(metadata, key = "id")
                             self$locomotor_act[[1]] <- behavr::behavr(d2, metadata)
                             #self$cacheKeys[1,2] <- digest::digest(self$locomotor_act[[1]], "xxhash64")
                           },
#                            
#' dailyAct
#'
#' @param subsetVal Yes/No value
#' @param env environment containing myCleanMice object
#'
#' @description function to generate the sum of the activity for each day and
#'     store it on a table.
#' 
                           dailyAct = function(env, subsetVal){ #substitute x with env in whole fun
                             # browser()
                             #switch to allow for data subsetting
                             switch(subsetVal,
                                    "Yes" = {
                                      filteredMice <- env$subsetting$miceListFiltered$pos
                                      range = c((env$subsetting$timespan[1]*1440),(env$subsetting$timespan[2]*1440))
                                    },
                                    "No" = {
                                      filteredMice <- seq_len(length(env$env2$myCleanMice))
                                      range = c(0,max(env$env2$Annotate$metaTable$Datapoints))
                                    })
                             startDay = range[1]/1440
                             mouseData <- env$env3$myCleanMice[filteredMice]
                             number <- 86400/env$App_settings$timepointDur #number of timepoints in a day

                             activity <- dplyr::tibble("id" = as.character(), #resolve similarity between table name and field Activity
                                                "Day" = as.numeric(),
                                                "Activity" = as.numeric(),
                                                "Cabinet" = as.numeric(),
                                                "Sex" = as.character(),
                                                "Genotype" = as.character())
                             for (h in seq_len(length(filteredMice))){
                               #if condition to handle time subsetting
                               if(subsetVal == "Yes"){
                                 range1 <- which(mouseData[[h]]$timepoint >= range[1]*60)
                                 range2 <- which(mouseData[[h]]$timepoint <= range[2]*60)
                                 range3 <- intersect(range1, range2)
                                 mouseData[[h]]$countsMinute <- mouseData[[h]]$countsMinute[range3]    #filter based on timepoint
                                 mouseData[[h]]$timepoint <- mouseData[[h]]$timepoint[range3]  #filter based on timepoint
                                 mouseData[[h]]$realTime <- mouseData[[h]]$realTime[range3]    #filter based on timepoint
                               }
                               d1 <- split(mouseData[[h]]$countsMinute, ceiling(seq_along(mouseData[[1]]$countsMinute)/1440))
                               d2 <- lapply(d1, sum)
                               d3 <- dplyr::tibble("id" = mouseData[[h]]$id,
                                            "Day" = seq(from = startDay, length.out = length(d2)),
                                            "Activity" = unlist(d2),
                                            "Cabinet" = mouseData[[h]]$cabinet,
                                            "Sex" = mouseData[[h]]$sex,
                                            "Genotype" = mouseData[[h]]$genotype,
                               )
                               activity <- rbind(activity, d3)
                             }
                             self$daily_act[[1]] <- activity
                             #create wide table for export
                             activity_wide <- activity %>% 
                               tidyr::pivot_wider(
                                 names_from = c(id, Genotype, Sex, Cabinet),
                                 values_from = Activity,
                                 names_glue = "{id}_{Sex}_{Genotype}_{Cabinet}",
                                 values_fill = 0
                                 )
                             self$daily_act[[2]] <- activity_wide
                           },
#                            
#                            checkIf2 = function(funEnv){
#                              if(is.null(Custom_tables$daily_act[[1]]) == TRUE){
#                                self$dailyAct(funEnv)
#                                shinyjs::show(id = "Dl2", anim = FALSE)
#                              }else{}
#                            },
                        
#' computePer
#'
#' @param method method to choose. Available are: "chi_sq_periodogram", 
#'     "ac_periodogram", "ls_periodogram". 
#' @param periodRange numeric, 
#' @param funenv Not used?
#' @description function to analyse data generated with behavrTable() to 
#'     extract period lenght in data if present.
#'     Describe Analysis methods and link to papers that compare the different
#'     methods
#'     
                           computePer = function(method, periodRange, funenv) {
                             # browser()
                             data <- self$locomotor_act[[1]]
                             meta <- self$metadata
                             perFun <- method
                             vals <- periodRange
                             switch(perFun,
                                    "chi_sq_periodogram" = {
                                      period <- zeitgebr::periodogram(Activity, data, period_range = c(behavr::hours(vals[1]), behavr::hours(vals[2])),
                                                                      resample_rate = 1/behavr::mins(6), alpha = 0.05, FUN = zeitgebr::chi_sq_periodogram)
                                    },
                                    "ac_periodogram" = {
                                      period <- zeitgebr::periodogram(Activity, data, period_range = c(behavr::hours(vals[1]), behavr::hours(vals[2])),
                                                                      resample_rate = 1/behavr::mins(6), alpha = 0.05, FUN = zeitgebr::ac_periodogram)
                                    },
                                    "ls_periodogram" = {
                                      period <- zeitgebr::periodogram(Activity, data, period_range = c(behavr::hours(vals[1]), behavr::hours(vals[2])),
                                                                      resample_rate = 1/behavr::mins(6), alpha = 0.05, FUN = zeitgebr::ls_periodogram)
                                    },
                                    "fourier_periodogram" = {
                                      period <- zeitgebr::periodogram(Activity, data, period_range = c(behavr::hours(vals[1]), behavr::hours(vals[2])),
                                                                      resample_rate = 1/behavr::mins(6), alpha = 0.05, FUN = zeitgebr::fourier_periodogram)
                                    },
                                    "cwt_periodogram" = {
                                      period <- zeitgebr::periodogram(Activity, data, period_range = c(behavr::hours(vals[1]), behavr::hours(vals[2])),
                                                                      resample_rate = 1/behavr::mins(6), alpha = 0.05, FUN = zeitgebr::cwt_periodogram)
                                    }
                                    )
                             periodPeaks <- zeitgebr::find_peaks(period, n_peaks = 2)
                             meta <- data.table::setDT(meta, key = "id")
                             all_periods <- behavr::behavr(periodPeaks, metadata = meta)
                             #create table with only first peak
                             first_peak <- periodPeaks[which(periodPeaks$peak == 1),]
                             #add column containing info on sex, genotype, cabinet
                             first_peak_add <- cbind(first_peak, meta[,c(1,2,3,4)])
                             #check if id in both columns match
                             compare <- match(first_peak_add[,1], first_peak_add[,7])
                             #remove second id column if matches the first
                             if(NA %in% compare == FALSE){
                               first_peak <- subset(first_peak_add, select = -7)
                             }
                             first_peak$period <- first_peak$period / 3600
                             self$periodograms[[1]] <- all_periods
                             self$periodograms[[2]] <- first_peak 
                             

                           },
#' create_table
#'
#' @param data the original data to be manipulated default to self
#' @param type different table manipulations on original data. requires an 
#'     handler present in the function
#' @param format the intended output of the table after exporting. defaulti is .csv
#' @param ... 
#'
#' @return a table to be stored in self$locomotor_act[[2]]
#'
#' @examples
                        create_table = function(type, format = ".csv", data = self$locomotor_act[[1]], ...){
                          #### manipulate csv file exported from actogram data to create a wide format table
                          # library("tidyr")
                          # library("magrittr")
                          # library("dplyr")
                          # 
                          data <- self$locomotor_act[[1]]
                          # add package names before functions
                          
                          switch(type, 
                          #### id_wide ####
                          "id_wide" = {
                          newtable <- data %>%tidyr::pivot_wider(names_from = c(id, Sex, Genotype),
                                                           values_from = Activity,
                                                           values_fill = 0)
                         #
                        },
                          ### sexmeans_id_wide ####
                          "sexmeans_id_wide" = {
                          # get list of all sexes
                          Sex <- unique(data$Sex)
                          # create two list to host data
                          datalist = list()
                          sexMeans = list()
                          # create table listing various ids and activity
                          newtable_sex <- data %>% tidyr::pivot_wider(names_from = c(id, Sex, Genotype),
                                                               values_from = Activity,
                                                               values_fill = 0)
                          # iterate for all sexes present in data
                          for (i in seq_along(Sex)){
                            # filter individual sexes
                            filter_sex <- which(data$Sex == Sex[i])
                            # create a wide table for e<ch sex
                            datalist[[i]] <- data[filter_sex, ] %>%tidyr::pivot_wider(names_from = c(id, Sex, Genotype),
                                                                                values_from = Activity,
                                                                                values_fill = 0)
                            # remove time column
                            sexMeans[[i]] <- datalist[[i]][, -1]
                            # create means
                            values <- rowMeans(sexMeans[[i]], na.rm = FALSE, dims = 1)
                            # add mean value to table
                            newtable_sex <- cbind(newtable_sex, values)
                            # get column names and change last one to the sex analysed
                            names <- names(newtable_sex)
                            names[length(names)] <- Sex[i]
                            # rename column names in table
                            colnames(newtable_sex) <- names
                          }
                        },
                          #### genmeans_id_wide ####
                         "genmeans_id_wide" = {
                           # get list of all sexes
                          Genotype <- unique(data$Genotype)
                          # create two list to host data
                          datalist = list()
                          genMeans = list()
                          # create table listing various ids and activity
                          newtable_gen <- data %>%tidyr::pivot_wider(names_from = c(id, Sex, Genotype),
                                                               values_from = Activity,
                                                               values_fill = 0)
                          # iterate for all sexes present in data
                          for (i in seq_along(Genotype)){
                            # filter individual sexes
                            filter_gen <- which(data$Genotype == Genotype[i])
                            # create a wide table for e<ch sex
                            datalist[[i]] <- data[filter_gen, ] %>%tidyr::pivot_wider(names_from = c(id, Sex, Genotype),
                                                                                values_from = Activity,
                                                                                values_fill = 0)
                            # remove time column
                            genMeans[[i]] <- datalist[[i]][, -1]
                            # create means
                            values <- rowMeans(genMeans[[i]], na.rm = FALSE, dims = 1)
                            # add mean value to table
                            newtable_gen <- cbind(newtable_gen, values)
                            # get column names and change last one to the sex analysed
                            names <- names(newtable_gen)
                            names[length(names)] <- Genotype[i]
                            # rename column names in table
                            colnames(newtable_gen) <- names
                          }
                         }
                          
                          #### creation of table with value summarised by Cabinet ####
                        } #end of create_table fun
#                            
#                            #,
#                            # 
#                            # save_table = function(file, file1){#, type, name){
#                            #   file1 <- 
#                            #   write.csv(self$daily_act[[1]], file, quote = FALSE, row.names = FALSE)
#                            # }
                         )
)

#' @noRd
