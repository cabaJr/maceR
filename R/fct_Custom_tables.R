#' Custom_tables 
#'
#' @title Custom_tables
#'
#' @description A fct function
#' container of R6 objects generator
#' @return The return value, if any, from executing the function.


# Custom tables class initializer
##### CUSTOM TABLES ###########
Custom_tables <- R6::R6Class("Custom_tables",
                         list(
#' @field metadata list of metadata
#' @field table1 table 1
#' @field table2 table 2
#' @field table3 table 3
#' @field table4 table 4
#' @field table5 table 5
#' @field cacheKeys table storing the cacheKeys to avoid reloading the same plots
                           metadata = NULL,
                           table1 = NULL,
                           table2 = NULL,
                           table3 = NULL,
                           table4 = NULL,
                           table5 = NULL,
                           cacheKeys = dplyr::tibble("table" = seq(1:8), #table to store keys of hashed tables
                                              "key" = 0),
#' compile
#'
#' @param env App_settings object to access the env containing myCleanMice obj
#'
#' @return
#' @export
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
                           }#,

#                            HHActivity = function(){
#                              d6 <- NULL
#                              for (i in seq_len(length(myCleanMice))){
#                                data <- myCleanMice[[i]]$countsMinute
#                                id <- as.factor(myCleanMice[[i]]$id)
#                                sex <- as.character(myCleanMice[[i]]$sex)
#                                genotype <- as.character(myCleanMice[[i]]$genotype)
#                                # split into daily chunks and get the avg
#                                d1 <- split(data, ceiling(seq_along(data)/1440))
#                                # elongate last chunk to 1440 and substitute NA with 0
#                                toAdd <- replicate((1440-length(d1[[length(d1)]])), 0)
#                                d1[[length(d1)]] <- append(d1[[length(d1)]], toAdd)
#                                d2 <- Reduce("+", d1)/length(d1)
#                                # reduce and get the sum of half hour chunks
#                                d3 <- split(d2, ceiling(seq_along(d2)/30))
#                                # from list of vectors of length 1 get the atomic values into a vector
#                                d4 <- unlist(lapply(d3, sum), recursive = TRUE, use.names = FALSE)
#                                d5 <- data.frame(c(1:48), id,  d4, sex, genotype)
#                                d6 <- rbind(d6, d5)# d4 <- data.frame(d1)
#                              }
#                              d7 <- tibble("time" = d6[,1],
#                                           "mouse" = d6$id,
#                                           "activity" = d6$d4,
#                                           "sex" = d6$sex,
#                                           "genotype" = d6$genotype)
#                              self$table3 <- d7
#                            },
#                            
#                            checkIf = function(funEnv, subsetPlot){
#                              # if(is.null(self$table1) == TRUE){ #add conditions to see if values changed and it's necessary to recalculate the table
#                              # if(funEnv$env2$Annotate$cacheKeys[1, 2] != self$cacheKeys[1, 2]){
#                              # browser()
#                              self$behavrTable(funEnv, subsetPlot)
#                              # }else{}
#                            },
#                            
#                            behavrTable = function(x, subsetVal){
#                              d2 <- NULL
#                              myCleanMice <- x$env2$myCleanMice
#                              #add a vector containing the myCleanMice numbers in the list: 
#                              # if no subsetting list all --> toLoad <- seq_len(length(myCleanMice)) 
#                              # if subsetting (subsetPlot == "Yes") --> toLoad <-  listMicefiltered[,1]
#                              # if (subsetVal == "Yes"){
#                              #   validate(
#                              #     need(is.null(x$subsetting$miceListFiltered) == FALSE, 
#                              #          message = showModal(modalDialog("You need to select a group of mice", title = "Data", easyClose = TRUE))
#                              #     )
#                              #   )
#                              # }
#                              range = c((x$subsetting$timespan[1]*1440),(x$subsetting$timespan[2]*1440))
#                              switch(subsetVal,
#                                     "Yes" = {
#                                       toLoad <- x$subsetting$miceListFiltered$pos
#                                     },
#                                     "No" = {
#                                       toLoad <- seq_len(length(myCleanMice))
#                                     })
#                              # browser()
#                              for (i in toLoad){
#                                range1 <- which(myCleanMice[[i]]$timepoint >= range[1]*60)
#                                range2 <- which(myCleanMice[[i]]$timepoint <= range[2]*60)
#                                range3 <- intersect(range1, range2)
#                                data <- myCleanMice[[i]]$countsMinute[range3]    #filter based on timepoint
#                                timepoint <- myCleanMice[[i]]$timepoint[range3]  #filter based on timepoint
#                                realtime <- myCleanMice[[i]]$realTime[range3]    #filter based on timepoint
#                                id <- as.character(myCleanMice[[i]]$id)
#                                sex <- as.character(myCleanMice[[i]]$sex)
#                                genotype <- as.character(myCleanMice[[i]]$genotype)
#                                d1 <- tibble("id" = id,
#                                             "t" = timepoint,
#                                             "Activity" = data,
#                                             "Sex" = sex,
#                                             "Genotype" = genotype)
#                                d2 <- rbind(d2, d1)
#                              }
#                              d2
#                              metadata <- self$metadata
#                              # browser()
#                              setDT(d2, key = "id")
#                              setDT(metadata, key = "id")
#                              self$table1 <- behavr(d2, metadata)
#                              #self$cacheKeys[1,2] <- digest::digest(self$table1, "xxhash64")
#                            },
#                            
#                            dailyAct = function(x){
#                              mouseData <- x$myCleanMice
#                              number <- 86400/x$App_settings$timepointDur #number of timepoints in a day
#                              
#                              activity <- tibble("id" = as.character(),
#                                                 "Day" = as.numeric(),
#                                                 "Activity" = as.numeric(),
#                                                 "Cabinet" = as.numeric(),
#                                                 "Sex" = as.character(),
#                                                 "Genotype" = as.character())
#                              for (h in seq_len(length(mouseData))){
#                                d1 <- split(mouseData[[h]]$countsMinute, ceiling(seq_along(mouseData[[1]]$countsMinute)/1440))
#                                d2 <- lapply(d1, sum)
#                                d3 <- tibble("id" = mouseData[[h]]$id,
#                                             "Day" = seq_len(length(d2)),
#                                             "Activity" = unlist(d2),
#                                             "Cabinet" = mouseData[[h]]$cabinet,
#                                             "Sex" = mouseData[[h]]$sex,
#                                             "Genotype" = mouseData[[h]]$genotype,
#                                )
#                                activity <- rbind(activity, d3)
#                              }
#                              self$table2 <- activity
#                            },
#                            
#                            checkIf2 = function(funEnv){
#                              if(is.null(Custom_tables$table2) == TRUE){
#                                self$dailyAct(funEnv)
#                                shinyjs::show(id = "Dl2", anim = FALSE)
#                              }else{}
#                            },
#                            
#                            computePer = function(method, periodRange, funenv) {
#                              data <- self$table1
#                              meta <- self$metadata
#                              perFun <- method
#                              vals <- periodRange
#                              # browser()
#                              switch(perFun,
#                                     "chi_sq_periodogram" = {
#                                       period <- zeitgebr::periodogram(Activity, data, period_range = c(hours(vals[1]), hours(vals[2])),
#                                                                       resample_rate = 1/mins(1), alpha = 0.01, FUN = chi_sq_periodogram)
#                                     },
#                                     "ac_periodogram" = {
#                                       period <- zeitgebr::periodogram(Activity, data, period_range = c(hours(vals[1]), hours(vals[2])),
#                                                                       resample_rate = 1/mins(1), alpha = 0.01, FUN = ac_periodogram)
#                                     },
#                                     "ls_periodogram" = {
#                                       period <- zeitgebr::periodogram(Activity, data, period_range = c(hours(vals[1]), hours(vals[2])),
#                                                                       resample_rate = 1/mins(1), alpha = 0.01, FUN = ls_periodogram)
#                                     })
#                              periodPeaks <- find_peaks(period, n_peaks = 2)
#                              meta <- setDT(meta, key = "id")
#                              period2 <- behavr(periodPeaks, metadata = meta)
#                              self$table4 <- period2
#                              
#                            }
#                            
#                            #,
#                            # 
#                            # save_table = function(file, file1){#, type, name){
#                            #   file1 <- 
#                            #   write.csv(self$table2, file, quote = FALSE, row.names = FALSE)
#                            # }
                         )
)

#' @noRd
