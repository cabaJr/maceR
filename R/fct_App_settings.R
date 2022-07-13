#' App_settings 
#'
#' @description App_settings R6 class object
#' @details A R6 class object to contain parameters of the app that can be
#'     accessible from the global.env, making possible to communicate between 
#'     different R6 objects
#'

App_settings <- R6::R6Class("App_settings",
                            public = list(
#' @field dashboardHeader_appearance TRUE/FALSE to enable/disable dashboard header
#' @field fileType type of files uploaded by user
#' @field dataList is a list of file paths uploaded by the user
#' @field metadata metadata filepath
#' @field listMice list of animals
#' @field discardRow how many rows to discard from each data file
#' @field timeDisplay how to display time
#' @field ExpStart what day and hour does the experiment starts?
#' @field timepointDur what is the sampling frequency?
#' @field subsetting list containing info about data subsetting
#' @field LDcondition list containing info on LD conditions
#' @field plotTab TRUE/FALSE value to know if the plot tab has been activated
#' @field env1 contains environment#1
#' @field env2 contains environment#2
#' @field env3 contains environment#3
#' @field env4 contains environment#4
#' @field env_msg contains the environment of Message obj
#' 
                              dashboardHeader_appearance = TRUE,
                              fileType = NULL,
                              dataList = NULL,
                              metadata = NULL,
                              listMice = NULL,
                              discardRow = NULL,
                              timeDisplay = NULL,
                              ExpStart = NULL,
                              timepointDur = NULL,
                              subsetting = list(miceListFiltered = NULL,
                                                timespan = NULL),
                              LDcondition = list(SLLD = NULL,
                                                 SLDD = NULL,
                                                 DPLD = NULL,
                                                 DPDD1 = NULL,
                                                 DPDD2 = NULL
                              ),
                              LDparams = list(light = NULL,
                                              DDcheck = NULL,
                                              DDstart = NULL
                                              ),
                              plotTab = FALSE,
                              env1 = NULL,
                              env2 = NULL,
                              env3 = NULL,
                              env4 = NULL,
                              env_msg = NULL,
                              
#' setData
#'
#' @param outsideData list of data files uploaded by the user
#'
#' @return
#' @export
#'
#' @examples
                              setData = function(outsideData){
                                self$dataList <- outsideData
                              },
                              
#' setMeta
#'
#' @param outsideMeta metadata file uploaded by the user
#'
#' @return
#' @export
#'
#' @examples
                              setMeta = function(outsideMeta){
                                # add checks for files to be in the correct format
                                self$metadata <- outsideMeta
                              },
                              
#' setListMice
#'
#' @param env environment
#'
#' @return
#' @export
#'
#' @examples
                              setListMice = function(env){
                                miceList <- env$env2$myCleanMice
                                d2 <- dplyr::tibble(
                                  "pos" = as.numeric(),
                                  "id" = as.character()
                                )
                                for(i in seq_along(1:length(miceList))){
                                  d1 <- dplyr::tibble(
                                    "pos" = i,
                                    "id" = miceList[[i]]$id
                                  )
                                  d2 <- rbind(d2, d1)
                                }
                                # browser()
                                env$listMice <- d2
                              },
                              
#' setListMiceFiltered
#'
#' @param x environment
#' @param idList list containing mice ids
#' @param sexList list containing mice sex
#' @param genList list containing mice genotypes
#' @param cabList list containing cabinet numbers
#'
#' @return
#' @export
#'
#' @examples
                              setListMiceFiltered = function(x, idList, sexList, genList, cabList){
                                metadata <- x$Annotate$metaTable
                                # browser()
                                metadata$id <- as.character(metadata$id)
                                idvector <- unlist(metadata[,1], use.names = FALSE)
                                listMice <- self$listMice
                                t1 = idList
                                t2 = idvector[which(metadata$Sex %in% sexList)]
                                t3 = idvector[which(metadata$Genotype %in% genList)]
                                t4 = idvector[which(metadata$Cabinet %in% cabList)]
                                # browser()
                                d1 <- c(t1, t2, t3, t4)
                                d2 <- unique(d1)
                                d3 <- listMice[listMice$id %in% d2,]
                                # browser()
                                self$subsetting$miceListFiltered = d3
                              },

#' saveLDparams
#'
#' @param x App_settings environment
#' @param light ligthOn value
#' @param ddVal Y/N value for DD
#' @param ddStart day in which DD starts
#'
#' @return
#' @export
#'
#' @examples
                              saveLDparams = function(x, light, ddVal, ddStart){
                                self$LDparams$light = light
                                self$LDparams$DDcheck = ddVal
                                self$LDparams$DDstart = ddStart
                              },
                                
                              
#' setLD
#'
#' @param x App_settings environment
#' @param light ligthOn value
#' @param ddVal Y/N value for DD
#' @param ddStart day in which DD starts
#'
#' @return
#' @export
#'
#' @examples
                              setLD = function(x, light, ddVal, ddStart){ #add more comments
                                qty <- length(x$env2$myCleanMice)
                                length1 <- max(x$env2$Annotate$metaTable$Datapoints)*60
                                per1End <- ddStart*86400
                                Llpha1 <- (0.4 / qty)
                                Llpha2 <- 0.02
                                # define DDstart and DDlength
                                DDday <- as.numeric(ddStart)
                                dataLen <- (length1%/%86400)-2
                                DDStart1 <- (1-((DDday-1)/dataLen))
                                DDStart2 <- (1-((DDday-2)/dataLen))
                                DDlength <- 24-as.numeric(light)
                                SLLD <- switch(ddVal,
                                               "Yes" = {ggetho::stat_ld_annotations(height = 1, alpha = Llpha1, outline = NA, period = behavr::hours(24), l_duration = behavr::hours(light), x_limits = c(0, per1End), phase = 0, ld_colours = c(NA, "black"))},
                                               "No" = {ggetho::stat_ld_annotations(height = 1, alpha = Llpha1, outline = NA, period = behavr::hours(24), l_duration = behavr::hours(light), phase = 0, ld_colours = c(NA, "black"))})
                                # check the rest of the function from here + implement value gathering inside server and function call
                                SLDD <- switch(ddVal,
                                               "Yes" = {ggetho::stat_ld_annotations(height = 1, alpha = Llpha1, outline = NA, period = behavr::hours(24), l_duration = behavr::hours(0), x_limits = c(per1End, length1), phase = 0, ld_colours = c(NA, "black"))},
                                               "No" = {NULL})
                                DPLD <- ggetho::stat_ld_annotations(height = 1, alpha = Llpha2, outline = NA, period = behavr::hours(24), l_duration = behavr::hours(light), phase = behavr::hours(6), ld_colours = c(NA, "black"))
                                DPDD1 <- switch(ddVal,
                                                "Yes" = {ggetho::stat_ld_annotations(height = DDStart1, alpha = Llpha2, outline = NA, period = behavr::hours(24), l_duration = behavr::hours(DDlength), x_limits = c(behavr::hours(0), behavr::hours(24)), phase = behavr::hours(18), ld_colours = c(NA, "black"))},
                                                "No" = {NULL})
                                DPDD2 <- switch(ddVal,
                                                "Yes" = {ggetho::stat_ld_annotations(height = DDStart2, alpha = Llpha2, outline = NA, period = behavr::hours(24), l_duration = behavr::hours(DDlength), x_limits = c(behavr::hours(24), behavr::hours(48)), phase = behavr::hours(18), ld_colours = c(NA, "black"))},
                                                "No" = {NULL})
                                self$LDcondition$SLLD = SLLD
                                self$LDcondition$SLDD = SLDD
                                self$LDcondition$DPLD = DPLD
                                self$LDcondition$DPDD1 = DPDD1
                                self$LDcondition$DPDD2 = DPDD2
                                # browser()
                              },
                              
#' updateTimeRange
#'
#' @param x to be rechecked (numeric)
#'
#' @return
#' @export
#'
#' @examples
                              updateTimeRange = function(x){
                                self$subsetting$timespan <- x
                              },
                              
#' setDiscRow
#'
#' @param x number of rows to discard (numeric)
#'
#' @return
#' @export
#'
#' @examples
                              setDiscRow = function(x){
                                self$discardRow = x
                              },
#' setTimeDisp
#'
#' @param x to display time in 12/24h format
#'
#' @return
#' @export
#'
#' @examples
                              setTimeDisp = function(x){
                                self$timeDisplay = x
                              },
                              
#' setExpstart
#'
#' @param x date value for when the experiment starts
#'
#' @return
#' @export
#'
#' @examples
                              setExpstart = function(x){
                                self$ExpStart = x
                              },
                              
#' setTimepointDur
#'
#' @param x sampling rate
#'
#' @return
#' @export
#'
#' @examples
#' no examples provided
                              setTimepointDur = function(x){
                                self$timepointDur = x
                              },
#' Initialize_DS
#'
#' @param server is it really needed?
#'
#' @return
#' @export
#'
                              initialize_DS = function(server){ #create individual functions that close various segments and call them here
                                #buttons to observe
                                shinyjs::hide(id = "DFsubsetRT", anim = FALSE)
                                shinyjs::hide(id = "DFsubsetRT2", anim = FALSE)
                                shinyjs::hide(id = "DFsubsetTP", anim = FALSE)
                                #related to serietype radiobutton
                                shinyjs::hide(id = "rtStart", anim = FALSE)
                                shinyjs::hide(id = "rtStartHour", anim = FALSE)
                                shinyjs::hide(id = "tpDuration", anim = FALSE)
                                # related to DFsubsetRT radiobutton
                                shinyjs::hide(id = "timeFrame1", anim = FALSE)
                                shinyjs::hide(id = "timeFrame2", anim = FALSE)
                                shinyjs::hide(id = "RTanalysis_starttime", anim = FALSE)
                                shinyjs::hide(id = "RTanalysis_endtime", anim = FALSE)
                                # related to DFsubsetTP radiobutton
                                shinyjs::hide(id = "TPanalysis_starttime", anim = FALSE)
                                shinyjs::hide(id = "TPanalysis_endtime", anim = FALSE)
                                # related to DFsubsetRT2 radiobutton
                                shinyjs::hide(id = "timeFrame3", anim = FALSE)
                                shinyjs::hide(id = "timeFrame4", anim = FALSE)
                                shinyjs::hide(id = "RTanalysis_starttime2", anim = FALSE)
                                shinyjs::hide(id = "RTanalysis_endtime2", anim = FALSE)
                                # related to DFsubsetRT2 radiobutton
                                shinyjs::hide(id = "TPanalysis_starttime2", anim = FALSE)
                                shinyjs::hide(id = "TPanalysis_endtime2", anim = FALSE)
                              }
                              # old initialize UI
                              
                                # self$clearActos()
                                # self$clearDPActos()
                                # self$clearDAct()
                                # self$clearPer()
                                # self$clearSubsetting()
                                # hide(id = "dataTab", anim = FALSE)        #hide data
                                # shinyjs::hide(id = "dailyTab", anim = FALSE)       #hide daily tab
                                # shinyjs::hide(id = "chooseId", anim = FALSE)       #hide individual id selection in DP actogram
                                # shinyjs::hide(id = "idCol", anim = FALSE)          #hide custom metadata mode
                                # shinyjs::hide(id = "sexCol", anim = FALSE)
                                # shinyjs::hide(id = "geneCol", anim = FALSE)
                                # shinyjs::hide(id = "cabCol", anim = FALSE)
                                # shinyjs::hide(id = "metafiltered", anim = FALSE)   #hide filtered metadata table
                                # shinyjs::hide(id = "tablemetafilter", anim = FALSE)
                                # shinyjs::hide(id = "alignData", anim = FALSE)
                                # shinyjs::hide(id = "timeFrame3", anim = FALSE)
                                # shinyjs::hide(id = "RTanalysis_starttime2", anim = FALSE)
                                # shinyjs::hide(id = "RTanalysis_endtime2", anim = FALSE)
                                # shinyjs::hide(id = "TPanalysis_starttime2", anim = FALSE)
                                # shinyjs::hide(id = "TPanalysis_endtime2", anim = FALSE)
                                # shinyjs::hide(id = "rtStart", anim = FALSE)
                                # shinyjs::hide(id = "rtStarthour", anim = FALSE)
                                # shinyjs::hide(id = "DFsubsetRT", anim = FALSE)
                                # shinyjs::hide(id = "timeFrame1", anim = FALSE)
                                # shinyjs::hide(id = "RTanalysis_starttime", anim = FALSE)
                                # shinyjs::hide(id = "RTanalysis_endtime", anim = FALSE)
                                # shinyjs::hide(id = "TPanalysis_starttime", anim = FALSE)
                                # shinyjs::hide(id = "TPanalysis_endtime", anim = FALSE)
                                # shinyjs::hide(id = "timeFrame4", anim = FALSE)
                                # shinyjs::hide(id = "DFsubsetRT2", anim = FALSE)
                                # shinyjs::hide(id = "Dl2", anim = FALSE) #hide download button for table2 before it's created
                                # shinyjs::hide(id = "chooseM", anim = FALSE) #HIDE button to select mouse table to display
                                # hide unnecessary buttons in box1_1 #
                                # shinyjs::hide(id = "serieType", anim = FALSE)
                                # shinyjs::hide(id = "DFsubsetTP", anim = FALSE)
                              # },
                              # clearActos = function(){
                              #   hide(id = "spin1_1", anim = FALSE)
                              #   hide(id = "actogram1", anim = FALSE)      #hide actograms
                              #   hide(id = "actogram_1", anim = FALSE)
                              #   hide(id = "spin1_2", anim = FALSE)
                              #   hide(id = "actogram2", anim = FALSE)
                              #   hide(id = "actogram_2", anim = FALSE)
                              #   hide(id = "spin1_3", anim = FALSE)
                              #   hide(id = "actogram3", anim = FALSE)
                              #   hide(id = "actogram_3", anim = FALSE)
                              #   hide(id = "spin1_4", anim = FALSE)
                              #   hide(id = "actogram4", anim = FALSE)
                              #   hide(id = "actogram_4", anim = FALSE)
                              # },
                              # clearDPActos = function(){
                              #   hide(id = "DPactogram1", anim = FALSE)    #hide double plotted acto
                              #   hide(id = "DPactogram_1", anim = FALSE)
                              #   hide(id = "DPactogram2", anim = FALSE)
                              #   hide(id = "DPactogram_2", anim = FALSE)
                              #   hide(id = "DPactogram3", anim = FALSE)
                              #   hide(id = "DPactogram_3", anim = FALSE)
                              #   hide(id = "DPactogram4", anim = FALSE)
                              #   hide(id = "DPactogram_4", anim = FALSE)
                              #   hide(id = "DPactogram5", anim = FALSE)
                              #   hide(id = "DPactogram_5", anim = FALSE)
                              #   hide(id = "spin2_1", anim = FALSE)        #hide spinners for double plotted acto
                              #   hide(id = "spin2_2", anim = FALSE)
                              #   hide(id = "spin2_3", anim = FALSE)
                              #   hide(id = "spin2_4", anim = FALSE)
                              #   hide(id = "spin2_5", anim = FALSE)
                              # },
                              # clearDAct = function(){
                              #   hide(id = "DAct1", anim = FALSE)          #hide daily activity
                              #   hide(id = "DAct_1", anim = FALSE)
                              #   hide(id = "DAct2", anim = FALSE)
                              #   hide(id = "DAct_2", anim = FALSE)
                              #   hide(id = "DAct3", anim = FALSE)
                              #   hide(id = "DAct_3", anim = FALSE)
                              #   hide(id = "DAct4", anim = FALSE)
                              #   hide(id = "DAct_4", anim = FALSE)
                              #   hide(id = "DAct5", anim = FALSE)
                              #   hide(id = "DAct_5", anim = FALSE)
                              #   hide(id = "DAct6", anim = FALSE)
                              #   hide(id = "DAct_6", anim = FALSE)
                              #   hide(id = "spin3_1", anim = FALSE)        #hide spinners for Daily activity
                              #   hide(id = "spin3_2", anim = FALSE)
                              #   hide(id = "spin3_3", anim = FALSE)
                              #   hide(id = "spin3_4", anim = FALSE)
                              #   hide(id = "spin3_5", anim = FALSE)
                              #   hide(id = "spin3_6", anim = FALSE)
                              # },
                              # clearPer = function(){
                              #   hide(id = "Per1", anim = FALSE)               #hide Periodogram plots
                              #   hide(id = "Per_1", anim = FALSE)
                              #   hide(id = "Per2", anim = FALSE)
                              #   hide(id = "Per_2", anim = FALSE)
                              #   hide(id = "Per3", anim = FALSE)
                              #   hide(id = "Per_3", anim = FALSE)
                              #   hide(id = "Per4", anim = FALSE)
                              #   hide(id = "Per_4", anim = FALSE)
                              #   hide(id = "Per5", anim = FALSE)
                              #   hide(id = "Per_5", anim = FALSE)
                              #   hide(id = "spin4_1", anim = FALSE)
                              #   hide(id = "spin4_2", anim = FALSE)
                              #   hide(id = "spin4_3", anim = FALSE)
                              #   hide(id = "spin4_4", anim = FALSE)
                              #   hide(id = "spin4_5", anim = FALSE)
                              # },
                              # clearSubsetting = function(){
                              #   shinyjs::hide(id = "idSubsetList", anim = FALSE)   #hide plot subset settings
                              #   shinyjs::hide(id = "sexSubsetList", anim = FALSE)
                              #   shinyjs::hide(id = "geneSubsetList", anim = FALSE)
                              #   shinyjs::hide(id = "cabSubsetList", anim = FALSE)
                              #   shinyjs::hide(id = "text11", anim = FALSE)
                              #   shinyjs::hide(id = "metaUniqueO", anim = FALSE)
                              #   shinyjs::hide(id = "timeSubset", anim = FALSE)
                              # },
                              # showSubsetting = function(){
                              #   shinyjs::show(id = "idSubsetList", anim = FALSE)   #hide plot subset settings
                              #   shinyjs::show(id = "sexSubsetList", anim = FALSE)
                              #   shinyjs::show(id = "geneSubsetList", anim = FALSE)
                              #   shinyjs::show(id = "cabSubsetList", anim = FALSE)
                              #   shinyjs::show(id = "text11", anim = FALSE)
                              #   shinyjs::show(id = "metaUniqueO", anim = FALSE)
                              #   shinyjs::show(id = "timeSubset", anim = FALSE)
                              # }
                            ))
