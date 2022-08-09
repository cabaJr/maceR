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
#' @field LDcondition list containing code to generate LD shading in actograms
#' @field LDparams list containing user defined LD condition settings
#' @field plotTab list containing values fro showing/hiding elements in Plots Tab
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
                              plotTab = list(tab = FALSE,
                                             DPActo = FALSE,
                                             acto = FALSE,
                                             dayAct = FALSE,
                                             period = FALSE,
                                             avgDay = FALSE
                                             ),
                              env1 = NULL,
                              env2 = NULL,
                              env3 = NULL,
                              env4 = NULL,
                              env_msg = NULL,
                              
#' setData
#'
#' @param outsideData list of data files uploaded by the user
#'
                              setData = function(outsideData){
                                self$dataList <- outsideData
                              },
                              
#' setMeta
#'
#' @param outsideMeta metadata file uploaded by the user
#'
                              setMeta = function(outsideMeta){
                                # add checks for files to be in the correct format
                                self$metadata <- outsideMeta
                              },
                              
#' setListMice
#'
#' @param env environment
#'
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
                              saveLDparams = function(x, light, ddVal, ddStart){
                                self$LDparams$light = light
                                self$LDparams$DDcheck = ddVal
                                self$LDparams$DDstart = ddStart
                              },
                                
                              
#' setLD
#'
#' @param env App_settings environment
#' @param light ligthOn value
#' @param ddVal Y/N value for DD
#' @param ddStart day in which DD starts
#'
                              setLD = function(env, light, ddVal, ddStart){ #add more comments
                                qty <- length(env$env2$myCleanMice)
                                length1 <- max(env$env2$Annotate$metaTable$Datapoints)*60
                                per1End <- ddStart*86400
                                Llpha1 <- (0.2 / qty)
                                Llpha2 <- 0.01
                                # define DDstart and DDlength
                                DDday <- as.numeric(ddStart)
                                dataLen <- (length1%/%86400)-2
                                if(ddStart <= 2){
                                  DDStart1 <- 1
                                  DDStart2 <- 1
                                }else{
                                DDStart1 <- (1-((DDday-1)/dataLen))
                                DDStart2 <- (1-((DDday-2)/dataLen))
                                }
                                DDlength <- 24-as.numeric(light)
                                SLLD <- switch(ddVal,
                                               "Yes" = {
                                                 ggetho::stat_ld_annotations(
                                                   height = 1, 
                                                   alpha = Llpha1, 
                                                   outline = NA, 
                                                   period = behavr::hours(24), 
                                                   l_duration = behavr::hours(light), 
                                                   x_limits = c(0, per1End), 
                                                   phase = behavr::hours(6), 
                                                   ld_colours = c(NA, "black")
                                                   )
                                                 },
                                               "No" = {
                                                 ggetho::stat_ld_annotations(
                                                   height = 1, 
                                                   alpha = Llpha1, 
                                                   outline = NA, 
                                                   period = behavr::hours(24), 
                                                   l_duration = behavr::hours(light), 
                                                   phase = behavr::hours(6), 
                                                   ld_colours = c(NA, "black")
                                                   )
                                                 }
                                               )
                                # check the rest of the function from here + implement value gathering inside server and function call
                                SLDD <- switch(ddVal,
                                               "Yes" = {
                                                 ggetho::stat_ld_annotations(
                                                   height = 1, 
                                                   alpha = Llpha1, 
                                                   outline = NA, 
                                                   period = behavr::hours(24), 
                                                   l_duration = behavr::hours(0), 
                                                   x_limits = c(per1End, length1), 
                                                   phase = 0, 
                                                   ld_colours = c(NA, "black"))},
                                               "No" = {NULL})
                                DPLD <- ggetho::stat_ld_annotations(
                                  height = 1, 
                                  alpha = Llpha2, 
                                  outline = NA, 
                                  period = behavr::hours(24), 
                                  l_duration = behavr::hours(light), 
                                  phase = behavr::hours(6), 
                                  ld_colours = c(NA, "black")
                                  )
                                DPDD1 <- switch(ddVal,
                                                "Yes" = {
                                                  ggetho::stat_ld_annotations(
                                                    height = DDStart1, 
                                                    alpha = Llpha2, 
                                                    outline = NA, 
                                                    period = behavr::hours(24), 
                                                    l_duration = behavr::hours(DDlength), 
                                                    x_limits = c(behavr::hours(0), 
                                                                 behavr::hours(24)), 
                                                    phase = behavr::hours(18), 
                                                    ld_colours = c(NA, "black"))},
                                                "No" = {NULL}
                                                )
                                DPDD2 <- switch(ddVal,
                                                "Yes" = {
                                                  ggetho::stat_ld_annotations(
                                                    height = DDStart2, 
                                                    alpha = Llpha2, 
                                                    outline = NA, 
                                                    period = behavr::hours(24), 
                                                    l_duration = behavr::hours(DDlength), 
                                                    x_limits = c(behavr::hours(24), 
                                                                 behavr::hours(48)), 
                                                    phase = behavr::hours(18), 
                                                    ld_colours = c(NA, "black")
                                                    )
                                                  },
                                                "No" = {NULL}
                                                )
                                self$LDcondition$SLLD = SLLD
                                self$LDcondition$SLDD = SLDD
                                self$LDcondition$DPLD = DPLD
                                self$LDcondition$DPDD1 = DPDD1
                                self$LDcondition$DPDD2 = DPDD2
                                # browser()
                              },
                              
#' updateTimeRange
#'
#' @param range to be rechecked (numeric)
#' @param env App settings environment
#'
                              updateTimeRange = function(range, env){
                                self$subsetting$timespan <- range
                                #also update LD setting when time subsetting
                                if(is.null(self$env1$myMice) == FALSE){
                                light <- self$LDparams$light
                                ddVal <- self$LDparams$DDcheck
                                ddstart <- self$LDparams$DDstart
                                newddStart <- ddstart - range[1]
                                if(newddStart<0){
                                  newddStart <- 0
                                }
                                self$setLD(env, light = light, ddVal = ddVal, ddStart = newddStart)
                                }
                              },
                              
#' setDiscRow
#'
#' @param x number of rows to discard (numeric)
#'
                              setDiscRow = function(x){
                                self$discardRow = x
                              },
#' setTimeDisp
#'
#' @param x to display time in 12/24h format
#'
                              setTimeDisp = function(x){
                                self$timeDisplay = x
                              },
                              
#' setExpstart
#'
#' @param x date value for when the experiment starts
#'
                              setExpstart = function(x){
                                self$ExpStart = x
                              },
                              
#' setTimepointDur
#'
#' @param x sampling rate
#'
                              setTimepointDur = function(x){
                                self$timepointDur = x
                              }
                            ))
