# #### manipulate csv file exported from actogram data to create a wide format table
# library("tidyr")
# library("magrittr")
# library("dplyr")
# 
# data <- read.csv("/Users/marcoferrari/Desktop/maceR_output/Counts_table2022-11-10.csv")
# # sort based on value of column 1
# # keep t (col2) as first colums
# # get list of unique values of col 1 (number of column + 1)
# 
# 
# #### creation of wide table with individual ids as columns ####
# 
# newtable <- data %>% pivot_wider(names_from = c(id, Sex, Genotype),
#                                      values_from = Activity,
#                                      values_fill = 0)
# 
# # gather all the unique ids present in col1
# ids <- unique(data[, 1])
# # create a new object that will contrain the final table
# newtable = NULL
# # create an object that will contain all the rownames
# rownames_list = NULL
# # add the first rowname
# rownames_list[1] <- "time"
# for (i in 1:length(ids)){
#   # extract only the data corresponding to the selected id
#   to_extract <- which(data$id == ids[i])
#   filtered <- data[to_extract, ]
#   # create the rowname from id, sex, and genotype and add it to the list
#   head <- paste(filtered[1, 1], filtered[1, 4], filtered[1, 5], sep = "_")
#   rownames_list[i+1] <- head
#   # isolate time
#   t <- filtered[, 2]
#   #if newtable object is empty, add the time
#   if(is.null(newtable)){
#     newtable <- cbind(newtable, t)
#   }
#   # isolate the activity
#   activity <- filtered[, 3]
#   # add activity data to the table
#   newtable <- cbind(newtable, activity)
# }
# #change rownames using the newly created ones
# colnames(newtable) <- rownames_list
# 
# 
# 
# 
# #### creation of table with value summarised by sex in column 2 and 3, ####
#     # followed by the values of individual ids 
# Sex <- unique(data$Sex)
# datalist = list()
# sexMeans = list()
# # old table listing various ids and activity
# newtable <- data %>% pivot_wider(names_from = c(id, Sex, Genotype),
#                                  values_from = Activity,
#                                  values_fill = 0)
# # filter males and females and create means
# for (i in seq_along(Sex)){
#   filter_sex <- which(data$Sex == Sex[i])
#   datalist[[i]] <- data[filter_sex, ] %>% pivot_wider(names_from = c(id, Sex, Genotype),
#                                                       values_from = Activity,
#                                                       values_fill = 0)
#   sexMeans[[i]] <- datalist[[i]][, -1]
#   sexMeans[[i]] <- rowMeans(sexMeans[[i]], na.rm = FALSE, dims = 1)
#   # sexMeans[[i]] %>% mutate(sexmean=select(., 2:ncol(sexMeans[[i]])) %>% rowMeans())
#   newtable <- cbind(newtable, sexMeans[[i]])
#   rename(newtable, Sex[i] = sexMeans[[i]])
#   
# }
# 
# activity_sex_mean <- data[, Sex == Sex[1]]
# newtable_sex <- data %>% pivot_wider(names_from = c(id, Sex, Genotype),
#                                      values_from = Activity, 
#                                      values_fill = 0,
#                                      values_fn = mean)
# 
# ### experiment ####
# 
# # steps:
# # get the list of unique values in Sex
# # filter the values of Id and Sex and put them in as many tables as the value
# # of len(sex)
# # calculate the rowmeans (excluding a possibel time column)
# # calculate the table of individual ids using pivot_wider
# # use a for cycle to add the column means calculated in position 2, so that 
# # you have time, then means, then individual ids
# 
# 
# # gather all the unique ids present in col1
# Sexes <- unique(data$Sex)
# 
# for (i in seq_along(Sex)){
#   datalist[i] <- data[which(data$Sex == Sex[i]), ]
# }
# # create a new object that will contrain the final table
# newtable = NULL
# # create an object that will contain all the rownames
# rownames_list = NULL
# # add the first rowname
# rownames_list[1] <- "time"
# for (i in 1:length(Sexes)){
#   # extract only the data corresponding to the selected id
#   to_extract <- which(data$Sex == Sexes[i])
#   filtered <- data[to_extract, ]
#   # create the rowname from id, sex, and genotype and add it to the list
#   head <- paste(filtered[1, 1], filtered[1, 4], filtered[1, 5], sep = "_")
#   rownames_list[i+1] <- head
#   # isolate time
#   t <- filtered[, 2]
#   #if newtable object is empty, add the time
#   if(is.null(newtable)){
#     newtable <- cbind(newtable, t)
#   }
#   # isolate the activity
#   activity <- filtered[, 3]
#   # add activity data to the table
#   newtable <- cbind(newtable, activity)
# }
# #change rownames using the newly created ones
# colnames(newtable) <- rownames_list
# #### creation of table with value summarised by cabinet ####
# 
# 
# #### creation of wide table for sum of daily activity ####
# activity <- read.csv("C:/Users/mf420/Desktop/MACE output/Total_daily_act2022-11-15.csv")
# 
# activity_wide <- activity %>% 
#   tidyr::pivot_wider(
#     names_from = c(id, Genotype, Sex, Cabinet),
#     values_from = Activity,
#     names_glue = "{id}_{Sex}_{Genotype}_{Cabinet}",
#     values_fill = 0
#   )













# 
# ### sexmeans_id_wide ####
# "avgDay_sexmeans_id_wide" = {
#   #temporary line to get data
#   data <- read.csv(file = "C:/Users/mf420/Desktop/average_day.csv")
#   data <- data[, -1]
#   # get list of all sexes
#   Sex <- unique(data$sex)
#   # create two list to host data
#   datalist = list()
#   sexMeans = list()
#   
#   # create table listing various ids and activity
#   newtable <- data %>% tidyr::pivot_wider(names_from = c(mouse, sex, genotype),
#                                               values_from = activity,
#                                               values_fill = 0)
#   # create table of same length of data
#   newtable_sex <- newtable[, 1]
#   #calculate errors
#   errors <- data %>% 
#     dplyr::group_by(sex, CT) %>%
#     dplyr::summarise(n = dplyr::n(),
#                      STDev = sd(activity),
#                      SEM = sd(activity)/sqrt(n))
#   # iterate for all sexes present in data
#   for (i in seq_along(Sex)){
#     # filter individual sexes
#     filter_sex <- data[data$sex == Sex[i], ]
#     error_sex <- errors[errors$sex == Sex[i], ]
#     # extract only the last three columns of errors
#     error_sex <- error_sex[, c(3:5)]
#     # create a wide table for each genotype
#     datalist[[i]] <- filter_sex %>%tidyr::pivot_wider(names_from = c(mouse, sex, genotype),
#                                                               values_from = activity,
#                                                               values_fill = 0)
#     # remove time column
#     sexMeans[[i]] <- datalist[[i]][, -1]
#     # create means
#     activity <- rowMeans(sexMeans[[i]], na.rm = FALSE, dims = 1)
#     # add calculation of SD and SEM as columns
#     values <- cbind(activity, error_sex)
#     # change rownames to the correct sex
#     newColnames <- colnames(values)
#     for (j in c(1:4)){
#     newColnames[j] <- paste(newColnames[j], Sex[i], sep = "_")
#     }
#     colnames(values) <- newColnames
#     # add mean value to table
#     newtable_sex <- cbind(newtable_sex, values)
#   }
#   #export
#   write.csv(newtable_sex, file = "C:/Users/mf420/Desktop/averageDay_bySex.csv", row.names = FALSE)
#   # add newtable_sex to list available for output
#   self$locomotor_act[[3]] <- newtable_sex
# }
# 
# 
# 
# "avgDay_genmeans_id_wide" = {
#   #temporary line to get data
#   data <- read.csv(file = "C:/Users/mf420/Desktop/average_day.csv")
#   data <- data[, -1]
#   # get list of all sexes
#   Gen <- unique(data$genotype)
#   # create two list to host data
#   datalist = list()
#   genMeans = list()
#   
#   # create table listing various ids and activity
#   newtable <- data %>% tidyr::pivot_wider(names_from = c(mouse, sex, genotype),
#                                           values_from = activity,
#                                           values_fill = 0)
#   # create table of same length of data
#   newtable_gen <- newtable[, 1]
#   #calculate errors
#   errors <- data %>% 
#     dplyr::group_by(genotype, CT) %>%
#     dplyr::summarise(n = dplyr::n(),
#                      STDev = sd(activity),
#                      SEM = sd(activity)/sqrt(n))
#   # iterate for all sexes present in data
#   for (i in seq_along(Gen)){
#     # filter individual sexes
#     filter_gen <- data[data$genotype == Gen[i], ]
#     error_gen <- errors[errors$genotype == Gen[i], ]
#     # extract only the last three columns of errors
#     error_gen <- error_gen[, c(3:5)]
#     # create a wide table for each genotype
#     datalist[[i]] <- filter_gen %>%tidyr::pivot_wider(names_from = c(mouse, sex, genotype),
#                                                       values_from = activity,
#                                                       values_fill = 0)
#     # remove time column
#     genMeans[[i]] <- datalist[[i]][, -1]
#     # create means
#     activity <- rowMeans(genMeans[[i]], na.rm = FALSE, dims = 1)
#     # add calculation of SD and SEM as columns
#     values <- cbind(activity, error_gen)
#     # change rownames to the correct sex
#     newColnames <- colnames(values)
#     for (j in c(1:4)){
#       newColnames[j] <- paste(newColnames[j], Gen[i], sep = "_")
#     }
#     colnames(values) <- newColnames
#     # add mean value to table
#     newtable_gen <- cbind(newtable_gen, values)
#   }
#   #export
#   write.csv(newtable_gen, file = "C:/Users/mf420/Desktop/averageDay_byGen.csv", row.names = FALSE)
#   # add newtable_gen to list available for output
#   self$locomotor_act[[3]] <- newtable_gen
# }


