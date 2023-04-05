library(shiny)
library(MASS)
library(dplyr)
library(tidyr)
library(reshape2)
library(DT)

server <- function(input, output, session) {
  
  #### reactive dataset
  files <- file.info(list.files(pattern = "\\.csv$", full.names = T))
  fileData <- reactiveFileReader(1000, session, rownames(files)[which.max(files$mtime)], read.csv2, sep = ",")
  
  ## event
  output$event <- renderText({
    df <- as.data.frame(fileData())
    df$event[1]
  })
  
  ## location & date
  output$location <- renderText({
    df <- as.data.frame(fileData())
    location <- paste(unlist(strsplit(gsub("\xa0", " ", df$location[1]), ",")), collapse = ", ")
    date <- df$fight_date[1]
    paste(location, date, sep = " - ")
  })
  
  ## bout, weightclass, time format
  output$bout <- renderText({
    df <- as.data.frame(fileData())
    bout <- df$bout[1]
    weightclass <- df$weight_class[1]
    time_format <- ifelse(df$time_format[1] == "5 Rnd (5-5-5-5-5)","5 Rnd","3 Rnd")
    paste(bout, weightclass, time_format, sep = ", ")
  })
  
  ## bout
  output$bout2 <- renderText({
    df <- as.data.frame(fileData())
    bout2 <- df$bout[1]
    paste(bout2)
  })
  
  ## fighter_r
  output$fighter_r <- renderText({
    df <- as.data.frame(fileData())
    df$fighter_r[1]
  })
  
  ## fighter_b
  output$fighter_b <- renderText({
    df <- as.data.frame(fileData())
    df$fighter_b[1]
  })
  
  #### load regression models
  ms <- readRDS("model_scores.rds")
  mw_R1R2 <- readRDS("model_redwins_R1R2.rds")
  
  ################ tab App
  
  ######## pred
  output$pred <- renderText({
    df <- as.data.frame(fileData())
    
    # recode "time" variable ###########################################
    #df$time[df$time == "05:00"] <- "5:00"
    #df$time[nrow(df)] <- "4:36"
    
    # recode "det" variable
    df$det_round <- colsplit(string=df$det, pattern = ", ", names=c("round", "time"))[,1]
    df$det_round <- gsub("End ", "", df$det_round)
    df$det_round[df$det_round == "Final"] <- paste("R",df$last_round[nrow(df)], sep = "")
    
    df$det_time <- colsplit(string=df$det, pattern = ", ", names=c("round", "time"))[,2]
    df$det_time_2 <- ifelse(df$det_time == "", df$det, 
                            colsplit(string=df$det, pattern = ", ", names=c("round", "time"))[,2])
    
    # add "age" variable
    df$fight_date_2 <- as.Date(df$fight_date, format="%Y-%m-%d")
    df$dob_r_2 <- as.Date(df$dob_r, format="%m/%d/%Y")
    df$dob_b_2 <- as.Date(df$dob_b, format="%m/%d/%Y")
    df$age_r <- lubridate::time_length(difftime(df$fight_date_2, df$dob_r_2), "years")
    df$age_b <- lubridate::time_length(difftime(df$fight_date_2, df$dob_b_2), "years")
    
    # recode "height" variable
    ht_r_2 <- gsub('\"', '', df$ht_r)
    ht_r_2 <- data.frame(ht_r_2)
    ht_r_2 <- ht_r_2 %>% separate(ht_r_2, c('feet', 'inches'), "'", convert = TRUE)
    ht_r_2_cm <- (12*as.numeric(ht_r_2$feet) + as.numeric(ht_r_2$inches))*2.54
    
    ht_b_2 <- gsub('\"', '', df$ht_b)
    ht_b_2 <- data.frame(ht_b_2)
    ht_b_2 <- ht_b_2 %>% separate(ht_b_2, c('feet', 'inches'), "'", convert = TRUE)
    ht_b_2_cm <- (12*as.numeric(ht_b_2$feet) + as.numeric(ht_b_2$inches))*2.54
    
    df$ht_r_2_cm <- ht_r_2_cm
    df$ht_b_2_cm <- ht_b_2_cm
    
    # recode "control_time" variable
    for (i in 1:nrow(df)) {
      df$ctrl_r_2[i] <- c(unlist(unclass(strptime(df$ctrl_r[i], format='%M:%S'))[c("min", "sec")]) %*% c(60,1))/60
      df$ctrl_b_2[i] <- c(unlist(unclass(strptime(df$ctrl_b[i], format='%M:%S'))[c("min", "sec")]) %*% c(60,1))/60
    }
    
    ## diff (fighter_r - fighter_b)
    # fighter characteristics
    df$age <- df$age_r - df$age_b
    df$height <- df$ht_r_2_cm - df$ht_b_2_cm
    
    # stats ESPN 
    df$knockdowns <- df$kd_r - df$kd_b
    df$significant_strikes_landed <- df$sigstr_val_r - df$sigstr_val_b
    df$total_strikes_attempted <- df$sigstr_tot_r - df$sigstr_tot_b
    df$body_landed <- df$bdy_val_r - df$bdy_val_b
    df$leg_landed <- df$lgs_val_r - df$lgs_val_b
    df$takedown_successful <- df$td_val_r - df$td_val_b
    df$takedown_attempted <- df$td_tot_r - df$td_tot_b
    df$submission_attempt <- df$subatt_r - df$subatt_b
    df$control_time <- df$ctrl_r_2 - df$ctrl_b_2
    
    #### pred: scores
    
    ## round1
    if ("End R1" %in% df$det == TRUE & df$last_round[nrow(df)] != 1) {
      df_R1_score <- df[match("End R1",df$det),c(which(names(df)=="knockdowns"),
                                                 which(names(df)=="significant_strikes_landed"),
                                                 which(names(df)=="total_strikes_attempted"),
                                                 which(names(df)=="body_landed"),
                                                 which(names(df)=="leg_landed"),
                                                 which(names(df)=="takedown_successful"),
                                                 which(names(df)=="takedown_attempted"),
                                                 which(names(df)=="submission_attempt"),
                                                 which(names(df)=="control_time"))]
      
      df_R1_score_pred <- predict(ms, df_R1_score, type = "probs")
      
      if (names(df_R1_score_pred)[which(df_R1_score_pred == max(df_R1_score_pred))] == "-3") {
        round1_score_r <- "7"
        round1_score_b <- "10"
      } else if (names(df_R1_score_pred)[which(df_R1_score_pred == max(df_R1_score_pred))] == "-2") {
        round1_score_r <- "8"
        round1_score_b <- "10"
      } else if (names(df_R1_score_pred)[which(df_R1_score_pred == max(df_R1_score_pred))] == "-1") {
        round1_score_r <- "9"
        round1_score_b <- "10"
      } else if (names(df_R1_score_pred)[which(df_R1_score_pred == max(df_R1_score_pred))] == "0") {
        round1_score_r <- "10"
        round1_score_b <- "10"
      } else if (names(df_R1_score_pred)[which(df_R1_score_pred == max(df_R1_score_pred))] == "1") {
        round1_score_r <- "10"
        round1_score_b <- "9"
      } else if (names(df_R1_score_pred)[which(df_R1_score_pred == max(df_R1_score_pred))] == "2") {
        round1_score_r <- "10"
        round1_score_b <- "8"
      } else {
        round1_score_r <- "10"
        round1_score_b <- "7"
      }
      
    } else {
      round1_score_r <- "X"
      round1_score_b <- "X"
    }
    
    output$round1_score_r <- renderText(round1_score_r)
    output$round1_score_b <- renderText(round1_score_b)
    
    ## round2
    if ("End R2" %in% df$det == TRUE & df$last_round[nrow(df)] != 2) {
      df_R2_score_cum <- df[match("End R2",df$det),c(which(names(df)=="knockdowns"),
                                                     which(names(df)=="significant_strikes_landed"),
                                                     which(names(df)=="total_strikes_attempted"),
                                                     which(names(df)=="body_landed"),
                                                     which(names(df)=="leg_landed"),
                                                     which(names(df)=="takedown_successful"),
                                                     which(names(df)=="takedown_attempted"),
                                                     which(names(df)=="submission_attempt"),
                                                     which(names(df)=="control_time"))]
      
      df_R2_score <- df_R2_score_cum - df_R1_score
      
      df_R2_score_pred <- predict(ms, df_R2_score, type = "probs")
      
      if (names(df_R2_score_pred)[which(df_R2_score_pred == max(df_R2_score_pred))] == "-3") {
        round2_score_r <- "7"
        round2_score_b <- "10"
      } else if (names(df_R2_score_pred)[which(df_R2_score_pred == max(df_R2_score_pred))] == "-2") {
        round2_score_r <- "8"
        round2_score_b <- "10"
      } else if (names(df_R2_score_pred)[which(df_R2_score_pred == max(df_R2_score_pred))] == "-1") {
        round2_score_r <- "9"
        round2_score_b <- "10"
      } else if (names(df_R2_score_pred)[which(df_R2_score_pred == max(df_R2_score_pred))] == "0") {
        round2_score_r <- "10"
        round2_score_b <- "10"
      } else if (names(df_R2_score_pred)[which(df_R2_score_pred == max(df_R2_score_pred))] == "1") {
        round2_score_r <- "10"
        round2_score_b <- "9"
      } else if (names(df_R2_score_pred)[which(df_R2_score_pred == max(df_R2_score_pred))] == "2") {
        round2_score_r <- "10"
        round2_score_b <- "8"
      } else {
        round2_score_r <- "10"
        round2_score_b <- "7"
      }
      
    } else {
      round2_score_r <- "X"
      round2_score_b <- "X"
    }
    
    output$round2_score_r <- renderText(round2_score_r)
    output$round2_score_b <- renderText(round2_score_b)
    
    ## round3
    if (df$time_format[1] == "5 Rnd (5-5-5-5-5)") {
      if ("End R3" %in% df$det == TRUE & df$last_round[nrow(df)] != 3) {
        df_R3_score_cum <- df[match("End R3",df$det),c(which(names(df)=="knockdowns"),
                                                       which(names(df)=="significant_strikes_landed"),
                                                       which(names(df)=="total_strikes_attempted"),
                                                       which(names(df)=="body_landed"),
                                                       which(names(df)=="leg_landed"),
                                                       which(names(df)=="takedown_successful"),
                                                       which(names(df)=="takedown_attempted"),
                                                       which(names(df)=="submission_attempt"),
                                                       which(names(df)=="control_time"))]
        
        df_R3_score <- df_R3_score_cum - df_R2_score_cum
        
        df_R3_score_pred <- predict(ms, df_R3_score, type = "probs")
        
        if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "-3") {
          round3_score_r <- "7"
          round3_score_b <- "10"
        } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "-2") {
          round3_score_r <- "8"
          round3_score_b <- "10"
        } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "-1") {
          round3_score_r <- "9"
          round3_score_b <- "10"
        } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "0") {
          round3_score_r <- "10"
          round3_score_b <- "10"
        } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "1") {
          round3_score_r <- "10"
          round3_score_b <- "9"
        } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "2") {
          round3_score_r <- "10"
          round3_score_b <- "8"
        } else {
          round3_score_r <- "10"
          round3_score_b <- "7"
        }
        
      } else {
        round3_score_r <- "X"
        round3_score_b <- "X"
      }
      
    } else if (df$time_format[1] == "3 Rnd (5-5-5)") {
      if ("End R3" %in% df$det == TRUE & df$last_round[nrow(df)] != 3) {
        df_R3_score_cum <- df[match("End R3",df$det),c(which(names(df)=="knockdowns"),
                                                       which(names(df)=="significant_strikes_landed"),
                                                       which(names(df)=="total_strikes_attempted"),
                                                       which(names(df)=="body_landed"),
                                                       which(names(df)=="leg_landed"),
                                                       which(names(df)=="takedown_successful"),
                                                       which(names(df)=="takedown_attempted"),
                                                       which(names(df)=="submission_attempt"),
                                                       which(names(df)=="control_time"))]
        
        df_R3_score <- df_R3_score_cum - df_R2_score_cum
        
        df_R3_score_pred <- predict(ms, df_R3_score, type = "probs")
        
        if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "-3") {
          round3_score_r <- "7"
          round3_score_b <- "10"
        } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "-2") {
          round3_score_r <- "8"
          round3_score_b <- "10"
        } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "-1") {
          round3_score_r <- "9"
          round3_score_b <- "10"
        } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "0") {
          round3_score_r <- "10"
          round3_score_b <- "10"
        } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "1") {
          round3_score_r <- "10"
          round3_score_b <- "9"
        } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "2") {
          round3_score_r <- "10"
          round3_score_b <- "8"
        } else {
          round3_score_r <- "10"
          round3_score_b <- "7"
        }
        
      } else if ("End R3" %in% df$det == TRUE & df$last_round[nrow(df)] == 3) {
        if (grepl("Dec", df$method[nrow(df)]) == TRUE) {
          df_R3_score_cum <- df[match("End R3",df$det),c(which(names(df)=="knockdowns"),
                                                         which(names(df)=="significant_strikes_landed"),
                                                         which(names(df)=="total_strikes_attempted"),
                                                         which(names(df)=="body_landed"),
                                                         which(names(df)=="leg_landed"),
                                                         which(names(df)=="takedown_successful"),
                                                         which(names(df)=="takedown_attempted"),
                                                         which(names(df)=="submission_attempt"),
                                                         which(names(df)=="control_time"))]
          
          df_R3_score <- df_R3_score_cum - df_R2_score_cum
          
          df_R3_score_pred <- predict(ms, df_R3_score, type = "probs")
          
          if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "-3") {
            round3_score_r <- "7"
            round3_score_b <- "10"
          } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "-2") {
            round3_score_r <- "8"
            round3_score_b <- "10"
          } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "-1") {
            round3_score_r <- "9"
            round3_score_b <- "10"
          } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "0") {
            round3_score_r <- "10"
            round3_score_b <- "10"
          } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "1") {
            round3_score_r <- "10"
            round3_score_b <- "9"
          } else if (names(df_R3_score_pred)[which(df_R3_score_pred == max(df_R3_score_pred))] == "2") {
            round3_score_r <- "10"
            round3_score_b <- "8"
          } else {
            round3_score_r <- "10"
            round3_score_b <- "7"
          }
          
        } else {
          round3_score_r <- "X"
          round3_score_b <- "X"
        }
        
      } else {
        round3_score_r <- "X"
        round3_score_b <- "X"
      }
    }
    
    output$round3_score_r <- renderText(round3_score_r)
    output$round3_score_b <- renderText(round3_score_b)
    
    ## round4
    if (df$time_format[1] == "3 Rnd (5-5-5)") {
      round4_score_r <- "None"
      round4_score_b <- "None"
      
    } else {
      if ("End R4" %in% df$det == TRUE  & df$last_round[nrow(df)] != 4) {
        df_R4_score_cum <- df[match("End R4",df$det),c(which(names(df)=="knockdowns"),
                                                       which(names(df)=="significant_strikes_landed"),
                                                       which(names(df)=="total_strikes_attempted"),
                                                       which(names(df)=="body_landed"),
                                                       which(names(df)=="leg_landed"),
                                                       which(names(df)=="takedown_successful"),
                                                       which(names(df)=="takedown_attempted"),
                                                       which(names(df)=="submission_attempt"),
                                                       which(names(df)=="control_time"))]
        
        df_R4_score <- df_R4_score_cum - df_R3_score_cum
        
        df_R4_score_pred <- predict(ms, df_R4_score, type = "probs")
        
        if (names(df_R4_score_pred)[which(df_R4_score_pred == max(df_R4_score_pred))] == "-3") {
          round4_score_r <- "7"
          round4_score_b <- "10"
        } else if (names(df_R4_score_pred)[which(df_R4_score_pred == max(df_R4_score_pred))] == "-2") {
          round4_score_r <- "8"
          round4_score_b <- "10"
        } else if (names(df_R4_score_pred)[which(df_R4_score_pred == max(df_R4_score_pred))] == "-1") {
          round4_score_r <- "9"
          round4_score_b <- "10"
        } else if (names(df_R4_score_pred)[which(df_R4_score_pred == max(df_R4_score_pred))] == "0") {
          round4_score_r <- "10"
          round4_score_b <- "10"
        } else if (names(df_R4_score_pred)[which(df_R4_score_pred == max(df_R4_score_pred))] == "1") {
          round4_score_r <- "10"
          round4_score_b <- "9"
        } else if (names(df_R4_score_pred)[which(df_R4_score_pred == max(df_R4_score_pred))] == "2") {
          round4_score_r <- "10"
          round4_score_b <- "8"
        } else {
          round4_score_r <- "10"
          round4_score_b <- "7"
        }
        
      } else {
        round4_score_r <- "X"
        round4_score_b <- "X"
      }
      
    }
    
    output$round4_score_r <- renderText(round4_score_r)
    output$round4_score_b <- renderText(round4_score_b)
    
    ## round5
    if (df$time_format[1] == "3 Rnd (5-5-5)") {
      round5_score_r <- "None"
      round5_score_b <- "None"
      
    } else {
      if ("End R5" %in% df$det == TRUE & df$last_round[nrow(df)] != 5) {
        df_R5_score_cum <- df[match("End R5",df$det),c(which(names(df)=="knockdowns"),
                                                       which(names(df)=="significant_strikes_landed"),
                                                       which(names(df)=="total_strikes_attempted"),
                                                       which(names(df)=="body_landed"),
                                                       which(names(df)=="leg_landed"),
                                                       which(names(df)=="takedown_successful"),
                                                       which(names(df)=="takedown_attempted"),
                                                       which(names(df)=="submission_attempt"),
                                                       which(names(df)=="control_time"))]
        
        df_R5_score <- df_R5_score_cum - df_R4_score_cum
        
        df_R5_score_pred <- predict(ms, df_R5_score, type = "probs")
        
        if (names(df_R5_score_pred)[which(df_R5_score_pred == max(df_R5_score_pred))] == "-3") {
          round5_score_r <- "7"
          round5_score_b <- "10"
        } else if (names(df_R5_score_pred)[which(df_R5_score_pred == max(df_R5_score_pred))] == "-2") {
          round5_score_r <- "8"
          round5_score_b <- "10"
        } else if (names(df_R5_score_pred)[which(df_R5_score_pred == max(df_R5_score_pred))] == "-1") {
          round5_score_r <- "9"
          round5_score_b <- "10"
        } else if (names(df_R5_score_pred)[which(df_R5_score_pred == max(df_R5_score_pred))] == "0") {
          round5_score_r <- "10"
          round5_score_b <- "10"
        } else if (names(df_R5_score_pred)[which(df_R5_score_pred == max(df_R5_score_pred))] == "1") {
          round5_score_r <- "10"
          round5_score_b <- "9"
        } else if (names(df_R5_score_pred)[which(df_R5_score_pred == max(df_R5_score_pred))] == "2") {
          round5_score_r <- "10"
          round5_score_b <- "8"
        } else {
          round5_score_r <- "10"
          round5_score_b <- "7"
        }
        
      } else if ("End R5" %in% df$det == TRUE & df$last_round[nrow(df)] == 5) {
        if (grepl("Dec", df$method[nrow(df)]) == TRUE) {
          df_R5_score_cum <- df[match("End R5",df$det),c(which(names(df)=="knockdowns"),
                                                         which(names(df)=="significant_strikes_landed"),
                                                         which(names(df)=="total_strikes_attempted"),
                                                         which(names(df)=="body_landed"),
                                                         which(names(df)=="leg_landed"),
                                                         which(names(df)=="takedown_successful"),
                                                         which(names(df)=="takedown_attempted"),
                                                         which(names(df)=="submission_attempt"),
                                                         which(names(df)=="control_time"))]
          
          df_R5_score <- df_R5_score_cum - df_R4_score_cum
          
          df_R5_score_pred <- predict(ms, df_R5_score, type = "probs")
          
          if (names(df_R5_score_pred)[which(df_R5_score_pred == max(df_R5_score_pred))] == "-3") {
            round5_score_r <- "7"
            round5_score_b <- "10"
          } else if (names(df_R5_score_pred)[which(df_R5_score_pred == max(df_R5_score_pred))] == "-2") {
            round5_score_r <- "8"
            round5_score_b <- "10"
          } else if (names(df_R5_score_pred)[which(df_R5_score_pred == max(df_R5_score_pred))] == "-1") {
            round5_score_r <- "9"
            round5_score_b <- "10"
          } else if (names(df_R5_score_pred)[which(df_R5_score_pred == max(df_R5_score_pred))] == "0") {
            round5_score_r <- "10"
            round5_score_b <- "10"
          } else if (names(df_R5_score_pred)[which(df_R5_score_pred == max(df_R5_score_pred))] == "1") {
            round5_score_r <- "10"
            round5_score_b <- "9"
          } else if (names(df_R5_score_pred)[which(df_R5_score_pred == max(df_R5_score_pred))] == "2") {
            round5_score_r <- "10"
            round5_score_b <- "8"
          } else {
            round5_score_r <- "10"
            round5_score_b <- "7"
          }
          
        } else {
          round5_score_r <- "X"
          round5_score_b <- "X"
        }
        
      } else {
        round5_score_r <- "X"
        round5_score_b <- "X"
      }
      
    }
    
    output$round5_score_r <- renderText(round5_score_r)
    output$round5_score_b <- renderText(round5_score_b)
    
    #
    roundnumber <- c(paste("R1:"),paste("R2:"),paste("R3:"),
                     paste("R4:"),paste("R5:"))
    scores_r <- c(round1_score_r,round2_score_r,round3_score_r,round4_score_r,round5_score_r)
    scores_b <- c(round1_score_b,round2_score_b,round3_score_b,round4_score_b,round5_score_b)
    scores_table <- data.frame(roundnumber,scores_r,scores_b)
    names(scores_table) <- c("Round","Red","Blue")
    
    output$scores_table <- renderTable(scores_table)
    
    #### pred: winner (from the start of R3)
    df_w <- df[nrow(df),] # on prend la derniere ligne du csv
    
    if (df$time_format[1] == "3 Rnd (5-5-5)") {
      if ("End R2" %in% df$det == TRUE & df$last_round[nrow(df)] != 2) {
        df_w_pred <- predict(mw_R1R2, newdata = df_w, type = "response")*100
      } else if ("R3" %in% df$det_round == TRUE) {
        df_w_pred <- predict(mw_R1R2, newdata = df_w, type = "response")*100
      } else {
        df_w_pred <- "Not applicable"
      }
      
    } else {
      df_w_pred <- "Not available"
    }
    
    df_w_pred2 <- ifelse(is.numeric(df_w_pred) == TRUE, paste(round(df_w_pred,3), "%"), df_w_pred)
    
    # output$w_pred_r <- renderText(
    #   ifelse(is.numeric(df_w_pred) == TRUE, paste(round(df_w_pred,3), "%"), df_w_pred)
    # )
    # 
    # output$w_pred_r_round <- renderText(df_w$det_round)
    # output$w_pred_r_time <- renderText(df_w$det_time_2)
    # output$w_pred_r_local <- renderText(df_w$time_local)
    
    #
    pred_r_table <- data.frame(df_w$det_round, df_w$det_time_2, df_w_pred2)
    names(pred_r_table) <- c("Round","Time","Probability")
    
    output$pred_r_table <- renderTable(pred_r_table)
    
    #
    ""
  })
  
  output$text2 <- renderText({
    df <- as.data.frame(fileData())
    df$fighter_r[1]
  })
  
}
