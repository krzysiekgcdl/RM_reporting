tab2_fun <- function(variables, all_data, my_doc, section_header_name) {
  
  
  data_tab1 <- all_data[is.na(all_data$Reference),c("PatientNr","Site","SV0_DS_DS_REGID","SV0_DS_DS_DS","SV0_TI_IE_TIYN","SV1_IE_IEYN_IEYN","DS_DS_DS_NCOMPLTYN","DS_DS_DS_NCOMPLT")]
  
  data_tab1[,c(3)] <- "Recruited"
  data_tab1[,c(4)] <- ifelse(is.na(data_tab1[,c(4)]),NA,"Signed consent")
  data_tab1[,c(5)] <- ifelse(is.na(data_tab1[,c(5)]),NA,"Screened")
  data_tab1[,c(6)] <- ifelse(is.na(data_tab1[,c(6)]),NA,"Enrolled")
  data_tab1[,c(7)] <- ifelse(is.na(data_tab1[,c(7)]),NA,"Completed the Study")
  data_tab1[,c(8)] <- ifelse(data_tab1[,c(8)]%in%"Screen Failure","Screen Failure",NA)
  
  data_tab1_1 <- data_tab1[,c(1,2,3)] 
  data_tab1_2 <- data_tab1[,c(1,2,4)] 
  data_tab1_3 <- data_tab1[,c(1,2,5)] 
  data_tab1_4 <- data_tab1[,c(1,2,6)] 
  data_tab1_5 <- data_tab1[,c(1,2,7)]
  data_tab1_6 <- data_tab1[,c(1,2,8)]
  
  colnames(data_tab1_1) <- c("PatientNr","Site","Status")
  colnames(data_tab1_2) <- c("PatientNr","Site","Status")
  colnames(data_tab1_3) <- c("PatientNr","Site","Status")
  colnames(data_tab1_4) <- c("PatientNr","Site","Status")
  colnames(data_tab1_5) <- c("PatientNr","Site","Status")
  colnames(data_tab1_6) <- c("PatientNr","Site","Status")
  
  data_tab1 <- rbind(data_tab1_1,data_tab1_2)
  data_tab1 <- rbind(data_tab1,data_tab1_3)
  data_tab1 <- rbind(data_tab1,data_tab1_4)
  data_tab1 <- rbind(data_tab1,data_tab1_5)
  data_tab1 <- rbind(data_tab1,data_tab1_6)
  
  rm(data_tab1_1)
  rm(data_tab1_2)
  rm(data_tab1_3)
  rm(data_tab1_4)
  rm(data_tab1_5)
  rm(data_tab1_6)
  
  data_tab1$Status <- factor(data_tab1$Status, levels = c("Recruited","Signed consent","Screened","Enrolled", "Completed the Study","Screen Failure"), labels = c("Recruited","Signed consent","Screened","Enrolled", "Completed the Study","Screen Failure"))
  
  data_tab1$All_Site <- ifelse(is.na(data_tab1$Site), 999, 1)
  var_label(data_tab1$All_Site) <- c("N (%)")  
  
  
  data_tab5_inc <- all_data[is.na(all_data$Reference),c("PatientNr","Site"
                                                        ,"SV0_TI_TI1_TI_IC1"
                                                        ,"SV0_TI_TI1_TI_IC2"
                                                        ,"SV0_TI_TI1_TI_IC3"
                                                        ,"SV0_TI_TI1_TI_IC4"
                                                        ,"SV0_TI_TI1_TI_IC5"
                                                        ,"SV0_TI_TI1_TI_IC6"
                                                        ,"SV0_TI_TI1_TI_IC7"
                                                        ,"SV0_TI_TI1_TI_IC8")]
  
  data_tab5_exc <- all_data[is.na(all_data$Reference),c("PatientNr","Site"
                                                        ,"SV0_TI_TI2_TI_EC1"
                                                        ,"SV0_TI_TI2_TI_EC1A"
                                                        ,"SV0_TI_TI2_TI_EC1B"
                                                        ,"SV0_TI_TI2_TI_EC2"
                                                        ,"SV0_TI_TI2_TI_EC3"
                                                        ,"SV0_TI_TI2_TI_EC4"
                                                        ,"SV0_TI_TI2_TI_EC5"
                                                        ,"SV0_TI_TI2_TI_EC6"
                                                        ,"SV0_TI_TI2_TI_EC7"
                                                        ,"SV0_TI_TI2_TI_EC8"
                                                        ,"SV0_TI_TI2_TI_EC9"
                                                        ,"SV0_TI_TI2_TI_EC10"
                                                        ,"SV0_TI_IE_TIYN"
                                                        ,"SV1_IE_IEYN_IEYN")]
  
  data_tab5_inc$crit_full_inc <- NA
  data_tab5_inc$crit_full_inc <- ifelse( rowSums((data_tab5_inc[,c(3:10)] == "Yes" | data_tab5_inc[,c(3:10)] == "NA" )) == 8, "Yes", "No")
  data_tab5_exc$crit_full_exc <- NA
  data_tab5_exc$SV0_TI_TI2_TI_EC1A <- ifelse(data_tab5_exc$SV0_TI_TI2_TI_EC1%in%"No", "No", data_tab5_exc$SV0_TI_TI2_TI_EC1A)
  data_tab5_exc$SV0_TI_TI2_TI_EC1B <- ifelse(data_tab5_exc$SV0_TI_TI2_TI_EC1%in%"No", "No", data_tab5_exc$SV0_TI_TI2_TI_EC1B)
  data_tab5_exc$crit_full_exc <- ifelse( rowSums((data_tab5_exc[,c(3:14)] == "No" | data_tab5_exc[,c(3:14)] == "NA")) == 12, "Yes", "No") 
  
  data_tab5 <- merge(data_tab5_inc,data_tab5_exc, by= c("PatientNr","Site"))
  
  rm(data_tab5_inc)
  rm(data_tab5_exc)
  
  data_tab5[,c("crit_full_inc","crit_full_exc")]
  data_tab5$crit_full <- NA
  data_tab5$crit_full <- ifelse( rowSums(is.na(data_tab5[,c("crit_full_inc","crit_full_exc")])) >= 1, "Empty", ifelse(rowSums(data_tab5[,c("crit_full_inc","crit_full_exc")] == "Yes") == 2, "Yes", "No"))
  data_tab5$crit_full <- factor(data_tab5$crit_full, levels = c("Yes", "No", "Empty"), labels = c("Yes", "No", "Empty"))
  
  data_tab5[,c("crit_full","SV0_TI_IE_TIYN","SV1_IE_IEYN_IEYN")]
  
  var_label(data_tab5$SV0_TI_IE_TIYN) <- "Subject qualified to participate in the study (V0)"
  var_label(data_tab5$SV1_IE_IEYN_IEYN) <- "Subject meet all criteria one hour prior to administration of the study drug (V1)"
  
  data_tab5$All_Site <- ifelse(is.na(data_tab5$Site), 999, 1)
  var_label(data_tab5$All_Site) <- c("N (%)") 
  
  ##### Listing
  data_listing5 <- data_tab5[!data_tab5$crit_full == "Yes",]
  #colSums(data_listing5[c(3:10)] == "Yes") == length(data_listing5$PatientNr)
  #colSums(data_listing5[c(13:22)] == "No" | data_listing5[c(13:22)] == "NA ) == length(data_listing5$PatientNr)
  data_listing5  <- data_listing5 [,c(1:10,12:23)]
  
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(3)]),"Empty In 1 |" ,ifelse(data_listing5[,c(3)] =="Yes", "", "In 1"))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(4)]),paste(data_listing5$cr_nr, "Empty In 2 |",sep = "") ,ifelse(data_listing5[,c(4)] =="Yes",data_listing5$cr_nr, paste(data_listing5$cr_nr, "In 2 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(5)]),paste(data_listing5$cr_nr, "Empty In 3 |",sep = "") ,ifelse(data_listing5[,c(5)] =="Yes",data_listing5$cr_nr, paste(data_listing5$cr_nr, "In 3 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(6)]),paste(data_listing5$cr_nr, "Empty In 4 |",sep = "") ,ifelse(data_listing5[,c(6)] =="Yes",data_listing5$cr_nr, paste(data_listing5$cr_nr, "In 4 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(7)]),paste(data_listing5$cr_nr, "Empty In 5 |",sep = "") ,ifelse(data_listing5[,c(7)] =="Yes",data_listing5$cr_nr, paste(data_listing5$cr_nr, "In 5 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(8)]),paste(data_listing5$cr_nr, "Empty In 6 |",sep = "") ,ifelse(data_listing5[,c(8)]%in%c("Yes","NA") ,data_listing5$cr_nr, paste(data_listing5$cr_nr, "In 6 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(9)]),paste(data_listing5$cr_nr, "Empty In 7 |",sep = "") ,ifelse(data_listing5[,c(9)]%in%c("Yes","NA")  ,data_listing5$cr_nr, paste(data_listing5$cr_nr, "In 7 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(10)]),paste(data_listing5$cr_nr, "Empty In 8 |",sep = "") ,ifelse(data_listing5[,c(10)] =="Yes",data_listing5$cr_nr, paste(data_listing5$cr_nr, "In 8 |",sep = "")))
  
  
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(11)]),paste(data_listing5$cr_nr, "Empty Ex 1 |",sep = ""),ifelse(data_listing5[,c(11)] =="No",data_listing5$cr_nr, paste(data_listing5$cr_nr, "Ex 1 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(12)]),paste(data_listing5$cr_nr, "Empty Ex 1A |",sep = ""),ifelse(data_listing5[,c(12)] =="No",data_listing5$cr_nr, paste(data_listing5$cr_nr, "Ex 1A |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(13)]),paste(data_listing5$cr_nr, "Empty Ex 1B |",sep = ""),ifelse(data_listing5[,c(13)] =="No",data_listing5$cr_nr, paste(data_listing5$cr_nr, "Ex 1B |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(14)]),paste(data_listing5$cr_nr, "Empty Ex 2 |",sep = ""),ifelse(data_listing5[,c(14)] =="No",data_listing5$cr_nr, paste(data_listing5$cr_nr, "Ex 2 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(15)]),paste(data_listing5$cr_nr, "Empty Ex 3 |",sep = ""),ifelse(data_listing5[,c(15)] =="No",data_listing5$cr_nr, paste(data_listing5$cr_nr, "Ex 3 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(16)]),paste(data_listing5$cr_nr, "Empty Ex 4 |",sep = ""),ifelse(data_listing5[,c(16)] =="No",data_listing5$cr_nr, paste(data_listing5$cr_nr, "Ex 4 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(17)]),paste(data_listing5$cr_nr, "Empty Ex 5 |",sep = ""),ifelse(data_listing5[,c(17)] =="No",data_listing5$cr_nr, paste(data_listing5$cr_nr, "Ex 5 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(18)]),paste(data_listing5$cr_nr, "Empty Ex 6 |",sep = ""),ifelse(data_listing5[,c(18)] =="No",data_listing5$cr_nr, paste(data_listing5$cr_nr, "Ex 6 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(19)]),paste(data_listing5$cr_nr, "Empty Ex 7 |",sep = ""),ifelse(data_listing5[,c(19)] =="No",data_listing5$cr_nr, paste(data_listing5$cr_nr, "Ex 7 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(20)]),paste(data_listing5$cr_nr, "Empty Ex 8 |",sep = ""),ifelse(data_listing5[,c(20)] =="No",data_listing5$cr_nr, paste(data_listing5$cr_nr, "Ex 8 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(21)]),paste(data_listing5$cr_nr, "Empty Ex 9 |",sep = ""),ifelse(data_listing5[,c(21)]%in%c("No","NA") ,data_listing5$cr_nr, paste(data_listing5$cr_nr, "Ex 9 |",sep = "")))
  data_listing5$cr_nr <- ifelse(is.na(data_listing5[,c(22)]),paste(data_listing5$cr_nr, "Empty Ex 10 |",sep = ""),ifelse(data_listing5[,c(22)] =="No",data_listing5$cr_nr, paste(data_listing5$cr_nr, "Ex 10 |",sep = "")))
  
  data_listing5  <- data_listing5 [,c("PatientNr","Site","cr_nr")]
  data_listing5_enrold <- data_tab1[data_tab1$Status%in%"Enrolled",]
  data_listing5_enrold <- data_listing5_enrold [,c("PatientNr","Site","Status")]
  data_listing5 <- merge(data_listing5 , data_listing5_enrold, by = c("PatientNr","Site"), all.x = TRUE)
  rm(data_listing5_enrold)
  data_listing5$Status <- ifelse(is.na(data_listing5$Status), "Missing", "Yes")
  
  colnames(data_listing5)<- c("Subject No","Site", "Eligibility Criteria nr (not meet or empty)","Patient Qualified")
  
  tab_5 <- tbl_summary(data = data_tab5,
                       include = c("SV0_TI_IE_TIYN"),
                       by = c('crit_full'),
                       type = all_continuous() ~ "continuous2",
                       statistic = all_continuous() ~ c("{N_nonmiss}"),
                       missing = "no",
                       digits = list(all_continuous() ~ c(0,1,1,1,1,1,1,1,1)
                                     , all_categorical()~c(0,1)))
  tab_5_1 <- tbl_summary(data = data_tab5,
                         include = c('All_Site'),
                         by = 'crit_full',
                         type = all_continuous() ~ "continuous2",
                         statistic = all_continuous() ~ c("{N_nonmiss}"),
                         missing = "no",
                         digits = list(all_continuous() ~ c(0,1,1,1,1,1,1,1,1)
                                       , all_categorical()~c(0,1)))
  
  tab_5_2 <- tbl_summary(data = data_tab5[!is.na(data_tab5$SV1_IE_IEYN_IEYN),],
                         include = c('Site',"SV1_IE_IEYN_IEYN"),
                         type = all_continuous() ~ "continuous2",
                         statistic = all_continuous() ~ c("{N_nonmiss}"),
                         missing = "no",
                         digits = list(all_continuous() ~ c(0,1,1,1,1,1,1,1,1)
                                       , all_categorical()~c(0,1)),
                         label = SV1_IE_IEYN_IEYN ~ "Subject meet all criteria one hour prior to administration of the study drug (V1)")
  
  tab_5 <- add_overall(tab_5, col_label = "**Overall**")
  tab_5_1 <- add_overall(tab_5_1, col_label = "**Overall**")
  tab_5 <- tbl_stack(list(tab_5, tab_5_1))
  tab_5 <- modify_header(tab_5, label = '', stat_1 = "**Fulfilled**",stat_2 = "**No fulfilled**",stat_3 = "**Form not completed**")
  tab_5 <- bold_labels(tab_5)
  tab_5 <- modify_footnote(tab_5, all_stat_cols() ~ NA)
  tab_5 <- as_flex_table(tab_5)
  # tab_5 <- font(tab_5, fontname = "Calibri", part = "all")
  tab_5  <- bold(tab_5 , bold = TRUE, part = "header")
  tab_5 <- add_header_row(tab_5, values = "Eligibility Criteria meet on V0 (Subjects with an open criteria form)", colwidths = 5)
  # tab_5 <- fontsize(tab_5, size = 9, part = "body")
  # tab_5 <- fontsize(tab_5, size = 10, part = "header")
  tab_5 <- width(tab_5, j=c(1:5), width = c(2,1,1,1,1))
  
  
  tab_5_2 <- as_flex_table(tab_5_2)
  # tab_5_2 <- font(tab_5_2, fontname = "Calibri", part = "all")
  tab_5_2  <- bold(tab_5_2 , bold = TRUE, part = "header")
  tab_5_2 <- add_header_row(tab_5_2, values = "Eligibility Criteria meet on V1", colwidths = 2)
  # tab_5_2 <- fontsize(tab_5_2, size = 9, part = "body")
  # tab_5_2 <- fontsize(tab_5_2, size = 10, part = "header")
  tab_5_2 <- width(tab_5_2, j=c(1:2), width = c(2,1))
  
  
  
  listing_5 <- flextable(data_listing5)
  # listing_5 <- fontsize(listing_5, i = NULL, j = 1:4, size = 8, part = "body")
  # listing_5<- fontsize(listing_5, i = NULL, j = 1:4, size = 9, part = "header")
  listing_5 <- theme_box(listing_5)
  listing_5<- width(listing_5, j=c(1:4), width = c(1,1,2.5,1))
  
  my_doc <- body_add_par(my_doc, section_header_name, style = "heading 1")
  my_doc <- body_add_par(my_doc, "Number of subjects:", style = "heading 2")
  my_doc <- body_add_flextable(my_doc, tab_5_2)
  my_doc <- body_add_par(my_doc, " ")
  my_doc <- body_add_par(my_doc, "Eligibility Criteria - not met or empty:", style = "heading 2")
  my_doc <- body_add_flextable(my_doc, listing_5)
  my_doc <- body_add_break(my_doc, pos = "after")
  
  # return(list(tab2 = tab_5_2, listing2 = listing_5))
  return(my_doc)
}

