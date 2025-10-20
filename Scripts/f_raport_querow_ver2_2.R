library(xml2)

raport_querow <- function(session, input, output, my_data) {
  dzis <- Sys.Date()
  
  #extracting export date from export name
  query_data_name <- sub("^[^_]*_","",query_data_name)
  query_data_name <- sub("\\_.*","",query_data_name)
  
  data_drop_year <- substr(query_data_name, 1, 4)
  data_drop_month <- substr(query_data_name, 5, 6)
  data_drop_day <- substr(query_data_name, 7, 8)
  formatted_date <- paste(data_drop_year, data_drop_month, data_drop_day, sep = "-")
  
  query_data_name <- formatted_date
  
  ###### wczytanie danych ze zbioru  ##########
  DATA.TMP <- my_data
  
  ###dane wczytane
  
  DATA.TMP$Component <- HTMLdecode(DATA.TMP$Component, named = TRUE, hex = TRUE, decimal = TRUE)
  
  ###### Funkcje tworzące punkty odcięcia na osiach histogramóW ###########
  brk <- function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)
  
  int_breaks <- function(x, n = 10) {
    l <- pretty(x, n)
    l[abs(l %% 1) < .Machine$double.eps ^ 0.5]
  }
  ###### przygotowanie danych do analizy  ##########
  
  ### changes added after RM's update
  
  DATA.TMP <- DATA.TMP %>%
    select(-Export.tag)
  
  # the "Open comment" status is not needed
  DATA.TMP <- DATA.TMP %>%
    filter(DATA.TMP$Status != "Open comment")
  
  # Number is not unique for every query - connecting query with patient number
  DATA.TMP$Query.ID <- paste0(DATA.TMP$Participant.number, "-", DATA.TMP$Number)
  DATA.TMP <- DATA.TMP %>%
    select(Query.ID, everything()) %>%
    select(-Number)
  
  # After the update they are not always in the correcgt order (created -> closed), so I fix that
  DATA.TMP <- DATA.TMP %>%
    group_by(Query.ID) %>%
    arrange(Date.created, .by_group = TRUE) %>%
    ungroup(Query.ID)
  
  
  ###
  
  
  DATA <- NULL
  for(queryNmbr in unique(DATA.TMP$Query.ID)){
    DATA.row <- 
      c("Query.ID" = queryNmbr    
        ,"Participant.number" = DATA.TMP$Participant.number[DATA.TMP$Query.ID == queryNmbr][1]
        ,"Status" = DATA.TMP$Status[DATA.TMP$Query.ID == queryNmbr][1]
        ,"Site.code"  = DATA.TMP$Site.code[DATA.TMP$Query.ID == queryNmbr][1]  
        ,"Site.name" = DATA.TMP$Site.name[DATA.TMP$Query.ID == queryNmbr][1]   
        ,"Chapter" = DATA.TMP$Chapter[DATA.TMP$Query.ID == queryNmbr][1]     
        ,"Subchapter" = DATA.TMP$Subchapter[DATA.TMP$Query.ID == queryNmbr][1]  
        ,"Reference" = DATA.TMP$Reference[DATA.TMP$Query.ID == queryNmbr][1]  
        ,"Paragraph" = DATA.TMP$Paragraph[DATA.TMP$Query.ID == queryNmbr][1]   
        ,"Component" = DATA.TMP$Component[DATA.TMP$Query.ID == queryNmbr][1]       
        ,"User.created" = DATA.TMP$User.created[DATA.TMP$Query.ID == queryNmbr][1]
        ,"Text" = DATA.TMP$Text[DATA.TMP$Query.ID == queryNmbr][1]  
        ,"Date.created"= DATA.TMP$Date.created[DATA.TMP$Query.ID == queryNmbr][1]  
        ,"LastUpdate.Date"= tail(DATA.TMP$Date.created[DATA.TMP$Query.ID == queryNmbr], n=1)
      )
    DATA <- rbind(DATA, DATA.row)
  }
  DATA <- as.data.frame(DATA)
  DATA[,"Status"] <- factor(DATA[,"Status"], levels = c("Open query","Answered query","Closed query"))
  # length(unique(DATA.TMP$Query.ID))
  # nrow(DATA)
  # 
  #print(head(DATA))
  
  DATA[,"jeden"] <- 1
  var_label(DATA[,"jeden"]) <- "Overall"
  # policzenie liczby ośrodków #####
  Nsites <- length(unique(DATA[,"Site.name"]))
  # policzenie czasów dla każdego quera ####
  
  # DATA[,"dayCreated"] <- as.Date.character(substr(DATA[,"Date.created"],1,10),
  #                                          tryFormats = c("%d.%m.%Y"))
  DATA[,"dayCreated"] <- as.Date(as.numeric(DATA[,"Date.created"]), origin = "1899-12-30")
  
  # DATA[,"dayLastUpdate"] <- as.Date.character(substr(DATA[,"LastUpdate.Date"],1,10),
  #                                         tryFormats = c("%d.%m.%Y"))
  
  DATA[,"dayLastUpdate"] <- as.Date(as.numeric(DATA[,"LastUpdate.Date"]), origin = "1899-12-30")
  
  DATA[,"Days.Since.Open"] <- as.numeric(Sys.Date() - DATA[,"dayCreated"])
  #print(DATA[,"Days.Since.Open"] )
  DATA[,"Days.To.Last.Update"] <- as.numeric(DATA[,"dayLastUpdate"] - DATA[,"dayCreated"])
  
  DATA[,"Days.Since.Open.cat"] <- cut(DATA[,"Days.Since.Open"],c (0,15,30,45,60, 999), include.lowest = TRUE)
  DATA[,"Days.To.Last.Update.cat"] <- cut(DATA[,"Days.To.Last.Update"],c (0,15,30,45,60, 999), include.lowest = TRUE)
  
  # Only taking into account top 10 queried fields for the last table (tab4)
  DATA_10 <- DATA %>% 
    add_count(Component, sort = TRUE)
  first10 <- head(unique(DATA_10$Component), n=10)
  DATA_10 <- DATA_10 %>%
    filter(Component %in% first10)
  
  #print("Znacznik1")
  
  # zestawienie liczby querów ogółem i x ośrodek ####
  print("Tab 1 się robi")
  tab1 <- tbl_summary(
    data = DATA,
    include = c("Site.name", "jeden"),
    by = "Status",
    percent = "row",
    digits = list(all_categorical() ~ c(0, 1))
    #statistic = list("jeden" ~"{n}","Status" ~ "{n} ({p}%)")
  )
  
  tab1 <- add_overall(tab1)
  
  #print("Znacznik2")
  tab1 <-
    modify_header(
      tab1,
      all_stat_cols(stat_0 = FALSE) ~ "**{level}**",
      label = '****',
      stat_0 = "Overall"
    )
  tab1 <- as_flex_table(tab1)
  
  
  
  tab1 <- width(tab1, j = 2:5, width = 1.1)
  #print(tab1)
  # zestawienie po czasie dla nierozwiązanych (open lub answered) querów ####
  DATA_Open <- DATA[DATA[, "Status"] == "Open query" | DATA[, "Status"] == "Answered query", ]
  DATA_Open <- copy_labels_from(DATA_Open, DATA)
  if(nrow(DATA_Open)>0){ # warunek na to czy w ogóle są nierozwiązane query
    print("Tab 2 się robi")
    tab2 <- tbl_summary(
      data = DATA_Open,
      include = c("Site.name", "jeden"),
      by = "Days.Since.Open.cat",
      percent = "row",
      digits = list(all_categorical() ~ c(0, 1))
    )
    tab2 <- add_overall(tab2)
    tab2 <-
      modify_header(
        tab2,
        all_stat_cols(stat_0 = FALSE) ~ "**{level}**",
        label = '****',
        stat_0 = "Overall"
      )
    
    # print(tab2)
    tab2 <- as_flex_table(tab2)
    tab2 <- width(tab2, j = 2:7, width = 1)
  }
  
  if(nrow(DATA_Open)>0){
    print("Wykres 2 się robi")
    #print(names(DATA_Open))
    #print(DATA_Open)
    # plot3 <- ggplot(data = DATA_Closed,
    #                 aes(x = Site.name, y = Days.To.Last.Update)) +
    #   geom_boxplot()
    
    
    
    
    plot2 <- ggplot(data = DATA_Open,
                    aes(x = Days.Since.Open)) +
      geom_histogram(alpha=0.6, binwidth = 1, fill = "#F46D43", colour = "black") +
      theme_bw() +
      theme(
        legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      xlab("Days since opening") +
      ylab("Number of queries") +
      #scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
      scale_x_continuous(breaks = function(x) int_breaks(x, n = 10))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      facet_wrap(.~Site.name) +
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1)))))
    
  } 
  ## zestawienia czasu zamykania querów
  DATA_Closed <- DATA[DATA[, "Status"] == "Closed query", ]
  DATA_Closed <- copy_labels_from(DATA_Closed, DATA)
  #print(DATA_Closed)
  if(nrow(DATA_Closed)>0){ # warunek na to czy w ogóle są zamknięte query
    print("Tab 3 się robi")
    tab3 <- tbl_summary(
      data = DATA_Closed,
      include = c("Site.name", "jeden"),
      by = "Days.To.Last.Update.cat",
      percent = "row",
      digits = list(all_categorical() ~ c(0, 1))
    )
    tab3 <- add_overall(tab3)
    tab3 <-
      modify_header(
        tab3,
        all_stat_cols(stat_0 = FALSE) ~ "**{level}**",
        label = '****',
        stat_0 = "Overall"
      )
    
    #print(tab3)
    tab3 <- as_flex_table(tab3)
    tab3 <- width(tab3, j = 2:7, width = 1)
  }
  if(nrow(DATA_Closed)>0){
    print("Wykres 3 się robi")
    #print(names(DATA_Closed))
    #print(DATA_Closed)
    # plot3 <- ggplot(data = DATA_Closed,
    #                 aes(x = Site.name, y = Days.To.Last.Update)) +
    #   geom_boxplot()
    
    brk <- function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)
    
    plot3 <- ggplot(data = DATA_Closed,
                    aes(x = Days.To.Last.Update)) +
      geom_histogram(alpha=0.6, binwidth = 1, fill = "#F46D43", colour = "black") +
      theme_bw() +
      theme(
        legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      xlab("Days to close") +
      ylab("Number of queries") +
      #scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
      scale_x_continuous(breaks = function(x) int_breaks(x, n = 10))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      facet_wrap(.~Site.name) +
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1)))))
    
    #ggsave(plot = plot3,filename = "plot3.png", width = 1000, height = 500, units = "px")
    
  }
  
  
  print("Tab 4 się robi")
  # zestawienia po zaquerowanym polu
  tab4 <- tbl_summary(
    data = DATA_10,
    include = c("Component", "jeden"),
    by = "Site.name",
    statistic = list(all_categorical() ~ "{n}"),
    sort = all_categorical() ~ "frequency"
  )
  
  tab4 <- add_overall(tab4)
  tab4 <-
    modify_header(
      tab4,
      all_stat_cols(stat_0 = FALSE) ~ "**{level}**",
      label = '****',
      stat_0 = "Overall"
    )
  #print(tab3)
  
  tab4 <- as_flex_table(tab4)
  tab4 <- width(tab4, j = 1, width = 2.5)
  
  
  print("gt tabele się zrobiły")
  
  
  my_doc <- read_docx(path = "Template/CDL_ roboczy papier firmowy_simple.docx")
  
  my_doc <- body_add_par(my_doc, "Queries Report" , style = "heading 1")
  
  my_doc <- body_add_par(my_doc, paste("Study name:", study_name) , style = "heading 3")
  
  my_doc <- body_add_par(my_doc, paste("Data export date:", query_data_name) , style = "heading 3")
  my_doc <- body_add_par(my_doc, paste("Report creation date:", Sys.Date()) , style = "heading 3")
  my_doc <- body_add_toc(my_doc, level = 2, pos = "after", style = NULL, separator = ";")
  my_doc <- body_add_break(my_doc)
  
  my_doc <- body_add_par(my_doc, "Table1: Queries by status and site" , style = "heading 2")
  my_doc <- body_add_flextable(my_doc, tab1)
  my_doc <- body_add_break(my_doc)
  print("tab1 ok")
  my_doc <- body_add_par(my_doc,
                         "Table2: Unresolved queries time [days] from raising, by site" ,
                         style = "heading 2")
  if(exists("tab2")){
    my_doc <- body_add_flextable(my_doc, tab2)
    my_doc <- body_add_gg(my_doc, plot2, width = 7, height = 5)
  } else {
    my_doc <- body_add_par(my_doc, "No unresolved queries")
  }
  print("tab2 ok")
  my_doc <- body_add_break(my_doc)
  my_doc <- body_add_par(my_doc, "Table3: Resolved queries timing [days], by site" , style = "heading 2")
  if(exists("tab3")){
    my_doc <- body_add_flextable(my_doc, tab3)
    my_doc <-  body_add_gg(my_doc, plot3)
  } else {
    my_doc <- body_add_par(my_doc, "No resolved queries")
  }
  
  print("tab3 ok")
  
  my_doc <- body_add_break(my_doc)
  my_doc <- body_add_par(my_doc, "Table4: Queried fields, by site" , style = "heading 2")
  my_doc <- body_add_flextable(my_doc, tab4)
  
  print("tab4 ok")
  
  # print(my_doc,
  #       target = paste0(
  #         "WYNIKI/",badanie_name,
  #         "_queries_report_",
  #         dzis,
  #         ".docx"
  #       ))
  
  
  # tabele wydrukowane
  
  return(my_doc)
  
}