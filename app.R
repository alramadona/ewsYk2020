# https://alramadona.shinyapps.io/ewsYk

library(shiny) 
library(shinydashboard) 

library(tidyverse)
library(zoo)
library(mgcv)

library(mosaic)
library(matrixStats)

library(googlesheets4)
gs4_deauth()

library(sf)
library(readr)

library(rmarkdown)
library(patchwork)

# UI ----------------------------------------------------------------------

ui <- dashboardPage( 
  dashboardHeader(
    title = "ewsYk v.0.1"
  ), 
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pemodelan", tabName = "model", icon = icon("line-chart")),
      menuItem("Help", tabName = "help", icon = icon("info"))
    ),
    div(style = "padding-left: 15px; padding-top: 40px; padding-right: 15px; ",
        p(class = "small", "Disclaimer:", tags$br(),
          # "We hope you will find our website and services helpful for your individual. 
          # Please be aware that any result you may find may be inaccurate. Any action you take upon 
          # the information on this website is strictly at your own risk, and we will not be liable 
          # for any losses and damages in connection with the use of our website."
        ))
  ), 
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "model", 
              h2("Sistem Peringatan Dini DBD"),
              fluidRow(
                ## left
                box(width = 3,
                    radioButtons("out_criteria", "kriteria outbreak",
                                 c(#"Rerata kasus 12 bulan terakhir"="r12",
                                   #"Rerata kasus 24 bulan terakhir"="r24",
                                   #"World Health Organization"="WHO",
                                   "Kementerian Kesehatan RI"="RI",
                                   #"rerata kasus (training set)"="meanTrain",
                                   "rerata kasus bulanan"="rerata",
                                   "pola minimum-maksimum"="minMax"),"rerata"),
                    tags$hr(),
                    checkboxInput("dat_local", "dataset lokal", value=TRUE),
                    fileInput('dat_upload', 'ATAU unggah dataset Anda di sini',
                              accept = c('text/csv','text/comma-separated-values',
                                         'text/tab-separated-values','text/plain',
                                         '.csv','.tsv')),
                    p('contoh format file .csv untuk diunggah:',
                      a(href = 'https://drive.google.com/drive/folders/1PJkHW4bhyV8q_hy-RRTh3nskpWpSsSyb?usp=sharing', 'dat_train.csv')),
                    numericInput("dat_t","training set (pct)", value=75),
                    textInput("dat_upload_na", "NA strings", "#N/A"),
                    checkboxInput('dat_upload_header', 'header', TRUE),
                    radioButtons('dat_upload_sep', 'separator',
                                 c(Comma=',',Semicolon=';',Tab='\t'),','),
                    radioButtons('dat_upload_quote', 'quote',
                                 c(None='','Double Quote'='"','Single Quote'="'"),'"')
                ),
                
                ## right
                box(width = 9, title = "pemodelan",
                    tabBox(width = 12, 
                           tabPanel(title = "peta sebaran kasus",
                                    h3(textOutput("mthE")),
                                    tags$br(),
                                    fluidRow(
                                      column(width=4,
                                             plotOutput("plotMap_village")),
                                      column(width=4,
                                             plotOutput("plotMap_subdistrict")),
                                      column(width=4,
                                             # radioButtons("dat_mth", "",
                                             #              c("Januari 2019"="Jan 2019",
                                             #                "Februari 2019"="Feb 2019",
                                             #                "Maret 2019"="Mar 2019",
                                             #                "April 2019"="Apr 2019",
                                             #                "Mei 2019"="May 2019",
                                             #                "Juni 2019"="Jun 2019",
                                             #                "Juli 2019"="Jul 2019",
                                             #                "Agustus 2019"="Aug 2019",
                                             #                "September 2019"="Sep 2019",
                                             #                "Oktober 2019"="Oct 2019",
                                             #                "November 2019"="Nov 2019",
                                             #                "Desember 2019"="Dec 2019"), "Jan 2019")
                                             plotOutput("plotMap_risk")
                                      )
                                    )),
                           tabPanel(title = "prediksi",
                                    h3(textOutput("mthP")),
                                    plotOutput("plotMod"),
                                    tags$br(),
                                    fluidRow(
                                      valueBoxOutput("pred_num", width = 6),
                                      valueBoxOutput("pred_out", width = 6)
                                    )
                           ),
                           tabPanel(title = "laporan",
                                    helpText(),
                                    downloadButton('downloadReport', 'download')
                           ),
                           tabPanel(title = "++")))
              )),
      tabItem(tabName = "help", 
              h2("Help"),
              fluidRow(tabBox(width = 12, 
                              #tabPanel(title = "methodology"),
                              #tabPanel(title = "tutorial"), 
                              tabPanel(title = "reference",
                                       div(style = "padding-left: 15px; padding-top: 40px;",
                                           p(class = "normal", "",
                                             a("Ramadona AL, et al. PLoS One. 2016",href="http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0152688"),
                                             tags$br(),
                                             #a("Ramadona AL, et al. arXiv. 2017",href="https://arxiv.org/abs/1608.05910"),
                                             tags$br()))),
                              tabPanel(title = "contact us",
                                       div(style = "padding-left: 15px; padding-top: 40px;",
                                           p(class = "normal", "",
                                             a("@alramadona",href="https://twitter.com/alramadona"),
                                             tags$br()))
                              ))))
    )
  ))


# SERVER ------------------------------------------------------------------

server <- function(input, output) {
  
  t <- reactive({input$dat_t})
  dat_local <- reactive({input$dat_local})
  
  datDen_master <- reactive({
    sD <- ""
    #datDen_master <- read_sheet(sD)
    datDen_master <- read.csv("datDen_master.csv")
    
    datDen_master$YM <- paste0(datDen_master$Y,"-",datDen_master$M)
    
    datDen_master$YM <- as.yearmon(datDen_master$YM, format="%Y-%m")
    datDen_master$den <- as.numeric(as.character(datDen_master$den))
    
    datDen_master <- select(datDen_master, YM,subdistrict,village,den)
    
    return(datDen_master)
  })
  
  datMet_master <- reactive({
    sM <- ""
    #datMet_master <- read_sheet(sM)
    datMet_master <- read.csv("datMet_master.csv")
    
    datMet_master$YM <- paste0(datMet_master$Y,"-",datMet_master$M)
    datMet_master$YM <- as.yearmon(datMet_master$YM, format="%Y-%m")
    
    datMet_master$temp <- as.numeric(as.character(datMet_master$temp))
    datMet_master$hum <- as.numeric(as.character(datMet_master$hum))
    datMet_master$rain <- as.numeric(as.character(datMet_master$rain))
    
    datMet_master <- select(datMet_master, YM,temp,hum,rain)
    
    return(datMet_master)
  })
  
  dat_mth <- reactive({
    datDen_master <- datDen_master()
    
    mth <- unique(datDen_master$YM)
    mth <- mth[length(mth)]
    
    return(mth)
  })
  
  datDen_district <- reactive({
    datDen_master <- datDen_master()
    
    datDen_district <- datDen_master %>% 
      group_by(YM) %>%
      summarize(den = sum(den, na.rm = TRUE))
    
    return(datDen_district)
  })
  
  datDen_subdistrict <- reactive({
    datDen_master <- datDen_master()
    
    datDen_subdistrict <- datDen_master %>% 
      group_by(YM, subdistrict) %>%
      summarize(den = sum(den, na.rm = TRUE))
    
    return(datDen_subdistrict)
  })
  
  datDen_village <- reactive({
    datDen_master <- datDen_master()
    
    datDen_village <- datDen_master %>% 
      group_by(YM, village) %>%
      summarize(den = sum(den, na.rm = TRUE))
    
    return(datDen_village)
  })
  
  datMod <- reactive({
    datMet_master <- datMet_master()
    
    if(dat_local()==TRUE){
      datDen_district <- datDen_district()
    }
    
    if(dat_local()==FALSE){
      
      validate(
        need(input$dat_upload != "", "Please select a data set")
      )
      
      inFile <- input$dat_upload
      datDen_district <- read.csv(inFile$datapath,
                                  header = input$dat_upload_header,
                                  sep = input$dat_upload_sep,
                                  quote = input$dat_upload_quote,
                                  na.strings = input$dat_upload_na)
      # datDen_district <- read.csv(inFile$datapath)
      
      datDen_district$YM <- paste0(datDen_district$Y,"-",datDen_district$M)
      
      datDen_district$YM <- as.yearmon(datDen_district$YM, format="%Y-%m")
      datDen_district$den <- as.numeric(as.character(datDen_district$den))
      
      datDen_district <- select(datDen_district, YM,den)
      }
    
    
    datMod <- merge(datMet_master, datDen_district, by="YM", all.y=T)
    return(datMod)
  })
  
  dat <- reactive({
    datMod <- datMod()
    t <- t()
    
    validate(
      need(t >= 60 && t <= 90, "Silakan tentukan rentang data training set antara 60-90 (pct)")
    )
    
    addYM <- as.yearmon(seq(as.Date(datMod$YM[nrow(datMod)]), by = "month", length.out = 3))
    
    datMod <- select(datMod, YM,temp,hum,rain,den)
    datMod <- rbind(datMod,
                    data.frame(YM=addYM[2:3],temp=rep(NA,2),hum=rep(NA,2),rain=rep(NA,2),den=rep(NA,2)))
    
    datMod$denL1 <- lag(datMod$den, 1)
    datMod$denL2 <- lag(datMod$den, 2)
    datMod$denL3 <- lag(datMod$den, 3)
    datMod$denL6 <- lag(datMod$den, 6)
    datMod$denL12 <- lag(datMod$den, 12)
    datMod$denL24 <- lag(datMod$den, 24)
    datMod$denL60 <- lag(datMod$den, 60)
    
    datMod$tempL1 <- lag(datMod$temp, 1)
    datMod$tempL2 <- lag(datMod$temp, 2)
    datMod$tempL3 <- lag(datMod$temp, 3)
    
    datMod$rainL1 <- lag(datMod$rain, 1)
    datMod$rainL2 <- lag(datMod$rain, 2)
    datMod$rainL3 <- lag(datMod$rain, 3)
    
    datDen <- select(datMod, YM,den)
    datDen$denL1 <- lag(datDen$den, 1)
    datDen$denL2 <- lag(datDen$den, 2)
    datDen$denL3 <- lag(datDen$den, 3)
    datDen$denL4 <- lag(datDen$den, 4)
    datDen$denL5 <- lag(datDen$den, 5)
    datDen$denL6 <- lag(datDen$den, 6)
    datDen$denL7 <- lag(datDen$den, 7)
    datDen$denL8 <- lag(datDen$den, 8)
    datDen$denL9 <- lag(datDen$den, 9)
    datDen$denL10 <- lag(datDen$den, 10)
    datDen$denL11 <- lag(datDen$den, 11)
    datDen$denL12 <- lag(datDen$den, 12)
    datDen$denL13 <- lag(datDen$den, 13)
    datDen$denL14 <- lag(datDen$den, 14)
    datDen$denL15 <- lag(datDen$den, 15)
    datDen$denL16 <- lag(datDen$den, 16)
    datDen$denL17 <- lag(datDen$den, 17)
    datDen$denL18 <- lag(datDen$den, 18)
    datDen$denL19 <- lag(datDen$den, 19)
    datDen$denL20 <- lag(datDen$den, 20)
    datDen$denL21 <- lag(datDen$den, 21)
    datDen$denL22 <- lag(datDen$den, 22)
    datDen$denL23 <- lag(datDen$den, 23)
    datDen$denL24 <- lag(datDen$den, 24)
    datDen$denL25 <- lag(datDen$den, 25)
    datDen$denL26 <- lag(datDen$den, 26)
    datDen$denL27 <- lag(datDen$den, 27)
    datDen$denL28 <- lag(datDen$den, 28)
    datDen$denL29 <- lag(datDen$den, 29)
    datDen$denL30 <- lag(datDen$den, 30)
    datDen$denL31 <- lag(datDen$den, 31)
    datDen$denL32 <- lag(datDen$den, 32)
    datDen$denL33 <- lag(datDen$den, 33)
    datDen$denL34 <- lag(datDen$den, 34)
    datDen$denL35 <- lag(datDen$den, 35)
    datDen$denL36 <- lag(datDen$den, 36)
    datDen$denL37 <- lag(datDen$den, 37)
    datDen$denL38 <- lag(datDen$den, 38)
    datDen$denL39 <- lag(datDen$den, 39)
    datDen$denL40 <- lag(datDen$den, 40)
    datDen$denL41 <- lag(datDen$den, 41)
    datDen$denL42 <- lag(datDen$den, 42)
    datDen$denL43 <- lag(datDen$den, 43)
    datDen$denL44 <- lag(datDen$den, 44)
    datDen$denL45 <- lag(datDen$den, 45)
    datDen$denL46 <- lag(datDen$den, 46)
    datDen$denL47 <- lag(datDen$den, 47)
    datDen$denL48 <- lag(datDen$den, 48)
    datDen$denL49 <- lag(datDen$den, 49)
    datDen$denL50 <- lag(datDen$den, 50)
    datDen$denL51 <- lag(datDen$den, 51)
    datDen$denL52 <- lag(datDen$den, 52)
    datDen$denL53 <- lag(datDen$den, 53)
    datDen$denL54 <- lag(datDen$den, 54)
    datDen$denL55 <- lag(datDen$den, 55)
    datDen$denL56 <- lag(datDen$den, 56)
    datDen$denL57 <- lag(datDen$den, 57)
    datDen$denL58 <- lag(datDen$den, 58)
    datDen$denL59 <- lag(datDen$den, 59)
    datDen$denL60 <- lag(datDen$den, 60)
    
    datMod$den_mL12 <- rowMeans2(as.matrix(datDen[,c(3:14)]))
    datMod$den_sdL12 <- rowSds(as.matrix(datDen[,c(3:14)]))
    datMod$den_m_1sdL12 <- datMod$den_mL12 + datMod$den_sdL12
    datMod$den_m_2sdL12 <- datMod$den_mL12 + datMod$den_sdL12 + datMod$den_sdL12
    
    datMod$den_mL24 <- rowMeans2(as.matrix(datDen[,c(3:26)]))
    datMod$den_sdL24 <- rowSds(as.matrix(datDen[,c(3:26)]))
    datMod$den_m_1sdL24 <- datMod$den_mL24 + datMod$den_sdL24
    datMod$den_m_2sdL24 <- datMod$den_mL24 + datMod$den_sdL24 + datMod$den_sdL24
    
    datMod$den_mL60 <- rowMeans2(as.matrix(datDen[,c(3:62)]))
    datMod$den_sdL60 <- rowSds(as.matrix(datDen[,c(3:62)]))
    datMod$den_m_1sdL60 <- datMod$den_mL60 + datMod$den_sdL60
    datMod$den_m_2sdL60 <- datMod$den_mL60 + datMod$den_sdL60 + datMod$den_sdL60
    
    datMod$den_min60 <- rowMins(as.matrix(datDen[,c(14,26,38,50,62)]))
    datMod$den_means60 <- rowMeans2(as.matrix(datDen[,c(14,26,38,50,62)]))
    datMod$den_max60 <- rowMaxs(as.matrix(datDen[,c(14,26,38,50,62)]))
    
    datMod$den_1L24 <- 1*datMod$denL24
    datMod$den_2L24 <- 2*datMod$denL24
    datMod$den_3L24 <- 3*datMod$denL24
    
    datMod$den_r5t <- rowMeans2(as.matrix(datDen[,c(14,26,38,50,62)]))
    datMod$den_r5tSD <- rowSds(as.matrix(datDen[,c(14,26,38,50,62)]))
    datMod$den_r5t1SD <- datMod$den_r5t + datMod$den_r5tSD
    datMod$den_r5t2SD <- datMod$den_r5t + datMod$den_r5tSD + datMod$den_r5tSD
    
    ###
    
    dat <- datMod[-c(1:60),]
    dat$YM <- as.yearmon(dat$YM, format="%b-%y")
    
    if(input$out_criteria=='r12'){
      dat$out_1 <- dat$den_mL12
      dat$out_2 <- dat$den_m_1sdL12
      dat$out_3 <- dat$den_m_2sdL12
    }
    
    if(input$out_criteria=='r24'){
      dat$out_1 <- dat$den_mL24
      dat$out_2 <- dat$den_m_1sdL24
      dat$out_3 <- dat$den_m_2sdL24
    }
    
    if(input$out_criteria=='WHO'){
      dat$out_1 <- dat$den_mL60
      dat$out_2 <- dat$den_m_1sdL60
      dat$out_3 <- dat$den_m_2sdL60
    }
    
    if(input$out_criteria=='RI'){
      dat$out_1 <- dat$den_1L24
      dat$out_2 <- dat$den_2L24
      dat$out_3 <- dat$den_3L24
    }
    
    if(input$out_criteria=='rerata'){
      dat$out_1 <- dat$den_r5t
      dat$out_2 <- dat$den_r5t1SD
      dat$out_3 <- dat$den_r5t2SD
    }
    
    if(input$out_criteria=='minMax'){
      dat$out_1 <- dat$den_min60
      dat$out_2 <- dat$den_means60
      dat$out_3 <- dat$den_max60
    }
    
    return(dat)
  })
  
  datTrain <- reactive({
    df <- dat()
    
    t <- t()
    t <- round((t/100)*nrow(df),0)
    
    df <- df[c(1:t),]
    return(df)
  })
  
  datTest <- reactive({
    df <- dat()
    
    t <- t()
    t <- round((t/100)*nrow(df),0)
    
    df <- df[c((t+1):nrow(df)),]
    return(df)
  })
  
  output$mthE <- renderText({ 
    dat_mth <- dat_mth()
    paste(dat_mth)
  })
  
  output$mthP <- renderText({ 
    dat_mth <- dat_mth()
    
    addYM <- as.yearmon(seq(as.Date(dat_mth), by = "month", length.out = 3))
    paste(addYM[length(addYM)])
  })
  
  centroid_sdistrict <- reactive({
    centroid_sdistrict <- read_csv("map/sdistrict_centroid.csv")
    return(centroid_sdistrict)
  })
  
  centroid_village <- reactive({
    centroid_village <- read_csv("map/village_centroid.csv")
    return(centroid_village)
  })
  
  area_sdistrict <- reactive({
    area_sdistrict <- st_read("map/yogyakarta-sdistrict.shp")
    return(area_sdistrict)
  })
  
  area_village <- reactive({
    area_village <- st_read("map/yogyakarta-village.shp")
    return(area_village)
  })
  
  output$plotMap_village <- renderPlot({
    dat_mth <- dat_mth()
    
    datDen_village <- datDen_village()
    centroid_village <- centroid_village()
    
    area_sdistrict <- area_sdistrict()
    area_village <- area_village()
    
    datMapV <- subset(datDen_village, YM==dat_mth)
    datMapV <- merge(centroid_village, datMapV, by="village")
    datMapV <- select(datMapV, YM,village,xcoord,ycoord,den)
    datMapV[datMapV == 0] <- NA
    
    ggplot() + 
      geom_sf(data = area_village, size = 0.5, color = "black", fill = NA) +
      geom_sf(data = area_sdistrict, size = 1.0, color = "black", fill = NA) +
      geom_point(data = datMapV, aes(x = xcoord, y = ycoord), size = datMapV$den, 
                 shape = 21, fill = "red") +
      ggtitle("Kasus Tingkat Kelurahan") + 
      coord_sf() +
      labs(x = "") +
      labs(y = "") +
      theme_bw()
    
  })
  
  output$plotMap_subdistrict <- renderPlot({
    dat_mth <- dat_mth()
    
    datDen_subdistrict <- datDen_subdistrict()
    centroid_sdistrict <- centroid_sdistrict()
    
    area_sdistrict <- area_sdistrict()
    area_village <- area_village()
    
    datMapD <- subset(datDen_subdistrict, YM==dat_mth)
    names(centroid_sdistrict)[2] <- "subdistrict"
    datMapD <- merge(centroid_sdistrict, datMapD, by="subdistrict")
    datMapD <- select(datMapD, YM,subdistrict,xcoord,ycoord,den)
    datMapD[datMapD == 0] <- NA
    
    ggplot() + 
      geom_sf(data = area_village, size = 0.5, color = "black", fill = NA) +
      geom_sf(data = area_sdistrict, size = 1.0, color = "black", fill = NA) +
      geom_point(data = datMapD, aes(x = xcoord, y = ycoord), size = datMapD$den, 
                 shape = 21, fill = "cyan") +
      ggtitle("Kasus Tingkat Kecamatan") + 
      coord_sf() +
      labs(x = "") +
      labs(y = "") +
      theme_bw()
    
  })
  
  output$plotMap_risk <- renderPlot({
    dat_mth <- dat_mth()
    
    datDen_subdistrict <- datDen_subdistrict()
    centroid_sdistrict <- centroid_sdistrict()
    
    area_sdistrict <- area_sdistrict()
    area_village <- area_village()
    
    # datMapD <- subset(datDen_subdistrict, YM==dat_mth)
    # names(centroid_sdistrict)[2] <- "subdistrict"
    # datMapD <- merge(centroid_sdistrict, datMapD, by="subdistrict")
    # datMapD <- select(datMapD, YM,subdistrict,xcoord,ycoord,den)
    # datMapD[datMapD == 0] <- NA
    
    ggplot() + 
      geom_sf(data = area_village, size = 0.5, color = "black", fill = NA) +
      geom_sf(data = area_sdistrict, size = 1.0, color = "black", fill = NA) +
      # geom_point(data = datMapD, aes(x = xcoord, y = ycoord), size = datMapD$den, 
      #            shape = 21, fill = "cyan") +
      ggtitle("RR per Kecamatan (coming soon)") + 
      coord_sf() +
      labs(x = "") +
      labs(y = "") +
      theme_bw()
    
  })
  
  gamMod <- reactive({
    dat_train <- datTrain()
    
    gamMod <- gam(den ~ s(tempL3,k=4) + s(rainL2,k=4) + s(rainL3,k=4) + s(denL2,k=4) + s(denL24,k=4),
                  family=quasipoisson, na.action=na.exclude, data=dat_train)
    
    return(gamMod)
  })
  
  datTrain_plot <- reactive({
    dat_train <- datTrain()
    gamMod <- gamMod()
    
    dat_train$predict <- predict(gamMod, type="response")
    
    if(dat_local()==TRUE){
      dat_train <- mutate(dat_train,
                          outObs = derivedFactor(
                            "400" = (den < out_1),
                            "425" = (den >= out_1 & den < out_2),
                            "450" = (den >= out_2 & den < out_3),
                            "475" = (den >= out_3),
                            .method = "first",
                            .default = 0
                          ))
      
      dat_train <- mutate(dat_train,
                          outPrd = derivedFactor(
                            "400" = (predict < out_1),
                            "425" = (predict >= out_1 & predict < out_2),
                            "450" = (predict >= out_2 & predict < out_3),
                            "475" = (predict >= out_3),
                            .method = "first",
                            .default = 0
                          ))
      
      dat_train <- mutate(dat_train,
                          outSpc = derivedFactor(
                            "1" = (outObs == outPrd),
                            .method = "first",
                            .default = 0
                          ))
    }
    
    if(dat_local()==FALSE){
      
      # validate(
      #   need(input$dat_upload != "", "Please select a data set")
      # )
      
      dat_train <- mutate(dat_train,
                          outObs = derivedFactor(
                            "100" = (den < out_1),
                            "110" = (den >= out_1 & den < out_2),
                            "120" = (den >= out_2 & den < out_3),
                            "130" = (den >= out_3),
                            .method = "first",
                            .default = 0
                          ))
      
      dat_train <- mutate(dat_train,
                          outPrd = derivedFactor(
                            "100" = (predict < out_1),
                            "110" = (predict >= out_1 & predict < out_2),
                            "120" = (predict >= out_2 & predict < out_3),
                            "130" = (predict >= out_3),
                            .method = "first",
                            .default = 0
                          ))
      
      dat_train <- mutate(dat_train,
                          outSpc = derivedFactor(
                            "1" = (outObs == outPrd),
                            .method = "first",
                            .default = 0
                          ))
    }
    
    return(dat_train)
  })
  
  datTest_plot <- reactive({
    dat_test <- datTest()
    gamMod <- gamMod()
    
    dat_test$predict <- predict(gamMod, type="response", newdata=dat_test)
    
    if(dat_local()==TRUE){
      
      dat_test <- mutate(dat_test,
                         outObs = derivedFactor(
                           "400" = (den < out_1),
                           "425" = (den >= out_1 & den < out_2),
                           "450" = (den >= out_2 & den < out_3),
                           "475" = (den >= out_3),
                           .method = "first",
                           .default = 0
                         ))
      
      dat_test <- mutate(dat_test,
                         outPrd = derivedFactor(
                           "400" = (predict < out_1),
                           "425" = (predict >= out_1 & predict < out_2),
                           "450" = (predict >= out_2 & predict < out_3),
                           "475" = (predict >= out_3),
                           .method = "first",
                           .default = 0
                         ))
      
      dat_test <- mutate(dat_test,
                         outSpc = derivedFactor(
                           "1" = (outObs == outPrd),
                           .method = "first",
                           .default = 0
                         ))
    }
    
    if(dat_local()==FALSE){
      
      # validate(
      #   need(input$dat_upload != "", "Please select a data set")
      # )
      
      dat_test <- mutate(dat_test,
                         outObs = derivedFactor(
                           "100" = (den < out_1),
                           "110" = (den >= out_1 & den < out_2),
                           "120" = (den >= out_2 & den < out_3),
                           "130" = (den >= out_3),
                           .method = "first",
                           .default = 0
                         ))
      
      dat_test <- mutate(dat_test,
                         outPrd = derivedFactor(
                           "100" = (predict < out_1),
                           "110" = (predict >= out_1 & predict < out_2),
                           "120" = (predict >= out_2 & predict < out_3),
                           "130" = (predict >= out_3),
                           .method = "first",
                           .default = 0
                         ))
      
      dat_test <- mutate(dat_test,
                         outSpc = derivedFactor(
                           "1" = (outObs == outPrd),
                           .method = "first",
                           .default = 0
                         ))
    }
    
    return(dat_test)
  })
  
  output$plotMod <- renderPlot({
    
    dat <- dat()
    dat_train <- datTrain_plot()
    dat_test <- datTest_plot()
    
    
    if(dat_local()==TRUE){
      
      plot(dat$YM, dat$out_3, ylim=c(0,900), type="l", xlab="", ylab="kasus", frame.plot=F, col="red")
      points(dat$YM, dat$den, type="h", col="grey75")
      
      points(dat_test$YM, dat_test$den, type="h", col="cadetblue1")
      abline(v=dat_test$YM[1], col="blue")
      abline(v=dat_test$YM[nrow(dat_test)-2], col="blue")
      
      points(dat_train$YM, dat_train$predict, type="p")
      points(dat_test$YM, dat_test$predict, type="p", col="blue")
      
      points(dat$YM, dat$out_1, type="l", col="darkgreen")
      points(dat$YM, dat$out_2, type="l", col="darkorange")
      points(dat$YM, dat$out_3, type="l", col="red")
      
      abline(h=c(400,425,450,475), col=c("darkgreen","darkorange","red","brown"), lty=3)

      dat_train$outObs <- as.numeric(as.character(dat_train$outObs))
      dat_train$outPrd <- as.numeric(as.character(dat_train$outPrd))
      
      dat_test$outObs <- as.numeric(as.character(dat_test$outObs))
      dat_test$outPrd <- as.numeric(as.character(dat_test$outPrd))
      
      points(dat_train$YM, dat_train$outObs, lwd=3, pch=4, col="grey75")
      points(dat_train$YM, dat_train$outPrd, pch=20, col="black")
      
      points(dat_test$YM, dat_test$outObs, lwd=3, pch=4, col="cadetblue1")
      points(dat_test$YM, dat_test$outPrd, pch=20, col="blue")
      
      train_Prd_1 <- sum(as.numeric(as.character(dat_train$outSpc)), na.rm=T)
      train_Prd_tot <- as.numeric(as.character(nrow(dat_train)-sum(is.na(dat_train$outSpc))))
      train_Prd_pct <- round(train_Prd_1/train_Prd_tot*100,2)
      
      test_Prd_1 <- sum(as.numeric(as.character(dat_test$outSpc)), na.rm=T)
      test_Prd_tot <- as.numeric(as.character(nrow(dat_test)-sum(is.na(dat_test$outSpc))))
      test_Prd_pct <- round(test_Prd_1/test_Prd_tot*100,2)
      
      text(dat_train$YM[2], 500, "training set", col="black", adj=c(0,0))
      text(dat_test$YM[2], 500, "external validation", col="blue", adj=c(0,0))
      text(dat_test$YM[nrow(dat_test)-2], 500, "", col="blue", adj=c(0,0))
      
      text(dat_train$YM[2], 350, paste0("akurasi ", train_Prd_pct, " %"), col="black", adj=c(0,0))
      text(dat_test$YM[2], 350, paste0("akurasi ", test_Prd_pct, " %"), col="blue", adj=c(0,0))
    }
    
    if(dat_local()==FALSE){
      
      plot(dat$YM, dat$out_3, ylim=c(0,150), type="l", xlab="", ylab="kasus", frame.plot=F, col="red")
      points(dat$YM, dat$den, type="h", col="grey75")
      
      points(dat_test$YM, dat_test$den, type="h", col="cadetblue1")
      abline(v=dat_test$YM[1], col="blue")
      abline(v=dat_test$YM[nrow(dat_test)-2], col="blue")
      
      points(dat_train$YM, dat_train$predict, type="p")
      points(dat_test$YM, dat_test$predict, type="p", col="blue")
      
      points(dat$YM, dat$out_1, type="l", col="darkgreen")
      points(dat$YM, dat$out_2, type="l", col="darkorange")
      points(dat$YM, dat$out_3, type="l", col="red")
      
      abline(h=c(100,110,120,130), col=c("darkgreen","darkorange","red","brown"), lty=3)
      
      dat_train$outObs <- as.numeric(as.character(dat_train$outObs))
      dat_train$outPrd <- as.numeric(as.character(dat_train$outPrd))
      
      dat_test$outObs <- as.numeric(as.character(dat_test$outObs))
      dat_test$outPrd <- as.numeric(as.character(dat_test$outPrd))
      
      points(dat_train$YM, dat_train$outObs, lwd=3, pch=4, col="grey75")
      points(dat_train$YM, dat_train$outPrd, pch=20, col="black")
      
      points(dat_test$YM, dat_test$outObs, lwd=3, pch=4, col="cadetblue1")
      points(dat_test$YM, dat_test$outPrd, pch=20, col="blue")
      
      train_Prd_1 <- sum(as.numeric(as.character(dat_train$outSpc)), na.rm=T)
      train_Prd_tot <- as.numeric(as.character(nrow(dat_train)-sum(is.na(dat_train$outSpc))))
      train_Prd_pct <- round(train_Prd_1/train_Prd_tot*100,2)
      
      test_Prd_1 <- sum(as.numeric(as.character(dat_test$outSpc)), na.rm=T)
      test_Prd_tot <- as.numeric(as.character(nrow(dat_test)-sum(is.na(dat_test$outSpc))))
      test_Prd_pct <- round(test_Prd_1/test_Prd_tot*100,2)
      
      text(dat_train$YM[2], 140, "training set", col="black", adj=c(0,0))
      text(dat_test$YM[2], 140, "external validation", col="blue", adj=c(0,0))
      text(dat_test$YM[nrow(dat_test)-2], 500, "", col="blue", adj=c(0,0))
      
      text(dat_train$YM[2], 90, paste0("akurasi ", train_Prd_pct, " %"), col="black", adj=c(0,0))
      text(dat_test$YM[2], 90, paste0("akurasi ", test_Prd_pct, " %"), col="blue", adj=c(0,0))
    }
    
  })
  
  output$pred_num <- renderValueBox({
    df <- datTest_plot()
    valueBox("kasus", round(df$predict[nrow(df)], 0), icon = icon("medkit"), color = "aqua" )
  })
  
  output$pred_out <- renderValueBox({
    df <- datTest_plot()
    
    if (df$outPrd[nrow(df)]==100) { valueBox("status", "NORMAL", icon = icon("exclamation-circle"), color = "green" ) }
    else if(df$outPrd[nrow(df)]==110) { valueBox("status", "WASPADA", icon = icon("exclamation-circle"), color = "yellow" ) }
    else if(df$outPrd[nrow(df)]==120) { valueBox("status", "SIAGA", icon = icon("exclamation-circle"), color = "orange" ) }
    else if(df$outPrd[nrow(df)]==130) { valueBox("status", "AWAS", icon = icon("exclamation-circle"), color = "red" ) }
    
    else if (df$outPrd[nrow(df)]==400) { valueBox("status", "NORMAL", icon = icon("exclamation-circle"), color = "green" ) }
    else if(df$outPrd[nrow(df)]==425) { valueBox("status", "WASPADA", icon = icon("exclamation-circle"), color = "yellow" ) }
    else if(df$outPrd[nrow(df)]==450) { valueBox("status", "SIAGA", icon = icon("exclamation-circle"), color = "orange" ) }
    else if(df$outPrd[nrow(df)]==475) { valueBox("status", "AWAS", icon = icon("exclamation-circle"), color = "red" ) }
    
    else { valueBox("data tidak lengkap", "", icon = icon("exclamation-circle"), color = "red" ) }
    
  })
  
  ###
  
  output$downloadReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.docx",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(dat_mth = dat_mth(), 
                     
                     area_sdistrict = area_sdistrict(),
                     area_village = area_village(),
                     
                     datDen_village = datDen_village(),
                     centroid_village = centroid_village(),
                     
                     datDen_subdistrict = datDen_subdistrict(),
                     centroid_sdistrict = centroid_sdistrict(),
                     
                     dat = dat(),
                     dat_train = datTrain_plot(),
                     dat_test = datTest_plot()
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
} 


# APP ---------------------------------------------------------------------

shinyApp(ui, server)