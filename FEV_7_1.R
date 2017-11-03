library(shiny)
library(shinythemes)
library(ggplot2)
library(shinyjs)
library(lme4) # to build linear mixed model  
library(lmerTest) # for outputing test results from the mixed model
library(plyr) #for merging data
library(MuMIn)
library(fBasics)
library(ROCR)
library(pROC)
library(ipw)
library(data.table)

source('~/Documents/RStudio projects1/20171008/Lungfunction2/FEV_functions.R')
source('~/Documents/RStudio projects1/20171008/Lungfunction2/FEV_na_inputs_check.R')



#Define UI for application that generates simulation, showing individualized
#prediction of adulthood lung function decline for Framingham offspring cohort analysis
ui <- fluidPage(
  theme = shinytheme("united"),
  tags$head(tags$script(src = "message-handler.js")),
  
  titlePanel("Individualized Prediction of Adulthood Lung Function Decline"),
  
  sidebarLayout(
    source('~/Documents/RStudio projects1/20171008/Lungfunction2/FEV_sidebarPanel.R'), #load left sidebar Panel that has the inputs

    mainPanel (
      tags$p("lmer summary:"),
      verbatimTextOutput("lmer_summary"),
      width = 5, class = 'rightAlign'
    )
  )
)

server <- function(input, output, session) {


  
  #load inputs
  observeEvent(input$load_inputs,{
    if(!file.exists('FEV_inputs.CSV')) {return(NULL)}
    loadedInputs <- read.csv('FEV_inputs.CSV')

    #load numeric values for the numericInput inputsr
    for (i in 1:(length(loadedInputs$FEV_input_names)-4)) {
      session$sendInputMessage(loadedInputs$FEV_input_names[i],  list(value = loadedInputs$FEV_input_num_vals[(i)]) )
    }
    #load strings for selectInput inputs
    for (i in (length(loadedInputs$FEV_input_names)-3):(length(loadedInputs$FEV_input_names))) {
      session$sendInputMessage(loadedInputs$FEV_input_names[i],  list(value = loadedInputs$FEV_input_char_vals[(i)]) )
    }
  })

  #save inputs
  observeEvent(input$save_inputs,{
    FEV_frame_labels <- FEV_input_labels()
    FEV_frame_num_values <- c(input$trig, #FEV_frame_num_values used to generate data frame column with numeric values only
                              input$hema,
                              input$alb,
                              input$glob,
                              input$alk_phos,
                              input$white_bc,
                              input$qrs,
                              input$alcohol,
                              input$wine,
                              input$cocktail,
                              input$height_square,
                              input$cum_smoke,
                              input$age,
                              input$follow_up_baseline,
                              # input$ba_use,
                              # input$dys_exer,
                              # input$noc_s,
                              # input$sex
                              -999,
                              -999,
                              -999,
                              -999
    )
    FEV_frame_char_values <- c("NULL", #FEV_frame_char_values used to generate data frame column with char values only
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               input$ba_use,
                               input$dys_exer,
                               input$noc_s,
                               input$sex
    )
    # FEV_data_frame <- data.frame(FEV_input_names=FEV_frame_labels, FEV_input_vals=FEV_frame_values)
    FEV_data_frame <- data.frame(FEV_input_names=FEV_frame_labels,
                                 FEV_input_num_vals=FEV_frame_num_values,
                                 FEV_input_char_vals = FEV_frame_char_values)
    write.csv( FEV_data_frame , file = 'FEV_inputs.csv')
  })
  
  
  source('~/Documents/RStudio projects1/20171008/Lungfunction2/FEV_na_inputs_check.R')

  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################  
  output$lmer_summary <- renderText({
    if (input$lmer_Submit_button == 0)
       return()

    #input$lmer_Submit_button
    isolate({
      file_name=BINARY_CODE_FROM_INPUTS(input$age,
                                        input$follow_up_baseline,
                                        input$trig,
                                        input$hema, 
                                        input$alb,
                                        input$glob,
                                        input$alk_phos,
                                        input$white_bc,
                                        input$qrs,
                                        input$alcohol,
                                        input$wine,
                                        input$cocktail,
                                        input$height_square,
                                        input$cum_smoke,
                                        input$sex,
                                        input$ba_use,
                                        input$dys_exer,
                                        input$noc_s
      )
      
      # full_file_name = paste(file_name,".csv")
      full_file_name = paste(paste(file_name,collapse=" "),".rds") #DK - updated 2017-10-29 because BINARY_CODE_FROM_INPUTS was updated from generating a single character to generating a vector of characters
      #2.0 if RDS file(for given inputs) exists, get lmer_summary from the rts file
      if(file.exists(full_file_name)){ 
        # 3.If file exists, Load lmer object from the file
        
        loaded_lmer_fn <- readRDS(full_file_name)
        
        summary_lmfin <- capture.output({
          print(summary(loaded_lmer_fn))
        })
        summary_lmfin
        
        # print(summary(loaded_lmer_fn))
        
        # con <- file("test.log")
        # sink(con, append=TRUE)
        # sink(con, append=TRUE, type="message")
        # 
        # # This will echo all input and not truncate 150+ character lines...
        # # source("script.R", echo=TRUE, max.deparse.length=10000)
        # print(summary(loaded_lmer_fn))
        # # Restore output to console
        # sink() 
        # sink(type="message")
        # 
        # # And look at the log...
        # # lmer_summary_logfile <- cat(readLines("test.log"), sep="\n")
        # # as.char(cat(readLines("test.log"), sep="\n"))
        # 
        # output$lmer_summary_output <- renderText({
        #   cat(readLines("test.log"), sep="\n")
        # })
        # # output$lmer_summary_output <- renderUI({
        # #   # paste("File for these inputs already exists\nSummary has been loaded from \n",full_file_name)
        # #   summary(loaded_lmer_fn)
        # # })
        # summary(loaded_lmer_fn)
        
      }
      else{
        # 4.If file does not exist

        #BINARY_CODE_DATAFRAME
        BINARY_INPUT_NAMES <- c('age','follow_up_baseline','trig','hema','alb','glob','alk_phos','white_bc','qrs','alcohol','wine','cocktail','height_square','cum_smoke','sex','ba_use','dys_exer','noc_s')
        BINARY_INPUT_VALUES <- c("1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1")
        BINARY_CODE_DATAFRAME <- data.frame(file_name, BINARY_INPUT_NAMES)
        
        #FACTOR_NAMES_DATAFRAME
        INPUTS <- c('age','follow_up_baseline','trig','hema','alb','glob','alk_phos','white_bc','qrs','alcohol','wine','cocktail','height_square','cum_smoke','sex','ba_use','dys_exer','noc_s')
        EQUATION_FACTORS1 <- c('age','year','triglycerides','hematocrit','albumin','globulin','ALP','WBC','QRS_intv','alcohol_indx','wine','cocktail','height2','cpackyr','sex','broncho','dyspnea_exc','night_sym')
        EQUATION_FACTORS2 <- c('agecat','year2','triglycerides:year','hematocrit:year','albumin:year','globulin:year','ALP:year','WBC:year','QRS_intv:year','alcohol_indx:year','wine:year','cocktail:year','height2:sex',NA,'sex:year','broncho:year','dyspnea_exc:year','night_sym:year')
        EQUATION_FACTORS3 <- c(NA,'(year|RANDOMID)','triglycerides:cpackyr',NA,'albumin:sex',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
        FACTORS_NAMES_DATAFRAME <- data.frame(INPUTS, EQUATION_FACTORS1, EQUATION_FACTORS2, EQUATION_FACTORS3)
        
        
        lmer_function_output <- FEV_calculate_lmer_fn(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)
        
        saveRDS(object=lmer_function_output,file = full_file_name,compress=TRUE, refhook = NULL)
        
        summary_lmfin <- capture.output({
          print(summary(lmer_function_output))
        })
        summary_lmfin

        # browser()
        
        
        # # output$lmer_summary_output <- renderUI({
        # #   # paste("File for these inputs already exists\nSummary has been loaded from \n",full_file_name)
        # #   summary(lmer_function_output)
        # # })
        # con <- file("test.log")
        # sink(con, append=TRUE)
        # sink(con, append=TRUE, type="message")
        # 
        # # This will echo all input and not truncate 150+ character lines...
        # # source("script.R", echo=TRUE, max.deparse.length=10000)
        # print(summary(lmer_function_output))
        # # Restore output to console
        # sink() 
        # sink(type="message")
        # 
        # # And look at the log...
        # # lmer_summary_logfile <- cat(readLines("test.log"), sep="\n")
        # cat(readLines("test.log"), sep="\n")
        # 
        # 
        # # renderUI({
        # #   summary(lmer_function_output)
        # # })
        
      }
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

    }) #end of isolate({...})
    
  })#end of output$lmer_summary <- renderTable
  
} #end of server <- function


#Run the application
shinyApp(ui = ui, server = server)