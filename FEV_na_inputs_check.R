FEV_check_for_na_in_inputs <- function(){
  react <- reactiveValues()                               #Create an object for storing reactive values; name of the object = react
  observe({                                               #Create a reactive observer - reactive expression in that it can read reactive values and call reactive expressions, reexecutes when dependencies change
    if(is.na(input$age)){return()}                        #is.na(input$age) tests is age input is not available; if age input is not available returns NULL
    
    if(input$age < 0){                                    #if entered age is less than 0, return 0
      react$age =0
      updateNumericInput(session, "age", age = react$age) #Change the value of a number input on the client -
    } else {                                              #Else update the age
      react$age <- input$age
    }
  }
  )
  #NULL for follow_up_baseline
  react <- reactiveValues()
  observe({
    if(is.na(input$follow_up_baseline)){return()}
    else  {
      react$follow_up_baseline <- input$follow_up_baseline
    }
  }
  )
  #NULL for triglycerides
  react <- reactiveValues()
  observe({
    if(is.na(input$trig)){return()}
    else  {
      react$trig <- input$trig
    }
  }
  )
  #NULL for hematocrit
  react <- reactiveValues()
  observe({
    if(is.na(input$hema)){return()}
    if(input$hema < 0){                                   
      react$hema =0
      updateNumericInput(session, "hema", hema = react$hema)
    } else  {
      react$hema <- input$hema
    }
  }
  )
  #NULL for albumin
  react <- reactiveValues()
  observe({
    if(is.na(input$alb)){return()}
    else  {
      react$alb <- input$alb
    }
  }
  )
  #NULL for globulin
  react <- reactiveValues()
  observe({
    if(is.na(input$glob)){return()}
    else  {
      react$glob <- input$glob
    }
  }
  )
  #NULL for Alkaline Phosphotase
  react <- reactiveValues()
  observe({
    if(is.na(input$alk_phos)){return()}
    else  {
      react$alk_phos <- input$alk_phos
    }
  }
  )
  #NULL for white blood cell count
  react <- reactiveValues()
  observe({
    if(is.na(input$white_bc)){return()}
    else  {
      react$white_bc <- input$white_bc
    }
  }
  )
  #NULL for QRS interval (hundredth of sec)
  react <- reactiveValues()
  observe({
    if(is.na(input$qrs)){return()}
    else  {
      react$qrs <- input$qrs
    }
  }
  )
  #NULL for alcohol index
  react <- reactiveValues()
  observe({
    if(is.na(input$alcohol)){return()}
    if(input$hema < 0){                                   
      react$hema =0
      updateNumericInput(session, "hema", hema = react$hema)
    } else  {
      react$alcohol <- input$alcohol
    }
  }
  )
  #NULL for wine intake
  react <- reactiveValues()
  observe({
    if(is.na(input$wine)){return()}
    if(input$hema < 0){                                   
      react$hema =0
      updateNumericInput(session, "hema", hema = react$hema)
    } else  {
      react$wine <- input$wine
    }
  }
  )
  #NULL for cocktail intake
  react <- reactiveValues()
  observe({
    if(is.na(input$cocktail)){return()}
    if(input$hema < 0){                                   
      react$hema =0
      updateNumericInput(session, "hema", hema = react$hema)
    } else  {
      react$cocktail <- input$cocktail
    }
  }
  )
  #NULL for Height
  react <- reactiveValues()
  observe({
    if(is.na(input$height_square)){return()}
    if(input$hema < 0){                                   
      react$hema =0
      updateNumericInput(session, "hema", hema = react$hema)
    } else  {
      react$height_square <- input$height_square
    }
  }
  )
  #NULL for cumulative smoke pack-year
  react <- reactiveValues()
  observe({
    if(is.na(input$cum_smoke)){return()}
    if(input$hema < 0){                                   
      react$hema =0
      updateNumericInput(session, "hema", hema = react$hema)
    } else  {
      react$cum_smoke <- input$cum_smoke
    }
  }
  )
}