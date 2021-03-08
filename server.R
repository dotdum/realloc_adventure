# server.R script for for Time-Use Reallocation shinydashboard
# Reactive objects (i.e., those dependent on widget input) are written here
# ------------------------------------------------------------------------------
options(shiny.error = browser)

## Server: all reactive values, output and ui elements
  server <- function(input, output, session) {
      
    
    # Initial Composition and Outcome Values
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Reactive objects based on user data from the participant-tab
    
  # Define initial times (hours plus minutes)
    init.time <- reactive({
      data.frame(
        Sleep = input$initSleep + input$initSleepmin/60,
        Screen = input$initScreen + input$initScreenmin/60,
        PA = input$initPA + input$initPAmin/60,
        QuietT = input$initQuietT + input$initQuietTmin/60,
        PassiveTrans = input$initPassiveTrans + input$initPassiveTransmin/60,
        School = input$initSchool + input$initSchoolmin/60,
        Domestic_SelfCare = input$initDomestic_SelfCare + input$initDomestic_SelfCaremin/60
      )
    })
    
  # Define initial composition
  # Based on input if it adds up to 24, otherwise based on mean composition
    
    ##it would be better if the user was prompted to make this sum to 24 h##**
    init.comp <- reactive({
      if (sum(init.time()) == 24) {
        acomp(init.time())
      } else {
        m.comp
      }
    })
    
  # Capture reallocated times
    reall.time <- reactive({
      c(input$Sleep, input$Screen, 
        input$PA, input$QuietT, input$PassiveTrans,
        input$School, input$Domestic_SelfCare)
    })
  
    
  # Define reactive times
    Rtime <- reactive({
      if (sum(reall.time()) == 0) {
        out <- init.time()[1,] + reall.time()/60
        names(out) <- c("Sleep","Screen", "PA", "QuietT","PassiveTrans", "School", "Domestic_SelfCare")
        return(out)
      } else {
        init.time()
      }
    })
    
  # Define reactive composition
    Rcomp <- reactive({
      acomp(Rtime())
    })
    
  # Define error text that checks whether initial time inputs adds up to 24
    output$err1 <- renderText({
      if (sum(init.time()) != 24) {
        diff <- 24 - sum(init.time())
        more.less <- ifelse(diff > 0, "add", "remove")
        hours <- floor(abs(diff))
        mins <- round((abs(diff) - hours)*60)
        hours.mins <- paste0(
          ifelse(hours == 0, "", paste(hours, "hour(s)")),
          ifelse(hours == 0 | mins == 0, "", " and "),
          ifelse(mins == 0, "", paste(mins, "minute(s)"))
        )
        paste(err1.string, "Please", more.less, hours.mins)
      } else {
        " "
      }
    })
    
  # Define error text that checks whether reallocation time inputs adds up to 0
    output$err2 <- renderText({
      if (sum(reall.time()) != 0) {
        diff <- 0 - sum(reall.time())
        more.less <- ifelse(diff > 0, "add", "remove")
        mins <- abs(diff)
        paste(err2.string, "Please", more.less, mins, "minute(s)")
      } else {
        " "
      }
    })
    
    # Define error text that checks whether reallocation time inputs have compositions > 0
    output$err3 <- renderText({
      rt_vec <- Rtime()
      comp_nms <- names(rt_vec)
      comp_vls_lt0 <- rt_vec <= 0
      
      if (any(comp_vls_lt0)) {
        paste0(
          "Currently there are time-use categories with negative time(s). \n",
          "This is because the re-allocation of time exceeds the corresponding ",
          "starting time in its time-use category. \n",
          "The listed time-use categories below provide the mintutes required ",
          "for there to be non-negative time categories (minutes in brackets): \n",
          paste(
            paste0(
              comp_nms[comp_vls_lt0], 
              "(", 
              round(-rt_vec[comp_vls_lt0] * 60, 0), 
              " minutes)"
            ), 
            collapse = ",\n"
          )
        )
      } else {
        " "
      }
    })
    
    
    observeEvent(input$reset_sliders, { 
      
      updateSliderInput(session, 'Sleep' ,value = 0)
      updateSliderInput(session, 'Screen' ,value = 0)
      updateSliderInput(session, 'PA' ,value = 0)
      updateSliderInput(session, 'QuietT' ,value = 0)
      updateSliderInput(session, 'PassiveTrans' ,value = 0)
      updateSliderInput(session, 'School' ,value = 0)
      updateSliderInput(session, 'Domestic_SelfCare' ,value = 0)
    
 
    })
    
    # Reactive composition output
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    # Output for reactive d3 histogram
    
    # Create reactive object to send to d3 script
    d3.data <- reactive({
      
      # If less than an hour show in minutes
      rounded.comp <- round(as.vector(Rcomp())*24, 1)
      comp.units <- rep(" hours", length(rounded.comp))
      comp.units[rounded.comp < 1] <- " mins"
      rounded.comp[rounded.comp < 1] <- round(as.vector(Rcomp())*1440)[rounded.comp < 1]
      
      # Output dataframe for d3 script
      data.frame(
        prop = as.vector(Rcomp())*2,  # affects maximal size of histogram columns
        val = rounded.comp,  # rounded composition values
        lab = activity,  # labels for histogram
        unit = comp.units  # composition units
      )
    })  # d3.data
    
    # Call d3 script to make histogram
    output$d3hist <- renderD3({
      r2d3(
        d3.data(),
        script = "script.js",  # and here we call the script,
        d3_version="5"
      )  # r2d3
    })  # renderD3
    
    
  
# Model Outcomes output
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Predicted outcomes from selected models on result-tab
 ##model for %Body fat  
  # Define predictions for initial composition
    init.pred.bf <- reactive({
      out <- 
        predict(
          model.bf, 
          newdata = list(
           ilr1=ilr(init.comp())[1],
           ilr2=ilr(init.comp())[2], 
           ilr3=ilr(init.comp())[3],
           ilr4=ilr(init.comp())[4],
           ilr5=ilr(init.comp())[5],
           ilr6=ilr(init.comp())[6],
           cov.sex = input$sex,
           cov.age = input$age,
           cov.sep = input$sep,
           cov.puberty =input$puberty
          ),  #list
          interval = "confidence"
        )  #predict
      out <- out[1, ]
      out <- exp(out) # because log transformed outcome

      return(out)
    })
    
    
  # Define predictions for reallocation composition
    reall.pred.bf <- reactive({
      out <- 
        predict(
          model.bf, 
          newdata = list(
            ilr1=ilr(Rcomp())[1],
            ilr2=ilr(Rcomp())[2], 
            ilr3=ilr(Rcomp())[3],
            ilr4=ilr(Rcomp())[4],
            ilr5=ilr(Rcomp())[5],
            ilr6=ilr(Rcomp())[6],
            cov.sex = input$sex,
            cov.age = input$age,
            cov.sep = input$sep,
            cov.puberty =input$puberty
          ),  #list
          interval = "confidence"
        )  #predict
      out <- out[1, ]
      out <- exp(out) #because log transformed outcome

      return(out)
    })

    
  # Compute difference between initial and reallocated predictions
   
  # Reallocation results
    output$specific.current <- renderText({
      pred_init <- init.pred.bf()
      sprintf("%3.1f%% [%3.1f:%3.1f]", pred_init[1], pred_init[2], pred_init[3])
    })
    
    output$specific.new <- renderText({
      pred_reall <- reall.pred.bf()
      sprintf("%3.1f%% [%3.1f:%3.1f]", pred_reall[1], pred_reall[2], pred_reall[3])
    })
    
    delta.pred <- reactive({
      pred_init <- init.pred.bf()
      pred_reall <- reall.pred.bf()
      pred_reall[1] - pred_init[1]
    })
    
    output$specific.diff <- renderText({
      sprintf("%3.1f%%", delta.pred())
    })
  
    
     
     
     
     
# Define session behaviour and error messages
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Reactive objects handling errors and debug
    
  # Close the R session when browser closes
    session$onSessionEnded(function() {
      stopApp()
    })

  # Open console for R session
    observe(label = "console", {
      if(input$console != 0) {
        options(browserNLdisabled = TRUE)
        isolate(browser())
      }
    })
  
  }  #server
  
