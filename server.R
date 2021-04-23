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
    
    shinyjs::hide(id = "plot1")
    # ---- composition_funcs ----
    
    init.time <- reactive({
      
      init_df <-
        data.frame(
          Sleep = input$initSleep + input$initSleepmin/60,
          Screen = input$initScreen + input$initScreenmin/60,
          PA = input$initPA + input$initPAmin/60,
          QuietT = input$initQuietT + input$initQuietTmin/60,
          PassiveTrans = input$initPassiveTrans + input$initPassiveTransmin/60,
          School = input$initSchool + input$initSchoolmin/60,
          Domestic_SelfCare = input$initDomestic_SelfCare + input$initDomestic_SelfCaremin/60
        )
      
      
      # check no NA values present: turn into 0s if so
      if (any(is.na(init_df))) {
        print(init_df)
        init_df[, c(is.na(init_df))] <- 0
        print(init_df)
      }
      
      # check for 0 values as we need to add 65% of min to those values for ilrs to work
      lt0_df <- unlist(init_df) <= 0
      
      sum_chk <- rowSums(init_df)
      
      if (any(lt0_df)) {
        
        # smallest non-zero time is 65% of a minute
        quantum_of_time <- 0.65 / 60
        
        n_lt0 <- sum(lt0_df)
        n_gt0 <- sum(!lt0_df)
        # 65% of a minute, smallest allocation, give to the poor
        init_df[, lt0_df] <-  quantum_of_time
        # take from the rich
        init_df[, !lt0_df] <- init_df[, !lt0_df] - n_lt0 * quantum_of_time / n_gt0  
             
        # make sure still sums to 1 day after small tweaks to account for 0s   
        if (sum_chk != rowSums(init_df)) {
          stop("Re-allocaiton of small time units to negate 0s unsuccessful")
        }
        
      } 
      
      if (debug_mode) {
        print(init_df)
      }
      
      return(init_df)

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
        names(out) <- activity_nms
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
          "Currently there are time-use categories with time(s) not greater than 0 mins. \n",
          "This is because the re-allocation of time exceeds the corresponding ",
          "starting time in its time-use category. \n",
          "The listed time-use categories below provide the mintutes required ",
          "for there to be positive time in categories (minutes in brackets): \n",
          paste(
            paste0(
              comp_nms[comp_vls_lt0], 
              " (", 
              round(-rt_vec[comp_vls_lt0] * 60 + 1, 0), 
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
        lab = activity_nms,  # labels for histogram
        unit = comp.units  # composition units
      )
    })  # d3.data
    
    # Call d3 script to make histogram
    output$d3hist <- renderD3({
      r2d3(
        d3.data(),
        script = "javascript/script.js",  # and here we call the script,
        d3_version="5"
      )  # r2d3
    })  # renderD3
    
    
  
# Model Outcomes output
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Predicted outcomes from selected models on result-tab
 ##model for %Body fat  
  # Define predictions for initial composition
    
    
    
    
    
    # ---- fat_outc ----
    
    
    init_pred_fat <- reactive({
      
      init_ilrs <- ilr(init.comp())
      names(init_ilrs) <- paste0("ilr", 1:length(init_ilrs))
      
      x0_init <- make_x0(beta_ln_fat, init_ilrs, input$sex, input$age, input$sep, input$puberty) 

      out_pr <- get_pred_bounds(beta_ln_fat, x0_init, vcov_ln_fat, resdf_ln_fat, bound =  0, alpha = 0.05)
      out_lo <- get_pred_bounds(beta_ln_fat, x0_init, vcov_ln_fat, resdf_ln_fat, bound = -1, alpha = 0.05)
      out_hi <- get_pred_bounds(beta_ln_fat, x0_init, vcov_ln_fat, resdf_ln_fat, bound = +1, alpha = 0.05)
      
      out_init <- c(out_pr, out_lo, out_hi)
      out_init <- exp(out_init) # because log transformed outcome
      
      if (debug_mode) {
        print(out_init)
      }
      
      return(out_init)
      
    })
    
    
  # Define predictions for reallocation composition
    reall_pred_fat <- reactive({
      
      realloc_ilrs <- ilr(Rcomp())
      names(realloc_ilrs) <- paste0("ilr", 1:length(realloc_ilrs))
      
      x0_realloc <- make_x0(beta_ln_fat, realloc_ilrs, input$sex, input$age, input$sep, input$puberty) 
      
      out_pr <- get_pred_bounds(beta_ln_fat, x0_realloc, vcov_ln_fat, resdf_ln_fat, bound =  0, alpha = 0.05)
      out_lo <- get_pred_bounds(beta_ln_fat, x0_realloc, vcov_ln_fat, resdf_ln_fat, bound = -1, alpha = 0.05)
      out_hi <- get_pred_bounds(beta_ln_fat, x0_realloc, vcov_ln_fat, resdf_ln_fat, bound = +1, alpha = 0.05)
      
      out_realloc <- c(out_pr, out_lo, out_hi)
      out_realloc <- exp(out_realloc) # because log transformed outcome
    
      if (debug_mode) {
        print(out_realloc)
      }
      
      return(out_realloc)
      
    })

    
    # Define predictions for reallocation composition
    delta_pred_fat <- reactive({
      
      
      init_ilrs <- ilr(init.comp())
      names(init_ilrs) <- paste0("ilr", 1:length(init_ilrs))
      
      realloc_ilrs <- ilr(Rcomp())
      names(realloc_ilrs) <- paste0("ilr", 1:length(realloc_ilrs))
      
      x0_delta <- 
        make_x0(beta_ln_fat, realloc_ilrs, input$sex, input$age, input$sep, input$puberty) -
        make_x0(beta_ln_fat, init_ilrs, input$sex, input$age, input$sep, input$puberty) 
      
      out_pr <- get_pred_bounds(beta_ln_fat, x0_delta, vcov_ln_fat, resdf_ln_fat, bound =  0, alpha = 0.05)
      out_lo <- get_pred_bounds(beta_ln_fat, x0_delta, vcov_ln_fat, resdf_ln_fat, bound = -1, alpha = 0.05)
      out_hi <- get_pred_bounds(beta_ln_fat, x0_delta, vcov_ln_fat, resdf_ln_fat, bound = +1, alpha = 0.05)
      
      out_delta <- c(out_pr, out_lo, out_hi)
      exp_out_delta <- c(out_pr, out_pr-1, out_pr+1) # exp(out_delta) # because log transformed outcome
      
      if (debug_mode) {
        print(x0_delta)
        print(out_delta)
      }
      
      return(exp_out_delta)
    })
    
    perc_change_fat <- reactive({ 100 * delta_pred_fat()[1] })
    
    
    # ---- psy_outc ----
    
    
    init_pred_psy <- reactive({
      
      init_ilrs <- ilr(init.comp())
      names(init_ilrs) <- paste0("ilr", 1:length(init_ilrs))
      
      x0_init <- make_x0(beta_psy, init_ilrs, input$sex, input$age, input$sep, input$puberty) 
      
      out_pr <- get_pred_bounds(beta_psy, x0_init, vcov_psy, resdf_psy, bound =  0, alpha = 0.05)
      out_lo <- get_pred_bounds(beta_psy, x0_init, vcov_psy, resdf_psy, bound = -1, alpha = 0.05)
      out_hi <- get_pred_bounds(beta_psy, x0_init, vcov_psy, resdf_psy, bound = +1, alpha = 0.05)
      
      out_init <- c(out_pr, out_lo, out_hi)
      
      if (debug_mode) {
        print(out_init)
      }
      
      return(out_init)
      
    })
    
    
    # Define predictions for reallocation composition
    reall_pred_psy <- reactive({
      
      realloc_ilrs <- ilr(Rcomp())
      names(realloc_ilrs) <- paste0("ilr", 1:length(realloc_ilrs))
      
      x0_realloc <- make_x0(beta_psy, realloc_ilrs, input$sex, input$age, input$sep, input$puberty) 
      
      out_pr <- get_pred_bounds(beta_psy, x0_realloc, vcov_psy, resdf_psy, bound =  0, alpha = 0.05)
      out_lo <- get_pred_bounds(beta_psy, x0_realloc, vcov_psy, resdf_psy, bound = -1, alpha = 0.05)
      out_hi <- get_pred_bounds(beta_psy, x0_realloc, vcov_psy, resdf_psy, bound = +1, alpha = 0.05)
      
      out_realloc <- c(out_pr, out_lo, out_hi)
      
      if (debug_mode) {
        print(out_realloc)
      }
      
      return(out_realloc)
      
    })
    
    
    # Define predictions for reallocation composition
    delta_pred_psy <- reactive({
      
      
      init_ilrs <- ilr(init.comp())
      names(init_ilrs) <- paste0("ilr", 1:length(init_ilrs))
      
      realloc_ilrs <- ilr(Rcomp())
      names(realloc_ilrs) <- paste0("ilr", 1:length(realloc_ilrs))
      
      x0_delta <- 
        make_x0(beta_psy, realloc_ilrs, input$sex, input$age, input$sep, input$puberty) -
        make_x0(beta_psy, init_ilrs, input$sex, input$age, input$sep, input$puberty) 
      
      out_pr <- get_pred_bounds(beta_psy, x0_delta, vcov_psy, resdf_psy, bound =  0, alpha = 0.05)
      out_lo <- get_pred_bounds(beta_psy, x0_delta, vcov_psy, resdf_psy, bound = -1, alpha = 0.05)
      out_hi <- get_pred_bounds(beta_psy, x0_delta, vcov_psy, resdf_psy, bound = +1, alpha = 0.05)
      
      out_delta <- c(out_pr, out_lo, out_hi)
      
      if (debug_mode) {
        print(x0_delta)
        print(out_delta)
      }
      
      return(out_delta)
    })
    
    perc_change_psy <- reactive({ 100 * delta_pred_psy()[1] / init_pred_psy()[1] })
    
    
    # ---- aca_outc ----
    
    
    init_pred_aca <- reactive({
      
      init_ilrs <- ilr(init.comp())
      names(init_ilrs) <- paste0("ilr", 1:length(init_ilrs))
      
      x0_init <- make_x0(beta_aca, init_ilrs, input$sex, input$age, input$sep, input$puberty) 
      
      out_pr <- get_pred_bounds(beta_aca, x0_init, vcov_aca, resdf_aca, bound =  0, alpha = 0.05)
      out_lo <- get_pred_bounds(beta_aca, x0_init, vcov_aca, resdf_aca, bound = -1, alpha = 0.05)
      out_hi <- get_pred_bounds(beta_aca, x0_init, vcov_aca, resdf_aca, bound = +1, alpha = 0.05)
      
      out_init <- c(out_pr, out_lo, out_hi)
      
      if (debug_mode) {
        print(out_init)
      }
      
      return(out_init)
      
    })
    
    
    # Define predictions for reallocation composition
    reall_pred_aca <- reactive({
      
      realloc_ilrs <- ilr(Rcomp())
      names(realloc_ilrs) <- paste0("ilr", 1:length(realloc_ilrs))
      
      x0_realloc <- make_x0(beta_aca, realloc_ilrs, input$sex, input$age, input$sep, input$puberty) 
      
      out_pr <- get_pred_bounds(beta_aca, x0_realloc, vcov_aca, resdf_aca, bound =  0, alpha = 0.05)
      out_lo <- get_pred_bounds(beta_aca, x0_realloc, vcov_aca, resdf_aca, bound = -1, alpha = 0.05)
      out_hi <- get_pred_bounds(beta_aca, x0_realloc, vcov_aca, resdf_aca, bound = +1, alpha = 0.05)
      
      out_realloc <- c(out_pr, out_lo, out_hi)
      
      if (debug_mode) {
        print(out_realloc)
      }
      
      return(out_realloc)
      
    })
    
    
    # Define predictions for reallocation composition
    delta_pred_aca <- reactive({
      
      
      init_ilrs <- ilr(init.comp())
      names(init_ilrs) <- paste0("ilr", 1:length(init_ilrs))
      
      realloc_ilrs <- ilr(Rcomp())
      names(realloc_ilrs) <- paste0("ilr", 1:length(realloc_ilrs))
      
      x0_delta <- 
        make_x0(beta_aca, realloc_ilrs, input$sex, input$age, input$sep, input$puberty) -
        make_x0(beta_aca, init_ilrs, input$sex, input$age, input$sep, input$puberty) 
      
      out_pr <- get_pred_bounds(beta_aca, x0_delta, vcov_aca, resdf_aca, bound =  0, alpha = 0.05)
      out_lo <- get_pred_bounds(beta_aca, x0_delta, vcov_aca, resdf_aca, bound = -1, alpha = 0.05)
      out_hi <- get_pred_bounds(beta_aca, x0_delta, vcov_aca, resdf_aca, bound = +1, alpha = 0.05)
      
      out_delta <- c(out_pr, out_lo, out_hi)
      
      if (debug_mode) {
        print(x0_delta)
        print(out_delta)
      }
      
      return(out_delta)
    })
    
    perc_change_aca <- reactive({ 100 * delta_pred_aca()[1] / init_pred_aca()[1] })
    
    
  # Compute difference between initial and reallocated predictions
   
  # Reallocation results
    # output$specific.current <- renderText({
    #   pred_init <- init_pred_fat()
    #   sprintf("%3.1f%% [%3.1f:%3.1f]", pred_init[1], pred_init[2], pred_init[3])
    # })
    # 
    # output$specific.new <- renderText({
    #   pred_reall <- reall_pred_fat()
    #   sprintf("%3.1f%% [%3.1f:%3.1f]", pred_reall[1], pred_reall[2], pred_reall[3])
    # })
    # 
    # output$specific.diff <- renderText({
    #   pred_delta <- delta_pred_fat()
    #   sprintf("%3.1f%% [%3.1f:%3.1f]", pred_delta[1], pred_delta[2], pred_delta[3])
    # })
    
    # ---- predictions ----
    
    output$pred_plot_1 <- renderPlot({
      
      init_fat <- init_pred_fat()
      reall_fat <- reall_pred_fat()
      init_psy <- init_pred_psy()
      reall_psy <- reall_pred_psy()
      init_aca <- init_pred_aca()
      reall_aca <- reall_pred_aca()

      
      plot_dat <-
        tibble(
          outc = rep(c("(a) fat", "(b) psy", "(c) aca"), each = 2),
          pred_cat = rep("Predicitons", 6),
          pred_type = rep(c("(1) initial", "(2) re-allocation"), 3),
        ) %>%
        bind_cols(
          .,
          as.data.frame(rbind(
            init_fat, reall_fat, 
            init_psy, reall_psy, 
            init_aca, reall_aca
          ))
        )
      
      if (debug_mode) {
        print(plot_dat)
      }
      
      plot_dat %>% 
        ggplot(., aes(x = pred_type, y = V1, col = outc)) +
        geom_point(size = 3) +
        geom_errorbar(aes(ymin = V2, ymax = V3), width = 0.1, linetype = 2) +
        theme_bw() +
        facet_grid(outc ~ pred_cat, scales = "free") +
        labs(
          x = "Prediction type",
          y = "Predicted value",
          col = "Outcome",
          title = "Predictions ",
          subtitle = "(initial and re-allocations)"
        )
      
    })
    
    output$pred_plot_2 <- renderPlot({
      
      delta_fat <- delta_pred_fat()
      delta_psy <- delta_pred_psy()
      delta_aca <- delta_pred_aca()
      
      plot_dat <-
        tibble(
          outc = c("(a) fat", "(b) psy", "(c) aca"),
          pred_cat = rep("Predcited Difference", 3),
          pred_type = rep("Difference", 3),
        ) %>%
        bind_cols(
          .,
          as.data.frame(rbind(
            delta_fat, delta_psy, delta_aca
          ))
        )
      
      if (debug_mode) {
        print(plot_dat)
      }
      
      ylo <- min(0, min(plot_dat$V2))
      yhi <- max(0, max(plot_dat$V3))
      
      plot_dat %>% 
        ggplot(., aes(x = outc, y = V1, col = outc)) +
        geom_hline(yintercept = 0, alpha = 0.5) +
        geom_point(size = 3) +
        geom_errorbar(aes(ymin = V2, ymax = V3), width = 0.1, linetype = 2) +
        theme_bw() +
        ylim(ylo, yhi) +
        # facet_grid(outc ~ pred_cat, scales = "free") +
        labs(
          x = "Prediction type",
          y = "Predicted difference",
          col = "Outcome",
          title = "Predicted difference ",
          subtitle = "(re-allocation - initial)"
        )
      
    })
    
    observeEvent(input$sh_but, {
      print(input$sh_but)
      if (is.null(input$sh_but)) {
        shinyjs::hide(id = "plot1")
      } else if (input$sh_but %% 2 == 0) {
        shinyjs::hide(id = "plot1")
      } else {
        shinyjs::show(id = "plot1")
      }
      
    })
    
    output$ui1 <- renderUI({
      fat_val <- perc_change_fat()
      if (is.na(fat_val) | is.null(fat_val)) fat_val <- 0
      pm <- ifelse(fat_val > 0, "+", "")
      bx_col <- ifelse(fat_val > 0, "maroon", ifelse(fat_val < 0, "lime", "black")) 
      
      valueBox(
        value = paste0(pm, sprintf("%3.1f%%", fat_val)), 
        subtitle = "Body fat change", 
        width = 4, 
        color = bx_col
      )
    })
    
    output$ui2 <- renderUI({
      psy_val <- perc_change_psy()
      if (is.na(psy_val) | is.null(psy_val)) psy_val <- 0
      pm <- ifelse(psy_val > 0, "+", "")
      bx_col <- ifelse(psy_val > 0, "lime", ifelse(psy_val < 0, "maroon", "black")) 
      
      valueBox(
        value = paste0(pm, sprintf("%3.1f%%", psy_val)), 
        subtitle = "Psycological change", 
        width = 4, 
        color = bx_col
      )
    })
    
    output$ui3 <- renderUI({
      aca_val <- perc_change_aca()
      if (is.na(aca_val) | is.null(aca_val)) aca_val <- 0
      pm <- ifelse(aca_val > 0, "+", "")
      bx_col <- ifelse(aca_val > 0, "lime", ifelse(aca_val < 0, "maroon", "black")) 
      
      valueBox(
        value = paste0(pm, sprintf("%3.1f%%", aca_val)), 
        subtitle = "Academic change", 
        width = 4, 
        color = bx_col
      )
    })
    
    
    
    # valueBox(value = sprintf("%3.1f%%", 3), subtitle = "Box 1", width = 4, color = "lime"),
    # valueBox(value = sprintf("%3.1f%%", 3), subtitle = "Box 1", width = 4, color = "lime"),
    # valueBox(
    #   value = sprintf("%3.1f%%", perc_change_aca()), 
    #   subtitle = "Academic change", 
    #   width = 4, 
    #   color = ifelse(1 > 0, "lime", "maroon")
    # ),
    
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
  
