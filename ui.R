# ui.R script for Time Reallocation shinydashboard 
# The user-interface and widget input for the Shiny application is defined here
# Sends user-defined input to server.R, calls created output from server.R


# ---- Header ----

header <- dashboardHeader(
	titleWidth = 750,
  title = "Compositional Isotemporal Substitution: Time-Reallocation Interface"
)  #dashboardHeader
  
# ---- Sidebar ----

# Determines how many tabs exist in the body of the ui

sidebar <- dashboardSidebar(
  width = 175, #Width of sidebar the same as width of header
  useShinyjs(),
  # Sidebar options
  sidebarMenu(
    menuItem(
      "Initial time-use", 
      tabName = "participant-tab", 
      icon = icon("child")
    ),  # menuItem.participant-tab
    menuItem(
      "Specify reallocations", 
      tabName = "time-tab", 
      icon = icon("time", lib = "glyphicon")
    )  # menuItem.time-tab
  )  # sidebarMenu
)  # dashboardSidebar

  
# ---- Body ----
  
# Main content of each tab, as determined by sidebar

body <- dashboardBody(
  tabItems(

    ### participant Information Tab
    
    tabItem(
      tabName = "participant-tab",
    
      # Covariate and Initial Composition Input
      fluidRow(

        tags$head(tags$style(HTML("
        .form-control.shinyjs-resettable.shiny-bound-input {
          width: 80px;
        }
        .control-label {
          color: rgb(51,51,51);
        }
        "))), 
        # general input column (left)
        
        box(
          title = "General Information", width = 3, status = "primary",
          column(
            width = 12,
            radioButtons(
              "sex", "Sex", 
              choices = list("Male" = 1, "Female" = 2), inline = FALSE
            )  # radioButtons.sex
          ), # column.sex
          column(
            width = 12,
            numericInput(
              "age", "Age (years)", 
              value = 12
            )  # numericInput.age
          ) # column.age
    
        ),  # box.participant-input
    
      # time allocations input column (centre)
        
        box(
          title = "Please Provide Current Time Allocations", width = 6, status = "primary",
          
          fluidRow(
            column(
              width = inp_wdth,
              strong("Sleep", style = "color: rgb(51,51,51);"),
              numericInput(
                "initSleep", "(hours)", width = "100%",
                value = 11, step = 1, min = 0, max = 23
              ),  # numericInput.initSleep.hours
            ),
            column(
              width = inp_wdth,
              br(),
              numericInput(
                "initSleepmin", "(mins)", width = "100%",
                value = 50, step = 1, min = 0, max = 59
              )  # numericInput.initSleep.minutes
            )
          ),  # fluidRow.initSleep
          
          fluidRow(
            column(
              width = inp_wdth,
              strong("Screen", style = "color: rgb(51,51,51);"),
              numericInput(
                "initScreen", "(hours)", width = "100%",
                value = 2, step = 1, min = 0, max = 23
              )  # numericInput.initScreen
            ),
            column(
              width = inp_wdth,
              br(),
              numericInput(
                "initScreenmin", "(mins)", width = "100%",
                value =55 , step = 1, min = 0, max = 59
              )  # numericInput.initScreen.minutes
            )
          ),  # fluidRow.initScreen
          
          fluidRow(
            column(
              width = inp_wdth,
              strong("Physical Activity", style = "color: rgb(51,51,51);"),
              numericInput(
                "initPA", "(hours)", width = "100%",
                value = 1, step = 1, min = 0, max = 23
              ) # numericInput.initPA.hours
            ),
            column(
              width = inp_wdth,
              br(),
              numericInput(
                "initPAmin", "(mins)", width = "100%",
                value = 57, step = 1, min = 0, max = 59
              )  # numericInput.initPA.minutes
            )
          ),  # fluidRow.initPA
          
          fluidRow(
            column(
              width = inp_wdth,
              strong("Quiet Time", style = "color: rgb(51,51,51);"),
              numericInput(
                "initQuietT", "(hours)", width = "100%",
                value = 1, step = 1, min = 0, max = 23
              ) # numericInput.initQuietT
            ),
            column(
              width = inp_wdth,
              br(),
              numericInput(
                "initQuietTmin", "(mins)", width = "100%",
                value = 11, step = 1, min = 0, max = 59
              )  # numericInput.initQuietT.minutes
            )
          ),  # fluidRow.initQuietT
          
          fluidRow(
            column(
              width = inp_wdth,
              strong("Passive Transport", style = "color: rgb(51,51,51);"),
              numericInput(
                "initPassiveTrans", "(hours)", width = "100%",
                value = 0, step = 1, min = 0, max = 23
              )  # numericInput.initPassiveTrans
            ),
            column(
              width = inp_wdth,
              br(),
              numericInput(
                "initPassiveTransmin", "(mins)", width = "100%",
                value = 35, step = 1, min = 0, max = 59
              )  # numericInput.initPassiveTrans.minutes
            )
          ),  # fluidRow.initSchool
          
          fluidRow(
            column(
              width = inp_wdth,
              strong("School-Related", style = "color: rgb(51,51,51);"),
              numericInput(
                "initSchool", "(hours)", width = "100%",
                value = 2, step = 1, min = 0, max = 23
              )  # numericInput.initSchool
            ),
            column(
              width = inp_wdth,
              br(),
              numericInput(
                "initSchoolmin", "(mins)", width = "100%",
                value = 9, step = 1, min = 0, max = 59
              )  # numericInput.initSchool.minutes
            )
          ),  # fluidRow.initSchool
          
          fluidRow(
            column(
              width = inp_wdth, 
              strong("Domestic & Self Care", style = "color: rgb(51,51,51);"),
              numericInput(
                "initDomestic_SelfCare", "(hours)", width = "100%",
                value = 3, step = 1, min = 0, max = 23
              )  # numericInput.initScreen
            ),
            column(
              width = inp_wdth,
              br(),
              numericInput(
                "initDomestic_SelfCaremin", "(mins)", width = "100%",
                value = 23, step = 1, min = 0, max = 59
              )  # numericInput.initDomestic_SelfCare.minutes
            ),
          ), # fluidRow.initDomestic_SelfCare
          
          # error msg if not == 24 hours
          div(textOutput("err1"), style = "color: red")
        ), # box.time-input
      
        # advanced input column (right)
      
        box(
          title = "Advanced Information", width = 3, status = "primary",
          
          column(
            width = 12,
            radioButtons(
              "puberty", "Pubertal Stage", 
              choices = list("Pre-pubertal" = 1, "Early Puberty" = 2,
              "Mid-pubertal" = 3, "Late Puberty" = 4, "Post-pubertal" = 5), 
              selected=3, inline=FALSE
            )  # radioButtons.puberty
          ),  # column.puberty column(width = 6,
          
          column(
            width = 12,
            numericInput(
              "sep", "SES (z-score)",
              value = 0, min = -3, max = 3, step = 0.5
            )  # numericInput.sep
          )  # column.sep
        ) # box advanced input
      ) # fluidRow
    ), # participant-tab
  
  ### Time Re-Allocation Tab
    tabItem(
      tabName = "time-tab",
    
      # Time re-allocation sliders
      
      column(width = 6,
        box(
          title = "Provide Time-Reallocations", width = 12, 
          sliderInput(
            "Sleep", "Sleep", 
            value = 0, min = -60, max = 60, ticks = FALSE
          ), # sliderInput.Sleep
          sliderInput(
            "Screen", "Screen Time", 
            value = 0, min = -60, max = 60, ticks = FALSE
          ), # sliderInput.DomSoc
          sliderInput(
            "PA", "Physical Activity", 
            value = 0, min = -60, max = 60, ticks = FALSE
          ), # sliderInput.PA
          sliderInput(
            "QuietT", "Quiet Time", 
            value = 0, min = -60, max = 60, ticks = FALSE
          ), # sliderQuietT
          sliderInput(
            "PassiveTrans", "Passive Transport", 
            value = 0, min = -60, max = 60, ticks = FALSE
          ), # sliderInput.PassiveTrans
          sliderInput(
            "School", "School-Related", 
            value = 0, min = -60, max = 60, ticks = FALSE
          ), # sliderInput.School
          sliderInput(
            "Domestic_SelfCare", "Domestic & SelfCare", 
            value = 0, min = -60, max = 60, ticks = FALSE
          ),  # sliderInput.Domestic_SelfCare
          div(textOutput("err2"), style = "color: red"),  
          div(textOutput("err3"), style = "color: orange"),
          actionButton("reset_sliders", label = "Reset sliders")
        ),# box.changetime,
        
        box(
          width = 12, align = "center",
          title = "New daily time composition",
          plotlyOutput("time_use_plot_1"),
        ) # box.treemap of time-compositions
        
      ), # column.left  		       
      
      # OUTCOMES    
      
      column(
        width = 6, align = "center",
        
        box(
          id = "colour_out", 
          width = 12,
          uiOutput("ui1"),
          uiOutput("ui2"),
          uiOutput("ui3")
        ),
        
        box(
          id = "showhide", 
          width = 12,
          actionButton(inputId = "sh_but", label = "show / hide advanced output"),
        ),
        
        box(id = "plot1", 
          width = 12,
          plotOutput("pred_plot_1"),
          plotOutput("pred_plot_2")
        ),
        
        br(),
        br(),
        p(disclaim_str, style = "font-size: 9pt;")
        
      ) # column.right
    )	# time-tab
  )  # tabItems
)  # dashboardBody

#  ---- UI_end ----

dashboardPage(header, sidebar, body)
  
