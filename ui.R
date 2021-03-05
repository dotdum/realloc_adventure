# ui.R script for Time Reallocation shinydashboard 
# The user-interface and widget input for the Shiny application is defined here
# Sends user-defined input to server.R, calls created output from server.R
# ------------------------------------------------------------------------------

# Header:
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  header <- dashboardHeader(
  	titleWidth = 250,
    title = "Compositional Isotemporal Substitution: Time-Reallocation Interface"
  )  #dashboardHeader
  
# Sidebar:
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Determines how many tabs exist in the body of the ui
  sidebar <- dashboardSidebar(
  	width = 250, #Width of sidebar the same as width of header
    
  # Sidebar options
    sidebarMenu(
  		menuItem("Current time-use composition", tabName = "participant-tab", 
  		  icon = icon("child")
  		),  # menuItem.participant-tab
  		menuItem("Specify reallocations", tabName = "time-tab", 
  		  icon = icon("time", lib = "glyphicon")
  		),  # menuItem.time-tab
  		br(),
  		div(style = "padding-left: 60px", actionButton("console", "Debug Console"))
    )  # sidebarMenu
    
  )  # dashboardSidebar
  
# Body: Main content of each tab, as determined by sidebar
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  body <- dashboardBody(
    tabItems(
      
  ### participant Information Tab
  		tabItem(tabName = "participant-tab",
  		  
  		# Covariate and Initial Composition Input
  		  fluidRow(
    			box(title = "General Information", width = 12, status = "primary",
    			  column(width = 4,
    			    radioButtons("sex", "Sex", 
                choices = list("Male" = 1, "Female" = 2), inline = TRUE
              )  # radioButtons.sex
    			  ), # column.sex
            column(width = 4,
              numericInput("age", "Age (years)", 
                value = 12
              )  # numericInput.age
            ), # column.age
    			  column(width = 4,
    			    numericInput("sep", "SES (z-score)",
                value = 0, min = -3, max = 3, step = 0.5
              )  # numericInput.sep
    			  ) , # column.sep
    			  column(width = 4,
    			         radioButtons("puberty", "Pubertal Stage", 
    			                      choices = list("Pre-pubertal" = 1, "Early Puberty" = 2,
    			                                     "Mid-pubertal" = 3, "Late Puberty" = 4, "Post-pubertal" = 5), 
    			                      selected=3, inline=TRUE
    			         )  # radioButtons.puberty
    			  )  # column.puberty
    			
    		  )  # box.participant-input
  		  ), # fluidRow.participant-input  
  		  
  		  fluidRow(
    			box(title = "Please Provide Current Time Allocations", width = 6, status = "primary",
    			  fluidRow(
    			    column(width = 8,
    			      numericInput("initSleep", "Sleep (hours)",
                  value = 11, step = 1
  			        )  # numericInput.initSleep.hours
    			    ),
    			    column(width = 4,
    			      numericInput("initSleepmin", "(mins)",
    			        value = 50, step = 1, min = 0, max = 60
    			      )  # numericInput.initSleep.minutes
    			    )
    			  ),  # fluidRow.initSleep
    			  
    			  fluidRow(
    			    column(width = 8,
    			      numericInput("initScreen", "Screen (hours)",
                  value = 2, step = 1
  			        )  # numericInput.initScreen
    			    ),
    			    column(width = 4,
    			      numericInput("initScreenmin", "(mins)",
                  value =55 , step = 1, min = 0, max = 60
  			        )  # numericInput.initScreen.minutes
    			    )
    			  ),  # fluidRow.initScreen
    			  
    			  fluidRow(
    			    column(width = 8,
      			    numericInput("initPA", "Physical Activity (hours)",
                  value = 1, step = 1
      			    ) # numericInput.initPA.hours
    			    ),
    			    column(width = 4,
    			      numericInput("initPAmin", "(mins)",
                  value = 57, step = 1, min = 0, max = 60
  			        )  # numericInput.initPA.minutes
    			    )
    			  ),  # fluidRow.initPA
    			  
    			  fluidRow(
    			    column(width = 8,
      			    numericInput("initQuietT", "Quiet Time (hours)",
                  value = 1, step = 1
      			    ) # numericInput.initQuietT
    			    ),
    			    column(width = 4,
    			      numericInput("initQuietTmin", "(mins)",
                  value = 11, step = 1, min = 0, max = 60
  			        )  # numericInput.initQuietT.minutes
    			    )
    			  ),  # fluidRow.initQuietT
    			  
    			  fluidRow(
    			    column(width = 8,
    			           numericInput("initPassiveTrans", "Passive Transport",
    			                        value = 0, step = 1
    			           )  # numericInput.initPassiveTrans
    			    ),
    			    column(width = 4,
    			           numericInput("initPassiveTransmin", "(mins)",
    			                        value = 35, step = 1, min = 0, max = 60
    			           )  # numericInput.initPassiveTrans.minutes
    			    )
    			  ),  # fluidRow.initSchool
    			  
    			  fluidRow(
    			    column(width = 8,
      			    numericInput("initSchool", "School-Related",
                  value = 2, step = 1
      			    )  # numericInput.initSchool
    			    ),
    			    column(width = 4,
    			      numericInput("initSchoolmin", "(mins)",
                  value = 9, step = 1, min = 0, max = 60
  			        )  # numericInput.initSchool.minutes
    			    )
    			  ),  # fluidRow.initSchool
    			  
    			  fluidRow(
    			    column(width = 8,
      			    numericInput("initDomestic_SelfCare", "Domestic & Self Care",
                  value = 3, step = 1
      			    )  # numericInput.initScreen
    			    ),
    			    column(width = 4,
    			      numericInput("initDomestic_SelfCaremin", "(mins)",
                  value = 23, step = 1, min = 0, max = 60
  			        )  # numericInput.initDomestic_SelfCare.minutes
    			    )
    			  ), # fluidRow.initDomestic_SelfCare
    			  div(textOutput("err1"), style = "color: red")
    			)# box.time-input
  		  ) 
  		), # participant-tab
      
  ### Time Re-Allocation Tab
  		tabItem(tabName = "time-tab",
  		  
  		# Time Allocation
  		  column(width = 6,
  		    box(title = "Provide Time-Reallocations", width = 12, 
            sliderInput("Sleep", "Sleep", 
              value = 0, min = -60, max = 60, ticks = FALSE
            ), # sliderInput.Sleep
  		      sliderInput("Screen", "Screen Time", 
  		        value = 0, min = -60, max = 60, ticks = FALSE
  		      ), # sliderInput.DomSoc
  		      sliderInput("PA", "Physical Activity", 
  		        value = 0, min = -60, max = 60, ticks = FALSE
  		      ), # sliderInput.PA
  		      sliderInput("QuietT", "Quiet Time", 
  		        value = 0, min = -60, max = 60, ticks = FALSE
  		      ), # sliderQuietT
  		      sliderInput("PassiveTrans", "Passive Transport", 
  		         value = 0, min = -60, max = 60, ticks = FALSE
  		      ), # sliderInput.PassiveTrans
  		      sliderInput("School", "School-Related", 
  		        value = 0, min = -60, max = 60, ticks = FALSE
  		      ), # sliderInput.School
  		      sliderInput("Domestic_SelfCare", "Domestic & SelfCare", 
  		        value = 0, min = -60, max = 60, ticks = FALSE
  		      ),  # sliderInput.Domestic_SelfCare
    			  div(textOutput("err2"), style = "color: red")
          ),# box.changetime,
  		    actionButton("reset_sliders", label = "Reset sliders")
  		  ), # column.left
  
  		# d3 Histogram
  		column(width = 6,
  		       box(width = 12, align = "center",
  		           d3Output("d3hist", width = "100%", height = "520px")
  		       ), # box.d3hist
  		       
  		 ##OUTCOMES      
  		# Estimated current
  		    box(title = "Initial Composition", width = 6, align = "left",
  		      valueBox(textOutput("specific.current"),"Estimated Body Fat% [95%CI]", width = 12)) , # box.outcome1
  		#Estimated new
  		      box(title = "New Composition", width = 6, align = "center",
  		          valueBox(textOutput("specific.new"), "Estimated Body Fat% [95%CI]", width = 12)),# box.outcome2
  		#Estimated difference between new and current %BF predictions
  		box(title = "", width = 12, align = "right",
  		    valueBox(textOutput("specific.diff"), "Difference", width = 12))# box.outcome3
  		  )  # column.right
  		)	# time-tab
    )  # tabItems
  )  # dashboardBody

# UI end
  dashboardPage(header, sidebar, body)
  