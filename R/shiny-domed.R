library(shiny)
library(DT)
# Much of this Shiny app has been developed by Claude AI
# I have added the 'domed()' function that it is based upon, and that has been 
# extracted from the rforSS3 library. In addition, I have given it a little 
# polish and added a few extras.

#' @title domeSS3 calculates domed selectivity curve, pattern24 for size in SS33
#' 
#' @description domeSS3 uses 6 parameters and a set of mean size or age classes 
#'     to calculate a domed selectivity curve with a maximum of 1.0 (rescaling 
#'     can be done outside the function), but has parameters for the selectivity 
#'     of the initial and final size/age classes. There is an ascending limb and 
#'     a descending limb with the potential of a plateau in between. The 
#'     description in the SS3 technical description, p 9-10 Methot and Wetzel
#'     (2013, supplementary material = Appendix A) is somewhat confusing, there
#'     is a peak2 but no peak1. the sequence of calculation needs to be peak2,
#'     J1 (join1), J2, t1min, t2min, asc, dsc, and then the domed selectivity.
#'     It is confusing because some of the parameters require highly modified 
#'     inverse logit transformation and some don't, so their values are not 
#'     intuitively obvious, and their descriptions in the User Manual are 
#'     misleading parameters 3 and 4 for example are not natural logs.
#'     The six parameters are:
#'     
#'     1. the age/size where selectivity first becomes 1.0, p[1], a positive
#'     value within the range of bin values
#'     
#'     2. a modified inverse logit of the width of the plateau, typically 
#'     -5 : 10, it is used to estimate the size/age where selectivity first 
#'     begins to decline = peak2
#'     
#'     3. the steepness of the ascending limb, a modified inverse logit, used 
#'     when calculating t1min and asc
#'     
#'     4. the steepness of the descending limb, a modified inverse logit, used 
#'     when calculating t2min and dsc
#'     
#'     5. the selectivity of the first age/size class, a modified inverse logit, 
#'     used when calculating asc
#'     
#'     6. the selectivity of the last age/size class,  a modified inverse logit, 
#'     used when calculating dsc 
#'     
#'     The descending limb of any dome shaped selectivity curves imply that the 
#'     fishing gear used is unable to collect all representatives of the larger 
#'     or older classes. The predicted numbers of smaller or younger animals, 
#'     that are only partially selected, are inflated because of the partial 
#'     selection. If any larger or older animals are, in fact, caught, then the 
#'     same inflation can happen to those animals as a result of the partial 
#'     selection implied by the dome shape. Small and young animals weigh 
#'     very little, the same cannot be said for the larger or older animals. 
#'     Some people refer to the extra biomass this phenomenon can imply as 
#'     'ghost biomass', even though it might be real. Whatever the case, when 
#'     using dome shaped selectivity it is best to be aware of this issue and 
#'     to be cautious about how this is interpreted. The 20* terms in the J1 
#'     and J2 factors are required to force the joins to be as effective as
#'     required (see Methot and Wetzel 2013). 
#'
#' @param p a vector of six parameters.
#' @param bins a vector of the mean of nL age/size classes
#'
#' @return a vector of selectivities
#' @export
#' 
#' @references Methot, R.D. and C.R, Wetzel (2013) Stock synthesis: A biological 
#'     and statistical framework for fish stock assessment and fishery management. 
#'     Supplementary material, Appendix A. Equs A1.30 - A1.34. 
#'     \emph{Fisheries Research} 142:86-99.
#'     
#'     Hurtado-Ferro, F., Punt, A.E., and K.T. Hill (2014) Use of multiple 
#'     selectivity patterns as a proxy for spatial structure. 
#'     \emph{Fisheries Research} 158:102-115.
#'
#' @examples
#' # from selex_length_example https://github.com/nmfs-ost/ss3-user-examples
#'   p <- c(45.8546,-3.18064,5.308,1.699,-999,0.75363)
#'   bins <- seq(11,99,2)
#'   sel <- domeSS3(p,bins)
#'   plot(bins,sel,type="l",xlab="Age",ylab="Selectivity",lwd=2)
domeSS3 <- function(p,bins) {
  nb <- length(bins)
  lw <- bins[2] - bins[1]
  peak2 <- p[1] + lw + (0.99 * bins[nb] - p[1] - lw)/(1+exp(-p[2]))
  J1 <- 1/(1 + exp(-20*(bins - p[1])/(1 + abs(bins - p[1]))))
  J2 <- 1/(1 + exp(-20*(bins - peak2)/(1 + abs(bins - peak2))))
  tlmin <- exp((-(bins[1] - p[1])^2)/exp(p[3]))
  t2min <- exp((-(bins[nb] - peak2)^2)/exp(p[4]))
  asc <- (1/(1 + exp(-p[5]))) + (1 - (1/(1 + exp(-p[5])))) * 
    ((exp((-(bins - p[1])^2)/exp(p[3])) - tlmin)/(1 - tlmin))
  dsc <- 1 + ((1/(1 + exp(-p[6]))) - 1) * 
    (exp((-(bins - peak2)^2)/exp(p[4])) - 1)/(t2min - 1)
  sel <- asc * (1 - J1) + J1*((1 - J2) + J2 * dsc)
  return(sel)
} # end of domess3


# Define UI-----------
ui <- fluidPage(
  titlePanel("Domed Selectivity Curve Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Function Parameters"), # function parameters------------------
      p("Six parameters p1 - p6 in SS33 Manual, p1 is nominal scale p2-p5 use an inverse logit."),
      p("Do read the relevent pages in Methot and  Wetzel (2013) and Methot et al (2025)."),
      
      numericInput("p1", 
                   "p1: peak - Age/size where selectivity = 1.0:", 
                   value = 45.8546,   # initial value
                   min = 0,           # range and step size 
                   max = 100, 
                   step = 0.1),
      
      numericInput("p2", 
                   "p2: top - inverse logit (InvLog) of plateau width:", 
                   value = -3.18065, 
                   min = -5, 
                   max = 15, 
                   step = 0.1),
      
      numericInput("p3", 
                   "p3: asc - InvLog of ascending limb steepness:", 
                   value = 5.30856, 
                   min = 0, 
                   max = 10, 
                   step = 0.1),
      
      numericInput("p4", 
                   "p4: dsc - InvLog of descending limb steepness, interaccts
                    with p6:", 
                   value = 1.69955, 
                   min = 0, 
                   max = 20, 
                   step = 0.1),
      
      numericInput("p5", 
                   "p5: initial - selectivity at first age/size bin (InvLog) 
                    ~ s = 1/(1 + exp(-p5))", 
                   value = -999, 
                   min = -1000, 
                   max = 5, 
                   step = 1),
      
      numericInput("p6", 
                   "p6: final - selectivity at first age/size bin (InvLog) 
                    ~ s = 1/(1 + exp(-p6))", 
                   value = 0.75363, 
                   min = -10, 
                   max = 10, 
                   step = 0.1),
      
      hr(),
      
      h4("Data Range"),  #  data for initial sizes---------
      
      numericInput("min_val", 
                   "Minimum Length/Age:", 
                   value = 11, 
                   min = 0, 
                   max = 80, 
                   step = 1),
      
      numericInput("max_val", 
                   "Maximum Length/Age:", 
                   value = 99, 
                   min = 10, 
                   max = 200, 
                   step = 1),
      
      numericInput("step_val", 
                   "Step size:", 
                   value = 2, 
                   min = 0.1, 
                   max = 5, 
                   step = 0.1),
      
      hr(),
      
      radioButtons("axis_label",    # axis label radio button-------------
                   "X-axis represents:", 
                   choices = list("Length" = "Length", "Age" = "Age"), 
                   selected = "Length"),
      
      hr(),
      
      downloadButton("download_data", "Download Data", class = "btn-primary"),
      
      br(), br(),
      
      h5("Parameter Summary:"),
      verbatimTextOutput("param_summary")
    ),
  
    mainPanel(            # main panel and tabs-----------------
      tabsetPanel(
        tabPanel("Plot",  #plot------
                 plotOutput("selectivity_plot", height = "500px"),
                 br(),
                 h4("Plot Interpretation:"),
                 p("• This domed selectivity curve shows the probability of selection across different lengths/ages. Values range from 0 to 1."),
                 p("• This is a differentiable double Normal selectivity function  with defined initial and final levels."),
                 p("• The example shown is from selex_length_example https://github.com/nmfs-ost/ss3-user-examples"),
                 p("• This double normal, patterns 24 (size) and 20 (age) is recommended in SS33 and uses 6 parameters p1 - p6."),
                 p("• The curve typically rises to a peak and then declines"),
                 p("• The p1 or peak and p2 or width parameters control where selectivity reaches maximum and begins declining"),
                 p("• The asc and dsc or p 3 and p4, control the steepness of the ascending and descending limbs"),
                 p("• Selmin (IL) and Selmax (IL) control selectivity at the smallest and largest sizes/ages (as natural log values)"),
                 p("• The parameter names relate to p1 - p6 in Methot et al (2024, p153)")
        ),
        
        tabPanel("Data Table", # data table--------------------
                 br(),
                 DT::dataTableOutput("data_table")
        ),
        
        tabPanel("Function Info",  # function info-------------------
                 br(),
                 h4("Domed Selectivity Function"),
                 p("A double Normal selectivity function with defined initial and final levels. Recommended in SS33."),
                 p("see supplementary material to Methot & Wetzel (2013), p8-9, especially equations A.1.30 - A.1.35"),
                 
                 h5("Parameters:"),
                 tags$ul(
                   tags$li("p1: peak - Age/size where selectivity first reaches 1.0 (maximum selectivity)"),
                   tags$li("p2: width - Age/size where selectivity first begins to decline from maximum"),
                   tags$li("p3: asc - Steepness of the ascending limb (higher = steeper ascent)"),
                   tags$li("p4: dsc - Steepness of the descending limb (higher = steeper descent)"),
                   tags$li("p5: initial selectivity of the first (smallest) age/size class, sel = 1/(1 + exp(-p5))"),
                   tags$li("p6: final selectivity of the last (largest) age/size class, sel = 1/(1 + exp(-p6))"),
                   tags$li("Found on page 148-149 of Methot et al (2025) these related to SS33 selectivity 
                           patterns 23 (size), 24 (size), 2 (legacy), and 20 (age)."),
                   tags$li("Read page 149 for special codes relating to SS33's implementation."),
                 ),
                 
                 h5("Typical Applications:"),
                 tags$ul(
                   tags$li("Fishing Gear Modelled selectivity - remember that it includes availability."),
                   tags$li("Stock assessment modelling"),
                   tags$li("Fisheries management options")
                 ),
                 
                 h5("References:"),  # references-------
                 tags$ul(
                   tags$li("Methot, R.D, and C.R Wetzel (2013) Stock synthesis: A biological and 
                            statistical framework for fish stock assessment and fishery management.
                           Fisheries Research 142:86-99. see the supplementary materials p8-9,
                           equations A.1.30 - A.1.35"),
                   tags$li("Methot, R.D., Wetzel, C.R., Taylor, I.G., Doering, K.L.,
                           Perl, E.F., and K.F Johnson (2025) Stock Synthesis User Manual Version
                           3.30.23.2, April 2025."),
                   tags$li("https://nmfs-ost.github.io/ss3-website/index.html, to access latest SS33 Manual and executable,"),
                   tags$li("https://github.com/nmfs-ost/ss3-user-examples, to access example SS33 files used here.")
                 ),
        ),
                 
        tabPanel("App Intent", # Shiny Intent--------------------
                 br(),
                 h4("Some Points Regarding its use"),
                 p("The parameter values for the domed selectivity option in SS33 are not all intuitively obvious as to what they imply."),
                 p("This app is designed to allow one to explore for plausible initial values."),
                 p("I already had an R function (Methot & Wetzel, 2013) and used Claude from Anthropic to generate a shiny app."),                
                 p("After much customization we had this app up and running. Claude was definitely useful for the HTML framework."),
                 p("Parameters p2 and p6 can be tricky to estimate because they can interact with others (see p230 in the Manual),"), 
                 p("This is especially the case if data on larger or older animals is sparse, as it tends to be!."),   
                 p("Nevertheless, these can be fixed and sensitivity tests can be made regarding what values influence the final results."),
                 br(),
                 p("If the user examines the code used in this shiny app you will see it follows a simple logical sequence."),
                 p("I have placed a function that plots the function I am interested in at the top, it is documented separately for clarity."),
                 p("One way I learn is to explore how others have written code to solve particular problems. This file is arranged so that ."),
                 p("others can relatively easliy modify it to suit their own needs for exploring how parameters affect the plot of functions."),
                 p("Feel free to take a copy and modfiy this code in any way that will be useful to you."),
                 br(),
                 p("If you have problems with the code, or the app fails, do please let me know so I can attempt to fix it."),
                 br(),
                 p("Malcolm Haddon, Hobart, 08/09/2025"),
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to calculate selectivity
  selectivity_data <- reactive({
    L <- seq(input$min_val, input$max_val, by = input$step_val)
    p <- c(input$p1, input$p2, input$p3, input$p4, input$p5, input$p6)
    
    # Calculate selectivity
    sel <- domeSS3(p, L)
    
    # Create data frame
    data.frame(
      Value = L,
      Selectivity = sel,
      stringsAsFactors = FALSE
    )
  })
  
  # Generate the plot
  output$selectivity_plot <- renderPlot({
    data <- selectivity_data()
    axis_name <- input$axis_label
    
    # Base R plotting
    plot(data$Value, data$Selectivity, 
         type = "b",  # both points and lines
         pch = 16,    # filled circles
         col = "blue",
         lwd = 2,     # line width
         cex = 0.8,   # point size
         xlab = axis_name,
         ylab = "Selectivity",
         main = "Domed Selectivity Curve",
         ylim = c(0, 1),
         las = 1)     # horizontal axis labels
    
    # Add grid
    grid(col = "lightgray", lty = "dotted")
    
    # Add reference lines
    abline(h = c(0.5, 1.0), lty = 2, col = "gray", lwd = 1)
    
    # Add subtitle with parameters
    mtext(paste("p1=", input$p1, ", p2=", input$p2, 
                ", p3=", input$p3, ", p4=", input$p4,
                ", p5=", input$p5, ", p6", input$p6), 
          side = 3, line = 0.5, cex = 1.0)
    
    # Add some diagnostic info
    mtext(paste("Min selectivity:", round(min(data$Selectivity), 4), 
                "| Max selectivity:", round(max(data$Selectivity), 4)), 
          side = 1, line = 4, cex = 0.7, col = "darkred")
  })
  
  # Generate data table
  output$data_table <- DT::renderDataTable({
    data <- selectivity_data()
    colnames(data) <- c(input$axis_label, "Selectivity")
    DT::datatable(data, 
                  options = list(pageLength = 100, scrollY = "500px"),
                  rownames = FALSE) %>%
      DT::formatRound("Selectivity", digits = 4)
  })
  
  # Parameter summary
  output$param_summary <- renderText({
    paste(
      "Peak1 =", input$p1, "\n",
      "Peak2 =", input$p2, "\n", 
      "Asc ln(width) =", input$p3, "\n",
      "Dsc ln(width) =", input$p4, "\n",
      "Selmin(ln) =", input$p5, "\n",
      "Selmax(ln) =", input$p6, "\n",
      "Range:", input$min_val, "to", input$max_val, "\n",
      "Step:", input$step_val
    )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("selectivity_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- selectivity_data()
      colnames(data) <- c(input$axis_label, "Selectivity")
      write.csv(data, file, row.names = FALSE)
    }
  )
}  # end of server loop

# Run the application
shinyApp(ui = ui, server = server)

