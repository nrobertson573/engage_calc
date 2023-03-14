library(shiny)
library(ggplot2)
library(DT)

source("functions_engage_calc.txt", local = TRUE)
char_base = read.table("char_base.txt", sep="\t", header=TRUE, strip.white=TRUE)
char_growth = read.table("char_growth.txt", sep="\t", header=TRUE, strip.white=TRUE)
char_max = read.table("char_max.txt", sep="\t", header=TRUE, strip.white=TRUE)
char_max[is.na(char_max)] = 0
class_base = read.table("class_base.txt", sep="\t", header=TRUE, strip.white=TRUE)
class_growth = read.table("class_growth.txt", sep="\t", header=TRUE, strip.white=TRUE)
class_growth[is.na(class_growth)] = 0
class_max = read.table("class_max.txt", sep="\t", header=TRUE, strip.white=TRUE)
att_names = c("hp","str","mag","dex","spd","def","res","lck","bld")




##aside from N, do I need any of these to run the sim?


##for now I'll hardcode these, but they really hsould be togglable later
# att_CC = NULL
# comp_bases = NULL
# att_comps = NULL

ui <- fluidPage(
  titlePanel("Fire Emblem Engage Attribute Distribution Calculator"),
  
  # uiOutput("mygithub"),
  # h6("Updated: 2023/2/20", style="color:black"),
  # headerPanel(""),

  tabsetPanel(
    tabPanel("Sim Paramaters",  
      fluidRow(
        column(6,
          selectizeInput(inputId = "char_oi", label = "Select a character", multiple=FALSE, choices = char_base$Name),
          "Select up to 5 ordered classes and the level you class change into them",
          
          fluidRow(
            column(3,
              selectizeInput(inputId = "class_1", label = "Class 1", multiple=FALSE, choices = c("none",class_base$Name)),
            ),
            column(3,
              numericInput("cclvl_1","Level 1",value=10,min=1,max=40,step=1),
            ),
          ),
          fluidRow(
            column(3,
                   selectizeInput(inputId = "class_2", label = "Class 2", multiple=FALSE, choices = c("none",class_base$Name)),
            ),
            column(3,
                   numericInput("cclvl_2","Level 2",value=10,min=1,max=40,step=1),
            ),
          ),
          fluidRow(
            column(3,
                   selectizeInput(inputId = "class_3", label = "Class 3", multiple=FALSE, choices = c("none",class_base$Name)),
            ),
            column(3,
                   numericInput("cclvl_3","Level 3",value=10,min=1,max=40,step=1),
            ),
          ),
          fluidRow(
            column(3,
                   selectizeInput(inputId = "class_4", label = "Class 4", multiple=FALSE, choices = c("none",class_base$Name)),
            ),
            column(3,
                   numericInput("cclvl_4","Level 4",value=10,min=1,max=40,step=1),
            ),
          ),
          fluidRow(
            column(3,
                   selectizeInput(inputId = "class_5", label = "Class 5", multiple=FALSE, choices = c("none",class_base$Name)),
            ),
            column(3,
                   numericInput("cclvl_5","Level 5",value=10,min=1,max=40,step=1),
            ),
          ),
          
          numericInput("flvl","Final Level",value=10,min=1,max=40,step=1),


        ),
        
        column(3,
          "Current Class Path",
          tableOutput("class_path_table"),
          # tableOutput("class_path_table_old"),
        ),
        
        column(3,
          numericInput("Nsims","Number of Monte Carlo Simulations", value = 1000, min = 5, max = 10000),
          actionButton("sim_start","Simulate"),
          textOutput("simrun_TF"), 
          conditionalPanel(
            condition = "input.sim_start > 0",
            "Class Path for Simulation in Memory",
            tableOutput("sim_in_mem_path"),
          ),
        ),
      ),
    ),
    
    ############################################################################
    tabPanel("res",

      selectizeInput(inputId = "class_oi", label = "Class of Interest", multiple=FALSE, choices = class_base$Name),
      actionButton("plotgo","Generate Plots"),
      headerPanel(""),
      column(2,
        tabsetPanel(
          tabPanel("graphstuff",
            checkboxInput("mean_TF", "Include Mean Line?", TRUE),
            checkboxInput("med_TF", "Include Median Line?", TRUE),
            checkboxInput("cap_TF", "Show Class Caps?", FALSE),
            checkboxInput("trunc_val_TF", "Truncate Distribution at Class Cap?", FALSE),
            checkboxInput("custom_q_TF", "Include Custom Quantile Line?", FALSE),
            conditionalPanel(
              condition = "input.custom_q_TF > 0",
              numericInput("custom_q", "Quantile of Interest", value = .05, min = .01, max = 1),
            ),
          ),
          tabPanel("comp stuff",
                   ##want to put this stuff in a conditional panel, so ask whether you want to make comps
                   ##if yes, then pull up the options
                   ##if no, then set values to NULL
            checkboxInput("make_comp", "Overlay Specified Values on Distributions?", FALSE),
            conditionalPanel(
              condition = "input.make_comp > 0",
              "Enter Class and Attributes to make the Comparison",
              # att_CC = NULL
              ##class of the comparison stats
              selectizeInput("problemchild", label = "Class", choices = class_base$Name),            
              
              # att_comps = NULL
              ##will need 9 of these, (one for each attribute), then will need to make the reactive object combining them later
              numericInput("att_cc_hp","HP", value=30, min = 0, max = 99),
              numericInput("att_cc_str","STR", value=10, min = 0, max = 99),
              numericInput("att_cc_mag","MAG", value=10, min = 0, max = 99),
              numericInput("att_cc_dex","DEX", value=10, min = 0, max = 99),
              numericInput("att_cc_spd","SPD", value=10, min = 0, max = 99),
              numericInput("att_cc_def","DEF", value=10, min = 0, max = 99),
              numericInput("att_cc_res","RES", value=10, min = 0, max = 99),
              numericInput("att_cc_lck","LCK", value=10, min = 0, max = 99),
              numericInput("att_cc_bld","BLD", value=10, min = 0, max = 99),

              ##maybe be silly and add a table of percentile estimates?
              actionButton("compute_percentiles", "Compute Percentiles"),
              tableOutput("att_comps_percentiles_table"),
            ),
# textOutput("comp_bases_text"),
# textOutput("att_comp_text"),
          ),
        ),
      ),
      
      column(10,
        plotOutput("graphs"),
      ),
    ),
    
    ############################################################################
    tabPanel("Numeric Outcomes",
             "I'll put something here later",
      ##ok so what do I want here?
      ##somehting where I can input an attribute and a value for which it will tell me the probability of having at least that much
      ##do I also want to plot the distribution/show cumulative distributions here?
      ##do I want to allow for probabilities of multiple thresholds? -> idea being survive a hit/double/hit hard enough?
    ),
    
    ############################################################################
    tabPanel("Raw Simulation Output",
      "Note: The following results include the class bases",
      DTOutput("testdt"),
    ),
  ),

)


server <- function(input, output, session) {

  ##generate link to readme
  url = a("https://github.com/nrobertson573/engage_calc", 
          href="https://github.com/nrobertson573/engage_calc#readme:")
  output$mygithub = renderUI({
    tagList("Github Readme link:", url)     
  })

  
  class_path = reactive({
    icv = c(input$class_1,input$class_2,input$class_3,input$class_4,input$class_5)
    icv[icv != "none"]
  })

  level_class_change = reactive({
    c(input$cclvl_1,input$cclvl_2,input$cclvl_3,input$cclvl_4,input$cclvl_5)[seq_len(length(class_path()))]
  })
  


################################################################################

  class_path_df = reactive({
    data.frame(
      "class" = c(char_base$Class[char_base$Name == input$char_oi],class_path()),
      "lvl_start" = as.integer(c(char_base$Level[char_base$Name == input$char_oi], 
                        rep(1, length(level_class_change()) ))),
      "lvl_end" = c(level_class_change(), input$flvl)
    )
  })
  
  # class_path_df_old = reactive({
  #   data.frame(
  #     "class" = c(char_base$Class[char_base$Name == input$char_oi],class_path()),
  #     "level" = c(char_base$Level[char_base$Name == input$char_oi],level_class_change())
  #   )
  # })
  
  output$class_path_table = renderTable({
    class_path_df()
  })
  # output$class_path_table_old = renderTable({
  #   class_path_df_new()
  # })

  simrun_r = reactive({
    length(tst()) == input$Nsims
  })
  output$simrun_TF = renderText({
    paste("Simulation Run?", simrun_r())
  })
  
  
  observeEvent(input$sim_start, {
    output$sim_in_mem_path = renderTable({
      isolate(class_path_df())
    })
  })

  
  ##maybe I want an observer of some kind to change a reactive value between ready/not ready/completed

  
################################################################################
  
  
  tst = eventReactive(input$sim_start,{
    showModal(modalDialog("Simulating..."))
    srobj = lapply(1:input$Nsims, function(i){
      single_sim(
        input$char_oi,class_path(),level_class_change(),input$flvl
      )
    })
    removeModal()
    srobj
  })
  
  observeEvent(input$sim_start, {
    updateSelectizeInput(session, inputId = "class_oi", choices = class_base$Name, selected = tail(class_path_df())[,1])
  })
    
  finalres = reactive({
    do.call(rbind,lapply(tst(),function(st){ tail(st,1) }))
  })

  coi_bases = reactive({
    class_base[class_base$Name == input$class_oi,-c(1,11)]
  })

  coi_caps = reactive({
    unlist(class_max[class_max$Name == input$class_oi,-1] +
                        c(0,char_max[char_max$Name == input$char_oi,-1],0) )
  })



#################################################################################################
#################################################################################################
#################################################################################################
  ggdfr = reactive({
    ##before putting it into a data.frame for ggplot, lets also adjust for class of interest bases
    ggdf = as.data.frame(matrix(mapply('+', finalres(), matrix(rep(coi_bases(),input$Nsims),ncol=9,byrow=TRUE)),ncol=9))
    #ggdf = as.data.frame(do.call(rbind,lapply(finalres(),function(frl){ frl + coi_bases()})))
    rownames(ggdf) = NULL
    colnames(ggdf) = att_names
    return(ggdf)
  })
  
output$testdt = renderDT({
  datatable(ggdfr(),rownames=NULL)
})

##start adding in the new input params?
  quants = reactive({
    if(input$med_TF == TRUE | input$custom_q_TF == TRUE){
      if(input$med_TF == TRUE){
        med = .5
      }else{
        med = NULL
      }
      if(input$custom_q_TF == TRUE){
        otherq = input$custom_q
      }else{
        otherq = NULL
      }
      quants_out = c(med,otherq)
    }else{
      quants_out = NULL
    }
    quants_out
  })

##start adding in comparison parameters
  comp_bases = reactive({
    if(input$make_comp == TRUE){
      rout = class_base[class_base$Name == input$problemchild, -c(1,11)]
    }else{
      rout = rep("danger", 9)
    }
    rout
  })
# output$comp_bases_text = renderText({
#   paste(comp_bases())
# })
##the issue seems to be something with this comp_bases -> renaming worked
   
  att_comps = reactive({
    if(input$make_comp == TRUE){
      rout = c(input$att_cc_hp,input$att_cc_str,input$att_cc_mag,input$att_cc_dex,input$att_cc_spd,input$att_cc_def,input$att_cc_res,input$att_cc_lck,input$att_cc_bld)
    }else{
      rout = rep(NULL,9)
    }
    rout
  })
# output$att_comp_text = renderText({
#   paste(att_comps())
# })
  
##now for percentiles
  att_comps_percentiles = reactive({
    ##caution, ac_vec is giving the values user input in term of comp_class
    ##so need to convert them to same scale as class of interest
    ac_vec = att_comps() - comp_bases() + coi_bases()
    perc = mapply( function(acv,simout){ mean(simout <= acv) }, 
                   acv = as.list(ac_vec), simout = as.list(ggdfr())
    )
    rout = data.frame(att_names, "percentiles" = perc)
    rownames(rout) = NULL
    colnames(rout) = c("Attribute", "Percentile")
    rout
  })
  
  observeEvent(input$compute_percentiles, {
    output$att_comps_percentiles_table = renderTable({
      isolate(att_comps_percentiles())
    })
  })  

  
    
  ggfeed = reactive({
    lapply(1:9, function(i){
      list(att = ggdfr()[,i], quant_set = quants(), mean_tf = input$mean_TF, cap_tf = input$cap_TF,
           coi_cap = coi_caps()[i], coi_base =coi_bases()[i], pers_val_tf = !input$trunc_val_TF, att_name = att_names[i],
           att_comp = att_comps()[i], comp_base = comp_bases()[i]
      )
    })
  })

  ggplotlist = reactive({
    lapply(ggfeed(),function(gg){
      att_ggplot(
        att = gg$att,
        quant_set = gg$quant_set,
        mean_tf = gg$mean_tf,
        cap_tf = gg$cap_tf,
        coi_cap = gg$coi_cap,
        coi_base = gg$coi_base,
        pers_val_tf = gg$pers_val_tf,
        att_name = gg$att_name,
        att_comp = gg$att_comp,
        comp_base = gg$comp_base
      )
    })
  })
  
observeEvent(input$plotgo,{
  output$graphs = renderPlot({
    isolate(gridExtra::grid.arrange(grobs=ggplotlist(),nrow = 3))
  }, res = 144, height = function() .8 * session$clientData$output_graphs_width)  # 
})
  # ##standard errors, quite small at N = 1000, kinda large at N = 100 -> 
  # apply(ggdf,2,sd)/sqrt(nrow(ggdf)-1)
  # 

  
}
##shiny::devmode(TRUE)
shinyApp(ui, server)
##in case I get hit by the app not loading in Rstudio pane, this line might work
##shinyApp(ui = ui, server = server,options=c(launch.browser = .rs.invokeShinyPaneViewer))