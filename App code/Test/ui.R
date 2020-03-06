
#user: jose...@hotmail.com
#ps: LifespanVariation
#user: DemoGraphs

library(shiny)
library(data.table)
library(DT)
library(plotly)

  # Sexx <- c('Male','Female')
  causes <- c("Infectious","Neoplasms","Circulatory","Mental","Endocrine","Digestive","Respiratory","External","Rest" )
  
  shinyUI(
    fluidPage(
      titlePanel('Saudi Arabia Health Profile'),
      img(src='Logo.png', align = "right"),
      
       navbarPage(
         'Canudas-Romo V & Aburto JM. "Saudi Arabia Health Profile". World Bank. 2018',
         position = c("fixed-bottom"),fluid = T),
      tags$style(type = 'text/css', '.navbar { background-color: white}',
                 '.navbar-default .navbar-brand {
                             color: grey;
                             font-size: 13px}'),
      
      
        tabsetPanel(
          tabPanel("Distribution of causes of death",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput( 'year.ind','Year',1999:2012, selected = 1999),
                       br(),
                       p('Select the year to show the cause-of-death distribution by age group. Source: Institute for Health Metrics and Evaluation (IHME). IHME GBD Saudi Arabia Cause-Specific Mortality Estimates.'),
                       br(),
                       width = 2
                     ),
                     mainPanel(
                         tabPanel('Proportions by cause of death', plotlyOutput('COD.Distribution',inline = F,width = '100%'))
                        )))
          ,
          tabPanel("Life years lost",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput( 'period.ind2','Years',c("2000-2005","2005-2010"), selected = "2000-2005"),
                       br(),
                       p('Age and cause decomposition of life years lost by sex. Source: Own caculations with UN life tables and GBD disease cause-of-death distribution'),
                       br(),
                       width = 2
                     ),
                     mainPanel(
                       tabsetPanel(
                         tabPanel('Age and cause-of-death analysis', imageOutput('YLL.plot',click = "plot_click",  # Equiv, to click=clickOpts(id="plot_click")
                                                                                hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                                                                                brush = brushOpts(id = "plot_brush"), width = '100%')),
                         tabPanel('Total contributions by cause of death to the change in LYL (Table)',
                                  dataTableOutput('YLL.table.males'),dataTableOutput('YLL.table.females'),hr(),hr(),hr())
                         ,
                         tabPanel('Total contributions by cause of death to the change in LYL (Figure)',plotlyOutput('YLL.plot2',height = 490,width = '100%'))
                         
                       )))),
          tabPanel("Burden of diseases over time",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput( 'period.ind1','Years',c("2000-2005","2005-2010"), selected = "2000-2005"),
                       br(),
                       p('Age and cause decomposition of life expectancy over time by sex. Source: Own caculations with UN lifetbales and GBD disease cause-of-death distribution'),
                       br(),
                       width = 2
                     ),
                     mainPanel(
                       tabsetPanel(
                         tabPanel('Decomposition of life expectancy over time', plotlyOutput('Decompostion.plot',width = '100%'),br(br()),dataTableOutput('DT.summary'))
                         ,
                         tabPanel('Total contributions by cause of death (Table)',dataTableOutput('Decompostion.table.males'),dataTableOutput('Decompostion.table.females'),hr(),hr(),hr())
                         ,
                         tabPanel('Total contributions by cause of death (Figure)',plotlyOutput('Decompostion.plot2',height = 490))
                       )))),
          tabPanel("Change on life expectancy scenarios",
                   sidebarLayout(
                     sidebarPanel(
                       h4('Reduction in cause of death (%)'),
                       sliderInput("cause_ind1", "Tuberculosis and HIV",
                                   min = 0, max = 100,value = 0, step = 5),
                       sliderInput("cause_ind2", "Infectious",
                                   min = 0, max = 100,value = 0, 0, step = 5),
                       sliderInput("cause_ind3", "Neoplasms",
                                   min = 0, max = 100,value = 0, 0, step = 5),
                       sliderInput("cause_ind4", "Cardiovascular diseases",
                                   min = 0, max = 100,value = 10, 0, step = 5),
                       sliderInput("cause_ind5", "Urogenital, blood and endocrine",
                                   min = 0, max = 100,value = 5, 0, step = 5),
                       sliderInput("cause_ind6", "Diabetes",
                                   min = 0, max = 100,value = 5, 0, step = 5),
                       sliderInput("cause_ind7", "Transport injuries",
                                   min = 0, max = 100,value = 0, 0, step = 5),
                       sliderInput("cause_ind8", "Unintentional injuries",
                                   min = 0, max = 100,value = 0, 0, step = 5),
                       sliderInput("cause_ind9", "Self-harm and interpersonal violence",
                                   min = 0, max = 100,value = 0, 0, step = 5),
                       br(),
                       p('Select the reduction'),
                       br(),
                       width = 2
                     ),
                     mainPanel(
                       tabsetPanel(
                         tabPanel('Hypothetical life expectancy', plotlyOutput('CauseDeleted',height = 600,width = 900))
                         
                       )))),
          
          tabPanel("Documentation",
                   tabsetPanel(
                     tabPanel('User manual'),
                     tabPanel('Data sources and methodology'),
                     tabPanel('Mini BOD narrative')
                   )
                   )
         
         
         )
      )
    )
  
  
#  devtools::install_github('hadley/ggplot2')


  