library(shiny)
library(data.table)
library(DT)
library(plotly)

  causes <- c("Infectious","Neoplasms","Circulatory","Mental","Endocrine","Digestive","Respiratory","External","Others" )
  
  shinyUI(
    fluidPage(
      titlePanel('The Saudi Arabian Health Profile'),

      tags$style(type = 'text/css', '.navbar { background-color: white}',
                 '.navbar-default .navbar-brand {
                             color: grey;
                             font-size: 13px}'),
      
        tabsetPanel(
          tabPanel("Overview",
                     mainPanel(
                       br(),
                       div(img(src='Logo3.png', align = "center"),style = 'text-align: center;'),
                       br(),
                       div(h3('The Saudi Arabian Health Profile interactive tool presents four different visualizations to show the past, 
                              present and future of causes of death and their impact on the Kingdom of Saudi Arabia\'s life expectancy.'),
                           style = "text-align:justify;"),
                       br(),
                       div(h3('Saudi Health Council correspondents'),
                           style = "text-align:justify;"),
                       div(h4('Nahar Alazemi, Taghred Alghaith Mohammed Alluhidan, 
                              Ayman Alhozenan, Adwa Alamri, Khalid Al-Moteiry, 
                              Nouf Alghamdi, Quds Alsafffer, Rimah Almohammed, 
                              Manal Alfarih, Mohammed AlTamimi, Abdulrahman Alshammari, 
                              Malak Alateeq, Maher Samkari, Nasser Alyahya and 
                              Zubaidah Almajed '),
                           style = "text-align:justify;"),
                       br(),
                       div(h3('Design and Development'),
                           style = "text-align:justify;"),
                       div(h4(a("Vladimir Canudas-Romo", href="https://researchers.anu.edu.au/researchers/canudas-romo-v"), 
                              'and',a("José Manuel Aburto", href="https://github.com/jmaburto")),
                           style = "text-align:justify;"),
                       br(),
                       br(),
                       br()
                       ))
          ,
          tabPanel("Distribution of causes of death",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput( 'year.ind','Year',c('2000-2005','2005-2010','2010-2015'), selected = '2000-2005'),
                       br(),
                       p('Select the year to show the cause-of-death distribution by age group. Source: Institute for Health Metrics and Evaluation (IHME). IHME GBD Saudi Arabia Cause-Specific Mortality Estimates.'),
                       br(),
                       p('Note: Data not available by cause of death under age 1.'),
                       width = 2
                     ),
                     mainPanel(
                         tabPanel('Proportions by cause of death', 
                                    verticalLayout(plotlyOutput('COD.Distribution',inline = F),
                                    br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                    p('This plot shows the proportion of causes of death by 
                                    age for the population of Saudi Arabia for a given period: 
                                    female, male and total. For example, for the total population in the years 2000-2005 in 
                                    the age-group 30-34 the main cause of death is transport injuries 
                                    representing 28% of the deaths, while at age 60-64 cardiovascular 
                                    mortality corresponds to 43% of all deaths in the age-group.'),
                                    br(),
                                   p('Note: the causes of death of “Others” includes causes that 
                                           represented small percentages above age 1 in the overall distribution 
                                           of death counts; causes included in this category were: 
                                           maternal, disease of the digestive system, 
                                           mental disorders, symptoms, signs and abnormal clinical 
                                           and laboratory findings not elsewhere classified, among 
                                           others.')
                                    ),fluid= F
                                  
                                  )
                        )))
          ,
          tabPanel("Life years lost",
                   
                     sidebarPanel(
                       p('Select a period to show LYL for that specific period.'),
                       selectInput( 'LYL.Ind','Period for LYL',c("2000-2005","2005-2010", "2010-2015"), selected = "2000-2005"),
                       br(),
                       width = 2
                     ),
                     mainPanel(
                       tabsetPanel(
                         tabPanel('Age and cause-of-death analysis',
                                  # imageOutput('YLL.plot',click = "plot_click",  # Equiv, to click=clickOpts(id="plot_click")
                                  #                                               hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                                  #                                               brush = brushOpts(id = "plot_brush"), width = '100%')
                                  plotlyOutput('YLL.plot',width = '100%'),br(br()),hr(),
                                  p('This plot presents the life years lost (or complement of life expectancy) for the population of Saudi Arabia. 
                                    It corresponds to the number of years lost due to the different causes of death. For example, in 2005-2010 male 
                                    life expectancy between birth and age 85 was 72 years, then life years lost were 13 years (= upper limit 85 – life expectancy 72). 
                                    A further advantage of using this tool is the possibility of calculating years lost due to different causes of death as shown in the Table below.'),
                                  br(),
                                  dataTableOutput('LYL.summary'),hr(),
                                  p('Note: the causes of death of “Others” includes causes that 
                                           represented small percentages in the overall distribution 
                                           of death counts; causes included in this category were: 
                                           maternal, neonatal, birth defects, still born, sudden 
                                           infant death syndrome, disease of the digestive system, 
                                           mental disorders, symptoms, signs and abnormal clinical 
                                           and laboratory findings not elsewhere classified, among 
                                           others.'), fluid = F
                                  )
                       ))),
          
          
          tabPanel("Changes in life expectancy over time",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput( 'period.ind1','Starting period',c("2000-2005","2005-2010"), selected = "2000-2005"),
                       br(),
                       uiOutput('vx'),
                       p('Age and cause decomposition of life expectancy over time by sex. Source: Own calculations with UN life tables and GBD disease cause-of-death distribution'),
                       br(),
                       br(),
                       p('Note: Data not available by cause of death for age 0.'),
                       width = 2
                     ),
                     mainPanel(
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       tabsetPanel(
                         tabPanel('Age- and cause-of-death contribution (plot)', plotlyOutput('Decompostion.plot',width = '100%'),br(br()),hr(),
                                  p('This plot presents the age- and cause-contribution to the difference in life expectancy between two periods. 
                                    For example, Saudi Arabian females had a life expectancy in 2000-2005 of 74.71 and by 2005-2010 this had increased 
                                    by 0.36 years to 75.07. The separation of these 0.36 years into ages and causes of death is shown in the plot. 
                                    For females Neoplasms, Diabetes and other endocrine diseases are among the main providers of the increase in life expectancy, 
                                    however, cardiovascular mortality increased in this period offsetting the life expectancy progress.'),
                                  dataTableOutput('DT.summary'), hr(),
                                  p('Note: the causes of death of “Others” includes causes that 
                                           represented small percentages in the overall distribution 
                                           of death counts; causes included in this category were: 
                                           maternal, neonatal, birth defects, still born, sudden 
                                           infant death syndrome, disease of the digestive system, 
                                           mental disorders, symptoms, signs and abnormal clinical 
                                           and laboratory findings not elsewhere classified, among 
                                           others.'))
                         ,
                         tabPanel('Total cause-of-death contribution (Table)',dataTableOutput('Decompostion.table'),hr(),
                                  p('Note: the causes of death of “Others” includes causes that 
                                           represented small percentages in the overall distribution 
                                           of death counts; causes included in this category were: 
                                           maternal, neonatal, birth defects, still born, sudden 
                                           infant death syndrome, disease of the digestive system, 
                                           mental disorders, symptoms, signs and abnormal clinical 
                                           and laboratory findings not elsewhere classified, among 
                                           others.')
                                  )
                       )))),
          tabPanel("Interactive life expectancy scenarios",
                   sidebarLayout(
                     sidebarPanel(
                       h4('Reduction in cause of death (%)'),
                       sliderInput("cause_ind0", "Infant mortality",
                                   min = 0, max = 100,value = 0, step = 5),
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
                         tabPanel('Hypothetical life expectancy', plotlyOutput('CauseDeleted',height = 600,width = 900)),
                         p(textOutput('GainText'), br(),
                            p('This plot corresponds to three different life expectancies for the Saudi Arabian population: In the period 2010-2015 
                              (76.01 and 73.11 years for females and males respectively), the UN forecast for 2025-2030 (78.25 and 74.99 years for females 
                              and males respectively), and the hypothetical life expectancy if reductions in specific causes of death (selected by the user) 
                              are achieved. For example, a 10% reduction in cardiovascular mortality and 5% in Diabetes and other endocrine diseases results in an 
                              increase in life expectancy to 76.49 and 73.56 for females and males, respectively.'))
                       )))),
          
          tabPanel("Documentation",
                   tabsetPanel(
                     tabPanel('User manual',h3('Download use manual'), downloadLink("UserManual", "Manual")),
                     tabPanel('Data sources and methodology',h3('Download. Add the pdf extension when downloading.'), downloadLink("methods", "Source and methods"))
                   )
                   )
         
         
         )
      )
    )