library('shiny')
library("shinydashboard")
library("shinyjs")
library('plotly')

pageWithSidebar(
  headerPanel('Welcome to Plotus!'),
  
  sidebarPanel(useShinyjs(),
    box(id = "main_params", width = 800, 
      h6('Select .xlsx file. Specify missing values if needed (blank by default)'),
      fileInput('file_upload', 'Upload data', accept=c("excel", "excel files", ".xlsx")),
      
      tags$hr(),
      selectInput('plot_type', 'Select plot type', c('Densityplot', 'Boxplot', 'Violin plot',  'Barplot', "Lollipop plot", 'Lineplot', 'Scatter plot', 'Scatter margin plot', 'Pie plot', 'Donut plot', 'Contingency table', 'Tableplot', 'Genomeplot', 'Cluster plot', 'Independent t-test',  'Paired t-test', 'Correlation plot', 'Radar plot')),
      selectInput('x', 'Select x variable', names(iris), selected = 'Species'),
      selectInput('y', 'Select y variable', names(iris))
      ),
    box(id = "title_labels", width = 800,
        textInput('plot_title', 'Specify plot title'),
        textInput('x_label', 'Specify X label'),
        textInput('y_label', "Specify Y label")
        )
   
    
  ),
  mainPanel(
    
    tabsetPanel(type = 'tabs', 
      tabPanel("Data check", DT::dataTableOutput('data'), verbatimTextOutput('summary'), verbatimTextOutput('summary2')),
      tabPanel("Plot the data", 
               plotOutput('plot_1'),
               box(id = 'plotly', width = 800, 
                   plotlyOutput("plot_2")
               ),
               
               actionButton(inputId = "button_1", label = "x/y, plot type"),
               actionButton(inputId = "button_2", label = "title, labels"),
               downloadButton("download_i_graph", "download i-graph")         
               
               )
      
    )
    
    
  )
  
)