library('shiny')
library("shinydashboard")
library("shinyjs")
library('plotly')
library('shinyhelper')




fluidPage(
#   tags$head(tags$style(
#   HTML('
#         body, label, input, button, select {
#             font-family: "Consolas";}')
# )),

                              # tags$head(tags$style(HTML("
                              #   .btn {
                              #   color:rgb(255,255,255);
                              #   text-align: left;
                              #   #border-color:rgb(0,144,197);
                              #   background-color:rgb(0,144,197);}
                              # 
                              #   # #gobutton:active {
                              #   # background: green;
                              #   # }
                              # 
                              #   .btn:hover{
                              #   #border-color:rgb(232,245,251);
                              #   background-color: rgb(232,245,251);color:   rgb(0,144,197);font-weight: bold;
                              #   }
                              #   .btn:focus{
                              #   background-color:green;
                              #   }
                              # 
                              #   "))),
  tags$head(tags$style(
    HTML('#title {
           color: #407088;
           font-size: 26px;
           font-family: Roboto;
           background-color: #fafafa;
           margin: 30px auto;
           width: 58%;
          }'))),
  tags$div(id="title", p('Plotus - data visualization and statistical analysis software')),
  
  
  
  sidebarPanel(useShinyjs(), # tags$style(".well {background-color: #FFF0B5;}"),
    
      fileInput('file_upload', 'Upload your data', accept=c("excel", "excel files", ".xlsx")) %>% helper(content = "upload_file", colour = 'steelblue'),
      # selectInput('theme_input', 'Select plot theme', choices = c('minimal', 'grey', 'classic', 'void')),
      box(id = 'summary_box', width = 800, 
        checkboxInput('summary_check', 'Show basic descriptive stats?') %>% helper(content = 'basic_desc', colour = 'steelblue'),
        checkboxInput('summary2_check', 'Show advanced descriptive stats?') %>% helper(content = 'adv_desc', colour = 'steelblue'),
        checkboxInput('contingency_check', 'Show contingency table?', value=T) %>% helper(content = 'contingency_check', colour = 'steelblue'),
        selectInput('contingency_columns', 'Specify factors for contingency table calculations', choices = NULL, multiple = T) %>% helper(content = 'contingency_factors', colour = 'steelblue')
      ),
     
      box(id='plot_type', width=800, 
        selectInput('plot_type', 'Select plot type', c('Histogram', 'Box plot', 'Paired box plot (paired t-test)', 'Violin plot',  'Bar plot', "Lollipop plot", 'Line plot', 'Scatter plot', 'Scatter margin plot', 'Correlation plot', 'Pie plot', 'Donut plot', 'Contingency table', 'Genomeplot (demo)', 'Cluster plot',  'Radar plot')),
        uiOutput('x'),
        uiOutput('x2'),
        sliderInput('bins_slider', 'Number of bins', min = 1, max = 100, value = 30),
        colourpicker::colourInput('custom_colour', 'Select custom colour', '#5D4E73'),
        uiOutput('y'),
        uiOutput('z'),
        sliderInput('size_slider', 'Select size', min = 1, max = 20, value = 1),
        checkboxInput('labels_check', 'Add numeric labels?', value = F),
        checkboxInput('faceting', 'Use faceting for grouping variable?', value = F),
        
        uiOutput('orientation'),
        uiOutput('order'),
        
        selectInput('pallete', 'Select color pallete for grouping variable', choices = c('npg', 'aaas', 'lancet', 'jco', 'ucscgb', 'uchicago', 'simpsons', 'rickandmorty')),
        radioButtons('color_fill_radio', 'Select method to draw boxplots with?', choices = c('color', 'fill'), inline = T),
        radioButtons('add_geoms_radio', 'Select additional elements to plot', choices = c('Boxplot' = 'boxplot', 'Points' = 'jitter', 'Mean/SE' = 'mean_se', 'Mean/SD' = 'mean_sd', 'None' = NA), inline = T),
        checkboxInput('boxplot_stat_check', 'Add statistical tests to the plot?', value = T),
        radioButtons('stat_method_radio', 'Select statistical method?', choices = c('Anova', 'Kruskal-Wallis', 'Student t-test', 'Wilcoxon test'), inline = T),
        radioButtons('multiple_adj_radio', 'Select multiple comparisons adjustment method', choices = c('Holm' = 'holm', 'Hochberg' = 'hochberg', 'Hommel' = 'hommel', 'Bonferroni' = 'bonferroni', 'FDR' = 'fdr'), inline = T),
        radioButtons('corr_method_radio', 'Select correlation method?', choices = c('pearson', 'spearman', 'kendall'), inline = T),
        radioButtons('reg_line_method_radio', 'Regression line method?', choices = c('none', 'reg.line', 'loess'), inline = T, selected = 'reg.line'),
        checkboxInput('conf_int_check', 'Show 95% confidense intervals?', value = F),
        radioButtons('margin_plot_radio', 'Margin plot method?', choices = c('hist', 'density', 'boxplot'), inline = T),
        
        checkboxInput('add_jitter', 'Add data points to plot?', value = F),
        
        box(id = 'histogram_checks', width = 800, 
        checkboxInput('density_check', 'Add density to histogram?'),
        checkboxInput('density_instead_check', 'Use density plot instead of histogram?'),
        checkboxInput('shapiro_check', 'Add Shapiro-Wilk normality test? (single histogram only)', value = T)
        )),
    box(id = "title_labels", width = 800,
        textInput('plot_title', 'Specify plot title'),
        textInput('x_label', 'Specify X label'),
        textInput('y_label', "Specify Y label"),
        sliderInput('title_size', 'Select title font size', min = 10, max = 25, value = 14),
        sliderInput('labels_size', 'Select labels font size', min = 10, max = 25, value = 14),
        sliderInput('x_y_size', 'Select xy-text size', min = 10, max = 20, value = 12),
        sliderInput('annotate_size', 'Select annotation font size', min = 3, max = 10, value = 4),
        sliderInput('legend_slider', 'Select legend font size', min = 5, max = 25, value = 12)
        ),
    box(id = 'download_box', width = 800,
        selectInput('plot_width', 'Select plot width', choices = c('5 inches' = 5, '8 inches' = 8, '10 inches' = 10, '12 inches' = 12, '15 inches' = 15), selected = 10),
        selectInput('plot_height', 'Select plot height', choices = c('5 inches' = 5, '8 inches' = 8, '10 inches' = 10, '12 inches' = 12, '15 inches' = 15), selected = 8),
        selectInput('plot_dpi', 'Please select plot quality (DPI)', choices = c('Low, 72dpi' = 72, 'High, 300dpi' = 300, 'Very high, 400dpi' = 400), selected = 300),
        radioButtons('format_radio', 'Select file extension', choices = c('.png', '.pdf'), inline = T),
        downloadButton('downloadPlot', 'Download Plot')
          
          )
   
    
  ),
  mainPanel(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    
    tabsetPanel(type = 'tabs', id='tabs',
      tabPanel("Data / summary", value = 'data_check', DT::dataTableOutput('data'), verbatimTextOutput('summary'), verbatimTextOutput('summary2'), verbatimTextOutput('contingency_table')),
      tabPanel("Plots", value = 'data_plot', 
               plotOutput('plot_1', height = 570),
               box(id = 'plotly', width = 800, 
                   plotlyOutput("plot_2")
               ),
               
              
               actionButton(inputId = "button_1", label = "plot type, x / y selection"),
               actionButton(inputId = "button_2", label = "title, labels, font size"),
               actionButton(inputId = 'download_button', 'Download settings'),
               downloadButton("download_i_graph", "download i-graph")
               
               
               ),
      tabPanel("Tutorial", value = 'tutorial', br(), 
               a("Click Here to listen to a beautiful music while working with Plotus", href="https://www.youtube.com/watch?v=Df3mkjtEwCY", target="_blank")
               )
      
    )
    
    
  )
  
 
)