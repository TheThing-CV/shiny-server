library('ggpubr')
library('shiny')
library('ggcorrplot')
library('ggdendro')
library('plotly')
library('htmlwidgets')
library('ggradar')
library('dplyr')
library('scales')
library('tibble')
library('readxl')
library('DT')
library('pastecs')
library('janitor')
library('tidyr')
library('colourpicker')
library('shinyjs')

function(input, output, session) {
  
  shinyjs::show("main_params")
  shinyjs::hide("title_labels")
  shinyjs::hide("plotly")
  shinyjs::hide("download_i_graph")
  shinyjs::hide('download_box')
  
  session_store <- reactiveValues()
  
  observe_helpers(withMathJax = TRUE, help_dir = "helper_mds")

  observe({
    if(input$tabs == 'data_check')
      {
        shinyjs::hide('plot_type')
        shinyjs::show('contingency_columns')
        shinyjs::show('summary_box')
        shinyjs::hide('title_labels')
        shinyjs::hide('download_box')
      }
    else
    {
      shinyjs::show('plot_type')
      shinyjs::hide('contingency_columns')
      shinyjs::hide('summary_box')
    }
    
    if(input$summary_check)
      shinyjs::show('summary')
    else
      shinyjs::hide('summary')
    
    if(input$summary2_check)
      shinyjs::show('summary2')
    else
      shinyjs::hide('summary2')
    
    if(input$contingency_check)
      shinyjs::show('contingency_table')
    else
      shinyjs::hide('contingency_table')
    
    x_even <- input$density_check %% 2 == 0
    
    updateCheckboxInput(session, "density_instead_check", value = if(input$density_check == F) F else x_even)
    
  })
  
 
  contingency_data <- reactive({
    starwars
  })
  
  iris_data <- reactive({
    iris
  })
  
  data <- reactive({
    file_to_read <- input$file_upload
    
    if(is.null(file_to_read))
      return()
    
    temp <- read_excel(file_to_read$datapath)
    temp <- mutate_if(temp, is.character, as.factor)
    updateSelectInput(session, 'contingency_columns', choices = names(select_if(temp, is.factor)))
    
    temp
  })
  
  output$data <- DT::renderDataTable({
    
      DT::datatable(data(), options = list( initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#719de3', 'color': '#fff'});",
        "}"), searchHighlight = TRUE, pageLength = 10, width="100%", scrollX = TRUE, lengthMenu = list(c(5, 10, 20, 50, -1), c('5', '10', '20', '50', 'All'))))     
      
   
   
   
    
    
  })
  
  output$summary <- renderPrint({
    
    file_to_read <- input$file_upload
    
    if(is.null(file_to_read))
      return()
    
    summary(data()) 
  })
  
  output$summary2 <- renderPrint({
    file_to_read <- input$file_upload
    
    if(is.null(file_to_read))
      return()
    
    temp <- select_if(data(), is.numeric)
    round(stat.desc(temp, norm = T), 3) 
  })
  
  output$contingency_table <- renderPrint({
    if(!is.null(input$contingency_columns))
      ftable(data()[, input$contingency_columns])
  })
  
  observeEvent(input$button_1, {
    shinyjs::show("plot_type")
    shinyjs::hide("title_labels")
    shinyjs::hide('download_box')
  })
  observeEvent(input$button_2, {
    shinyjs::show("title_labels")
    shinyjs::hide("plot_type")
    shinyjs::hide('download_box')
    
  })
  
  observeEvent(input$download_button, {
    shinyjs::hide("plot_type")
    shinyjs::hide("title_labels")
    shinyjs::show('download_box')
  })
  
 
  
  shinyjs::show("main_params")
  shinyjs::hide("title_labels")
  shinyjs::hide("plotly")
  shinyjs::hide("download_i_graph")
  shinyjs::hide('download_box')
  
  output$x <- renderUI({
    if(is.null(data()))
      return();
    
    
    if(input$plot_type == 'Histogram' || input$plot_type == 'Boxplot'){
      selectInput('x_var', 'Select variable to plot', choices = names(select_if(data(), is.numeric)))
      
    }
    
    })
  
  output$y <- renderUI({
    if(is.null(data()))
      return();
    
    if(input$plot_type == 'Histogram'){
      selectInput('by_group', 'Select grouping variable', choices = c('None', names(select_if(data(), is.factor))))
    }
    
    else if (input$plot_type == 'Boxplot'){
      selectInput('x_group', 'Select grouping variable', choices = c(names(select_if(data(), is.factor))))
    }
  })
  
  output$z <- renderUI({
    if(is.null(data()))
      return();
    if (input$plot_type == 'Boxplot'){
      selectInput('by_group', 'Select additional grouping variable', choices = c('None', names(select_if(data(), is.factor))))
    }
    
  })
  
  output$orientation <- renderUI({
    if(is.null(data()))
      return();
    
    if (input$plot_type == 'Boxplot'){
      selectInput('box_orientation', 'Select orientation', choices = c('vertical', 'horizontal'))
    }
  })
  
  plotInput <- reactive({
    if(is.null(data()))
      return();
    
    x_label <- if(input$x_label != '') input$x_label else NULL
    y_label <- if(input$y_label != '') input$y_label else NULL
    plot_title <- if(input$plot_title != '') input$plot_title else NULL
    
    ##############################################################################################################
    ###########################################   BOXPLOT  #######################################################
    ##############################################################################################################
    
      if(input$plot_type == 'Boxplot') {
        shinyjs::hide('bins_slider')
        shinyjs::hide('custom_colour')
        shinyjs::hide('histogram_checks')
        shinyjs::show('add_jitter')
        shinyjs::show('color_fill_radio')
        
        # shinyjs::show("download_i_graph")
        # 
        # shinyjs::show('plotly')
        
        temp <- drop_na(data())
        
        # my_comparisons <- list( c("setosa", "versicolor"), c("versicolor", "virginica"), c("setosa", "virginica"))
        
        if(input$color_fill_radio == 'color')
            p <- ggboxplot(temp, x = input$x_group, y = input$x_var,  color = if(input$by_group != 'None') input$by_group else input$x_group, palette = input$pallete,
                 add = if(input$add_jitter) 'jitter' else NA, xlab = x_label, ylab = y_label, title = plot_title, ggtheme = theme_minimal(),  orientation = input$box_orientation) 
        else 
            p <- ggboxplot(temp, x = input$x_group, y = input$x_var,  fill = if(input$by_group != 'None') input$by_group else input$x_group, palette = input$pallete,
                 add = if(input$add_jitter) 'jitter' else NA, xlab = x_label, ylab = y_label, title = plot_title, ggtheme = theme_minimal(),  orientation = input$box_orientation) 
        
        p <- p + font("xlab", size = input$labels_size, color = "black") + font("ylab", size = input$labels_size, color = "black") +
          font("xy.text", size = input$x_y_size, color = "black") + font("title", size = input$title_size, color = "DarkGray", face = "bold.italic")
        ggpar(p, font.legend = c(input$legend_slider, 'plain', 'black')) 
        
        p + stat_compare_means(method = switch(input$stat_method_radio, 'Anova' = 'anova', 'Kruskal-Wallis' = 'kruskal.test', 'Student t-test' = 't.test', 'Wilcoxon test' = 'wilcox.test'), size = input$annotate_size
                             # , label.y=6.5
                             )
        #   stat_compare_means(
        #     # comparisons = my_comparisons
        #     )
    } 
    
    ##############################################################################################################
    ###########################################   END BOXPLOT  ###################################################
    ##############################################################################################################
    
    ##############################################################################################################
    ###########################################   HISTOGRAM  #####################################################
    ##############################################################################################################
    
    else if(input$plot_type == 'Histogram') {
      shinyjs::hide("download_i_graph")
      shinyjs::hide("plotly")
      shinyjs::show('bins_slider')
      shinyjs::show('custom_colour')
      shinyjs::show('histogram_checks')
      shinyjs::hide('add_jitter')
      shinyjs::hide('color_fill_radio')
      
      
      temp <- drop_na(data())
      col <- input$x_var

      res <- shapiro.test(data()[[col]])
      disp <- paste0('Shapiro-Wilk normality: ', round(res$statistic, 4),  '; p-value: ', round(res$p.value, 4))

      
      if(!input$density_instead_check){
        p <- gghistogram(temp, x = input$x_var, fill = if(input$by_group != 'None') input$by_group else input$custom_colour, bins = input$bins_slider, palette = input$pallete,
                         add = "mean", add_density = input$density_check, rug = TRUE, xlab = x_label, ylab = y_label, title = plot_title, ggtheme = theme_minimal()) + 
          annotate("text", x=Inf, y = Inf, label = if(input$by_group == 'None' && input$shapiro_check) disp else '', vjust=1, hjust=1, size = input$annotate_size) 
      }
      
      else
        p <- ggdensity(temp, x = input$x_var, fill = if(input$by_group != 'None') input$by_group else input$custom_colour, bins = input$bins_slider, palette = input$pallete,
                       add = "mean", rug = TRUE, xlab = x_label, ylab = y_label, title = plot_title, ggtheme = theme_minimal()) +
        annotate("text", x=Inf, y = Inf, label = if(input$by_group == 'None' && input$shapiro_check) disp else '', vjust=1, hjust=1, size = input$annotate_size)
     
      #  if(input$theme_input == 'minimal')
      #   p <- p + theme_minimal()
      # else if(input$theme_input == 'grey')
      #   p <- p + theme_grey()
      # else if(input$theme_input == 'classic')
      #   p <- p + theme_classic()
      # else if(input$theme_input == 'void')
      #   p <- p + theme_void()
      
      p <- p + font("xlab", size = input$labels_size, color = "black") + font("ylab", size = input$labels_size, color = "black") +
        font("xy.text", size = input$x_y_size, color = "black") + font("title", size = input$title_size, color = "DarkGray", face = "bold.italic")
      
      p <- ggpar(p, font.legend = c(input$legend_slider, 'plain', 'black'))
      
      
      if(input$faceting && input$by_group != 'None')
        facet(p, facet.by = input$by_group)
      else
        p 
    }
    
    ##############################################################################################################
    ###########################################   END HISTOGRAM  #################################################
    ##############################################################################################################
    
    
    ##############################################################################################################
    ###########################################   VIOLIN  ########################################################
    ##############################################################################################################
    
    else if(input$plot_type == 'Violin plot') {
      shinyjs::hide("download_i_graph")
      
      
      shinyjs::hide("plotly")
      ggviolin(ToothGrowth, "dose", "len", fill = "supp",
               palette = "jco", 
               add.params = list(fill = "white"))
    }
    
    ##############################################################################################################
    ###########################################   END VIOLIN  ####################################################
    ##############################################################################################################
    
    
    ##############################################################################################################
    ###########################################   BARPLOT  #######################################################
    ##############################################################################################################
    else if(input$plot_type == 'Barplot') {
      shinyjs::hide("download_i_graph")
      
      shinyjs::hide("plotly")
      ggbarplot(ToothGrowth, x = "dose", y = "len", add = "mean_se",
                color = "supp", palette = "jco", fill = 'supp',
                position = position_dodge(0.8))+
        stat_compare_means(aes(group = supp), label = "p.signif", label.y = 29)    
    } 
    
    ##############################################################################################################
    ###########################################   END BARPLOT  ###################################################
    ##############################################################################################################
    
    else if(input$plot_type == 'Lollipop plot') {
      shinyjs::hide("download_i_graph")
      
      
      shinyjs::hide("plotly")
      dfm <- mtcars
      dfm$cyl <- as.factor(dfm$cyl)
      dfm$name <- rownames(dfm)
      ggdotchart(dfm, x = "name", y = "mpg",
                 color = "cyl",                                # Color by groups
                 palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
                 sorting = "ascending",                        # Sort value in descending order
                 add = "segments",                             # Add segments from y = 0 to dots
                 ggtheme = theme_pubr()                        # ggplot2 theme
      )
    }
    
    else if(input$plot_type == 'Lineplot') {
      shinyjs::hide("download_i_graph")
      
      
      shinyjs::hide("plotly")
      ggline(ToothGrowth, x = "dose", y = "len", add = "mean_se",
             color = "supp", palette = "jco", size=.8)+
        stat_compare_means(aes(group = supp), label = "p.signif", 
                           label.y = c(16, 25, 29)) 
    } 
    
    else if(input$plot_type == 'Contingency table') {
      shinyjs::hide("download_i_graph")
      
      
      shinyjs::hide("plotly")
      data <- read.delim(
        system.file("demo-data/housetasks.txt", package = "ggpubr"),
        row.names = 1
      )
      
      ggballoonplot(data, show.label = TRUE, fill = "value", size = 15, ggtheme = theme_minimal()) + gradient_fill(c("steelblue", "white", "orange"))
    } 
    
    else if(input$plot_type == 'Tableplot'){
      shinyjs::hide("download_i_graph")
      
      
      shinyjs::hide("plotly")
      stable <- desc_statby(iris, measure.var = input$y,
                            grps = input$x)
      stable <- stable[, c("Species", "length", "min", "max", "iqr", "mad", "mean", "sd", "se", "ci", "range", "cv", "var")]
      stable.p <- ggtexttable(stable, rows = NULL,
                              theme = ttheme("mBlue"))
      
      stable.p
    }
    
    else if(input$plot_type == 'Genomeplot') {
      shinyjs::hide("download_i_graph")
      
      
      shinyjs::hide("plotly")
      ggmaplot(diff_express, main = expression("Group 1" %->% "Group 2"),
               fdr = 0.05, fc = 2, size = 0.4,
               palette = c("#B31B21", "#1465AC", "darkgray"),
               genenames = as.vector(diff_express$name),
               legend = "top", top = 20,
               font.label = c("bold", 11), label.rectangle = TRUE,
               font.legend = "bold",
               font.main = "bold",
               ggtheme = ggplot2::theme_minimal())
    } 
    
    else if(input$plot_type == 'Cluster plot') {
      shinyjs::hide("download_i_graph")
      
      
      shinyjs::hide("plotly")
      hc <- hclust(dist(USArrests), "ave")
      ggdendrogram(hc, rotate = FALSE, size = 2)
    }
    
    else if(input$plot_type == 'Independent t-test') {
      shinyjs::hide("download_i_graph")
      
      
      shinyjs::hide("plotly")
      p <- ggboxplot(ToothGrowth, x = "supp", y = "len",
                     color = "supp", palette = "jco",
                     add = "jitter", size = .8)
      #  Add p-value
      p + stat_compare_means(method = 't.test')
    }
    
    
    else if(input$plot_type == 'Paired t-test') {
      shinyjs::hide("download_i_graph")
      
      
      shinyjs::hide("plotly")
      ggpaired(ToothGrowth, x = "supp", y = "len",
               color = "supp", line.color = "gray", line.size = 0.4,
               palette = "jco")+
        stat_compare_means(paired = TRUE)
    }
    
    else if(input$plot_type == 'Correlation plot') {
      shinyjs::hide("download_i_graph")
      
      
      shinyjs::hide("plotly")
      corr <- round(cor(mtcars), 1)
      p.mat <- cor_pmat(mtcars)
      ggcorrplot(corr, hc.order = TRUE, type = "lower",
                 lab = TRUE, p.mat = p.mat)
    } 
    
    else if(input$plot_type == 'Scatter plot') {
      shinyjs::hide("download_i_graph")
      
      
      shinyjs::hide("plotly")
      df <- mtcars
      df$cyl <- as.factor(df$cyl)
      ggscatter(df, x = "wt", y = "mpg",
                add = "reg.line",                         # Add regression line
                color = "cyl", palette = "jco",           # Color by groups "cyl"
                shape = "cyl",                            # Change point shape by groups "cyl"
                fullrange = TRUE,                         # Extending the regression line
                rug = TRUE                                # Add marginal rug
      )+
        stat_cor(aes(color = cyl), label.x = 3)           # Add correlation coefficient
    }
    
    else if(input$plot_type == 'Scatter margin plot') {
      shinyjs::hide("download_i_graph")
      
      
      shinyjs::hide("plotly")
      ggscatterhist(
        iris, x = "Sepal.Length", y = "Sepal.Width",
        color = "Species", size = 3, alpha = 0.6,
        palette = c("#00AFBB", "#E7B800", "#FC4E07"),
        margin.params = list(fill = "Species", color = "black", size = 0.2)
      )
    }
    
    else if(input$plot_type == 'Pie plot') {
      shinyjs::hide("download_i_graph")
      
      
      shinyjs::hide("plotly")
      df <- data.frame(
        group = c("Male", "Female", "Child"),
        value = c(25, 25, 50))
      labs <- paste0(df$group, " (", df$value, "%)")
      ggpie(df, "value", label = labs,
            lab.pos = "in", lab.font = "white",
            fill = "group", color = "white",
            palette = c("#00AFBB", "#E7B800", "#FC4E07"))
    }
    
    else if(input$plot_type == 'Donut plot') {
      shinyjs::hide("download_i_graph")
      
      
      shinyjs::hide("plotly")
      df <- data.frame(
        group = c("Male", "Female", "Child"),
        value = c(25, 25, 50))
      labs <- paste0(df$group, " (", df$value, "%)")
      ggdonutchart(df, "value", label = labs,
                   lab.pos = "in", lab.font = "white",
                   fill = "group", color = "white",
                   palette = c("#00AFBB", "#E7B800", "#FC4E07"))
    }
    
    else if(input$plot_type == 'Radar plot') {
      shinyjs::hide("download_i_graph")
      
      mtcars_radar <- mtcars %>% 
        as_tibble(rownames = "group") %>% 
        mutate_at(vars(-group), rescale) %>% 
        tail(4) %>% 
        select(1:10)
      
      ggradar(mtcars_radar)
    }
    
  })

  output$plot_1 <- renderPlot({
    width_i = strtoi(input$plot_width)
    height_i = strtoi(input$plot_height)
    dpi_i = strtoi(input$plot_dpi)
    
    if(input$format_radio == '.pdf'){
      pdf("ggplot.pdf", width = width_i, height = height_i)
      print(plotInput())
      dev.off()
    }
    else {
      ggsave("ggplot.png", plotInput(),  width = width_i, height = height_i, dpi = dpi_i, units = "in")
    }
    
    print(plotInput())
  })
  
  output$plot_2 <- renderPlotly({
    if(input$plot_type == 'Boxplot') {
      p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
      
      
      session_store$plt <- p
      
      session_store$plt
    }
   
  })
  
  
  output$download_i_graph <- downloadHandler(
    filename = function() {
      paste("i_graph-", Sys.time(), ".html", sep = "")
    },
    content = function(file) {
      # export plotly html widget as a temp file to download.
      saveWidget(as_widget(session_store$plt), file, selfcontained = TRUE)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      if(input$format_radio == '.pdf')
        'ggplot.pdf'
      else
        'ggplot.png'
    },
    content = function(file) {
      file.copy(if(input$format_radio == '.pdf') 'ggplot.pdf' else 'ggplot.png', file, overwrite=TRUE)
    }
  )
  
}