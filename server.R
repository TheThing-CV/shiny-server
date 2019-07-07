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



function(input, output, session) {
  session_store <- reactiveValues()
  
  selected_data <- reactive({
    iris[, c(input$x, input$y)]
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
    temp
  })
  
  output$data <- DT::renderDataTable({
    
      DT::datatable(data())     
      
   
   
   
    
    
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
  
  observeEvent(input$button_1, {
    shinyjs::show("main_params")
    shinyjs::hide("title_labels")
  })
  observeEvent(input$button_2, {
    shinyjs::show("title_labels")
    shinyjs::hide("main_params")
    
    
  })
  
 
  
  shinyjs::show("main_params")
  shinyjs::hide("title_labels")
  shinyjs::hide("plotly")
  shinyjs::hide("download_i_graph")

  output$plot_1 <- renderPlot({
    x_label <- if(input$x_label != '') input$x_label else NULL
    y_label <- if(input$y_label != '') input$y_label else NULL
    plot_title <- if(input$plot_title != '') input$plot_title else NULL
    
    if(input$plot_type == 'Boxplot') {
      shinyjs::show("download_i_graph")
      
      shinyjs::show('plotly')
      my_comparisons <- list( c("setosa", "versicolor"), c("versicolor", "virginica"), c("setosa", "virginica"))
      ggboxplot(selected_data(), x = input$x, y = input$y, color = input$x, 
                add = 'jitter', xlab = x_label, ylab = y_label, title = plot_title, ggtheme = theme_minimal()) + 
                  stat_compare_means(method='anova', label.y=6.5) + 
                  stat_compare_means(comparisons = my_comparisons)
    } 
    
    else if(input$plot_type == 'Densityplot') {
        shinyjs::hide("download_i_graph")
      

         shinyjs::hide("plotly")
        p <- ggdensity(selected_data(), x = input$y,
                  add = "mean", rug = TRUE,
                  color = input$x, fill = input$x, xlab = x_label, ylab = y_label, title = plot_title, ggtheme = theme_minimal())
        p
      
    }
    
    else if(input$plot_type == 'Violin plot') {
      shinyjs::hide("download_i_graph")
      

      shinyjs::hide("plotly")
      ggviolin(ToothGrowth, "dose", "len", fill = "supp",
               palette = "jco", 
                add.params = list(fill = "white"))
    }
    
    
    else if(input$plot_type == 'Barplot') {
      shinyjs::hide("download_i_graph")
      

      shinyjs::hide("plotly")
      ggbarplot(ToothGrowth, x = "dose", y = "len", add = "mean_se",
                color = "supp", palette = "jco", fill = 'supp',
                position = position_dodge(0.8))+
        stat_compare_means(aes(group = supp), label = "p.signif", label.y = 29)    
    } 
    
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
  
}