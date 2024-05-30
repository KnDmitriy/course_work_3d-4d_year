# library(shiny)
# library(readxl)
# library(dplyr)
# library(tidytext)
# library(ggplot2)
# 
# ui <- fluidPage(
#   titlePanel("Analyze Multiple Excel Files"),
#   sidebarLayout(
#     sidebarPanel(
#       fileInput("file1", "Choose XLSX File 1", accept = ".xlsx"),
#       fileInput("file2", "Choose XLSX File 2", accept = ".xlsx"),
#       fileInput("file3", "Choose XLSX File 3", accept = ".xlsx")
#     ),
#     mainPanel(
#       plotOutput("plot1"),
#       tableOutput("table1"),
#       plotOutput("plot2"),
#       tableOutput("table2"),
#       plotOutput("plot3"),
#       tableOutput("table3"),
#       tableOutput("pivot_table")
#     )
#   )
# )
# 
# server <- function(input, output) {
#   process_file <- function(file_input) {
#     req(file_input)
#     data <- read_excel(file_input$datapath)
# 
#     text_data <- data %>%
#       unnest_tokens(word, everything())
# 
#     # Убираем стоп-слова
#     data(stop_words)
#     text_data <- text_data %>%
#       anti_join(stop_words, by = "word")
# 
#     # Находим частоту слов
#     word_freq <- text_data %>%
#       count(word, sort = TRUE)
# 
#     return(list(
#       plot = ggplot(word_freq[1:10, ], aes(x = reorder(word, n), y = n)) +
#         geom_bar(stat = "identity") +
#         coord_flip() +
#         labs(title = "Most Frequent Words", x = "Word", y = "Frequency"),
#       table = head(word_freq, 10)
#     ))
#   }
# 
#   output$plot1 <- renderPlot({
#     process_file(input$file1)$plot
#   })
# 
#   output$table1 <- renderTable({
#     process_file(input$file1)$table
#   })
# 
#   output$plot2 <- renderPlot({
#     process_file(input$file2)$plot
#   })
# 
#   output$table2 <- renderTable({
#     process_file(input$file2)$table
#   })
# 
#   output$plot3 <- renderPlot({
#     process_file(input$file3)$plot
#   })
# 
#   output$table3 <- renderTable({
#     process_file(input$file3)$table
#   })
# 
#   output$pivot_table <- renderTable({
#     files <- list(
#       file1 = process_file(input$file1)$table,
#       file2 = process_file(input$file2)$table,
#       file3 = process_file(input$file3)$table
#     )
#     combined_data <- bind_rows(files, .id = "file")
#     pivot_data <- combined_data %>%
#       group_by(word) %>%
#       summarize_all(sum) %>%
#       mutate_all(as.numeric) %>%
#       pivot_wider(names_from = file, values_from = n, values_fill = 0)
#     pivot_data
#   })
# }
# 
# shinyApp(ui = ui, server = server)
# 
# 



# 
# library(shiny)
# library(readxl)
# library(dplyr)
# library(tidytext)
# library(ggplot2)
# 
# ui <- fluidPage(
#   titlePanel("Analyze Excel File"),
#   sidebarLayout(
#     sidebarPanel(
#       fileInput("file", "Choose XLSX File", accept = ".xlsx"),
#       actionButton("analyze", "Analyze")
#     ),
#     mainPanel(
#       plotOutput("barPlot"),
#       tableOutput("wordTable")
#     )
#   )
# )
# 
# server <- function(input, output) {
#   observeEvent(input$analyze, {
#     req(input$file)
#     data <- read_excel(input$file$datapath)
# 
#     # Преобразуем все данные в текст
#     text_data <- data %>%
#       unnest_tokens(word, everything())
# 
#     # Убираем стоп-слова
#     data(stop_words)
#     text_data <- text_data %>%
#       anti_join(stop_words, by = "word")
# 
#     # Находим частоту слов
#     word_freq <- text_data %>%
#       count(word, sort = TRUE)
# 
#     output$barPlot <- renderPlot({
#       ggplot(word_freq[1:10, ], aes(x = reorder(word, n), y = n)) +
#         geom_bar(stat = "identity") +
#         coord_flip() +
#         labs(title = "Most Frequent Words", x = "Word", y = "Frequency")
#     })
# 
#     output$wordTable <- renderTable({
#       head(word_freq, 10)
#     })
#   })
# }
# 
# shinyApp(ui = ui, server = server)

# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 


install_or_load_pack <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}
packages <- c("ggplot2",  "data.table", "wordcloud", "tm", "wordcloud2", "tidytext", "devtools", "dplyr", 'tidyverse', 'readxl', 'udpipe', 'writexl', 'xlsx', 'rlang')
install_or_load_pack(packages)

library(shiny)
library(readxl)
library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud2)


wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}




load_stopwords <- function() {
  female_names_rus <- read.csv("/Users/DmitryKonorov/Desktop/Основная_учеба/3й_курс/Курсовая_3й_курс/Course_work_3d_course_First/Data/female_names_rus.txt", header=FALSE)
  male_names_rus <- read.csv("/Users/DmitryKonorov/Desktop/Основная_учеба/3й_курс/Курсовая_3й_курс/Course_work_3d_course_First/Data/male_names_rus.txt", header=FALSE)
  male_surnames_rus <- read.csv("/Users/DmitryKonorov/Desktop/Основная_учеба/3й_курс/Курсовая_3й_курс/Course_work_3d_course_First/Data/male_surnames_rus.txt", header=FALSE)
  extra_stop_words <- c('и','димитровграда', 'димитровград', 'ульяновскаяобласть', 'ульяновск', 'ульяновский', 'саранск', 'саранска', 'мордовие', 'рм', 'рма', 'мордовия', 'мордовский', 'заец', 'idюрий', 'главамарийэл', 'марий', 'эл', 'марийэл', 'эть', 'васил', 'чурин', 'кировский', 'кировскаяобласть', 'вятский', 'мельниченко', 'месяц', 'оренбургнуть', 'объясняемрф', 'провести', 'инвестор', 'вести', 'реализация', 'башкортостанный', 'радий', 'подписать', 'проект', 'пермский', 'пермскийкрай', 'край', 'прикамья', 'краевой', 'задача', 'важно', 'оренбуржец', 'оренбург', 'новость', 'подчеркнуть', 'оренбуржье', 'оренбургский', 'оренбургскаяобласть', 'поддержка', 'часть', 'км', 'валерийрадаеть', 'олегнуть', 'должен', 'около', 'рассказать', 'глава', 'губернатор', 'развитие', 'январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь', 'город', 'ecom', 'казань', 'подробность', 'подробный', 'радия', 'процент', 'уф', 'часть', 'вопрос', 'делать', 'сделать', 'благодаря', 'участие', 'пройти', 'идти', 'создать', 'создавать', 'дать',  'рамка', 'место', 'первый', 'получить', 'удмуртия', 'радай', 'юлие', 'пензенский', 'пенза', 'пензенскаяобласть', 'новый', 'лучший', 'самый', 'работа', 'рабочий', 'работать', 'региональный', 'нижегородскаяобладать', 'clubнижегородский', 'нижегородскаяобласть', 'нижегородский', 'нижний', 'новгород', 'чувашия', 'чувашие', 'обть', "бaшҡортостать", "бaшҡортостан", 'командахабиров', 'рб', 'миллиард', 'башкирия', 'башкортостан', 'башкортостана', 'мый', 'аный', 'мухаметшина', 'мухаметшин', 'реть', 'рф', 'день', 'отметить', 'число', 'миллион', 'ход', 'президент','страна', 'тысяча', 'рубль', 'доллар', 'район', 'итог', 'татарстан', 'татарстать', 'российский', 'ма', 'область', 'республика', 'саратовский', 'татарстан', 'татарстана', 'самарский','экономический', 'экономика', 'регион', 'год', "миннихан", "рт", "россия", "рустам", "руст", 'россия', 'конкурентоспособность', 'инновация', 'инвестиция', 'инвестиционный', 'рустамминнихан', 'дмитрий', 'азаров', 'саратовскаяобласть', 'саратовская', 'самарскаяобласть', 'азар', 'стать', 'rn«', 'твой', 'сих', 'ком', 'свой',
                        'слишком', 'нами', 'всему', 'будь', 'саму', 'чаще', 'ваше', 'наш', 'затем', 'еще', 'наши', 'ту', 'каждый',
                        'мочь', 'весь', 'этим', 'наша', 'своих', 'оба', 'который', 'зато', 'те', 'вся', 'ваш', 'такая', 'теми', 'ею', 'нередко',
                        'также', 'чему', 'собой', 'нем', 'вами', 'ими', 'откуда', 'такие', 'тому', 'та', 'очень', 'нему',  'д',
                        'алло', 'оно', 'кому', 'тобой', 'таки', 'мой', 'нею', 'ваши', 'ваша', 'кем', 'мои',
                        'однако', 'сразу', 'свое', 'ними', 'всё', 'неё', 'тех', 'хотя', 'всем', 'тобою', 'тебе', 'одной', 'другие',
                        'буду', 'моё', 'своей', 'такое', 'всею', 'будут', 'своего', 'кого', 'свои', 'мог', 'нам', 'особенно', 'её',
                        'наше', 'кроме', 'вообще', 'вон', 'мною', 'никто', 'это', 'изза', 'именно', 'поэтому', 'будьт', 'являться', 'чувашский', 'тыса', 'смочь', 'ваший', 'гльба', 'ать', 'уть', 'ивать', 'ольги', 'пенз', 'ер', 'иметь', 'олегнуть', 'сг', 'например', 'сообщить', 'сообщать', 'среди', 'нть', 'пер', 'зспермь', 'края', 'ради', 'назвать', 'важный')
  stopwords_combined <- paste(c(stopwords("russian"), extra_stop_words,
                                tolower(male_names_rus$V1),
                                tolower(male_surnames_rus$V1),
                                tolower(female_names_rus$V1)), collapse = "|")
}

stopwords_combined <- load_stopwords()

ui <- fluidPage(
  titlePanel("Аналиировать файлы Excel"),
  actionButton("add", "Добавить вкладку", icon = icon("plus-circle")),
  #actionButton(shinyInput("remove_btn", rv$counter), "Remove", icon = icon("minus-circle")),
  tabsetPanel(id = "tabs",
    # tabPanel(title = "Main",
    #          value = "main",
    #          
    #          ## CONTENT PANEL ----- :
    #          p("Add a new tab"),
    #          #actionButton("add", "Add", icon = icon("plus-circle"))
    # ),
    tabPanel("Файл 1",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Выберите XLSX файл 1", accept = ".xlsx"),
                 actionButton("analyze1", "Анализировать файл 1"),
                 downloadButton("downloadData1", "Скачать результаты анализа файла 1")
               ),
               mainPanel(
                 plotOutput("barPlot1"),
                 #plotOutput("wordcloud1"),
                 wordcloud2Output("wordcloud1"),
                 tableOutput("wordTable1")


               )
             )
    ),
    tabPanel("Сравнить файлы",
             mainPanel(
               actionButton("compare_files_btn", "Сравнить введенные файлы"),
               tableOutput("compare_files_table")
             )
    ),
    # tabPanel("File 2",
    #          sidebarLayout(
    #            sidebarPanel(
    #              fileInput("file2", "Choose XLSX File 2", accept = ".xlsx"),
    #              actionButton("analyze2", "Analyze File 2")
    #            ),
    #            mainPanel(
    #              plotOutput("barPlot2"),
    #              tableOutput("wordTable2")
    #            )
    #          )
    # ),
    # tabPanel("File 3",
    #          sidebarLayout(
    #            sidebarPanel(
    #              fileInput("file3", "Choose XLSX File 3", accept = ".xlsx"),
    #              actionButton("analyze3", "Analyze File 3")
    #            ),
    #            mainPanel(
    #              plotOutput("barPlot3"),
    #              tableOutput("wordTable3")
    #            )
    #          )
    # )
  )
)

server <- function(input, output, session) {
  
  
  
  data <- reactive({
    get(input$barPlot1)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(input$file1, ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(data(), file)
    }
  )
  
  shinyInput <- function(name, id) paste(name, id, sep = "_")
  rv <- reactiveValues(counter = 1L)
  
  
 
  
  observeEvent(input$add, {
    rv$counter <- rv$counter + 1L
    ## GO TO THE NEWLY CREATED TAB:
    updateTabsetPanel(session, "tabs", shinyInput("new_tab", rv$counter))
  }, ignoreInit = TRUE)
  
  observeEvent(input$add, {
    appendTab(inputId = "tabs",
              tabPanel(title = paste("Файл", rv$counter),
                       value = shinyInput("new_tab", rv$counter),
                       sidebarLayout(
                         sidebarPanel(
                           fileInput(paste("file", rv$counter, sep=''), "Выберите XLSX файл", accept = ".xlsx"),
                           actionButton(paste("analyze", rv$counter, sep=''), paste("Анализировать файл", rv$counter)),
                           downloadButton(paste("downloadData", rv$counter, sep=''), paste("Скачать результаты анализа файла", rv$counter)),
                           actionButton(shinyInput("remove_btn", rv$counter), "Удалить вкладку", icon = icon("minus-circle"))
                         ),
                         mainPanel(
                           plotOutput(paste("barPlot", rv$counter, sep='')),
                           #plotOutput("wordcloud1"),
                           wordcloud2Output(paste("wordcloud", rv$counter, sep='')),
                           tableOutput(paste("wordTable", rv$counter, sep=''))

                         )
                       )
              ),

              # tabPanel(title = paste("Файл", rv$counter),
              #          value = shinyInput("new_tab", rv$counter),
              #
              #          ## CONTENT PANEL ----- :
              #          tags$h4(paste("This is tab:", rv$counter)),
              #          p("Remove this tab"),
              #          actionButton(shinyInput("remove_btn", rv$counter), "Remove", icon = icon("minus-circle"))
              # )
    )
  })

  ## REACTIVITY TO ARRANGE TAB NAMES:
  current.tab <- eventReactive(input$tabs, {
    # # don't accidentally remove main tab:
    #input$tabs
    if (!identical(input$tabs, "file1")) {
      input$tabs
    } else {
      NULL
    }
  })
  
  ## OBSERVERS FOR THE REMOVE BTNS:
  observe({
    if (rv$counter > 0L) {
      lapply(seq(rv$counter), function(x) {
        observeEvent(input[[paste("remove_btn", x, sep = "_")]], {
          removeTab(inputId = "tabs", target = current.tab())
          # 
        })
      })
    }
  })
  
  
  
  
  
  
  
  

  clean_corpus <- function(corpus_to_use){
    corpus_to_use %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace) %>%
      tm_map(content_transformer(function(x) iconv(x, to='UTF-8'))) %>%
      tm_map(removeNumbers) %>%
      tm_map(content_transformer(tolower)) 
  }
  
  # df = reactive({
  #   req(input$file1)
  #   
  #   df <- read_excel(input$file1$datapath)
  #   
  #   return(df)
  # })
  get_preprocessed_texts_word_list <- function(file) {
    
    
    
    req(file)
    input_data <- read_excel(file$datapath)
    #input_data <- df
    
    load_stopwords()
    corp_city_df <- clean_corpus(VCorpus(VectorSource(input_data)))
    corp_city_df[["1"]][["content"]] <- gsub("[\U{1F600}-\U{1F64F}\U{1F300}-\U{1F5FF}\U{1F680}-\U{1F6FF}\U{1F1E0}-\U{1F1FF}\U{2500}-\U{2BEF}\U{2702}-\U{27B0}\U{24C2}-\U{1F251}\U{1f926}-\U{1f937}\U{10000}-\U{10ffff}\u{2640}-\u{2642}\u{2600}-\u{2B55}\u{200d}\u{23cf}\u{23e9}\u{231a}\u{fe0f}\u{3030}]", "", corp_city_df[["1"]][["content"]], perl = TRUE)
    if (!file.exists('russian-gsd-ud-2.5-191206.udpipe'))
    {
      gsd_model_raw <- udpipe_download_model(language = "russian-gsd")
    }
    gsd_model <- udpipe_load_model(file = 'russian-gsd-ud-2.5-191206.udpipe')
    x <- udpipe_annotate(gsd_model, x = corp_city_df[["1"]][["content"]],  parser = "none")
    x <- as.data.frame(x)
    x$lemma <- noquote(x$lemma)
    x$lemma <- str_replace_all(x$lemma, "[[:punct:]]", "")
    tmp <- x$lemma
    tmp <- str_replace_all(x$lemma, paste("\\b(", stopwords_combined, ")\\b"), "")
    tmp <- str_replace_all(tmp, '№', '')
    tmp <- str_replace_all(tmp, '−', '')
    tmp <- str_replace_all(tmp, '—', '')
    tmp <- str_replace_all(tmp, 'правительстворазвитие', 'правительство')
    tmp <- str_replace_all(tmp, 'правительстворб', 'правительство')
    tmp <- str_replace_all(tmp, 'цифровый', 'цифровой')
    tmp <- str_replace_all(tmp, 'научныймощность', 'научный мощность')
    tmp <- str_replace_all(tmp, 'club', '')
    tmp <- str_replace_all(tmp, 'правительствомарийэть', 'правительство')
    tmp <- str_replace_all(tmp, 'цура', 'цур')
    tmp <- tmp[sapply(tmp, nchar) > 0]
    return(tmp)
  }

  
  
  
  
  analyze_and_render <- function(file_input, plot_output, table_output, wordcloud_output) {
    #observeEvent(file_input(), {
      #word_freq <- analyze_file(file_input())
      
      preprocessed_texts_word_list <- get_preprocessed_texts_word_list(file_input)
      d <- as.data.frame(sort(table(preprocessed_texts_word_list), decreasing = TRUE))
      colnames(d) <- c("word", "freq")
     
      word_freq <- d
      #d$ratio <- d$freq / sum(d$freq) * 100
      d$tf <- d$freq / nrow(d)
      #get_preprocessed_texts_word_list(file_input())
      output[[plot_output]] <- renderPlot({
         
        ggplot(word_freq[1:10, ], aes(x = reorder(word, freq), y = freq)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          labs(title = "Наиболее часто встречающиеся слова", x = "Слова", y = "Частота встречаемости")
      })
      output[[table_output]] <- renderTable({
        colnames(word_freq) <- c("Слово", "Частота встречаемости слова в корпусе текстов")
        head(word_freq, 10)
      })
      #wordcloud_rep <- repeatable(wordcloud)
      # output[[word_cloud]] <- renderWordcloud2({
      #   #wordcloud2(d$word, d$freq, random.order=FALSE, rot.per=0.2, scale=c(3, .5), max.words=190, colors=brewer.pal(8, "Dark2"))
      #   wordcloud2(word_freq)
      # })
      # output[[wordcloud_output]] <- renderPlot({
      #   wordcloud(d$word, d$freq, colors=brewer.pal(8, "Dark2"))
      # })
      output[[wordcloud_output]] <- renderWordcloud2({
        wordcloud2a(word_freq, size = 0.35)
      })
      
      return(d)
      # random.order=FALSE, rot.per=0.2, scale=c(3, .5), max.words=190,
      
      # output$word_cloud = renderWordcloud2({
      #   wordcloud2(word_freq)
      # })
    #})
  }

  # analyze_file <- function(file) {
  #   req(file)
  #   data <- read_excel(file$datapath)
  # 
  #   # Преобразуем все данные в текст
  #   text_data <- data %>%
  #     unnest_tokens(word, everything())
  # 
  #   # Убираем стоп-слова
  #   data(stop_words)
  #   text_data <- text_data %>%
  #     anti_join(stop_words, by = "word")
  # 
  #   # Находим частоту слов
  #   word_freq <- text_data %>%
  #     count(word, sort = TRUE)
  # 
  #   return(word_freq)
  # }

  observeEvent(input[["analyze1"]], {
    analyze_and_render(input[["file1"]], "barPlot1", "wordTable1", "wordcloud1")
    # word_freq1 <- analyze_file(input$file1)
    # output$barPlot1 <- renderPlot({
    #   ggplot(word_freq1[1:10, ], aes(x = reorder(word, n), y = n)) +
    #     geom_bar(stat = "identity") +
    #     coord_flip() +
    #     labs(title = "Most Frequent Words (File 1)", x = "Word", y = "Frequency")
    # })
    # 
    # output$wordTable1 <- renderTable({
    #   head(word_freq1, 10)
    # })
  })
  observeEvent(input[[paste("file", rv$counter, sep='')]], {
    p('sdfsdfds')
    analyze_and_render(input[[paste("file", rv$counter, sep='')]], 
        paste("barPlot", rv$counter, sep=''), paste("wordTable", rv$counter, sep=''), paste("wordcloud", rv$counter, sep=''))

  })

 
}

shinyApp(ui = ui, server = server)





# 
# 
# 
# 
# 
# 
# 
# 
# 
# library(shiny)
# library(shinyFiles)
# library(readxl)
# library(dplyr)
# library(tidytext)
# library(ggplot2)
# library(purrr)
# 
# ui <- fluidPage(
#   titlePanel("Анализ регионов по разным периодам"),
#   sidebarLayout(
#     sidebarPanel(
#       actionButton("addTab", "Добавить регион"),
#       actionButton("analyzeSum", "Вывести таблицу анализа для всех введенных файлов"),
#       shinySaveButton("save", "Сохранить все результаты анализа файла", "Сохранить", filetype = list(xlsx = "xlsx"))
#     ),
#     mainPanel(
#       tabsetPanel(id = "tabs",
#                   tabPanel("Summary", tableOutput("summaryTable"))
#       )
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   tab_counter <- reactiveVal(0)
#   all_files <- reactiveVal(list())
#   volumes <- getVolumes()()
# 
#   observe({
#     shinyFileChoose(input, "save", roots = volumes, session = session)
#   })
# 
#   save_path <- reactive({
#     parseSavePath(volumes, input$save)$datapath
#   })
# 
#   observeEvent(input$addTab, {
#     count <- tab_counter() + 1
#     tab_counter(count)
# 
#     tab_id <- paste0("tab", count)
#     file_id <- paste0("file", count)
#     analyze_id <- paste0("analyze", count)
#     plot_id <- paste0("barPlot", count)
#     table_id <- paste0("wordTable", count)
# 
#     new_sidebar <- tagList(
#       fileInput(file_id, paste("Выбрать XLSX файл", count), accept = ".xlsx"),
#       actionButton(analyze_id, "Анализировать файл")
#     )
# 
#     new_tab <- tabPanel(
#       title = paste("Файл", count),
#       fluidRow(
#         column(12, plotOutput(plot_id)),
#         column(12, tableOutput(table_id))
#       )
#     )
# 
#     insertUI(
#       selector = "#sidebarPanel",
#       where = "beforeEnd",
#       ui = new_sidebar
#     )
# 
#     insertUI(
#       selector = "#tabs",
#       where = "beforeEnd",
#       ui = new_tab
#     )
# 
#     observeEvent(input[[analyze_id]], {
#       req(input[[file_id]])
#       word_freq <- analyze_file(input[[file_id]])
# 
#       output[[plot_id]] <- renderPlot({
#         ggplot(word_freq[1:10, ], aes(x = reorder(word, n), y = n)) +
#           geom_bar(stat = "identity") +
#           coord_flip() +
#           labs(title = "Наиболее часто встречающиеся слова", x = "Слова", y = "Частота встречаемости")
#       })
# 
#       output[[table_id]] <- renderTable({
#         head(word_freq, 10)
#       })
# 
#       files <- all_files()
#       files[[tab_id]] <- word_freq
#       all_files(files)
#     })
#   })
# 
#   observeEvent(input$analyzeSum, {
#     files <- all_files()
#     combined_data <- bind_rows(files, .id = "file")
#     pivot_data <- combined_data %>%
#       group_by(word) %>%
#       summarize(total_frequency = sum(n, na.rm = TRUE)) %>%
#       arrange(desc(total_frequency))
#     output$summaryTable <- renderTable({
#       pivot_data
#     })
#   })
# 
#   observeEvent(input$save, {
#     path <- save_path()
#     if (!is.null(path)) {
#       files <- all_files()
#       combined_data <- bind_rows(files, .id = "file")
#       pivot_data <- combined_data %>%
#         group_by(word) %>%
#         summarize(total_frequency = sum(n, na.rm = TRUE)) %>%
#         arrange(desc(total_frequency))
#       write.csv(pivot_data, file.path(path, "summary_analysis.csv"))
#     }
#   })
# 
#   analyze_file <- function(file) {
#     req(file)
# 
#     data <- read_excel(file$datapath)
# 
#     text_data <- data %>%
#       unnest_tokens(word, everything())
# 
#     data(stop_words)
#     text_data <- text_data %>%
#       anti_join(stop_words, by = "word")
# 
#     word_freq <- text_data %>%
#       count(word, sort = TRUE)
# 
#     return(word_freq)
#   }
# }
# 
# shinyApp(ui = ui, server = server)

