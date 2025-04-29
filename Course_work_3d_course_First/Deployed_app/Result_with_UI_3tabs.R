install_or_load_pack <- function(packs){
  create.pkg <- packs[!(packs %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(packs, library, character.only = TRUE)
}
packages <- c("ggrepel", "ggplot2",  "data.table", "tm", "wordcloud2", "tidytext", "dplyr", 'tidyverse', 'readxl', 'udpipe', 'writexl', 'openxlsx', 'rlang', 'lsa', 'shiny')
install_or_load_pack(packages)
# Нужен ли пакет "Rcpp"? Все ли из подключаемых пакетов нужны? Как это проверить?
# Проверить можно с помощью убирания пакета из загружаемых и попытки запуска программы,
# загрузки файлов и их сравнения.
# Убраны пакеты wordcloud, Rcpp, devtools



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
  
  female_names_rus <- read.csv("female_names_rus.txt", header=FALSE)
  male_names_rus <- read.csv("male_names_rus.txt", header=FALSE)
  male_surnames_rus <- read.csv("male_surnames_rus.txt", header=FALSE)
  stop_words_expanded <- c('и', 'й', 'г', 'единаяроссия', 'единый', 'время', 'территория', 'димитровграда', 'димитровград', 
                           'чебоксар', 'ядринский', 'житель', 'компания', 'министр', 'дом', 'общественный',
                           'программа', 'мероприятие', 'условие', 'ситуация', 'гражданин', 'группа', 'организация',
                           'система', 'оао', 'ооо', 'пао', 'зао', 'центр', 'руководитель', 'конкурс', 'решить', 'говорить',
                           'состав', 'уровень', 'адрес', 'дело', 'заместитель', 'просто', 'данный', 'сайт', 'позолять',
                           'директор', 'полный', 'час', 'неделя', 'рука', 'ядрин', 'погода', 'выбрать',
                           'средство', 'принять', 'чебоксарский', 'пункт', 'получать', 'второй', 'количество',
                           
                           'найти', 'pgrunews', 'хороший', 'занятие', 'ныжный', 'номер', 'отлично', 'лс',
                           'состояние', 'цена', 'вид', 'руба', 'зеркадать', 'ть', 'торг', 'нужный', 'муниципальный',
                           'округ', 'ип', 'ссылка', 'задний', 'друг', 'замить', 'бесплатный', 
                           'участник', 'слово', 'прогноз', 'удобный', 'тд', 'летний', 'лето',
                           'астраханский', 'астрахань', 'астраханскаяобласть', 'федеральный', 'государственный',
                           'министерство', 'команда', 'асраханец', 'еррб', 'россии', 'ербашкортостан', 'мера',
                           'гтркбашкортостан', 'необходимый', 'возможность', 'предварительный', 'отделение',
                           'объект', 'погодавбашкирия', 'gtrkrb', 'проходить', 'правительство', 'премьерминистр',
                           'правительстворб', 'период', 'пресечить', 'сотрудник', 'гтрквяток', 'еркир', 'мс',
                           'мс', 'ночь', 'мй', 'обещать', 'ожидаться', 'c',
                           
                           'правительствомарийэть', 'йошкарола', 'цур', 'цура', 'врио', 'федерация', 'исполнять',
                           'врио', 'временно', 'заседание', 'эфир', 'цель', 'направить', 'внимание',
                           'встреча', 'зайцин', 'сфера', 'национальный', 'улица', 'провдоить', 
                           'нижегородскийрайон', 'нижнийновгород', 'пространство', 'урка',
                           'нижегородец', 'администрация', 'департамент', 'горький', 'шалабай', 'номинация',
                           'градус', 'ребенок', 'ребёнок', 'ый', 'находится', 'находиться', 'сильный',
                           'перй', 'ул', 'прогнозируть', 'красноярский', 'самар', 'толльятъ', 'тольять',
                           'толльять','тольятть', 'ярин', 'иметься', 'самарь', 'требоваться', 'зп', 'сутка','двое',
                           'магазин', 'саратов', 'сарат', 'составить', 'течение', 'случай', 'общий', 'энгельсский',
                           'саратовец', 'саратовый', 'балашовский', 'начало', 'областной', 'составлять',
                           'учреждение', 'совет', 'комитет', 'казанский', 'средний', 'азнакаевский',
                           'удмуртский', 'ижевска', 'мояудмуртия', 'ува', 'увинский', 'ижевсок', 'ижевск', 'астраханец',
                           'кировчанин',
                           
                           'образовательный', 'этап', 'астраханец', 'проведение', 'спортивный', 'дорожный',
                           'градус', 'федеральный', 'цуринформировать', 'заявка', 'спортсмен', 'победитель', 'всероссийский',
                           'результат', 'учебный', 'современный', 'цурмарийэл', 'зарегистрировать', 'совещание',
                           'деятельность', 'акция', 'проведение', 'мероприятие', 'главный', 'несколько', 'любой',
                           'площадка', 'комиссия', 'зона', 'помощь', 'специалист', 'окружающий', 'профессиональный',
                           'сельский', 'ремний', 'предложение', 'планироваться', 'местный', 'онапомни', 'небольшой',
                           'планироваться', 'председатель', 'совещание', 'обращение', 'республиканский', 'дополнительный',
                           'индивидуальный', 'работник', 'режим', 'решение', 'управление', 'марийский', 'позволять',
                           'деятельность', 'текущий', 'обсудить', 'поблагодарить', 'посетить', 'направление', 'тема',
                           'орган', 'установить', 'участок', 'важноenn', 'пятница', 'свердлово', 'транспортный',
                           'программа', 'ерпенза', 'коронавирусный', 'необходимый', 'возраст', 'возможность',
                           'ситуация', 'средство', 'условие', 'уровень', 'количество', 'неделя', 'участник',
                           'состояние', 'данный', 'позволять', 'адрес', 'охотничий', 'помогать', 'данный',
                           'проводиться', 'здоровый', 'образ', 'активность', 'принять', 'httpspgrunews', 'профессия',
                           'районный', 'действовать', 'заниматься', 'вместе', 'телефон', 'телефона', 'пробегнуть',
                           'отличный', 'хотеть', 'ггра', 'сарансок', 'создание', 'смотреть', 'легкой',
                           'оренбургское', 'профилактический', 'обстоятельство', 'поступить', 'произойти', 'сообщение',
                           'следовать', 'пермь', 'застать', 'желать', 'линия', 'пусть', 'ближайший', 'рядом',
                           'махонин', 'требование', 'внимательный', 'появиться', 'установить', 'режим', 'активный',
                           'несколько', 'категория', 'позволять', 'смен', 'сменный', 'энгельс', 'срок', 'балаковский',
                           'начать', 'план', 'прошлый', 'мр', 'услуга', 'лицо', 'специальный', 'заявка', 'httpskazanfirstrunews',
                           "требовать", "править", "информация", 'обращение', 'править', 'население', 'подготовка', 'ремонт', 
                           'план', 'комплекс', 'маршрут', 'снег', 'гражданин', 'общественный', 'продукт', 'товар', 'дом', 
                           'мероприятие', 'система', 'сохранение', 'проблема', 'труд',  'деньги',  'мир', 'проверка', 
                           'обратиться', 'возбудить', 'факт', 'движение', 
                           'избегать', 'соблюдение', 'явление', 'здание', 'данные', 'правило', 'график', 'источник', 'продукция',
                           'дорога', 'транспорт', 'татарский', 'азнакаево', 'следующий', 'татарстанный', 'парламент', 
                           'httpsvkcomappformidformid', 'выходный', 'ахмадинур', "управлять", "объем", "происшествие",
                           "использование", "обеспечение", "использование", "зауралье", "оперативный", "представитель",
                           "голосоаваний", "секретарь", "рустема", "документ", "считать", "минувший", "столица", "международный",
                           "дождь", "м", "кубок", "фонд", "поздравляе", "мужчина", "женщина", "проводить", "пробный",
                           "узнать", "материал", "статья", "отдел", "денежный", "полицейский", "сторона", "допустить", 
                           "вещество", "соблюдать", "вызов", "мобильный", "знакомый", "родной", "прохождение",
                           "плата", "выплата", "качество", "официальный", "комфортный", "оборудование", "обязанность",
                           "добавить", "предоставление", "размер", "поставить", "касаться", "подготовить", "известный",
                           "госсоветерт", "госсоветерт", "ч", "отношение", "завтра",
                           
                           'ульяновскаяобласть', 'ульяновск', 'ульяновский', 'саранск', 'саранска', 'мордовие', 'рм', 'рма', 'мордовия', 
                           'мордовский', 'заец', 'idюрий', 'главамарийэл', 'марий', 'эл', 'марийэл', 'эть', 'васил', 'чурин', 'кировский', 
                           'кировскаяобласть', 'вятский', 'мельниченко', 'месяц', 'оренбургнуть', 'объясняемрф', 'провести', 'инвестор', 
                           'вести', 'реализация', 'башкортостанный', 'радий', 'подписать', 'проект', 'пермский', 'пермскийкрай', 'край', 
                           'прикамья', 'краевой', 'задача', 'важно', 'оренбуржец', 'оренбург', 'новость', 'подчеркнуть', 'оренбуржье', 
                           'оренбургский', 'оренбургскаяобласть', 'поддержка', 'часть', 'км', 'валерийрадаеть', 'олегнуть', 'должен', 
                           'около', 'рассказать', 'глава', 'губернатор', 'развитие', 'январь', 'февраль', 'март', 'апрель', 'май', 
                           'июнь', 'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь', 'город', 'ecom', 'казань', 'подробность', 
                           'подробный', 'радия', 'процент', 'уф', 'часть', 'вопрос', 'делать', 'сделать', 'благодаря', 'участие', 'пройти', 
                           'идти', 'создать', 'создавать', 'дать',  'рамка', 'место', 'первый', 'получить', 'удмуртия', 'радай', 'юлие', 
                           'пензенский', 'пенза', 'пензенскаяобласть', 'новый', 'лучший', 'самый', 'работа', 'рабочий', 'работать', 
                           'региональный', 'нижегородскаяобладать', 'clubнижегородский', 'нижегородскаяобласть', 'нижегородский', 
                           'нижний', 'новгород', 'чувашия', 'чувашие', 'обть', "бaшҡортостать", "бaшҡортостан", 'командахабиров', 'рб', 
                           'миллиард', 'башкирия', 'башкортостан', 'башкортостана', 'мый', 'аный', 'мухаметшина', 'мухаметшин', 'реть', 
                           'рф', 'день', 'отметить', 'число', 'миллион', 'ход', 'президент','страна', 'тысяча', 'рубль', 'доллар', 'район', 
                           'итог', 'татарстан', 'татарстать', 'российский', 'ма', 'область', 'республика', 'саратовский', 'татарстан', 'татарстана', 'самарский','экономический', 'экономика', 'регион', 'год', "миннихан", "рт", "россия", "рустам", "руст", 'россия', 'конкурентоспособность', 'инновация', 'инвестиция', 'инвестиционный', 'рустамминнихан', 'дмитрий', 'азаров', 'саратовскаяобласть', 'саратовская', 'самарскаяобласть', 'азар', 'стать', 'rn«', 'твой', 'сих', 'ком', 'свой',
                           'слишком', 'нами', 'всему', 'будь', 'саму', 'чаще', 'ваше', 'наш', 'затем', 'еще', 'наши', 'ту', 'каждый',
                           'мочь', 'весь', 'этим', 'наша', 'своих', 'оба', 'который', 'зато', 'те', 'вся', 'ваш', 'такая', 'теми', 'ею', 'нередко',
                           'также', 'чему', 'собой', 'нем', 'вами', 'ими', 'откуда', 'такие', 'тому', 'та', 'очень', 'нему',  'д',
                           'алло', 'оно', 'кому', 'тобой', 'таки', 'мой', 'нею', 'ваши', 'ваша', 'кем', 'мои',
                           'однако', 'сразу', 'свое', 'ними', 'всё', 'неё', 'тех', 'хотя', 'всем', 'тобою', 'тебе', 'одной', 'другие',
                           'буду', 'моё', 'своей', 'такое', 'всею', 'будут', 'своего', 'кого', 'свои', 'мог', 'нам', 'особенно', 'её',
                           'наше', 'кроме', 'вообще', 'вон', 'мною', 'никто', 'это', 'изза', 'именно', 'поэтому', 'будьт', 'являться', 
                           'чувашский', 'тыса', 'смочь', 'ваший', 'гльба', 'ать', 'уть', 'ивать', 'ольги', 'пенз', 'ер', 'иметь', 'олегнуть', 
                           'сг', 'например', 'сообщить', 'сообщать', 'среди', 'нть', 'пер', 'зспермь', 'края', 'ради', 'назвать', 'важный',
                           'ик', 'ульяновсок', 'ульяновска', 'russia', 'reg', 'видео', 'russianpolice', 'mvd', 'police', 'ульяновскаяобласть', 'ульяновскаобласть',
                           'русский', 'личный'
  )
  
  
  stop_words_short <- c('и', 'й', 'г', 'единаяроссия', 'единый', 'время', 'территория', 'димитровграда', 'димитровград', 
                        'чебоксар', 'ядринский', 'житель', 'компания', 'министр', 'дом', 'общественный',
                        'программа', 'мероприятие', 'условие', 'ситуация', 'гражданин', 'группа', 'организация',
                        'система', 'оао', 'ооо', 'пао', 'зао', 'центр', 'руководитель', 'конкурс', 'решить', 'говорить',
                        'состав', 'уровень', 'адрес', 'дело', 'заместитель', 'просто', 'данный', 'сайт', 'позолять',
                        'директор', 'полный', 'час', 'неделя', 'рука', 'ядрин', 'погода', 'выбрать',
                        'средство', 'принять', 'чебоксарский', 'пункт', 'получать', 'второй', 'количество',
                        'нижегородец', 'энгельсский', 'саратовец', 'саратовый', 'балашовский', 'саратов', 'сарат',
                        'градус', 'ребенок', 'ребёнок', 'ый', 'находится', 'находиться', 'сильный', 'перй', 'ул', 
                        'прогнозируть', 'красноярский', 'самар', 'толльятъ', 'тольять', 'толльять','тольятть', 'ярин',
                        
                        
                        'найти', 'pgrunews', 'хороший', 'занятие', 'ныжный', 'номер', 'отлично', 'лс',
                        'состояние', 'цена', 'вид', 'руба', 'зеркадать', 'ть', 'торг', 'нужный', 'муниципальный',
                        'округ', 'ип', 'ссылка', 'задний', 'друг', 'замить', 'бесплатный', 
                        'ульяновскаяобласть', 'ульяновск', 'ульяновский', 'саранск', 'саранска', 'мордовие', 'рм', 'рма', 'мордовия', 
                        'мордовский', 'заец', 'idюрий', 'главамарийэл', 'марий', 'эл', 'марийэл', 'эть', 'васил', 'чурин', 'кировский', 
                        'кировскаяобласть', 'вятский', 'мельниченко', 'месяц', 'оренбургнуть', 'объясняемрф', 'провести', 'инвестор', 
                        'вести', 'реализация', 'башкортостанный', 'радий', 'подписать', 'проект', 'пермский', 'пермскийкрай', 'край', 
                        'прикамья', 'краевой', 'задача', 'важно', 'оренбуржец', 'оренбург', 'новость', 'подчеркнуть', 'оренбуржье', 
                        'оренбургский', 'оренбургскаяобласть', 'поддержка', 'часть', 'км', 'валерийрадаеть', 'олегнуть', 'должен', 
                        'около', 'рассказать', 'глава', 'губернатор', 'развитие', 'январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 
                        'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь', 'город', 'ecom', 'казань', 'подробность', 'подробный', 
                        'радия', 'процент', 'уф', 'часть', 'вопрос', 'делать', 'сделать', 'благодаря', 'участие', 'пройти', 'идти', 'создать', 
                        'создавать', 'дать',  'рамка', 'место', 'первый', 'получить', 'удмуртия', 'радай', 'юлие', 'пензенский', 'пенза', 
                        'пензенскаяобласть', 'новый', 'лучший', 'самый', 'работа', 'рабочий', 'работать', 'региональный', 'нижегородскаяобладать', 
                        'clubнижегородский', 'нижегородскаяобласть', 'нижегородский', 'нижний', 'новгород', 'чувашия', 'чувашие', 'обть', "бaшҡортостать", 
                        "бaшҡортостан", 'командахабиров', 'рб', 'миллиард', 'башкирия', 'башкортостан', 'башкортостана', 'мый', 'аный', 'мухаметшина', 
                        'мухаметшин', 'реть', 'рф', 'день', 'отметить', 'число', 'миллион', 'ход', 'президент','страна', 'тысяча', 'рубль', 'доллар', 
                        'район', 'итог', 'татарстан', 'татарстать', 'российский', 'ма', 'область', 'республика', 'саратовский', 'татарстан', 'татарстана', 
                        'самарский','экономический', 'экономика', 'регион', 'год', "миннихан", "рт", "россия", "рустам", "руст", 'россия', 'конкурентоспособность', 
                        'инновация', 'инвестиция', 'инвестиционный', 'рустамминнихан', 'дмитрий', 'азаров', 'саратовскаяобласть', 'саратовская', 'самарскаяобласть', 
                        'азар', 'стать', 'rn«', 'твой', 'сих', 'ком', 'свой',
                        'слишком', 'нами', 'всему', 'будь', 'саму', 'чаще', 'ваше', 'наш', 'затем', 'еще', 'наши', 'ту', 'каждый',
                        'мочь', 'весь', 'этим', 'наша', 'своих', 'оба', 'который', 'зато', 'те', 'вся', 'ваш', 'такая', 'теми', 'ею', 'нередко',
                        'также', 'чему', 'собой', 'нем', 'вами', 'ими', 'откуда', 'такие', 'тому', 'та', 'очень', 'нему',  'д',
                        'алло', 'оно', 'кому', 'тобой', 'таки', 'мой', 'нею', 'ваши', 'ваша', 'кем', 'мои',
                        'однако', 'сразу', 'свое', 'ними', 'всё', 'неё', 'тех', 'хотя', 'всем', 'тобою', 'тебе', 'одной', 'другие',
                        'буду', 'моё', 'своей', 'такое', 'всею', 'будут', 'своего', 'кого', 'свои', 'мог', 'нам', 'особенно', 'её',
                        'наше', 'кроме', 'вообще', 'вон', 'мною', 'никто', 'это', 'изза', 'именно', 'поэтому', 'будьт', 'являться', 'чувашский', 'тыса', 'смочь', 'ваший', 'гльба', 'ать', 'уть', 'ивать', 'ольги', 'пенз', 'ер', 'иметь', 'олегнуть', 'сг', 'например', 'сообщить', 'сообщать', 'среди', 'нть', 'пер', 'зспермь', 'края', 'ради', 'назвать', 'важный')
  extra_stop_words <- stop_words_expanded
  stopwords_combined <- paste(c(stopwords("russian"), extra_stop_words,
                                tolower(male_names_rus$V1),
                                tolower(male_surnames_rus$V1),
                                tolower(female_names_rus$V1)), collapse = "|")
}

stopwords_combined <- load_stopwords()

ui <- fluidPage(
  titlePanel("Анализ регионов по разным периодам"),
  tabsetPanel(id = "tabs",
              tabPanel("Период 1",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file1", "Выберите Excel файл 1", accept = ".xlsx"),
                           actionButton("analyze1", "Анализировать файл 1"),
                           
                         ),
                         mainPanel(
                           plotOutput("barPlot1"),
                           wordcloud2Output("wordcloud1"),
                           #plotOutput("wordcloud1"),
                           tableOutput("wordTable1")
                         )
                       )
              ),
              tabPanel("Период 2",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file2", "Выберите Excel файл 2", accept = ".xlsx"),
                           actionButton("analyze2", "Анализировать файл 2"),
                           
                         ),
                         mainPanel(
                           plotOutput("barPlot2"),
                           wordcloud2Output("wordcloud2"),
                           tableOutput("wordTable2")
                         )
                       )
              ),
              tabPanel("Период 3",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file3", "Выберите Excel файл 3", accept = ".xlsx"),
                           actionButton("analyze3", "Анализировать файл 3"),
                           
                         ),
                         mainPanel(
                           plotOutput("barPlot3"),
                           wordcloud2Output("wordcloud3"),
                           tableOutput("wordTable3")
                         )
                       )
              ),
              tabPanel("Сравнить файлы",
                       
                       actionButton("compareFilesBtn", "Сравнить проанализированные файлы"),
                       tableOutput("compareFilesTable"),
                       plotOutput("dynamicPlotAll"),
                       plotOutput("dynamicPlotLimited")
              )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  files_preprocessed_data <- reactiveValues()
  clean_corpus <- function(corpus_to_use){  # все команды этой функции совпадают с соотв-ми командами алгоритма для 14 регионов
    corpus_to_use %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace) %>%
      tm_map(content_transformer(function(x) iconv(x, to='UTF-8'))) %>%
      tm_map(removeNumbers) %>%
      tm_map(content_transformer(tolower)) 
  }
  get_preprocessed_texts_word_list <- function(file) {   # все команды этой функции совпадают с соотв-ми командами алгоритма для 14 регионов
    req(file)
    input_data <- as.data.frame(read_excel(file$datapath, col_names = FALSE)) 
    load_stopwords()
    corp_city_df <- clean_corpus(VCorpus(VectorSource(input_data)))
    corp_city_df[["1"]][["content"]] <- gsub("[\U{1F600}-\U{1F64F}\U{1F300}-\U{1F5FF}\U{1F680}-\U{1F6FF}\U{1F1E0}-\U{1F1FF}\U{2500}-\U{2BEF}\U{2702}-\U{27B0}\U{24C2}-\U{1F251}\U{1f926}-\U{1f937}\U{10000}-\U{10ffff}\u{2640}-\u{2642}\u{2600}-\u{2B55}\u{200d}\u{23cf}\u{23e9}\u{231a}\u{fe0f}\u{3030}\U{00B0}\U{20BD}]", "", corp_city_df[["1"]][["content"]], perl = TRUE)
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
    tmp <- str_replace_all(tmp, 'правительстворазвитие', 'развитие')
    # tmp <- str_replace_all(tmp, 'правительстворб', 'правительство')
    tmp <- str_replace_all(tmp, 'цифровый', 'цифровой')
    tmp <- str_replace_all(tmp, 'научныймощность', 'научный мощность')
    tmp <- str_replace_all(tmp, 'club', '')
    tmp <- str_replace_all(tmp, 'ветр', 'ветер')
    tmp <- str_replace_all(tmp, 'школьник', 'школа')
    tmp <- str_replace_all(tmp, 'школьный', 'школа')
    # tmp <- str_replace_all(tmp, 'правительствомарийэть', 'правительство')
    tmp <- str_replace_all(tmp, 'молние', 'молния')
    # tmp <- str_replace_all(x$lemma, paste("\\b(", stopwords_combined, ")\\b"), "") # без этого остается часто повторяющееся слово "правительство"
    
    tmp <- str_replace_all(tmp, 'полицияроссия', 'полиция')
    tmp <- str_replace_all(tmp, 'осуждеть', 'осуждать')
    tmp <- str_replace_all(tmp, 'умвд', 'мвд') 
    tmp <- tmp[!grepl("http|vk", tmp)]  # Удаление терминов, содержащих http или vk
    tmp <- tmp[sapply(tmp, nchar) > 0]
    return(tmp)
  }
  analyze_and_render <- function(file_input, plot_output, table_output, wordcloud_output) {   # все команды этой функции совпадают с соотв-ми командами алгоритма для 14 регионов
    preprocessed_texts_word_list <- get_preprocessed_texts_word_list(file_input)
    d <- as.data.frame(sort(table(preprocessed_texts_word_list), decreasing = TRUE))
    colnames(d) <- c("word", "freq")
    word_freq <- d
    d$tf <- d$freq / nrow(d)
    output[[plot_output]] <- renderPlot({
      ggplot(word_freq[1:10, ], aes(x = reorder(word, freq), y = freq)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Наиболее часто встречающиеся слова", x = "Слова", y = "Частота встречаемости") +
        theme_gray(base_size = 18)
    })
    output[[table_output]] <- renderTable({
      colnames(word_freq) <- c("Слово", "Частота встречаемости слова в корпусе текстов")
      head(word_freq, 10)
    })
    # output[[wordcloud_output]] <- renderPlot({
    #     wordcloud(d$word, d$freq, colors=brewer.pal(8, "Dark2"))
    #   })
    output[[wordcloud_output]] <- renderWordcloud2({
      wordcloud2a(word_freq, size = 0.45)
    })
    return(d)
  }
  
  observeEvent(input$analyze1, {
    files_preprocessed_data[["df_1"]] <- analyze_and_render(input[["file1"]], "barPlot1", "wordTable1", "wordcloud1")
  })
  observeEvent(input$analyze2, {
    files_preprocessed_data[["df_2"]] <- analyze_and_render(input[["file2"]], "barPlot2", "wordTable2", "wordcloud2")
  })
  observeEvent(input$analyze3, {
    files_preprocessed_data[["df_3"]] <- analyze_and_render(input[["file3"]], "barPlot3", "wordTable3", "wordcloud3")
  })
  observeEvent(input[["compareFilesBtn"]], {
    d_all <- Filter(Negate(is.null), list(files_preprocessed_data[["df_1"]], files_preprocessed_data[["df_2"]], files_preprocessed_data[["df_3"]])) 
    cos.mat <- NULL
    if (length(d_all) == 1) {
    }
    if (length(d_all) == 2) {
      d_all <- full_join(d_all[[1]], d_all[[2]], by='word')
      d_all <- d_all %>% replace(is.na (.), 0)
      tf_idf <- select(d_all, 'word', 'freq.x', 'tf.x', 'freq.y','tf.y')
      names(tf_idf) <- c('word', 'freq1', 'tf1', 'freq2', 'tf2')
      tdm_df <- select(d_all, 'word', 'freq.x', 'freq.y')
      names(tdm_df) <- c('word', 'freq1', 'freq2')
      tdm_df <- tdm_df %>% mutate(num_of_occurrences = rowSums(select(tdm_df, 'freq1', 'freq2') != 0))
      tdm_df <- tdm_df %>% mutate(idf = log(4 / (1 + num_of_occurrences) + 1))
      
      tdm_df_with_dynamism <- tdm_df
      tdm_df_with_dynamism$freq_all <- tdm_df_with_dynamism$freq1 + tdm_df_with_dynamism$freq2
      # ???
      tdm_df_with_dynamism$dynamism <- (tdm_df_with_dynamism$freq2 - tdm_df_with_dynamism$freq1) / ifelse(tdm_df_with_dynamism$freq1 != 0, tdm_df_with_dynamism$freq1, 1)
      
      # Средний абсолютный прирост
      tdm_df_with_dynamism$dynamism <- (tdm_df_with_dynamism$freq2 - tdm_df_with_dynamism$freq1) / 2
      # Средний коэффициент роста (Средний темп роста)
      # tdm_df_with_dynamism$dynamism <- sqrt((tdm_df_with_dynamism$freq3 / ifelse(tdm_df_with_dynamism$freq1 != 0, tdm_df_with_dynamism$freq1, 1)))
      
      
      tf_idf <- tf_idf %>% mutate(num_of_occurrences = tdm_df$num_of_occurrences)
      tf_idf <- tf_idf %>% mutate(idf = tdm_df$idf)
      tf_idf <- tf_idf %>% mutate(tf_idf1 = tf1 * idf)
      tf_idf <- tf_idf %>% mutate(tf_idf2 = tf2 * idf)
      tf_idf_only <- select(tf_idf, 'tf_idf1', 'tf_idf2')
      names(tf_idf_only) <- c("Период 1", "Период 2")
      cos.mat <- cosine(as.matrix(tf_idf_only))  # Removes the first column for cosine calculation
    }
    if (length(d_all) == 3) {
      d_all <- full_join(full_join(d_all[[1]], d_all[[2]], by='word'), d_all[[3]], by='word')
      d_all <- d_all %>% replace(is.na (.), 0)
      tf_idf <- select(d_all, 'word', 'freq.x', 'tf.x', 'freq.y','tf.y', 'freq', 'tf')
      names(tf_idf) <- c('word', 'freq1', 'tf1', 'freq2', 'tf2', 'freq3', 'tf3')
      tdm_df <- select(d_all, 'word', 'freq.x', 'freq.y', 'freq')
      names(tdm_df) <- c('word', 'freq1', 'freq2', 'freq3')
      tdm_df <- tdm_df %>% mutate(num_of_occurrences = rowSums(select(tdm_df, 'freq1', 'freq2', 'freq3') != 0))
      tdm_df <- tdm_df %>% mutate(idf = log(4 / (1 + num_of_occurrences) + 1))
      
      tdm_df_with_dynamism <- tdm_df
      tdm_df_with_dynamism$freq_all <- tdm_df_with_dynamism$freq1 + tdm_df_with_dynamism$freq2 + tdm_df_with_dynamism$freq3
      # ???
      # tdm_df_with_dynamism$dynamism <- (tdm_df_with_dynamism$freq3 - tdm_df_with_dynamism$freq1 + 1) / (tdm_df_with_dynamism$freq1 + 1)
    
      # Средний абсолютный прирост
      tdm_df_with_dynamism$dynamism <- (tdm_df_with_dynamism$freq3 - tdm_df_with_dynamism$freq1) / 2
      # Средний коэффициент роста (Средний темп роста)
      # tdm_df_with_dynamism$dynamism <- sqrt((tdm_df_with_dynamism$freq3 / ifelse(tdm_df_with_dynamism$freq1 != 0, tdm_df_with_dynamism$freq1, 1)))
      
      
      
      tf_idf <- tf_idf %>% mutate(num_of_occurrences = tdm_df$num_of_occurrences)
      tf_idf <- tf_idf %>% mutate(idf = tdm_df$idf)
      tf_idf <- tf_idf %>% mutate(tf_idf1 = tf1 * idf)
      tf_idf <- tf_idf %>% mutate(tf_idf2 = tf2 * idf)
      tf_idf <- tf_idf %>% mutate(tf_idf3 = tf3 * idf)
      tf_idf_only <- select(tf_idf, 'tf_idf1', 'tf_idf2', 'tf_idf3')
      names(tf_idf_only) <- c("Период 1", "Период 2", "Период 3")
      cos.mat <- cosine(as.matrix(tf_idf_only))
    }
    
    # ifelse(max(tdm_df_with_dynamism$freq_all) != 0, max(tdm_df_with_dynamism$freq_all), 1)  значит следующее.
    # Если max(tdm_df_with_dynamism$freq_all) != 0, то вернуть max(tdm_df_with_dynamism$freq_all),
    # иначе вернуть 1.
    tdm_df_with_dynamism$freq_all_normalized <- (tdm_df_with_dynamism$freq_all) / ifelse(max(tdm_df_with_dynamism$freq_all) != 0, max(tdm_df_with_dynamism$freq_all), 1)  
    tdm_df_with_dynamism$dynamism_normalized <- (tdm_df_with_dynamism$dynamism) / ifelse(max(tdm_df_with_dynamism$dynamism) != 0, max(tdm_df_with_dynamism$dynamism), 1)  
    tdm_df_with_dynamism$freq_all_and_dynamism_normalized <- tdm_df_with_dynamism$dynamism_normalized + tdm_df_with_dynamism$freq_all_normalized
    # Сортировка датафрейма по столбцу freq_all_and_dynamism_normalized по убыванию
    tdm_df_with_dynamism <- tdm_df_with_dynamism[order(tdm_df_with_dynamism$freq_all_and_dynamism_normalized, decreasing = TRUE),] 
    
    
    
    output$compareFilesTable <- renderTable({
      cos.mat
    })
    output$dynamicPlotAll <- renderPlot({
      # Вывод всех слов на графике, кроме тех, которые пересекаются
      # При этом подписываются некоторые слова, хотя точки на графике есть для всех слов.
      ggplot(tdm_df_with_dynamism, aes(x = dynamism, y = freq_all, label = word)) +
        geom_point() +
        geom_text_repel(max.overlaps = 10, max.time = 0.2) +
        labs(x = "Динамика", y = "Значимость", title = "Тренд-карта для всех слов") +
        theme_minimal()
    })
    output$dynamicPlotLimited <- renderPlot({
      amount_of_words_in_plot <- 30
      # Вывод графика для amount_of_words_in_plot слов без пересечений слов на графике. 
      # При этом подписываются некоторые слова, хотя точки на графике есть для всех слов.
      
      # Непонятно, какой результат при смещении оси координат нужен
      # min_abs_dynamism <- abs(min(tdm_df_with_dynamism$dynamism))
      # Смещение оси координат так, чтобы все значения динамики были >= 0. 
      # Для этого для всех выводимых слов к значениям динамики 
      # прибавляют модуль минимального значения динамики
      ggplot(tdm_df_with_dynamism[1:amount_of_words_in_plot, ], aes(x = dynamism, y = freq_all, label = word)) +
        geom_point() +
        geom_text_repel(max.overlaps = 40) +
        labs(x = "Динамика", y = "Значимость", title = paste0("Тренд-карта для ", amount_of_words_in_plot, " слов")) +
        theme_classic()
    })
  })
}

shinyApp(ui = ui, server = server)

