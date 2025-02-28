# Установка и загрузка необходимых пакетов
install.packages(c("igraph", "ggraph", "tidygraph"))
library(igraph)
library(ggraph)
library(tidygraph)

# Создаем случайный граф (замените своими данными)
set.seed(42)
nodes <- data.frame(name = paste0("Node", 1:150))  # 150 узлов
edges <- data.frame(from = sample(nodes$name, 150, replace = TRUE),
                    to = sample(nodes$name, 150, replace = TRUE))

# Создаем объект графа
graph <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)

# Добавляем кластеры (группы узлов)
V(graph)$group <- cluster_louvain(graph)$membership  

# Визуализация
ggraph(graph, layout = "fr") +  # "fr" — алгоритм Фрухтермана-Рейнгольда
  geom_edge_link(aes(alpha = 0.5), color = "gray") +
  geom_node_point(aes(color = as.factor(group)), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  theme(legend.position = "none")  # Убираем легенду

