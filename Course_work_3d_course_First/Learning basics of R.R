lst <- list(
  x = c(-5, 2), 
  y = c(TRUE, FALSE), 
  z = c("a", "b")
)
lst[[2]]
lst$x


a <- c(1,2,3,4)

c <- a[a > 0]
c

x <- list("a" = 2.5, "b" = TRUE, "c" = 1:3)
x[[3]][1]

x <- c(20, 10, 15, 23)
y <- c("Aug", "Sep", "Oct", "Nov")
names(x) <- y
x[3:4]

speed_vector <- c("medium", "slow", "slow", "medium", "fast")
factor_speed_vector <- factor(speed_vector, ordered = TRUE, levels = c("slow", "medium", "fast"))

a <- TRUE
b <- 3
a + b # 4

num_matrix <- matrix(1:9, byrow = TRUE, nrow = 3)
rbind(num_matrix)
rowSums(num_matrix)

month <- list(
  x = c("Jun", "May", "Apr"), 
  y = c("Sep", "Jul", "Aug")
)
month[[1]][2]

'f' == "f"

name <- c("Python", "R", "SQL")
type <- c("Practice", "Project", "Lecture")
number <- c(100, 430, 200)
courses <- data.frame(name, type, number)
courses[1:2, 1]

mat1 <- matrix(1:9, ncol = 3, byrow = TRUE)
mat2 <- matrix(9:1, ncol = 3)
mat1 * mat2

a <- 2332
print(a)
