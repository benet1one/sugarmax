

f <- function(x, a, b = 2) {
    asdf <- (rowSums(x) - a)
    sum(asdf) - (b - 3)^2
}

problem <- objective(f) |>
    variable(x, dimensions = c(2, 3), upper = 10) |>
    variable(a, dimensions = 2, lower = 4:3) |>
    maximize()

print(problem)


problem <- objective(f) |>
    variable(x, dimensions = c(2, 3), upper = 10) |>
    variable(a, dimensions = 2, lower = 4:3) |>
    variable(b, dimensions = 1, lower = 1, upper = 8) |>
    maximize()

print(problem)
