
test_that("main", {
    one <- 1

    f <- function(x, a, b = 2) {
        y <- (rowSums(x) - a)^4
        sum(y) + (a[1] - 25)^2 + one/b
    }

    s <- objective(f) |>
        variable(x, dimensions = c(2, 3), upper = 10) |>
        variable(a, dimensions = 2, lower = c(0, 40)) |>
        minimize(max_iter = 10e3)

    print(s)

    expect_equal(
        do.call(f, s$solution)[1],
        s$objective_value
    )
    expect_equal(
        rowSums(s$solution$x) |> round(),
        c(25, 30)
    )
    expect_equal(
        s$solution$a |> round() |> c(),
        c(25, 40)
    )


    sb <- objective(f) |>
        variable(x, dimensions = c(2, 3), upper = 10) |>
        variable(a, dimensions = 2, lower = c(0, 40)) |>
        variable(b, lower = 0, upper = 4) |>
        minimize(max_iter = 10e3)

    print(sb)

    expect_equal(
        do.call(f, sb$solution)[1],
        sb$objective_value
    )
    expect_equal(
        sb$solution$b[1],
        4
    )

    expect_error(
        objective(f) |> variable(d),
        "`d` is not a parameter in the objective function"
    )
})
