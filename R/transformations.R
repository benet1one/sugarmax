
bind <- function(x, lower, upper) {
    l <- is.finite(lower) & is.infinite(upper)
    u <- is.infinite(lower) & is.finite(upper)
    lu <- is.finite(lower) & is.finite(upper)

    x[l] <- exp(x[l]) + lower[l]
    x[u] <- -exp(x[u]) + upper[u]
    x[lu] <- inv_logit(x[lu]) * (upper[lu] - lower[lu]) + lower[lu]

    x
}

unbind <- function(y, lower, upper) {
    l <- is.finite(lower) & is.infinite(upper)
    u <- is.infinite(lower) & is.finite(upper)
    lu <- is.finite(lower) & is.finite(upper)

    y[l] <- log(y[l] - lower[l])
    y[u] <- log(-y[u] + upper[u])
    y[lu] <- logit( (y[lu] - lower[lu]) / (upper[lu] - lower[lu]) )

    y
}

logit <- function(x) {
    log(x / (1 - x))
}
inv_logit <- function(x) {
    1 / (1 + exp(-x))
}

