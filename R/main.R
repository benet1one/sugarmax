
#' Define an objective function to optimize.
#'
#' @param fun Function with an arbitrary number of parameters. Parameters with default
#' values don't need to be optimized, but they can if they're added as [variable()]s.
#'
#' @returns A `sugarmax_problem` object.
#' @export
#'
#' @examples
objective <- function(fun) {
    stopifnot(rlang::is_function(fun))
    list(fun = fun, variables = list(), last_ind = 0L) |>
        structure(class = "sugarmax_problem")
}

#' Add a variable to optimize.
#'
#' Parameters in the objective function that aren't added will be left to their default value.
#'
#' @param .problem Problem created with [objective()].
#' @param symbol Name of the parameter in the objective function. Does not need to be a string.
#' @param dimensions Integer vector of length one or more giving the maximal indices in each dimension.
#' For instance, a scalar has a dimension of `1`, a vector of N elements has a dimension of `N`,
#' and an NxM matrix has a dimension of `c(N, M)`.
#' @param lower Lower bound for the variable. Can be a scalar or an array of `dim = dimensions`.
#' @param upper Upper bound for the variable. Can be a scalar or an array of `dim = dimensions`.
#' @param inits Initial value for the variable. Can be a scalar or an array of `dim = dimensions`.
#'
#' @returns A `sugarmax_problem` object.
#' @export
#'
#' @examples
variable <- function(.problem, symbol, dimensions = 1, lower = -Inf, upper = +Inf, inits = NA) {
    symbol <- rlang::ensym(symbol) |> format()
    stopifnot(
        symbol %in% methods::formalArgs(.problem$fun),
        rlang::is_integerish(dimensions, finite = TRUE),
        is.numeric(lower),
        is.numeric(upper),
        is.numeric(inits) || all(is.na(inits))
    )

    if (length(lower) == 1L)
        lower <- array(lower, dim = dimensions)
    if (length(upper) == 1L)
        upper <- array(upper, dim = dimensions)
    if (length(inits) == 1L)
        inits <- array(inits, dim = dimensions)

    lower <- as.array(lower)
    upper <- as.array(upper)
    inits <- as.array(inits)

    # browser()

    stopifnot(
        all(dim(lower) == dimensions),
        all(dim(upper) == dimensions),
        all(dim(inits) == dimensions)
    )

    spec_inits <- !is.na(inits)

    if (any(spec_inits)) {
        stopifnot(
            inits[spec_inits] >= lower[spec_inits],
            inits[spec_inits] <= upper[spec_inits]
        )
    }


    n <- prod(dimensions)

    .problem$variables[[symbol]] <- list(
        symbol = symbol,
        dimensions = dimensions,
        lower = lower,
        upper = upper,
        inits = inits,
        indices = 1:n + .problem$last_ind
    )

    .problem$last_ind <- .problem$last_ind + n
    .problem
}

variable_environment <- function(x, variables) {
    lapply(variables, \(v) array(x[v$indices], dim = v$dimensions))
}

#' Optimize a problem.
#'
#' Minimize or maximize a problem.
#'
#' @rdname solve
#'
#' @param .problem Problem created with [objective()] and [variable()].
#' @param rel_tol Relative convergence tolerance. The algorithm stops if it is unable to improve the
#' objective value by a factor of `reltol * (abs(val) + reltol)` at a step.
#' @param abs_tol The absolute convergence tolerance.
#' The algorithm stops if the objective value is better than `abs_tol`.
#' @param method The method to be used. See details in [stats::optim()].
#' @param ... Other arguments passed to [stats::optim()].
#'
#' @returns A list with elements:
#' - `$internal`: The output of [stats::optim()].
#' - `$objective_value`: Numeric, the value of the function at the optimum.
#' - `$solution`: Named list of numbers, the value of each variable at the optimum.
#' @export
#'
#' @examples
minimize <- function(.problem,
                     rel_tol = sqrt(.Machine$double.eps), abs_tol = NULL,
                     method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
                     ...) {
    .solve(
        .problem,
        .maximize = FALSE,
        rel_tol = rel_tol,
        abs_tol = abs_tol,
        method = method,
        ...
    )
}
#' @rdname solve
maximize <- function(.problem,
                     rel_tol = sqrt(.Machine$double.eps), abs_tol = NULL,
                     method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
                     ...) {
    .solve(
        .problem,
        .maximize = TRUE,
        rel_tol = rel_tol,
        abs_tol = abs_tol,
        method = method,
        ...
    )
}


.solve <- function(.problem, .maximize = FALSE,
                   method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
                   gr = NULL, hessian = FALSE,
                   rel_tol = sqrt(.Machine$double.eps),
                   abs_tol = NULL, ...) {

    vars <- .problem$variables
    lower <- sapply(vars, \(v) v$lower) |> unlist()
    upper <- sapply(vars, \(v) v$upper) |> unlist()
    inits <- sapply(vars, \(v) v$inits) |> unlist()

    spec_inits <- !is.na(inits)
    unbound_inits <- rep(0, .problem$last_ind)
    unbound_inits[spec_inits] <- unbind(inits[spec_inits], lower, upper)

    fn <- function(unbound_x) {
        bound_x <- bind(unbound_x, lower, upper)
        env <- variable_environment(bound_x, vars)
        do.call(.problem$fun, env)
    }

    control <- list(
        fnscale = if (.maximize) -1  else +1,
        reltol = rel_tol,
        abstol = abs_tol,
        ...
    )

    opt <- stats::optim(fn = fn, par = unbound_inits, control = control,
                        gr = gr, hessian = hessian, method = method)

    solution <- opt$par |>
        bind(lower, upper) |>
        variable_environment(vars)

    list(
        internal = opt,
        objective_value = opt$value,
        solution = solution
    )
}
