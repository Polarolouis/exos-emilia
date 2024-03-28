library("matlib")

generate_invertible_matrix <- function(n, intensity = 2, max_tries = 10) {
    M <- matrix(rpois(n^2, intensity), nrow = n)
    try <- 0
    while (is.infinite(determinant(M)$modulus) && try < max_tries) {
        try <- try + 1
        M <- matrix(rpois(n^2, intensity), nrow = n)
    }

    if (try >= max_tries) {
        stop("Pas de matrice correcte trouvée !")
    }

    return(M)
}

system_expr <- function(M, b) {
    n <- nrow(M)
    system_str <- "\\begin{cases}"


    for (i in 1:n) {
        paste()
        system_str <- paste(system_str, paste(turn_matrix_row_in_system_line(M[i, ], b[i]), "\\\\"), collapse = " \\\\ \\n")
    }
    system_str <- paste(system_str, "\\end{cases}", sep = " ")
    return(system_str)
}

#' Cette fonction écrit l'équation i du système
turn_matrix_row_in_system_line <- function(M_i, b_i) {
    # Replace zero coefficients with spaces and generate variable terms
    coeffs <- sapply(M_i, function(coeff) ifelse(coeff == 0, " ", ifelse(coeff == 1, "", coeff)))
    variables <- sapply(seq_len(length(M_i)), function(idx) ifelse(M_i[idx] == 0, " ", paste0("x_", idx)))

    # Combine coefficients and variables with appropriate spacing
    equation_terms <- mapply(function(coeff, var) ifelse(coeff != " ", paste0(coeff, var), " "), coeffs, variables)
    equation <- paste(equation_terms[equation_terms != " "], collapse = " + ")

    # If all coefficients are zero, set equation to "0"
    if (equation == "") {
        equation <- "0"
    }

    # Concatenate the equation with the right-hand side
    equation <- paste(equation, " = ", b_i)

    return(equation)
}

compute_x <- function(M, b) {
    x <- inv(M) %*% b
    return(x)
}

generate_full_system <- function(M = NULL, b = NULL, x = NULL, n = NULL) {
    if (is.null(c(M, b, x, n))) {
        stop("All arguments are null !")
    }

    #  If the matrix is not given
    if (is.null(M) && is.null(b) && !(is.null(x)) && !(is.null(n))) {
        #  We want to generate a system with n equations and a solution x
        M <- generate_invertible_matrix(n)
        b <- M %*% x
    }

    if (is.null(M) && is.null(x) && !(is.null(b)) && !(is.null(n))) {
        #  We want to generate a system with n equations and a right handside b
        M <- generate_invertible_matrix(n)
        x <- solve(a = M, b = b)
    }

    #  If the matrix is given
    if (!(is.null(M)) && is.null(x) && !(is.null(b))) {
        #  Only x is missing
        x <- solve(a = M, b = b)
    }

    if (!(is.null(M)) && is.null(b) && !(is.null(x))) {
        #  Only b is missing
        b <- M %*% x
    }

    return(list(M = M, x = x, b = b))
}

# Define a generic method that transforms an object x in a LaTeX string
as_latex <- function(x, ...) {
    UseMethod("as_latex", x)
}

# Define a class latex for LaTeX expressions
as_latex.character <- function(x) {
    structure(
        paste(x, collapse = " "),
        class = c("latex", "character")
    )
}

# A character string of class latex is rendered in display mode
# Define a knit_print() method for the latex class
knit_print.latex <- function(x, ...) {
    knitr::asis_output(
        paste0("$$", x, "$$")
    )
}

# Now, define a method as_latex for matrix
as_latex.matrix <- function(x, ...) {
    as_latex(c(
        "\\begin{bmatrix}",
        paste(
            x,
            rep(c(rep("&", nrow(x) - 1), "\\\\"), ncol(x)),
            collapse = ""
        ),
        "\\end{bmatrix}"
    ))
}

# Indicate to knitr that matrix are rendered as latex
knit_print.matrix <- function(x, ...) {
    knitr::knit_print(as_latex(t(x)))
}

# Build a knitr inline hook to display inline latex in inline mode
default_inline_hook <- knitr::knit_hooks$get("inline")
knitr::knit_hooks$set(inline = function(x) {
    x <- paste(gsub("\\$\\$", "", x))
    default_inline_hook(x)
})