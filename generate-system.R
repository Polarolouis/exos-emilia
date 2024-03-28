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
        system_str <- paste(system_str, paste(turn_matrix_row_in_system_line(M[i, ], b[i]), "\\\\"), collapse = " \\\\ ")
    }
    system_str <- paste(system_str, "\\end{cases}", sep = " ")
    return(system_str)
}

#' Cette fonction écrit l'équation i du système
turn_matrix_row_in_system_line <- function(M_i, b_i) {
    # Replace zero coefficients with spaces and generate variable terms
    coeffs <- sapply(M_i, function(coeff) ifelse(coeff == 0, " ", coeff))
    variables <- sapply(seq_len(length(M_i)), function(idx) ifelse(M_i[idx] == 0, " ", paste0("x_", idx)))
    
    # Combine coefficients and variables with appropriate spacing
    equation_terms <- mapply(function(coeff, var) ifelse(coeff != " ", paste0("&", coeff, var, "&"), "& & &"), coeffs, variables)
    equation <- paste(equation_terms[equation_terms != "& & &"], collapse = " + ")
    
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