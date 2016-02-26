#==========================================
#
#  Odds Ratio
#
#===========================================
#'
#'@source http://global.oup.com/us/companion.websites/fdscontent/uscompanion/us/static/
#'companion.websites/9780195089653/TextFiles/oddsratioWaldproc.txt
#'
oddsratioWald.proc <- function(n00, n01, n10, n11, alpha = 0.05){
    #
    #  Compute the odds ratio between two binary variables, x and y,
    #  as defined by the four numbers nij:
    #
    #    n00 = number of cases where x = 0 and y = 0
    #    n01 = number of cases where x = 0 and y = 1
    #    n10 = number of twcases where x = 1 and y = 0
    #    n11 = number of cases where x = 1 and y = 1
    #
    OR <- (n00 * n11)/(n01 * n10)
    #
    #  Compute the Wald confidence intervals:
    #
    siglog <- sqrt((1/n00) + (1/n01) + (1/n10) + (1/n11))
    zalph <- qnorm(1 - alpha/2)
    logOR <- log(OR)
    loglo <- logOR - zalph * siglog
    loghi <- logOR + zalph * siglog
    #
    ORlo <- exp(loglo)
    ORhi <- exp(loghi)
    #
    oframe <- data.frame(LowerCI = ORlo, OR = OR, UpperCI = ORhi, alpha = alpha)
    oframe
}


#==========================================
#
#  twoXtwo
#
#===========================================
#' @title Crée un tableau 2 x 2
#' @description Crée un tableau 2 x 2 à partir de deux vecteurs x et y
#' @usage twoXtwo(x, y, dnames=c("x", "y"))
#' @source http://www.r-bloggers.com/sometimes-table-is-not-the-answer-a-faster-2x2-table/
#' @param x un vecteur
#' @param y un vecteur
#' @dnames vecteur de 2 éléments. Nom des vecteurs x et y
#' @return une table 2 x 2
#' @examples v1 <- c(1,1,1,0,0,1,0,1,0,1)
#'           v2 <- c(1,1,0,0,0,1,1,1,0,1)
#'           twoXtwo(v1,v2)
#' @export
#' 
twoXtwo = function(x, y, dnames=c("x", "y")){
    tt <- sum( x &  y)
    tf <- sum( x & !y)
    ft <- sum(!x &  y)
    ff <- sum(!x & !y)
    tab = matrix(c(ff, tf, ft, tt), ncol=2)
    n = list(c("FALSE", "TRUE"), c("FALSE", "TRUE"))
    names(n) = dnames
    dimnames(tab) = n
    tab = as.table(tab)
    dim
    tab
}


#==========================================
#
#  Barnard test
#
#===========================================
#' @title Réalise le test de Barnard pour tableau 2x 2
#' @description Le test de barnard rdt une alternative plus précise au test de Fisher exact.
#' @source http://www.r-statistics.com/wp-content/uploads/2010/02/Barnard.R.txt
#' 
Barnard <- function(data, Tbx = 100, to.plot = TRUE, 
                    to.print = TRUE) {
    #Examples:
    #Convictions <-matrix(c(2, 10, 15, 3), nrow = 2, dimnames =
    #   list(c('Dizygotic', 'Monozygotic'), c('Convicted', 'Not
    #   convicted')))
    #Barnard(Convictions)
    #Compare this to:
    #fisher.test(Convictions, alternative = 'less')
    #This table did not work on the previous code due to
    #   computational difficulty:
    #Barnard(matrix(c(10,1,200,5),2,2))
    if (sum(dim(data) == c(2, 2)) != 2) {
        stop("Input matrix must be a 2x2 matrix")
    }
    if (sum(is.finite(data)) != 4 || !is.numeric(data)) {
        stop("All X values must be numeric and finite")
    }
    if (sum(data[1, ]) <= 0 || sum(data[2, ]) <= 0 || sum(data[, 
                                                               1]) <= 0 || sum(data[, 2]) <= 0) {
        stop("Need at least one observation in each row or column")
    }
    Cs <- c(sum(data[, 1]), sum(data[, 2]))
    N <- sum(Cs)
    I <- matrix(0, Cs[1] + 1, Cs[2] + 1)
    for (i in 1:(Cs[1] + 1)) {
        I[i, ] <- rep(i, 1, Cs[2] + 1) - 1
    }
    J <- matrix(0, Cs[1] + 1, Cs[2] + 1)
    for (j in 1:(Cs[2] + 1)) {
        J[, j] <- t(rep(j, 1, Cs[1] + 1) - 1)
    }
    TX <- (I/Cs[1] - J/Cs[2])/sqrt(((I + J)/N) * (1 - ((I + J)/N)) * 
                                       sum(1/Cs))
    TX[which(is.na(TX))] = 0
    #Wald Statistic:
    TXO <- abs(TX[data[1] + 1, data[3] + 1])
    idx <- matrix(0, Cs[1] + 1, Cs[2] + 1)
    idx[which(TX >= TXO)] <- 1
    B <- Cs + 1
    npa <- seq(1e-04, 0.9999, length = Tbx)
    LP <- log(npa)
    ALP <- log(1 - npa)
    E <- list(I + J)
    for (i in 2:Tbx) {
        E[[i]] <- E[[1]]
    }
    F <- list(N - E[[1]])
    for (i in 2:Tbx) {
        F[[i]] <- F[[1]]
    }
    CF <- list(sum(lgamma(B)) - (lgamma(I + 1) + lgamma(J + 1) + 
                                     lgamma(B[1] - I) + lgamma(B[2] - J)))
    for (i in 2:Tbx) {
        CF[[i]] <- CF[[1]]
    }
    replaced1 <- list(0)
    for (i in 1:Tbx) {
        replaced1[[i]] <- matrix(LP[i], Cs[1] + 1, Cs[2] + 1)
    }
    replaced2 <- list(0)
    for (i in 1:Tbx) {
        replaced2[[i]] <- matrix(ALP[i], Cs[1] + 1, Cs[2] + 1)
    }
    S <- list(0)
    for (i in 1:Tbx) {
        S[[i]] <- exp(CF[[i]] + E[[i]] * replaced1[[i]] + F[[i]] * 
                          replaced2[[i]])
    }
    replaced3 <- list(0)
    for (i in 1:Tbx) {
        replaced3[[i]] <- idx
    }
    Snew <- {
    }
    for (i in 1:Tbx) {
        Snew <- c(Snew, S[[i]])
    }
    nidx <- (Cs[1] + 1) * (Cs[2] + 1)
    dummy1 <- {
    }
    for (i in 1:Tbx) {
        dummy1 <- c(dummy1, Snew[which(replaced3[[i]] > 0) + 
                                     nidx * (i - 1)])
    }
    cols <- sum(idx[idx == 1])
    dummy2 <- matrix(0, cols, Tbx)
    for (i in 1:Tbx) {
        dummy2[, i] <- dummy1[(cols * i):(cols * (i + 1) - 1) - 
                                  (cols - 1)]
    }
    P <- {
    }
    for (i in 1:Tbx) {
        P <- c(P, sum(dummy2[, i]))
    }
    #1-tailed p-value:
    PV1 <- max(P)
    #2-tailed p-value:
    PV2 <- min(2 * PV1, 1)
    #Nuisance parameter:
    np <- npa[P == PV1]
    if (to.print) {
        cat("\n", noquote(paste(paste(paste("2x2 matrix Barnard's exact test:", 
                                            Tbx, seq = ""), paste(paste(Cs[1] + 1, "x", sep = ""), 
                                                                  Cs[2] + 1, sep = ""), sep = ""), "tables were evaluated")))
        cat("\n", noquote("-----------------------------------------------------------"))
        cat("\n", noquote(c("Wald statistic = ", format(TXO, 
                                                        digits = 5, scientific = FALSE))))
        cat("\n", noquote(c("Nuisance parameter = ", format(np, 
                                                            digits = 5, scientific = FALSE))))
        cat("\n", noquote(c("p-values: ", "1-tailed = ", format(PV1, 
                                                                digits = 5, scientific = FALSE), "2-tailed = ", format(PV2, 
                                                                                                                       digits = 5, scientific = FALSE))))
        cat("\n", noquote("-----------------------------------------------------------"), 
            "\n", "\n")
    }
    if (to.plot) {
        plot(npa, P, type = "l", main = "Barnard's exact P-value", 
             xlab = "Nuisance parameter", ylab = "P-value")
        points(np, PV1, col = 2)
    }
}
#Examples:
# Convictions <-matrix(c(2, 10, 15, 3), nrow = 2, dimnames
#   = list(c('Dizygotic', 'Monozygotic'), c('Convicted',
#   'Not convicted')))
# Barnard(Convictions)
# fisher.test(Convictions, alternative = 'less') 