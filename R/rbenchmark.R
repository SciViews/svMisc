#' R Benchmark 2.6
#'
#' This is a benchmark of base R with 15 tests of various common (matrix)
#' calculations and programming techniques like loops, vector calculation,
#' recursion, etc.
#'
#' @param runs Number of times each test is run (3 by default).
#' @param x A **rbenchmark** object
#' @param ... Further arguments (not used yet)
#'
#' @return An **rbenchmark** object with the timing of all 15 tests.
#' @export
#'
#' @details
#' This code is reworked from the R Benchmark 2.5 adapted by Simon Urbanek
#' (https://mac.r-project.org/benchmarks/) from my initial implementation ,
#' itself inspired from Matlab code by Stephan Steinhaus. In comparison to
#' version 2.5, this one is included in a function and returns a **rbenchmark**
#' objects that prints in a very similar way to the original code. However,
#' only functions from base R packages (including \{stats\} and \{utils\}) are
#' used, where previous versions also used recommended package \{Matrix\} and
#' possibly CRAN package \{SuppDists\}. Expect some slight differences.
#'
#' Some tests in sections I and II use BLAS/LAPACK code. Their results are
#' heavily dependent on the BLAS implementation that you choose. The default R
#' BLAS is single-threaded and is rather slow, but it well tested and certified
#' to be accurate. Use a good multi-threaded BLAS alternative for much improved
#' results (sometimes 10x faster or more), like ATLAS, OpenBLAS, Intel MKL, ...
#' See https://cran.r-project.org/web/packages/gcbd/vignettes/gcbd.pdf. Use
#' `utils::sessionInfo()` to know which BLAS version R currently uses.
#'
#' Beside multi-threaded BLAS, all tests are single-threaded. This benchmark
#' does not test full parallel potential of R. Also, other key aspects like read
#' and write of data on disk of from databases are not tested. As usual, take
#' these artificial benchmarks with a grain of salt: it may not represent the
#' speed of your actual calculations since it depends mainly on the functions
#' you use and on your programming style...
#'
#' @examples
#' \dontrun{
#'   # This can be slow
#'   rbenchmark()
#' }
rbenchmark <- function(runs = 3L) {
  if (!is.numeric(runs) || length(runs) != 1 || runs < 1 || runs > 10)
    stop("runs must be a single integer between 1 and 10")
  runs <- as.integer(runs)

  version <- "2.6"

  times <- rep(0, 15L * runs)
  dim(times) <- c(15L, runs)
  colnames(times) <- 1L:runs
  rownames(times) <- c(
    "random matrix", "matrix^1000", "sorting", "cross-product", "regression",
    "fft", "eigenvalues", "determinant", "cholesky", "inverse",
    "fibonacci", "hilbert", "gcd", "toeplitz", "escoufier"
  )

  options(object.size = 100000000)

  pb <- txtProgressBar(max = 15L * runs, style = 3)

  # I. Matrix calculation

  # (1) random matrix: Creation, transp., deformation of a 2500x2500 matrix
  for (i in 1L:runs) {
    setTxtProgressBar(pb, i)
    a <- 0
    b <- 0
    invisible(gc())
    times[1L, i] <- system.time({
      a <- matrix(rnorm(2500 * 2500) / 10, ncol = 2500, nrow = 2500)
      b <- t(a)
      dim(b) <- c(1250, 5000)
      a <- t(b)
    })[3]
  }

  # (2) matrix^1000: 2400x2400 normal distributed random matrix ^1000
  for (i in 1L:runs) {
    setTxtProgressBar(pb, runs + i)
    a <- abs(matrix(rnorm(2500 * 2500) / 2, ncol = 2500, nrow = 2500))
    b <- 0
    invisible(gc())
    times[2L, i] <- system.time({
      b <- a^1000
    })[3]
  }

  # (3) sorting: Sorting of 7,000,000 random values
  for (i in 1L:runs) {
    setTxtProgressBar(pb, 2 * runs + i)
    a <- rnorm(7000000)
    b <- 0
    invisible(gc())
    times[3L, i] <- system.time({
      b <- sort(a, method = "quick") # Sort is modified in v. 1.5.x
      # And there is now a quick method that better suits this test
    })[3]
  }


  # (4) cross-product: 2800x2800 cross-product matrix (b = a' * a)
  for (i in 1L:runs) {
    setTxtProgressBar(pb, 3 * runs + i)
    a <- rnorm(2800 * 2800)
    dim(a) <- c(2800, 2800)
    b <- 0
    invisible(gc())
    times[4L, i] <- system.time({
      b <- crossprod(a) # Equivalent to: b <- t(a) %*% a
    })[3]
  }

  # (5) regression: Linear regr. over a 3000x3000 matrix (c = a \\ b')
  for (i in 1L:runs) {
    setTxtProgressBar(pb, 4 * runs + i)
    # Was in version 2.5
    #a <- new("dgeMatrix", x = rnorm(2000 * 2000), Dim = as.integer(c(2000, 2000)))
    a <- rnorm(2000 * 2000)
    dim(a) <- c(2000, 2000)
    b <- as.double(1:2000)
    c <- 0
    invisible(gc())
    times[5L, i] <- system.time({
      c <- solve(crossprod(a), crossprod(a, b))
      # Another solution, sometimes slower, sometimes faster
      #c <- lsfit(a, b, intercept = FALSE)$coef
    })[3]

    # This is the old method
    #a <- rnorm(600 * 600)
    #dim(a) <- c(600, 600)
    #b <- 1:600
    #invisible(gc())
    #times[5L, i] <- system.time({
    #  qra <- qr(a, tol = 1e-7)
    #  c <- qr.coef(qra, b)
    #})[3]
  }

  # II. Matrix functions

  # (1) fft: FFT over 2,400,000 random values
  for (i in 1L:runs) {
    setTxtProgressBar(pb, 5 * runs + i)
    a <- rnorm(2400000)
    b <- 0
    invisible(gc())
    times[6L, i] <- system.time({
      b <- fft(a)
    })[3]
  }

  # (2) eigenvalues: Eigenvalues of a 640x640 random matrix
  for (i in 1L:runs) {
    setTxtProgressBar(pb, 6 * runs + i)
    a <- array(rnorm(600 * 600), dim = c(600, 600))
    b <- 0
    # Only needed if using eigen.Matrix(): Matrix.class(a)
    invisible(gc())
    times[7L, i] <- system.time({
      b <- eigen(a, symmetric = FALSE, only.values = TRUE)$Value
      # Rem: on my machine, it is faster than:
      #	 b <- La.eigen(a, symmetric = FALSE, only.values = TRUE, method = "dsyevr")$Value
      #	 b <- La.eigen(a, symmetric = FALSE, only.values = TRUE, method = "dsyev")$Value
      #  b <- eigen.Matrix(a, vectors = FALSE)$Value
    })[3]
  }

  # (3) determinant: Determinant of a 2500x2500 random matrix
  for (i in 1L:runs) {
    setTxtProgressBar(pb, 7 * runs + i)
    a <- rnorm(2500 * 2500)
    dim(a) <- c(2500, 2500)
    #Matrix.class(a)
    b <- 0
    invisible(gc())
    times[8L, i] <- system.time({
      b <- determinant(a, logarithm = FALSE)
    })[3]
  }

  # (4) cholesky: Cholesky decomposition of a 3000x3000 matrix
  for (i in 1L:runs) {
    setTxtProgressBar(pb, 8 * runs + i)
    # Was in version 2.5
    #a <- crossprod(new("dgeMatrix", x = rnorm(3000 * 3000),
    #  Dim = as.integer(c(3000, 3000))))
    # Matrix must be real symmetric positive-definite square matrix
    #a <- forceSymmetric(a)
    a <- runif(3000 * 3000, min = 1, max = 100)
    dim(a) <- c(3000, 3000)
    a <- crossprod(a)
    b <- 0
    invisible(gc())
    times[9L, i] <- system.time({
      b <- chol(a)
    })[3]
  }

  # (5) inverse: Inverse of a 1600x1600 random matrix
  for (i in 1L:runs) {
    setTxtProgressBar(pb, 9 * runs + i)
    # Was in version 2.5
    #a <- new("dgeMatrix", x = rnorm(1600 * 1600),
    #  Dim = as.integer(c(1600, 1600)))
    a <- rnorm(1600 * 1600)
    dim(a) <- c(1600, 1600)
    b <- 0
    invisible(gc())
    times[10L, i] <- system.time({
      b <- solve(a)
      # Rem: faster than
      #b <- qr.solve(a)
    })[3]
  }

  # III. Programmation

  # (1) fibonacci: 3,500,000 Fibonacci numbers calculation (vector calc)
  for (i in 1L:runs) {
    setTxtProgressBar(pb, 10 * runs + i)
    a <- floor(runif(3500000) * 1000)
    b <- 0
    phi <- 1.6180339887498949
    sqrt5 <- sqrt(5)
    invisible(gc())
    times[11L, i] <- system.time({
      b <- (phi^a - (-phi)^(-a)) / sqrt5
    })[3]
  }

  # (2) hilbert: Creation of a 3000x3000 Hilbert matrix (matrix calc)
  for (i in 1L:runs) {
    setTxtProgressBar(pb, 11 * runs + i)
    a <- 3000
    b <- 0
    invisible(gc())
    times[12L, i] <- system.time({
      b <- rep(1:a, a)
      dim(b) <- c(a, a)
      b <- 1 / (t(b) + 0:(a - 1))
      # Rem: this is twice as fast as the following code proposed by R programmers
      # a <- 1:a; b <- 1 / outer(a - 1, a, "+")
    })[3]
  }

  # (3) gcd: Grand common divisors of 400,000 pairs (recursion)
  gcd2 <- function(x, y) {
    if (sum(y > 1.0E-4) == 0) {
      x
    } else {
      y[y == 0] <- x[y == 0]
      Recall(y, x %% y)
    }
  }
  for (i in 1L:runs) {
    setTxtProgressBar(pb, 12 * runs + i)
    a <- ceiling(runif(400000) * 1000)
    b <- ceiling(runif(400000) * 1000)
    c <- 0
    invisible(gc())
    times[13L, i] <- system.time({
      c <- gcd2(a, b) # gcd2() is a recursive function
    })[3]
  }

  # (4) toeplitz: Creation of a 500x500 Toeplitz matrix (loops)
  for (i in 1L:runs) {
    setTxtProgressBar(pb, 13 * runs + i)
    a <- rep(0, 500 * 500)
    dim(a) <- c(500, 500)
    invisible(gc())
    times[14L, i] <- system.time({
      # Rem: there are faster ways to do this
      # but here we want to time loops (500 * 500 'for' loops)!
      for (j in 1:500) {
        for (k in 1:500) {
          a[k, j] <- abs(j - k) + 1
        }
      }
    })[3]
  }

  # (5) escoufier: Escoufier's method on a 45x45 matrix (mixed)
  Trace <- function(y) {# Calculate the trace of a matrix (sum of its diagonal elements)
    sum(diag(y))
  }
  #was: Trace <- function(y) {
  #  sum(c(y)[1 + 0:(min(dim(y)) - 1) * (dim(y)[1] + 1)], na.rm = FALSE)
  #}
  for (i in 1L:runs) {
    setTxtProgressBar(pb, 14 * runs + i)
    x <- abs(rnorm(45 * 45))
    dim(x) <- c(45, 45)
    p <- 0
    vt <- 0
    vr <- 0
    RV <- 0
    vrt <- 0
    Rvmax <- 0
    x2 <- 0
    R <- 0
    Rxx <- 0
    Rxy <- 0
    Ryy <- 0
    Ryx <- 0
    rvt <- 0
    invisible(gc())
    times[15L, i] <- system.time({
      # Calculation of Escoufier's equivalent vectors
      p <- ncol(x)
      vt <- 1:p                                  # Variables to test
      vr <- NULL                                 # Result: ordered variables
      RV <- 1:p                                  # Result: correlations
      vrt <- NULL
      for (j in 1:p) {                           # loop on the variable number
        Rvmax <- 0
        for (k in 1:(p - j + 1)) {               # loop on the variables
          x2 <- cbind(x, x[, vr], x[, vt[k]])
          R <- cor(x2)                           # Correlations table
          Ryy <- R[1:p, 1:p]
          Rxx <- R[(p + 1):(p + j), (p + 1):(p + j)]
          Rxy <- R[(p + 1):(p + j), 1:p]
          Ryx <- t(Rxy)
          rvt <- Trace(Ryx %*% Rxy) / sqrt(Trace(Ryy %*% Ryy) * Trace(Rxx %*% Rxx)) # RV calculation
          if (rvt > Rvmax) {
            Rvmax <- rvt                         # test of RV
            vrt <- vt[k]                         # temporary held variable
          }
        }
        vr[j] <- vrt                             # Result: variable
        RV[j] <- Rvmax                           # Result: correlation
        vt <- vt[vt != vr[j]]                    # reidentify variables to test
      }
    })[3]
  }

  close(pb)
  structure(times, version = version, class = "rbenchmark")
}

#' @export
#' @rdname rbenchmark
print.rbenchmark <- function(x, ...) {
  version <- attr(x, "version")
  runs <- ncol(x)

  timing <- function(x, n, digits = 3L) {
    if (n == 0) {# Total timing
      times <- x
    } else {# Timing of a single test
      times <- x[n, ]
    }
    res <- sum(times) / ncol(x)
    formatC(res, digits = digits, format = "f", width = 4L + digits, flag = " ")
  }

  timing_section <- function(x, n, digits = 3L) {
    runs <- ncol(x)
    if (n == 0) {# Total timing
      start <- 1
      end <- 15
    } else {# Timing of one of the three sections
      start <- 1 + (n - 1) * 5
      end <- start + 4
    }
    times5tests <- sort(apply(x[start:end, ], 1L, mean))
    # We calculate a trimmed mean for all 5 tests (two extremes eliminated)
    res <- exp(mean(log(times5tests[-c(1, length(times5tests))])))
    formatC(res, digits = digits, format = "f", width = 4L + digits, flag = " ")
  }

  cat0 <- function(...) {
    cat(..., sep = "")
  }

  cat0("\n   R Benchmark ", version, "\n")
  cat0("   ===============\n")
  cat0("Number of times each test is run__________________________:  ", runs)
  cat0("\n\n")
  cat0("   I. Matrix calculation\n")
  cat0("   ---------------------\n")
  cat0("Creation, transp., deformation of a 2500x2500 matrix (sec):", timing(x, 1L), "\n")
  cat0("2400x2400 normal distributed random matrix ^1000____ (sec):", timing(x, 2L), "\n")
  cat0("Sorting of 7,000,000 random values__________________ (sec):", timing(x, 3L), "\n")
  cat0("2800x2800 cross-product matrix (b = a' * a)_________ (sec):", timing(x, 4L), "\n")
  cat0("Linear regr. over a 3000x3000 matrix (c = a \\ b')___ (sec):", timing(x, 5L), "\n")
  cat0("                --------------------------------------------------\n")
  cat0("                Trimmed geom. mean (2 extremes eliminated):", timing_section(x, 1L), "\n\n")

  cat0("   II. Matrix functions\n")
  cat0("   --------------------\n")
  cat0("FFT over 2,400,000 random values____________________ (sec):", timing(x, 6L), "\n")
  cat0("Eigenvalues of a 640x640 random matrix______________ (sec):", timing(x, 7L), "\n")
  cat0("Determinant of a 2500x2500 random matrix____________ (sec):", timing(x, 8L), "\n")
  cat0("Cholesky decomposition of a 3000x3000 matrix________ (sec):", timing(x, 9L), "\n")
  cat0("Inverse of a 1600x1600 random matrix________________ (sec):", timing(x, 10L), "\n")
  cat0("                --------------------------------------------------\n")
  cat0("                Trimmed geom. mean (2 extremes eliminated):", timing_section(x, 2L), "\n\n")

  cat0("   III. Programming\n")
  cat0("   ----------------\n")
  cat0("3,500,000 Fibonacci numbers calculation (vector calc)(sec):", timing(x, 11L), "\n")
  cat0("Creation of a 3000x3000 Hilbert matrix (matrix calc) (sec):", timing(x, 12L), "\n")
  cat0("Grand common divisors of 400,000 pairs (recursion)__ (sec):", timing(x, 13L), "\n")
  cat0("Creation of a 500x500 Toeplitz matrix (loops)_______ (sec):", timing(x, 14L), "\n")
  cat0("Escoufier's method on a 45x45 matrix (mixed)________ (sec):", timing(x, 15L), "\n")
  cat0("                --------------------------------------------------\n")
  cat0("                Trimmed geom. mean (2 extremes eliminated):", timing_section(x, 3L), "\n\n")

  cat0("Total time for all 15 tests_________________________ (sec):", timing(x, 0L), "\n")
  cat0("Overall mean (sum of I, II and III trimmed means/", runs, ")_ (sec):", timing_section(x, 0L), "\n")
  #cat0("                      --- End of test ---\n\n")

  invisible(x)
}
