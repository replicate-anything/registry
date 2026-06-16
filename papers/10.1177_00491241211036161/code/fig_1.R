# Example figure — Bounding Causes of Effects With Mediators
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.1177_00491241211036161
# Run from the paper's code/ folder: Rscript fig_1.R

library(ggplot2)

make_fig_1 <- function(data = NULL) {
  if (!is.null(data)) {
    return(data)
  }

  markov <- function(t, r) {
    matrix(c(t - r + 1, 1 - t - r, 1 - t + r, 1 + t + r) / 2, 2, 2)
  }

  bounds_T <- function(T) {
    L <- function(T) max(0, (T[2, 2] - T[1, 2]) / T[2, 2])
    H <- function(T) min(1, T[1, 1] / T[2, 2])

    tau <- T[2, 2] - T[1, 2]
    rho <- T[2, 2] - T[1, 1]

    T2 <- markov(tau^.5, rho / (1 + tau^.5))

    best_het <- function(T) {
      matrix(
        c(
          min(1, T[1, 2] * 2),
          2 * T[1, 2] - min(1, T[1, 2] * 2),
          2 * T[2, 2] - min(1, T[2, 2] * 2),
          min(1, T[2, 2] * 2)
        ),
        2, 2
      )
    }

    best_het_bounds <- function(T) {
      M <- best_het(T)
      M1 <- matrix(c(1 - M[1, ], M[1, ]), 2, 2)
      M2 <- matrix(c(1 - M[2, ], M[2, ]), 2, 2)
      c(
        L = (M[2, 2] / (M[1, 2] + M[2, 2])) * L(M2) +
          (M[1, 2] / (M[1, 2] + M[2, 2])) * L(M1),
        H = (M[2, 2] / (M[1, 2] + M[2, 2])) * H(M2) +
          (M[1, 2] / (M[1, 2] + M[2, 2])) * H(M1)
      )
    }

    rbind(
      simple = c(L(T), H(T)),
      unobserved = c(L(T), L(T)),
      monotone = c(L(T), L(T)),
      two_step_homog = c(L(T2)^2, H(T2^2)),
      inifinte_homog = c(tau^.5 * (1 + rho / (1 - tau)), min(1, tau^(rho / (1 - tau)))),
      best_lower = c(T[1, 1], T[1, 1]),
      best_cov_un = c(L = H(T), H = H(T)),
      best_cov_ob = c(1, 1)
    )
  }

  bound_names <- c(
    "Simple bounds",
    "Unobserved mediators",
    "Monotonic",
    "Two step homogeneous",
    "Infinite step homogeneous",
    "Best (positive) mediator",
    "Best (unobserved) covariate",
    "Best (observed) covariate"
  )

  rho_values <- round(c(-.5, 0, .5), 2)

  bounds_list <- lapply(rho_values, function(rho) {
    do.call(rbind, lapply(c(.1, .25), function(tau) {
      T <- markov(tau, rho)
      bounds <- as.data.frame(bounds_T(T))
      bounds$rho <- as.factor(rho)
      bounds$tau <- as.factor(tau)
      bounds$label <- rownames(bounds)
      bounds
    }))
  })

  df <- do.call(rbind, bounds_list)
  df$names <- rep(bound_names, nrow(df) / length(unique(df$label)))

  structure(
    df,
    bound_names = bound_names,
    rho_values = rho_values
  )
}

format_fig_1 <- function(object) {
  if (inherits(object, "ggplot")) {
    return(object)
  }
  if (!is.data.frame(object)) {
    stop(
      "Expected a bounds data frame from make_fig_1(). ",
      "Rebuild with: Rscript scripts/build_artifacts.R papers/10.1177_00491241211036161",
      call. = FALSE
    )
  }

  bound_names <- c(
    "Simple bounds",
    "Unobserved mediators",
    "Monotonic",
    "Two step homogeneous",
    "Infinite step homogeneous",
    "Best (positive) mediator",
    "Best (unobserved) covariate",
    "Best (observed) covariate"
  )
  rho_values <- round(c(-.5, 0, .5), 2)

  dat <- object
  dat$names <- factor(dat$names, levels = rev(bound_names))
  dat$rho <- paste("rho ==", dat$rho)
  dat$tau <- paste("tau ==", dat$tau)
  dat$rho <- factor(dat$rho, levels = paste0("rho == ", rho_values))

  ggplot2::ggplot(dat, ggplot2::aes(x = names, ymin = L, ymax = H, color = names)) +
    ggplot2::geom_point(ggplot2::aes(y = L), position = ggplot2::position_dodge(.8)) +
    ggplot2::geom_point(ggplot2::aes(y = H), position = ggplot2::position_dodge(.8)) +
    ggplot2::geom_linerange(position = ggplot2::position_dodge(.8)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    ggplot2::coord_flip(ylim = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::labs(
      y = "Probability of causation",
      x = "",
      shape = "\u03C1",
      colour = "\u03C1"
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_grid(rho ~ tau, labeller = ggplot2::label_parsed) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_color_grey(start = .6, end = 0)
}

make_fig_1() |> format_fig_1()
