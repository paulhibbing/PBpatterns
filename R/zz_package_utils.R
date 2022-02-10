n_epochs <- function(minute_threshold, epoch_length_sec) {
  minute_threshold * 60 / epoch_length_sec
}

n_minutes <- function(epochs, epoch_length_sec) {
  epochs * epoch_length_sec / 60
}

valid_wear <- function(is_wear, x) {

  stopifnot(!anyNA(is_wear))

  if (is.data.frame(x) & is.character(is_wear)) {

    stopifnot(
      exists(is_wear, x),
      is.logical(x[ ,is_wear])
    )

  } else {

    stopifnot(is.logical(is_wear))

    n <- if (is.data.frame(x)) 1:nrow(x) else length(x)

    if (length(is_wear)==1) {
      if (!is_wear) warning(
        "Setting `is_wear = FALSE` will result in no bouts being detected",
        call. = FALSE
      )
      is_wear <- rep(is_wear, n)
    } else{
      stopifnot(length(is_wear) == n)
    }

  }

  is_wear

}

valid_valid_indices <- function(
  valid_indices = NULL, x, return_logical = FALSE
) {

  if (anyNA(valid_indices)) stop(
    "`valid_indices` cannot contain NA values",
    call. = FALSE
  )

  if (is.data.frame(x)) x <- 1:nrow(x)

  stopifnot(

    inherits(
      valid_indices,
      c("logical", "integer", "numeric")
    ) |
    is.null(valid_indices)

  )

  if (is.numeric(valid_indices)) {
    stopifnot(all(valid_indices %in% seq(x)))
  } else {
    stopifnot(length(valid_indices) %in% c(0:1, length(x)))
  }

  operation <- which(c(
    is.null(valid_indices),
    is.logical(valid_indices),
    is.numeric(valid_indices)
  ))

  if (return_logical) {

    switch(
      operation,
      rep(TRUE, length(x)),
      valid_indices,
      seq(x) %in% valid_indices,
      stop("Problem with `valid_indices`", call. = FALSE)
    )

  } else {

    switch(
      operation,
      seq(x),
      which(valid_indices),
      valid_indices,
      stop("Problem with `valid_indices`", call. = FALSE)
    )

  }

}
