#' ALARM Predictions
#'
#' @description
#'
#' Makes predictions for the absolute risk of lung cancer mortality for
#'   never and ever smokers, based on a set of covariates, at a chosen time
#'   horizon.
#'
#' The models used to make predictions are based on the following study
#'   ([https://doi.org/10.1093/jnci/djac176](https://doi.org/10.1093/jnci/djac176)):
#'
#'   Warkentin MT, Tammemägi MC, Espin-Garcia O, Budhathoki S, Liu G, Hung RJ.
#'   Lung Cancer absolute risk models for mortality in an Asian population using
#'   the China Kadoorie Biobank. JNCI: Journal of the National Cancer Institute.
#'   2022 Dec 1;114(12):1665-73.
#'
#' @param data Data frame containing covariate values at which to produce
#'   absolute risk predictions. See [`validate_data()`] for information on the
#'   expected format of `data`.
#' @param time Numeric. Time horizon (in years) at which to make predictions.
#'   Default is `5`.
#' @param progress Logical. Should progress bars be shown? Default is `FALSE`.
#' @param ... Not currently used.
#'
#' @return A `data.frame` with same number of rows as `newdata`, and in the
#'   same order. The `data.frame` will contain the original columns plus the
#'   added column `ALARM_pred`, which is the predicted absolute risk of lung
#'   cancer mortality at time `time`.
#'
#' @details
#' `predictALARM()` supports parallel processing using the `mirai` package.
#'   For small numbers of participants, parallel processing may actually be
#'   slower than sequential processing due to the additional overhead. However,
#'   for large numbers of participants, parallel processing can provide
#'   considerable computational efficiency.
#'
#'   For example, to create multiple background R processes capable of
#'   performing predictions in parallel:
#'
#'   ```r
#'   library(mirai)
#'   daemons(4) # number of background processes
#'   predictALARM(data)
#'   ```
#'
#' @seealso [`validate_data()`]
#'
#' @md
#'
#' @examples
#'
#' data <- data.frame(age = 70, sex = 1, fhx_cancer = 1,
#'                    phx_cancer = 0, fev1fvc = 70, phx_lungdx = 1,
#'                    hhinc = 3, bmi = 30,
#'                    smk_status = c(1, 2), smk_duration = c(NA, 40),
#'                    smk_cigpday = c(NA, 20))
#' predictALARM(data)
#'
#' @export
predictALARM <- function(data, time = 5, progress = FALSE, ...) {
  rlang::is_scalar_integerish(time)
  rlang::is_scalar_logical(progress)
  rlang::inherits_all(data, "data.frame")
  rlang::check_dots_empty()

  validate_data(data)

  if (progress & mirai::daemons_set()) {
    progress <- paste0(
      "Parallel ",
      "(",
      mirai::info()['connections'],
      " daemons)"
    )
  }

  data_orig <- data

  data_fmt <- format_data(data)

  smk_never <- dplyr::filter(data_fmt, smk_ever == 0L)

  smk_ever <- dplyr::filter(data_fmt, smk_ever == 1L)

  if (nrow(smk_never) >= 1L) {
    smk_never <- predictNS(smk_never, time, progress)
  }

  if (nrow(smk_ever) >= 1L) {
    smk_ever <- predictES(smk_ever, time, progress)
  }

  data_preds <-
    dplyr::bind_rows(smk_never, smk_ever) |>
    dplyr::arrange(.order) |>
    dplyr::select(-.order)

  data_orig$ALARM_pred <- data_preds$ALARM_pred
  data_orig
}

predictNS <- function(data, time, progress) {
  data_nest <- tidyr::nest(data, .by = .order)

  preds <- purrr::map_dbl(
    data_nest$data,
    purrr::in_parallel(
      \(data) {
        model <- ALARM::ALARM_NS()
        flexsurv::pmatrix.fs(
          x = model,
          trans = attr(model, 'trans'),
          t = time,
          newdata = data
        )[1, 2]
      },
      time = time
    ),
    .progress = progress
  )

  data_nest$ALARM_pred <- preds

  tidyr::unnest(data_nest, data) |>
    dplyr::ungroup()
}

predictES <- function(data, time, progress) {
  data_nest <- tidyr::nest(data, .by = .order)

  preds <- purrr::map_dbl(
    data_nest$data,
    purrr::in_parallel(
      \(data) {
        model <- ALARM::ALARM_ES()
        flexsurv::pmatrix.fs(
          x = model,
          trans = attr(model, 'trans'),
          t = time,
          newdata = data
        )[1, 2]
      },
      time = time
    ),
    .progress = progress
  )

  data_nest$ALARM_pred <- preds

  tidyr::unnest(data_nest, data) |>
    dplyr::ungroup()
}

format_data <- function(data) {
  data |>
    dplyr::rename(
      age_entry = age,
      female = sex
    ) |>
    dplyr::mutate(
      smk_status = dplyr::case_when(
        smk_status == 1L ~ 'never',
        smk_status == 2L ~ 'former',
        smk_status == 3L ~ 'current'
      ),
      smk_former = dplyr::if_else(smk_status == 'former', 1L, 0L),
      smk_ever = dplyr::if_else(smk_status == 'never', 0L, 1L),
      smk_status = factor(smk_status),
      smk_former = factor(smk_former),
      fev1fvc_p5 = fev1fvc / 5,
      smk_duration_p5 = smk_duration / 5,
      smk_cigpday_p10 = smk_cigpday / 10,
      .order = 1:dplyr::n()
    )
}

utils::globalVariables(
  c(
    'age',
    'sex',
    'smk_former',
    'fev1fvc',
    'smk_duration',
    'smk_cigpday',
    'smk_ever',
    'ALARM',
    '.order',
    'smk_status',
    'predictNS',
    'predictES'
  )
)
