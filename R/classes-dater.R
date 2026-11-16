#' Turning-point dating rules
#'
#' A `Dater` holds the censoring rules of a turning-point algorithm. The
#' built-in daters implement the Bry and Boschan (1971) logic as simplified by
#' Harding and Pagan (2002) and the OECD (Gyomai, Ahmad and Astolfi 2017,
#' p. 285):
#'
#' 1. a peak (trough) is a local maximum (minimum) within `window`
#'    observations on each side;
#' 2. peaks and troughs must alternate; of two consecutive peaks (troughs) the
#'    higher (lower) is kept;
#' 3. turning points closer than `end_exclusion` observations to either end
#'    are dropped, as are first or last peaks (troughs) lower (higher) than the
#'    end values;
#' 4. a cycle (peak to peak or trough to trough) lasts at least `min_cycle`
#'    observations and a phase at least `min_phase` observations.
#'
#' The defaults (window / phase / cycle / end exclusion) differ by dater:
#'
#' * `oecd_dater()`: 5 / 9 / 24 / 6 months, 2 / 3 / 8 / 2 quarters; the
#'   phase and cycle lengths of the OECD (Gyomai, Ahmad and Astolfi 2017,
#'   p. 285);
#' * `bry_boschan()`: 5 / 5 / 15 / 6 months, 2 / 2 / 5 / 2 quarters; the
#'   original routine (Bry and Boschan 1971), used by the OECD before 2008
#'   (Gyomai and Guidetti 2012, annex B);
#' * `bbq()`: the same values, after Harding and Pagan (2002).
#'
#' The OECD gives the phase and cycle lengths in months only; the quarterly
#' values of `oecd_dater()` (3 and 8 quarters) are their translation by
#' cyclind, not an OECD rule. The window and the end exclusion are not
#' given by the OECD and keep the Bry-Boschan values. Parameters left at
#' `NA` are resolved from the series frequency, so every dater can be
#' applied to monthly or quarterly data.
#'
#' **Phase rule.** When a phase is shorter than `min_phase`, a peak and a
#' trough have to go. With `phase_rule = "symmetric"` (the default) every
#' admissible removal (one turning point of a short phase together with
#' the less extreme of its two neighbours of the other type) is tried, and
#' the one that keeps the largest total amplitude of the turning-point path
#' (sum of absolute differences between consecutive turning points) is
#' applied, one at a time until no short phase is left. The rule keeps the
#' most extreme peaks and troughs and does not depend on the direction of
#' time: dating the reversed series gives the same turning points, reversed
#' (apart from exact ties). With `phase_rule = "bbq"` the later turning point
#' of a short phase is dropped, the rule of cyclind 0.1.0, which reproduces
#' the BBQ program of Harding and Pagan as implemented in the archived
#' 'BCDating' package more closely but can lose the deepest trough of a
#' crisis (see `harness/adr/ADR-004.md` in the source package). The OECD
#' does not say which turning point of a short phase it drops.
#'
#' @param window Half-width of the local extremum window.
#' @param min_phase Minimum length of an expansion or contraction.
#' @param min_cycle Minimum length of a full cycle.
#' @param end_exclusion Number of observations at each end in which no turning
#'   point is accepted.
#' @param phase_rule How a phase shorter than `min_phase` is removed:
#'   `"symmetric"` (default; keeps the most extreme turning points) or
#'   `"bbq"` (drops the later turning point, as cyclind 0.1.0).
#' @return A `Dater` object.
#' @references
#' Bry, G. and Boschan, C. (1971). *Cyclical Analysis of Time Series: Selected
#' Procedures and Computer Programs*. NBER, New York.
#'
#' Gyomai, G., Ahmad, N. and Astolfi, R. (2017). The OECD system of composite
#' leading indicators. In *Handbook on Cyclical Composite Indicators*,
#' chapter 10, 271--291. Eurostat, Luxembourg.
#'
#' Gyomai, G. and Guidetti, E. (2012). *OECD System of Composite Leading
#' Indicators*. OECD Publishing, Paris.
#'
#' Harding, D. and Pagan, A. (2002). Dissecting the cycle: a methodological
#' investigation. *Journal of Monetary Economics*, 49(2), 365--381.
#' @seealso [date_turning_points()]
#' @examples
#' oecd_dater()
#' bry_boschan()
#' bbq(min_cycle = 6, phase_rule = "bbq")
#' @name Dater-class
#' @aliases Dater
#' @exportClass Dater
NULL

setClass("Dater", representation(
  "VIRTUAL", label = "character", window = "numeric", min_phase = "numeric",
  min_cycle = "numeric", end_exclusion = "numeric", phase_rule = "character"
), prototype(phase_rule = "symmetric"))

#' @rdname Dater-class
setClass("BryBoschan", contains = "Dater")

#' @rdname Dater-class
setClass("BBQ", contains = "Dater")

#' @rdname Dater-class
setClass("OecdDater", contains = "BryBoschan")

phase_rules <- c("symmetric", "bbq")

check_dater_args <- function(window, min_phase, min_cycle, end_exclusion,
                             phase_rule) {
  if (!is.character(phase_rule) || length(phase_rule) != 1L ||
      !phase_rule %in% phase_rules) {
    cyclind_abort("`phase_rule` must be \"symmetric\" or \"bbq\".",
                  "cyclind_error_argument")
  }
  check_number(window, "window", lower = 1, integer = TRUE, allow_na = TRUE)
  check_number(min_phase, "min_phase", lower = 1, integer = TRUE,
               allow_na = TRUE)
  check_number(min_cycle, "min_cycle", lower = 2, integer = TRUE,
               allow_na = TRUE)
  check_number(end_exclusion, "end_exclusion", lower = 0, integer = TRUE,
               allow_na = TRUE)
}

#' @rdname Dater-class
#' @export
bry_boschan <- function(window = NA, min_phase = NA, min_cycle = NA,
                        end_exclusion = NA,
                        phase_rule = c("symmetric", "bbq")) {
  new_dater("BryBoschan", "Bry-Boschan", window, min_phase, min_cycle,
            end_exclusion, phase_rule)
}

#' @rdname Dater-class
#' @export
oecd_dater <- function(window = NA, min_phase = NA, min_cycle = NA,
                       end_exclusion = NA,
                       phase_rule = c("symmetric", "bbq")) {
  new_dater("OecdDater", "Bry-Boschan, OECD phase and cycle lengths", window,
            min_phase, min_cycle, end_exclusion, phase_rule)
}

new_dater <- function(class, label, window, min_phase, min_cycle,
                      end_exclusion, phase_rule) {
  if (identical(phase_rule, phase_rules)) phase_rule <- phase_rules[1]
  check_dater_args(window, min_phase, min_cycle, end_exclusion, phase_rule)
  new(class, label = label, window = as.numeric(window),
      min_phase = as.numeric(min_phase), min_cycle = as.numeric(min_cycle),
      end_exclusion = as.numeric(end_exclusion), phase_rule = phase_rule)
}

#' @rdname Dater-class
#' @export
bbq <- function(window = NA, min_phase = NA, min_cycle = NA,
                end_exclusion = NA, phase_rule = c("symmetric", "bbq")) {
  new_dater("BBQ", "Harding-Pagan BBQ", window, min_phase, min_cycle,
            end_exclusion, phase_rule)
}

# Defaults of the censoring rules, expressed per frequency.
setGeneric("dater_defaults", function(dater, freq) {
  standardGeneric("dater_defaults")
})
setMethod("dater_defaults", "Dater", function(dater, freq) {
  if (freq == 12) {
    list(window = 5, min_phase = 5, min_cycle = 15, end_exclusion = 6)
  } else {
    list(window = 2, min_phase = 2, min_cycle = 5, end_exclusion = 2)
  }
})
# Gyomai, Ahmad and Astolfi (2017, p. 285): phases of at least 9 and cycles
# of at least 24 months; 3 and 8 quarters is the translation by cyclind.
setMethod("dater_defaults", "OecdDater", function(dater, freq) {
  if (freq == 12) {
    list(window = 5, min_phase = 9, min_cycle = 24, end_exclusion = 6)
  } else {
    list(window = 2, min_phase = 3, min_cycle = 8, end_exclusion = 2)
  }
})

# Phase rule of a dater; objects saved before it existed keep the rule of
# cyclind 0.1.0.
dater_phase_rule <- function(dater) {
  if (methods::.hasSlot(dater, "phase_rule") &&
      length(dater@phase_rule) == 1L) {
    dater@phase_rule
  } else {
    "bbq"
  }
}

resolve_dater <- function(dater, freq) {
  d <- dater_defaults(dater, freq)
  list(
    window = resolve_default(dater@window, d$window),
    min_phase = resolve_default(dater@min_phase, d$min_phase),
    min_cycle = resolve_default(dater@min_cycle, d$min_cycle),
    end_exclusion = resolve_default(dater@end_exclusion, d$end_exclusion),
    phase_rule = dater_phase_rule(dater)
  )
}

#' Turning points of a series
#'
#' Objects of class `TurningPoints` store the peaks and troughs of one series.
#'
#' @slot name Series name.
#' @slot series The dated series (`ts`).
#' @slot index Positions of the turning points in `series`.
#' @slot type `"peak"` or `"trough"` for each turning point.
#' @slot params Censoring rules actually applied.
#' @seealso [date_turning_points()], [as.data.frame,TurningPoints-method]
#' @exportClass TurningPoints
setClass("TurningPoints", representation(
  name = "character", series = "ts", index = "integer", type = "character",
  params = "list"
), validity = function(object) {
  if (length(object@index) != length(object@type)) {
    return("`index` and `type` must have the same length.")
  }
  if (!all(object@type %in% c("peak", "trough"))) {
    return("`type` must be 'peak' or 'trough'.")
  }
  if (is.unsorted(object@index, strictly = TRUE)) {
    return("`index` must be strictly increasing.")
  }
  if (length(object@type) > 1L &&
      any(object@type[-1] == object@type[-length(object@type)])) {
    return("Peaks and troughs must alternate.")
  }
  TRUE
})

#' Date the turning points of a series
#'
#' @param dater A [Dater-class] object.
#' @param x A monthly or quarterly `ts`. Leading and trailing `NA` are
#'   ignored; internal `NA` are not allowed.
#' @param name Optional series name stored in the result.
#' @param ... Unused.
#' @return A [TurningPoints-class] object.
#' @examples
#' x <- ts(sin(2 * pi * (1:240) / 48), frequency = 12, start = 2000)
#' tp <- date_turning_points(bry_boschan(), x)
#' as.data.frame(tp)
#' @export
setGeneric("date_turning_points", function(dater, x, ...) {
  standardGeneric("date_turning_points")
})
