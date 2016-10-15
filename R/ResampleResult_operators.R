#' @title Get predictions from resample results.
#'
#' @description
#' Very simple getter.
#'
#' @param res [\code{ResampleResult}]\cr
#'   The result of \code{\link{resample}} run with \code{keep.pred = TRUE}.
#' @return [\code{ResamplePrediction}].
#' @export
#' @family resample
getRRPredictions = function(res) {
  if (is.null(res$pred))
    stopf("The 'pred' slot is empty because the ResampleResult was generated with keep.pred = FALSE.")
  else
    res$pred
}

#' @title Get task description from resample results.
#'
#' @description
#' Get a summarizing task description.
#'
#' @param res [\code{ResampleResult}]\cr
#'   The result of \code{\link{resample}}.
#' @return [\code{TaskDesc}].
#' @export
#' @family resample
getRRTaskDescription = function(res) {
  res$task.desc
}

# This function creates Prediction objects for each resample iteration (separately, for train and test set)
getPredictionList = function(res, ...) {
  assertClass(res, "ResampleResult")
  # We need to force keep.pred = TRUE (will be checked in getRRPredictions)
  pred = getRRPredictions(res)
  predict.type = pred$predict.type
  time = pred$time
  task.desc = getRRTaskDescription(res)

  # split by train and test set
  set = levels(pred$data$set)

  # get prediction objects for train and test set
  prediction = lapply(set, function(s) {
    # split by resample iterations
    p.split = subset(pred$data, set == s)
    p.split = split(p.split, as.factor(p.split$iter))
    # create prediction object for each resample iteration
    p.split = lapply(p.split, function (p) {
      # get predictions based on predict.type
      if (predict.type == "prob") {
        y = p[,grepl("^prob[.]", colnames(p))]
        # we need to remove the "prob." part in the colnames, otherwise
        # makePrediction thinks that the factor starts with "prob."
        colnames(y) =  gsub("^prob[.]", "", colnames(y))
      } else {
        y = p$response
      }
      makePrediction(task.desc, id = p$id,
        truth = p$truth, y = y, row.names = p$id,
        predict.type = predict.type, time = NA, ...)
    })
    # add time info afterwards
    for(i in 1:length(p.split)) p.split[[i]]$time = time[i]
    return(p.split)
  })
  ret = setNames(prediction, set)
  if (is.null(ret$train)) ret = append(ret, list(train = NULL))
  if (is.null(ret$test)) ret = append(ret, list(test = NULL))
  return(ret[c("train", "test")])
}

#' @title Compute new measures for existing ResampleResult
#' @description
#'  Adds new measures to an existing \code{ResampleResult}.
#' @param res [\code{ResampleResult}]\cr
#'   The result of \code{\link{resample}} run with \code{keep.pred = TRUE}.
#' @template arg_measures
#' @return [\code{\link{ResampleResult}}].
#' @export
#' @family resample
addRRMeasure = function(res, measures) {
  assertClass(res, "ResampleResult")
  if (inherits(measures, "Measure")) measures = list(measures)

  # check if measures are missing in ResampleResult object
  measures.id = vcapply(measures, function(x) x$id)
  missing.measures = setdiff(measures.id, colnames(res$measures.test))

  # if there are missing measures
  if (length(missing.measures) != 0) {
    # get list of prediction objects per iteration from resample result
    pred = getPredictionList(res)

    # recompute missing performance for train and/or test set
    set = names(pred)[!vlapply(pred, is.null)]
    perf = setNames(lapply(set, function(s) {
      as.data.frame(do.call("rbind", lapply(pred[[s]], function(p) {
        ret = performance(p, measures)
        matrix(ret, ncol = length(measures), dimnames = list(NULL, names(ret)))
      })))
    }), set)

    # add missing measures to resample result
    if (is.null(perf$train))
      res$measures.train[, missing.measures] = NA else
        res$measures.train = cbind(res$measures.train, perf$train[, missing.measures, drop = FALSE])
    if (is.null(perf$test))
      res$measures.test[, missing.measures] = NA else
        res$measures.test = cbind(res$measures.test, perf$test[, missing.measures, drop = FALSE])
    aggr = vnapply(measures[measures.id %in% missing.measures], function(m) {
      m$aggr$fun(task = NULL, perf.test = res$measures.test[, m$id], perf.train = res$measures.train[, m$id], measure = m)
    })
    names(aggr) = vcapply(measures[measures.id %in% missing.measures], measureAggrName)
    res$aggr = c(res$aggr, aggr)
  }
  return(res)
}
