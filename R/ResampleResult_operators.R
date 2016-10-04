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

getPredictionList = function(res, ...) {
  assertClass(res, "ResampleResult")

  predict.type = res$pred$predict.type
  task.desc = getRRTaskDescription(res)
  time = res$pred$time #res$runtime/res$pred$instance$desc$iters

  # split by train and test set
  set = unique(res$pred$data$set)

  # get prediction object for train and test set
  prediction = lapply(set, function(s) {
    pred = subset(res$pred$data, set == s)
    pred.split = split(pred, as.factor(pred$iter))
    # split by resample iterations
    prediction = lapply(pred.split, function (pred) {
      # get predictions based on predict.type
      if (predict.type == "prob") {
        y = pred[,grepl("^prob[.]", colnames(pred))]
        colnames(y) =  gsub("^prob[.]", "", colnames(y))
      } else {
        y = pred$response
      }
      makePrediction(task.desc, id = pred$id,
        truth = pred$truth, y = y, row.names = pred$id,
        predict.type = predict.type, time = NA, ...)
    })
    # add time info
    for(i in 1:length(prediction)) prediction[[i]]$time = time[i]
    return(prediction)
  })
  setNames(prediction, set)
}

addRRMeasure = function(res, measures) {
  assertClass(res, "ResampleResult")
  if (inherits(measures, "Measure")) measures = list(measures)

  measures.id = vcapply(measures, function(x) x$id)
  missing.measures = setdiff(measures.id, colnames(res$measures.test))
  # if there are missing measures
  if (length(missing.measures) != 0) {
    # get list of prediction objects per iteration from resample result
    pred = getPredictionList(res)
    set = names(pred)

    perf = setNames(lapply(set, function(s) {
      as.data.frame(do.call("rbind", lapply(pred[[s]], function(p) {
        ret = performance(p, measures)
        matrix(ret, ncol = length(measures), dimnames = list(NULL, names(ret)))
      })))
    }), names(pred))

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
