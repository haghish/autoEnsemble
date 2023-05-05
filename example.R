


ids <- h2o.get_ids(grid)

evaluate(id = ids)



ens <- ensemble(models = ids, training_frame = prostate)

a <- h2o.getModel("Grid_GBM_RTMP_sid_9793_235_model_R_1683219973182_35467_model_50")
a
perf <- h2o::h2o.performance(a)
perf
as.numeric(h2o::h2o.auc(perf))
