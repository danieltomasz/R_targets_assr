
# in the file below I want to load itpc and 
# 1) compute the differences between conditions
itpc_compare_targets <- list(
  tar_target(
    joined_data_for_itpc_comparison_run,
    analysis_df,                        # just forward the object
    description = "Alias of analysis_df for ITPC comparison stage"
  )
)