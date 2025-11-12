install.packages("devtools")
devtools::install_github("tomzhang255/CCR")
library(CCR)
ccr_setup()

# agentic traits
ccr_loadings_agentic <- ccr_wrapper(data_file = "Downloads/tf.csv", 
            data_col = "transcript", q_file = "Downloads/agentic traits.xlsx - Sheet1.csv", q_col = "questiontext")

write.csv(ccr_loadings_agentic, "ccr_agentic.csv")

# communal traits
ccr_loadings_communal <- ccr_wrapper(data_file = "Downloads/tf.csv", 
                            data_col = "transcript", q_file = "Downloads/communal.xlsx - Sheet1.csv", q_col = "questiontext")

write.csv(ccr_loadings_communal, "ccr_communal.csv")
