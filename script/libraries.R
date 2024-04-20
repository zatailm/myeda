load_mypkg <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  lapply(pkg, function(x) {
    suppressMessages(suppressPackageStartupMessages(library(x, character.only = TRUE)))
  })
}
packages <- c(
  'dplyr', 'tidyr','forcats', 'ggcorrplot', 'ggforce', 'ggplot2', 'ggridges', 'gratia',
  'grid','lubridate', 'mgcv', 'NLP', 'patchwork', 'purrr', 'RColorBrewer', 'readxl', 
  'scales','sf', 'sp', 'stringr', 'terra', 'tidyr', 'tm', 'treemapify', 'wordcloud', 
  'gt', 'viridis', 'reshape2', 'ape', 'spdep', 'strucchange', 'forecast', 'trend','cowplot',
  'ggfortify', 'magrittr', 'tseries', 'stats', 'caret', 'ggraph', 'ranger', 'xts',
  'tidygraph', 'xgboost', 'pdp', 'fmsb', 'MASS', 'splines', 'ggrepel', 'segmented', 
  'anomalize', 'stats', 'trend', 'tibbletime', 'Rbeast', 'tsoutliers', 'gt', 'kableExtra',
  'easyalluvial', 'shadowtext', 'ggstream', 'ggstatsplot', 'themezata', 'ggpubr')

load_mypkg(packages)

# Note:
#   Some package has conflicts with another package.

theme_set(theme_zata(base.size = 9, leg.pos = 'top', leg.jus = 'left', strip.bg = NA) +
            theme(
              plot.title = element_text(size = 8, face = 'bold'),
              axis.title = element_text(size = 8),
              strip.text = element_text(size = 8, face = 'bold')
            ))
