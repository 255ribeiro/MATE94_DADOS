library(dplyr)

TravisData <- readRDS('TravisData.RDS')
commitData  <- readRDS('TravisData.RDS')
unnest()
# Agrupando por build
TravisData_build <- TravisData %>% 
  group_by(tr_build_id) %>% 
  summarise(
    # Status da build:
    # Se falhou para qualquer job = FALSE
    # else = TRUE
    
    build_successful = if_else( any(build_successful == FALSE), FALSE, TRUE),
    
    gh_project_name = unique(gh_project_name)
  )
  