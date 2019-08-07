library(dplyr)
library(tidyr)
library(pander)
library(gmodels)
library(ggplot2)
library(lubridate)
library(bursts)
library(DescTools)


travis  <- readRDS('travis-sample.rds')
commits <- readRDS('commits-sample.rds')

# nível das rajadas
kLevel <- 2

# dataset vazio para armazenar os resultados
build_brust <- data.frame(  )

# Status da build como vetor logico
travis <- travis %>%  mutate(status_logic = if_else(tr_status == 'passed', TRUE, FALSE) )


# Relaciona cada build job com os commits correspondentes
travis_commitsQ3 <- travis %>%
  select(tr_job_id, git_all_built_commits) %>%
  mutate(git_commit_id = strsplit(git_all_built_commits, "#"))  %>%
  unnest(git_commit_id) %>%
  select(tr_job_id, git_commit_id)

Job_commitQ3 <-travis %>%
  inner_join(travis_commitsQ3, by = c("tr_job_id"))





# tratndo datas dos commits
commits <- commits %>% mutate(time = ymd_hms(date))




#preparando dataset para o loop
proj.loop  <- commits

####Loop
while( nrow(proj.loop) > 0 ) {
#nome do projeto atual
proj.nome  <- proj.loop$gh_project_name[1]


str(proj.nome)

# separando o projeto atual
proj.atual <- proj.loop %>% filter( gh_project_name == proj.nome )
# retirando projeto atual do loop
proj.loop  <- proj.loop %>% filter( gh_project_name != proj.nome )
# apagando commits concomitantes
proj.atual <- proj.atual %>% group_by(time) %>%
  summarise(gh_project_name =unique(gh_project_name),
            git_commit_id = max(git_commit_id))

# algoritmo de kleinberg 
k <- kleinberg(proj.atual$time)

#plot.bursts(k)

# separando um nivel de rajada
k <- subset(k, level == kLevel)
# ordenando os breaks
breaks = sort(c(k$start, k$end))
# criando variavel logica isBurst
# se o commit pertence a um brust <- TRUE se não <- FALSE
proj.atual <- proj.atual %>%
  mutate(isBurst = cut(time, breaks=breaks, labels=FALSE)) %>%
  mutate(isBurst = if_else(is.na(isBurst), F, isBurst %% 2 == 1))

# relacionadno comits e status

proj.atual <- proj.atual  %>% inner_join(Job_commitQ3, by = c("git_commit_id", "gh_project_name"))

# Agrupando por build
proj.builds <- proj.atual %>% 
  group_by(tr_build_id) %>% 
  summarise(  
    status_logic = unique(status_logic),
    # se ao menos um dos commits é rajada, a build e considerada rajada
    isBurst = any(isBurst),
    burst_passed = status_logic & isBurst,
    gh_project_name = unique(gh_project_name)
  )


  if( length(proj.builds) != 0)
      
    
    { build_brust <- rbind(build_brust, proj.builds ) }#end if
                                                        
    
  }# end while

# fim do loop

build_brust <- build_brust %>% mutate( status_factor = factor(status_logic),
                                       isBurst_factor = factor(isBurst))
                                       

# salvando arquivo
#saveRDS(build_brust, file="buildt_brust.RDS")
# tabela de contingencia
tabela <- with(build_brust, table(status_factor, isBurst_factor))
tabela

mosaicplot(tabela, main = 'Status da Buils x Rajada', xlab = 'Status da Build', ylab = 'Rajada', type ="pearson")


# Tabela de contingência e teste qui-quadrado
with(build_brust,CrossTable(status_factor,isBurst_factor, chisq = T))
CramerV(tabela)

