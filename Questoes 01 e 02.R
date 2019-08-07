#carregando bibliotecas

library(dplyr)
library(tidyr)
library(pander)
library(gmodels)
library(ggplot2)



# carregando bases de dados
travis <- readRDS('travis-sample.rds')
commits <- readRDS('commits-sample.rds')

# Relaciona cada build job com os commits correspondentes
travis_commits <- travis %>%
  select(tr_job_id, git_all_built_commits) %>%
  mutate(git_commit_id = strsplit(git_all_built_commits, "#"))  %>%
  unnest(git_commit_id) %>%
  select(tr_job_id, git_commit_id)



# Cria data frame em que cada linha representa um par (job, commit)
Job_commit <-travis %>%
  inner_join(travis_commits, by = c("tr_job_id")) %>%
  inner_join(commits, by = c("git_commit_id", "gh_project_name"))

# autores por projeto
autores_por_projeto <- Job_commit %>%
  group_by(gh_project_name) %>%
  summarise(n_autores = n_distinct(author_email))

# Status da build como vetor logico
travis <- travis %>%  mutate(status_logic = if_else(tr_status == 'passed', TRUE, FALSE) )

# Agrupando por build
status_por_build <- travis %>% 
                        group_by(tr_build_id) %>% 
                            summarise(  # tratando variavel status da build como vetor logico
                                        status_logic = unique(status_logic),
                                        gh_lang = unique(gh_lang),
                                        gh_project_name = unique(unique(gh_project_name))
                                      )

# Agrupando por projeto
status_por_proj <- status_por_build %>% 
                        group_by(gh_project_name) %>% 
                                                summarise(   gh_lang = unique(gh_lang),
                                                             # Numero total de builds
                                                             total_build     = n(),
                                                             # numero de builds que passaram
                                                             total_passou = sum(status_logic),
                                                             # propor??o de sucesso e fracasso das builds
                                                             status_prop = total_passou/total_build
                                                          )

# numero de autores e status da build agrupados por projeto
status.n_autores <- inner_join(autores_por_projeto, status_por_proj, by = "gh_project_name")

# variavel ordinal do n?mero de autores
status.n_autores <- status.n_autores %>%
            mutate(  
              n_autores_ordinal = if_else(n_autores > quantile(n_autores, .75), 'GG',
                                    if_else(n_autores > quantile(n_autores, .50), 'G', 
                                      if_else(n_autores > quantile(n_autores, .25), 'M', 'P'))) %>%
                                          factor( ordered = TRUE ,levels = c('P', 'M', 'G', 'GG') )
                  )
# variavel ordinal da propor??o de sucesso da build
status.n_autores <- status.n_autores %>%
  mutate(  
    status_ordinal = if_else(status_prop > .80, '80-100',
                                if_else(status_prop > .60, '60-80', 
                                        if_else(status_prop > .40, '40-60', 
                                          if_else(status_prop > .20, '20-40', '0-20' )))
                                
                                ) %>%
      factor( ordered = TRUE ,levels = c('0-20', '20-40', '40-60', '60-80', '80-100') )
        )
# variavel nominal da linguagem
status.n_autores <- status.n_autores %>%
  mutate(  
    lang_nominal = factor(gh_lang)
  )

# medidas da vari?vel do numero de autores
status.n_autores$n_autores %>% mean()
status.n_autores$n_autores %>% quantile(probs = seq(0.25, .75, 0.25))
status.n_autores$n_autores %>% sd()
status.n_autores$n_autores %>% max()
status.n_autores$n_autores %>% min()




#medidas da vari?vel da propor??o de falhas na build
status.n_autores$status_prop %>% mean()
status.n_autores$status_prop %>% quantile(probs = seq(0.25, .75, 0.25))
status.n_autores$status_prop %>% sd()
status.n_autores$status_prop %>% max()
status.n_autores$status_prop %>% min()

# Graficos

# boxplot
                
 (ggplot( status.n_autores, aes(n_autores_ordinal, status_prop))
       
       + geom_boxplot(fill = 'darkolivegreen3')
       + geom_hline( yintercept = median(status.n_autores$status_prop), color = "red4")
       + geom_hline( yintercept = mean(status.n_autores$status_prop), color = "Blue4")
       + labs(x = "Tamanho das Equipes",
              y = "Propor??o de Sucessos",
              title = "Tamanho das equipe x Sucessso da Build",
              subtitle = "Boxplot",
              caption = "Figura 01" )
       + theme_minimal()
)

# Dispers?o
(ggplot( status.n_autores, aes( n_autores, status_prop))
  
  +  geom_vline( xintercept = quantile(status.n_autores$n_autores,
                              probs = seq(.25, .75, 0.25)), color = 'Orange')
  +  geom_hline( yintercept = quantile(status.n_autores$status_prop,
                                       probs = seq(.25, .75, 0.25)), color = 'Red')
  + stat_smooth(method = 'lm', se = F, color = 'black')

  + geom_point()
  + labs(
          x = "Numero de autores do projeto",
          y = "Popor??o sucesso na build por projeto",
         
         title = " N?mero de Autores no Projeto x Sucesso na Build",
         subtitle = "Dispers?o",
         caption = "Figura 02")
  
  + theme_minimal()
)

# Dispers?o 2
(ggplot( status.n_autores, aes(status_prop, n_autores))
  
  +  geom_hline( yintercept = quantile(status.n_autores$n_autores,
                                       probs = seq(.25, .75, 0.25)), color = 'Orange')
  +  geom_vline( xintercept = quantile(status.n_autores$status_prop,
                                       probs = seq(.25, .75, 0.25)), color = 'Red')
  + stat_smooth(method = 'lm', se = F, color = 'black')
  
  + geom_point()
  + labs(x = "Popor??o sucesso na build por projeto",
         y = "Numero de autores do projeto",
         title = "Sucesso na Build x N?mero de Autores no Projeto",
         subtitle = "Dispers?o",
         caption = "Figura 02")
  
  + theme_minimal()
)




# Barras lado a lado

(ggplot(status.n_autores, aes(n_autores_ordinal ,fill = status_ordinal ,  order = status_ordinal))
  
  + geom_bar(position ='dodge')
  + labs(x = "Tamanho das Equipes",
          y = '',
         title = "Tamanhos de Equipes x Sucesso na Build",
         subtitle = "Barras",
         caption = "Figura 03")
  
  + theme_minimal()
  + scale_fill_brewer(palette = 'Oranges', name="Sucesso (%)")
)


# Barras propor??o
(ggplot(status.n_autores, aes(n_autores_ordinal ,fill = status_ordinal ,  order = status_ordinal))
  
  + geom_bar(position ='fill')
  + labs(x = "Tamanho das Equipes",
         y = '',
         title = "Tamanhos de Equipes x Sucesso na Build",
         subtitle = "Barras",
         caption = "Figura 04")
  + theme_minimal()
  + scale_fill_brewer(palette = 'Oranges', name="Sucesso (%)")
)


# Tabela de contingencia

with(status.n_autores,CrossTable(n_autores_ordinal,status_ordinal, chisq = T))

# Correla??o

with (status.n_autores, cor.test(n_autores, status_prop,  method = "kendall"))

shapiro.test(status.n_autores$n_autores)

shapiro.test(status.n_autores$status_prop)

#### Quest?o 02


# Graficos

# boxplot

(ggplot( status.n_autores, aes(lang_nominal, status_prop))
  
  + geom_boxplot(fill = 'darkolivegreen3')
  + geom_hline( yintercept = median(status.n_autores$status_prop), color = "red4")
  + geom_hline( yintercept = mean(status.n_autores$status_prop), color = "Blue4")
  + labs(x = "Linguagem",
         y = "Propor??o de Sucessos",
         title = "Linguagem x Sucessso da Build",
         subtitle = "Boxplot",
         caption = "Figura 05" )
  + theme_minimal()
)

# Barras lado a lado

(ggplot(status.n_autores, aes(lang_nominal ,fill = status_ordinal ,  order = status_ordinal))
  
  + geom_bar(position ='dodge')
  + labs(x = "Linguagem",
         y = '',
         title = "Linguagem x Sucesso na Build",
         subtitle = "Barras",
         caption = "Figura 06")
  
  + theme_minimal()
  + scale_fill_brewer(palette = 'Oranges', name="Sucesso (%)")
)

# Barras propor??o
(ggplot(status.n_autores, aes(lang_nominal ,fill = status_ordinal ,  order = status_ordinal))
  
  + geom_bar(position ='fill')
  + labs(x = "Linguagem",
         y = '',
         title = "Linguagem x Sucesso na Build",
         subtitle = "Barras",
         caption = "Figura 07")
  + theme_minimal()
  + scale_fill_brewer(palette = 'Oranges', name="Sucesso (%)")
)



# Tabela de contingencia

with(status.n_autores,CrossTable(lang_nominal,status_ordinal, chisq = T))

### primeiro quartil
with (status.n_autores, quantile(n_autores, .25))

