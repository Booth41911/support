library(gh) # devtools::install_github("r-pkgs/gh")
library(magrittr) # CRAN
library(stringr) # CRAN
library(git2r) # CRAN

org_name = "STATS-432Sp2017"
token_path = "~/Documents/Work/Teaching and TA/s432sp2017/"


pushAllRepos <- function(dir.path, # no trailing /
                         gh.user.name, gh.passwd,
                         org.name = org_name,
                         token.path = token_path){
  
  exist.repos = list.dirs(dir.path, recursive = FALSE)
  repos = lapply(exist.repos, repository)
  for(repo in seq_along(repos)){
    cat("Pushing", exist.repos[repo], "...\n")
    try(push(repos[[repo]], credentials = cred_user_pass(gh.user.name,gh.passwd)))
  }
}

addFiles2Repos <- function(repo.prefix, commit.message, pathToStarterRepo, filesToCopy,
                           starter.repo.suffix = '0',
                           gh.user.name, gh.passwd,
                           org.name = org_name,
                           token.path = token_path){
  
  token = readLines(paste0(token.path, "github_token.txt"))
  exist.repos = gh("/orgs/:org/repos", org = org.name, .token = token, .limit=Inf) 
  repo_names = sapply(exist.repos, function(x) x$name)
  selected_repos = str_detect(repo_names,repo.prefix) %>% repo_names[.]
  selected_repos = selected_repos[!grepl(starter.repo.suffix,selected_repos)]
  
  for(repo in selected_repos){
    cat("Updating", repo, "...\n")
    # org_url = paste0('git@github.com:',org.name,'/',repo,'.git')
    org_url = paste0("https://github.com/",org.name,"/",repo,".git")
    
    path = file.path(tempdir(),repo)
    dir.create(path, recursive=TRUE)
    
    local_repo = clone(org_url, path, progress=TRUE,
        credentials = cred_user_pass(gh.user.name, gh.passwd))
    files = list.files(pathToStarterRepo)
    anyFiles = (files %in% filesToCopy)
    if(!any(anyFiles)) stop('No files to copy were found')
    files = files[anyFiles]
    try({
      for(file in files)
      {
        file.copy(paste0(pathToStarterRepo, file), path, overwrite = TRUE)
        add(local_repo, basename(file))
      }
      
      commit(local_repo, commit.message)
      push(local_repo, credentials = cred_user_pass(gh.user.name,gh.passwd))
    })
    
    unlink(path, recursive=TRUE)
  }
}




createTeamsAndRepos <- function(github.accounts, assigned.team.number, prefix,
                                starter.repo.suffix = '0',
                                org.name = org_name, 
                                token.path = token_path){
  token = readLines(paste0(token.path, "github_token.txt"))
  exist.repos = gh("/orgs/:org/repos", org = org.name, .token = token, .limit=Inf) 
  exist.repo.names = sapply(exist.repos, function(x) x$name)
  starter.repo = grepl(paste0(prefix, starter.repo.suffix), exist.repo.names)
  if(any(grepl(prefix, exist.repo.names[!starter.repo]))){
    stop("Prefix appears to already exist on github (repos).")
  }
  team_ids = createTeams(github.accounts, assigned.team.number, prefix, org.name = org.name,
              token.path = token.path)
  for(team in names(team_ids))
  {
    Sys.sleep(1)
    
    repo_name = team
    cat("Creating ", repo_name, " for ",team," (", team_ids[team],")\n",sep="")
    
    try({
      gh("POST /orgs/:org/repos", 
         org = org.name,
         name=repo_name, private=TRUE, team_id=team_ids[team], 
         auto_init=TRUE, gitignore_template="R",
         .token=token)
    })
    
    Sys.sleep(2)
    
    try({
      gh("PUT /teams/:id/repos/:org/:repo", 
         id = team_ids[team], org = org.name, repo = repo_name,
         permission="push",
         .token=token)
    })
  }
}

# This function assigns individuals to teams
# Takes in a vector of accounts, team numbers, and the prefix associated with the assignment
# Optionally a different organization and path to the Github API token
# The result is the creation of all teams, and individuals assigned to those teams
createTeams <- function(github.accounts, assigned.team.number, team.prefix, 
                        org.name = org_name, 
                        token.path = token_path){
  
  token = readLines(paste0(token.path, "github_token.txt"))
  exist.teams = gh("/orgs/:org/teams", org = org.name, 
                   .token = token, .limit=Inf) # gets all team names from github
  exist.ids = sapply(exist.teams, function(x) x$id)
  names(exist.ids) = sapply(exist.teams, function(x) x$name)
  if(any(grepl(team.prefix, names(exist.ids)))){
    stop("Prefix appears to already exist on github (teams).") 
  }
  if(length(github.accounts) != length(assigned.team.number)){
    stop("Need to have a team number for each student.")
  }
  
  team.assignments = paste0(team.prefix, assigned.team.number)
  teams = sort(unique(team.assignments))
  
  for (team in teams) {
    Sys.sleep(0.2)
    
    cat("Adding ", team, "...\n", sep = "")
    gh(
      "POST /orgs/:org/teams",
      org = org.name,
      name = team,
      privacy = "closed",
      .token = token
    )
  }
  
  teams = gh("/orgs/:org/teams", org = org.name, 
             .token = token, .limit=Inf) # gets all team names from github
  team_ids = sapply(teams, function(x) x$id)
  names(team_ids) = sapply(teams, function(x) x$name) # names them
  
  for (i in seq_along(assigned.team.number)) {
    Sys.sleep(0.2)
    
    team = team.assignments[i]
    acc = github.accounts[i]
    id = team_ids[team] # match to the github names
    
    cat("Adding ", acc, " to ", team, "...\n", sep = "")
    
    gh(
      "PUT /teams/:id/memberships/:username",
      id = id,
      username = acc,
      role = "member",
      .token = token
    )
  }
  invisible(team_ids[!(team_ids %in% exist.ids)])
}


grab_repos <- function(repo_pattern, targetpath="./", verbose=TRUE, 
                       gh.user.name, gh.passwd, ignore_files=NULL,
                       org.name=org_name, 
                       token.path = token_path){
  stopifnot(!missing(repo_pattern))
  
  token = readLines(paste0(token.path, "github_token.txt"))
  
  exist.repos = gh("/orgs/:org/repos", org = org.name, .token = token, .limit=Inf) 
  exist.repo.names = sapply(exist.repos, function(x) x$name)
  
  selected_repos = str_detect(exist.repo.names,repo_pattern) %>% exist.repo.names[.]
  
  i = 0
  for(repo in selected_repos){
    i = i + 1
    cat("Grabbing", repo, "...\n")
    # org_url = paste0('git@github.com:',org.name,'/',repo,'.git')
    org_url = paste0("https://github.com/",org.name,"/",repo,".git")
    
    path = file.path(tempdir(),repo)
    dir.create(path,recursive = TRUE)
    local_repo = clone(org_url, path, progress=TRUE,
                       credentials = cred_user_pass(gh.user.name, gh.passwd))
    if(!is.null(ignore_files)) system(paste('rm', ignore_files))
    system(paste('mv', path, targetpath))
  }
}

knit_rmds <- function(local_path, ignore_file = NULL){
  stopifnot(dir.exists(local_path))
  
  repos = list.dirs(path = local_path, full.names = TRUE, recursive = FALSE)      
  
  for(repo in repos){
    cat("Knitting in", repo, "...\n")
    
    try({
      rmds = list.files(repo, pattern="[Rr]md$")
      if(!is.null(ignore_file)){
        skip = str_detect(rmds, ignore_file)
        rmds = rmds[!skip]
      }
      path_to_rmds = paste0(repo, '/', rmds)
      lapply(path_to_rmds, rmarkdown::render, quiet=FALSE)        
    })
  }
}

create_projects <- function(directory){ # no trailing /
  all_dirs = list.dirs(directory, recursive = FALSE)
  txt = c(
'Version: 1.0

RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: knitr
LaTeX: pdfLaTeX'
  )
  dirs = all_dirs[substr(all_dirs,1,1)!='.'] # Remove hidden
  nn = sapply(strsplit(dirs,'/'), function(x) x[length(x)])
  for(ii in seq_along(dirs)) writeChar(txt,paste0(dirs[ii],'/',nn[ii],'.Rproj'))
  invisible(dirs)
}


