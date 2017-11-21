## Convert to function for next year

token_path = "~/Documents/Work/Teaching and TA/s432sp2017/"
repo_prefix = "HW6_Team"
path_to_starter = "~/Documents/Work/Teaching and TA/s432sp2017/HW6_Team0/"
files_vector = c('.gitignore','README.md','hw6.Rmd','nacv.csv')
commit_message = 'Added starter code'
starter_suffix = '0'
your_gh_username = 'dajmcdon'
your_gh_passwd = '1951xcty'
shuffle_captains = TRUE
load('teamAssignments.Rdata') ## Adds Team.Assisgnments dataframe and captains vector
source('github_api_functions.R')



nteams = length(captains)
nstudents = nrow(Team.Assignments)
if(shuffle_captains){
  new_teams = sample(rep(1:nteams, length.out = nstudents), size=nstudents)
}else{
  new_teams = vector(length=nstudents)
  new_teams[Team.Assignments$member.id %in% captains] = sample.int(nteams)
  new_teams[new_teams==0] = sample(rep(1:nteams, length.out = nstudents-nteams), size=nstudents-nteams)
}
Team.Assignments[[length(Team.Assignments)+1]] = new_teams
names(Team.Assignments)[length(Team.Assignments)] = gsub('_','',repo_prefix)

save(Team.Assignments, captains, file='teamAssignments.Rdata')

createTeamsAndRepos(Team.Assignments$member.id, new_teams, repo_prefix, starter.repo.suffix = starter_suffix,
                   token.path = token_path)
Sys.sleep(2)

addFiles2Repos(repo_prefix, commit_message, path_to_starter, files_vector, starter.repo.suffix = starter_suffix,
               token.path = token_path,
               gh.user.name=your_gh_username, gh.passwd=your_gh_passwd)
