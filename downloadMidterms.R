## Usage for timed downloads:
##   1. Enable atrun:
##      a. make sure you have sudo access
##      b. at a prompt use "sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.atrun.plist"
##   2. Make sure computer will wake before running (can enable in System Preferences > Energy Saver)
##   3. at a prompt use "at" followed by the runtime, hit enter
##   4. Type "R CMD BATCH /path/to/script", hit enter
##   5. Type Ctrl-D

## None of the above actually works. Debug later

source('~/Documents/Work/Teaching and TA/s432sp2017/Support/github_api_functions.R')
grab_repos('midtermexam-', "~/Desktop/midterms", gh.user.name = 'dajmcdon', gh.passwd = '1951xcty')