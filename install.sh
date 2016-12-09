
# on local
  devtools::build_vignettes() # ;  devtools::clean_vignettes()
  # git
  system("cd /home/mihai/ownCloud/PACKAGES/sdb/ && git add  -A && git commit -a -m 'remove gui' && git push")


# on host
    

  # Package
  ssh -t valcu@scidb.mpio.orn.mpg.de "sudo R -e \"devtools::install_git('/ds/raw_data_kemp/GIT/sdb')\" && sudo restart shiny-server"

  # settings & dependencies (run once)
  ssh -t valcu@scidb.mpio.orn.mpg.de "sudo R -e \"sdb::saveCredentials('valcu', 'ar**s', 'scidb.mpio.orn.mpg.de', path = '/home/shiny/.sdb')\" "

  # install main interface 
  sudo R -e "cat(\"source(system.file('ui', 'server.R', package = 'sdb'))\", file = '/srv/shiny-server/server.R'); cat(\"source(system.file('ui', 'ui.R', package = 'sdb'))\", file = '/srv/shiny-server/ui.R')" && sudo restart shiny-server


