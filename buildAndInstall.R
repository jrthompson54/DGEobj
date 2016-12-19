#I use this to build and locally install the package while testing.
#Not intended for use by end Users!

x = getwd()
setwd ("~/R/lib/pkgsrc/DGEobj/")
pkg = "~/R/lib/pkgsrc/DGEobj_0.1.0.tar.gz"
library(devtools);document();load_all;build()
install.packages(pkg, repos=NULL, type="source")
setwd(x)

#install from Git
#After pushing to git...
install_git("http://biogit.pri.bms.com/thompj27/DGEresult")
install_git("http://biogit.pri.bms.com/thompj27/DGEresult", repos=BiocInstaller::biocinstallRepos())

#for dev
install_git("http://biogit.pri.bms.com/thompj27/DGE.Tools", branch="dev", repos=BiocInstaller::biocinstallRepos())

