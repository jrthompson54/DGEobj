#I use this to build and locally install the package while testing.
#Not intended for use by end Users!

x = getwd()
setwd ("~/R/lib/pkgsrc/DGEobj/")
library(devtools)
document()
load_all()
build()
pkg = "~/R/lib/pkgsrc/DGEobj_0.9.9.tar.gz"
install.packages(pkg, repos=NULL, type="source")
setwd(x)

#install from Git
#After pushing to git...
install_git("http://biogit.pri.bms.com/thompj27/DGEobj")
install_git("http://biogit.pri.bms.com/thompj27/DGEobj", repos=BiocInstaller::biocinstallRepos())

#for dev
install_git("http://biogit.pri.bms.com/thompj27/DGE.Tools", branch="dev", repos=BiocInstaller::biocinstallRepos())

