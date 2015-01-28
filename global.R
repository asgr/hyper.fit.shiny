usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1]))
        install.packages(p, dep = TRUE)
    require(p, character.only = TRUE)
}

useGitPackage <- function(p, link) {
    if (!is.element(p, installed.packages()[,1]))
        install_github(link)
    require(p, character.only = TRUE)
}

usePackage("magicaxis")
usePackage("MASS")
usePackage("rgl")
usePackage("devtools")
usePackage("shinyRGL")
useGitPackage("LaplacesDemon", "Statisticat/LaplacesDemon")
useGitPackage("hyper.fit", "asgr/hyper.fit")

data(TFR)
data(MJB)
data(GAMAsmVsize)