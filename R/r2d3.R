## Functions to create USSC-themed D3.js templates in R
## Zoe Meers
## The United States Studies Centre at the University of Sydney


d3_idealpoints <- function(data, width=NULL, height=NULL){
    r2d3::r2d3(
        data=data,
        d3_version = "3", 
        script=system.file("d3/ideal/scripts/long_r2d3.js",
        css = "css/style.css", 
        dependencies = "dependencies/timeStamp.js")
    )
}
