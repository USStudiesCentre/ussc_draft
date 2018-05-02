.onLoad <- function(libname, pkgname) {
    # load ussc fontd
    ussc_fonts()
    ################ Set Default Colors for Monochromatic Plots 
    ################ 
    update_geom_defaults("bar", list(fill = "#009de3"))
    update_geom_defaults("point", list(colour = "#1c396e"))
    update_geom_defaults("line", list(colour = "#ed1b35"))
}
