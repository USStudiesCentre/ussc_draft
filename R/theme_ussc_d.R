## Functions to create USSC ggplot2 theme
## Zoe Meers
## The United States Studies Centre at the University of Sydney

#' Load USSC fonts.
#' @description
#' Loads USSC fonts into R from the local font book. Download necessary fonts from the fonts folder in this package and add to font book (if you have not done so before).
#' @usage 
#' ussc_fonts()
#' @details 
#' One the function runs, you have access to four font types:
#' * neosanspro
#' * neosansproLight
#' * univers
#' * universLight
#' @author 
#' Zoe Meers
ussc_fonts <- function(){
    univers <- quartzFont(paste("Univers LT Pro",
                                c("65 Bold","45 Light Oblique","45 Light", "55 Roman")))
    universLight <- quartzFont(paste("Univers LT Pro",
                                     c("45 Light", "55 Roman","65 Bold","45 Light Oblique")))
    neosanspro <- quartzFont(paste("Neo Sans Pro",
                                   c("Bold", "Italic", "Regular", "Medium")))
    neosansproLight <- quartzFont(paste("Neo Sans Pro",
                                        c("Light", "Light Italic", "Regular", "Medium")))
    quartzFonts(univers=univers)
    quartzFonts(universLight=universLight)
    quartzFonts(neosanspro=neosanspro)
    quartzFonts(neosansproLight=neosansproLight)
}

#' Main USSC theme
#' @description
#' Calls the main USSC theme. A reconstructed theme_bw() ggplot2 theme. 
#' @section Building upon `theme_ussc`:
#' The function is setup in such a way that you can customize your own one by just
#' wrapping the call and changing the parameters. See source for examples.
#' @usage 
#' theme_ussc_d()
#' @param 
#' grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param 
#' axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param 
#' ticks ticks if `TRUE` add ticks
#' @param 
#' border border if `TRUE` add plot border
#' @examples 
#' Create ggplot theme using main USSC theme
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) + geom_point(size = 4, alpha=0.4) +  theme_ussc_d(grid="XY", ticks=FALSE, border=TRUE) + labs(title="Neo Sans Pro Header", x="Univers Font: Sepal Width", y="Univers Font: Sepal Length") + scale_colour_ussc("blue")
#' @author 
#' Zoe Meers
theme_ussc_d <- function(grid = TRUE, axis = FALSE, ticks = FALSE, border=TRUE){
    basic_theme <- theme_bw(base_family="univers",
             base_size = 11.5) + 
        theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
              axis.ticks = element_line(colour = "#cccccc"),
              text = element_text(colour="#444444"),
              axis.title.x = element_text(size=9, "univers"),
              axis.title.y = element_text(size=9, "univers"),
              axis.text.x = element_text(size = 9, "univers"),
              axis.text.y = element_text(size = 7, "univers"),
              legend.text = element_text(size=7, "universLight"),
              legend.title = element_text(size=9, "univers"),
              legend.key.size = unit(x = 9,units = "pt"),
              plot.title= element_text(size=14, family = "neosanspro"),
              plot.caption = element_text(size=7, family="universLight"),
              plot.subtitle = element_text(size=10, family="universLight"),
              plot.background = element_blank(),
              strip.background = element_blank(),
              legend.background = element_blank()
              
        )
    
    if (inherits(grid, "character") | grid == TRUE) {
        
        basic_theme <- basic_theme + theme(panel.grid=element_line(color="#cccccc", size=0.2))
        basic_theme <- basic_theme + theme(panel.grid.major=element_line(color="#cccccc", size=0.2))
        basic_theme <- basic_theme + theme(panel.grid.minor=element_line(color="#cccccc", size=0.15))
        
        if (inherits(grid, "character")) {
            if (regexpr("X", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.major.x=element_blank())
            if (regexpr("Y", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.major.y=element_blank())
            if (regexpr("x", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.minor.x=element_blank())
            if (regexpr("y", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.minor.y=element_blank())
        }
        
    } else {
        basic_theme <- basic_theme + theme(panel.grid=element_blank())
    }
    
    if (inherits(axis, "character") | axis == TRUE) {
        basic_theme <- basic_theme + theme(axis.line=element_line(color="#cccccc", size=0.15))
        if (inherits(axis, "character")) {
            axis <- tolower(axis)
            if (regexpr("x", axis)[1] < 0) {
                basic_theme <- basic_theme + theme(axis.line.x=element_blank())
            } else {
                basic_theme <- basic_theme + theme(axis.line.x=element_line(color="#cccccc", size=0.15))
            }
            if (regexpr("y", axis)[1] < 0) {
                basic_theme <- basic_theme + theme(axis.line.y=element_blank())
            } else {
                basic_theme <- basic_theme + theme(axis.line.y=element_line(color="#cccccc", size=0.15))
            }
        } else {
            basic_theme <- basic_theme + theme(axis.line.x=element_line(color="#cccccc", size=0.15))
            basic_theme <- basic_theme + theme(axis.line.y=element_line(color="#cccccc", size=0.15))
        }
    } else {
        basic_theme <- basic_theme + theme(axis.line=element_blank())
    }
    if (!border) {
        basic_theme <- basic_theme + theme(panel.border =  element_blank())
    } else {
        basic_theme <- basic_theme + theme(panel.border = element_rect(size=0.15, color="#cccccc"))
    }
    if (!ticks) {
        basic_theme <- basic_theme + theme(axis.ticks = element_blank())
        basic_theme <- basic_theme + theme(axis.ticks.x = element_blank())
        basic_theme <- basic_theme + theme(axis.ticks.y = element_blank())
    } else {
        basic_theme <- basic_theme + theme(axis.ticks = element_line(size=0.15))
        basic_theme <- basic_theme + theme(axis.ticks.x = element_line(size=0.15))
        basic_theme <- basic_theme + theme(axis.ticks.y = element_line(size=0.15))
        basic_theme <- basic_theme + theme(axis.ticks.length = grid::unit(5, "pt"))
    }
    basic_theme
}
#' USSC theme with light Univers font
#' @description
#' A theme_bw() with light Univers font
#' @section Building upon `theme_ussc_d`:
#' The function is setup in such a way that you can customize your own one by just
#' wrapping the call and changing the parameters. See source for examples.
#' @usage 
#' theme_uscc_univers_light_d()
#' @param 
#' grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param 
#' axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param 
#' ticks ticks if `TRUE` add ticks
#' @param 
#' border border if `TRUE` add plot border
#' @examples 
#' Create ggplot theme using light Univers USSC theme
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) + geom_point(size = 4, alpha=0.4) +  theme_ussc_univers_light_d(axis=TRUE, border=FALSE, grid='xy') + labs(title="Light Univers Header", x="Sepal Width", y="Sepal Length") + scale_colour_ussc()
#' @author 
#' Zoe Meers
theme_ussc_univers_light_d <- function(grid = TRUE, axis = FALSE, ticks = FALSE, border=TRUE){
    basic_theme <- theme_bw(base_family="universLight") + 
        theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
              text = element_text(colour="#444444"),
              panel.border = element_rect(colour = "#cccccc"),
              axis.ticks = element_line(colour = "#444444"),
              axis.title.x = element_text(size=9),
              axis.title.y = element_text(size=9),
              axis.text.x = element_text(size = 9),
              axis.text.y = element_text(size = 7),
              legend.text = element_text(size=7),
              legend.title = element_text(size=9),
              legend.key.size = unit(x = 9,units = "pt"),
              plot.subtitle=element_text(size=10),
              plot.title= element_text(size=14),
              plot.caption =element_text(size=7),
              plot.background = element_blank(),
              strip.background = element_blank(),
              legend.background = element_blank()
        )
    
    if (inherits(grid, "character") | grid == TRUE) {
        
        basic_theme <- basic_theme + theme(panel.grid=element_line(color="#cccccc", size=0.2))
        basic_theme <- basic_theme + theme(panel.grid.major=element_line(color="#cccccc", size=0.2))
        basic_theme <- basic_theme + theme(panel.grid.minor=element_line(color="#cccccc", size=0.15))
        
        if (inherits(grid, "character")) {
            if (regexpr("X", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.major.x=element_blank())
            if (regexpr("Y", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.major.y=element_blank())
            if (regexpr("x", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.minor.x=element_blank())
            if (regexpr("y", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.minor.y=element_blank())
        }
        
    } else {
        basic_theme <- basic_theme + theme(panel.grid=element_blank())
    }
    
    if (inherits(axis, "character") | axis == TRUE) {
        basic_theme <- basic_theme + theme(axis.line=element_line(color="#cccccc", size=0.15))
        if (inherits(axis, "character")) {
            axis <- tolower(axis)
            if (regexpr("x", axis)[1] < 0) {
                basic_theme <- basic_theme + theme(axis.line.x=element_blank())
            } else {
                basic_theme <- basic_theme + theme(axis.line.x=element_line(color="#cccccc", size=0.15))
            }
            if (regexpr("y", axis)[1] < 0) {
                basic_theme <- basic_theme + theme(axis.line.y=element_blank())
            } else {
                basic_theme <- basic_theme + theme(axis.line.y=element_line(color="#cccccc", size=0.15))
            }
        } else {
            basic_theme <- basic_theme + theme(axis.line.x=element_line(color="#cccccc", size=0.15))
            basic_theme <- basic_theme + theme(axis.line.y=element_line(color="#cccccc", size=0.15))
        }
    } else {
        basic_theme <- basic_theme + theme(axis.line=element_blank())
    }
    if (!border) {
        basic_theme <- basic_theme + theme(panel.border =  element_blank())
    } else {
        basic_theme <- basic_theme + theme(panel.border = element_rect(size=0.15, color="#cccccc"))
    }
    if (!ticks) {
        basic_theme <- basic_theme + theme(axis.ticks = element_blank())
        basic_theme <- basic_theme + theme(axis.ticks.x = element_blank())
        basic_theme <- basic_theme + theme(axis.ticks.y = element_blank())
    } else {
        basic_theme <- basic_theme + theme(axis.ticks = element_line(size=0.15))
        basic_theme <- basic_theme + theme(axis.ticks.x = element_line(size=0.15))
        basic_theme <- basic_theme + theme(axis.ticks.y = element_line(size=0.15))
        basic_theme <- basic_theme + theme(axis.ticks.length = grid::unit(5, "pt"))
    }
    basic_theme
}

#' USSC theme with Neo Sans Pro font
#' @description
#' theme_bw() with Neo Sans Pro font
#'  @section Building upon `theme_ussc_d`:
#' The function is setup in such a way that you can customize your own one by just
#' wrapping the call and changing the parameters. See source for examples.
#' @usage 
#' theme_ussc_neosanspro_d()
#' @param 
#' grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param 
#' axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param 
#' ticks ticks if `TRUE` add ticks
#' @param 
#' border border if `TRUE` add plot border
#' @examples 
#' Create ggplot theme using Neo Sans Pro USSC theme
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) + geom_point(size = 4, alpha=0.4) +  theme_ussc_neosanspro_d(grid="XYxy", border=FALSE) + labs(title="Neo Sans Pro Header", x="Sepal Width", y="Sepal Length") + scale_colour_ussc()
#' @author 
#' Zoe Meers

theme_ussc_neosanspro_d <- function(grid = TRUE, axis = FALSE, ticks = FALSE, border=TRUE){
    basic_theme <- theme_bw(base_family="neosanspro") + 
        theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
              text = element_text(colour="#444444"),
              panel.border = element_rect(colour = "#cccccc"),
              axis.ticks = element_line(colour = "#444444"),
              axis.title.x = element_text(size=9),
              axis.title.y = element_text(size=9),
              axis.text.x = element_text(size = 9),
              axis.text.y = element_text(size = 7),
              legend.text = element_text(size=7),
              legend.title = element_text(size=9),
              legend.key.size = unit(x = 9,units = "pt"),
              plot.subtitle=element_text(size=10, family="neosansproLight"),
              plot.title= element_text(size=14),
              plot.caption =element_text(size=7, family="neosansproLight"),
              plot.background = element_blank(),
              strip.background = element_blank(),
              legend.background = element_blank()
        )
    
    if (inherits(grid, "character") | grid == TRUE) {
        
        basic_theme <- basic_theme + theme(panel.grid=element_line(color="#cccccc", size=0.2))
        basic_theme <- basic_theme + theme(panel.grid.major=element_line(color="#cccccc", size=0.2))
        basic_theme <- basic_theme + theme(panel.grid.minor=element_line(color="#cccccc", size=0.15))
        
        if (inherits(grid, "character")) {
            if (regexpr("X", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.major.x=element_blank())
            if (regexpr("Y", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.major.y=element_blank())
            if (regexpr("x", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.minor.x=element_blank())
            if (regexpr("y", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.minor.y=element_blank())
        }
        
    } else {
        basic_theme <- basic_theme + theme(panel.grid=element_blank())
    }
    
    if (inherits(axis, "character") | axis == TRUE) {
        basic_theme <- basic_theme + theme(axis.line=element_line(color="#cccccc", size=0.15))
        if (inherits(axis, "character")) {
            axis <- tolower(axis)
            if (regexpr("x", axis)[1] < 0) {
                basic_theme <- basic_theme + theme(axis.line.x=element_blank())
            } else {
                basic_theme <- basic_theme + theme(axis.line.x=element_line(color="#cccccc", size=0.15))
            }
            if (regexpr("y", axis)[1] < 0) {
                basic_theme <- basic_theme + theme(axis.line.y=element_blank())
            } else {
                basic_theme <- basic_theme + theme(axis.line.y=element_line(color="#cccccc", size=0.15))
            }
        } else {
            basic_theme <- basic_theme + theme(axis.line.x=element_line(color="#cccccc", size=0.15))
            basic_theme <- basic_theme + theme(axis.line.y=element_line(color="#cccccc", size=0.15))
        }
    } else {
        basic_theme <- basic_theme + theme(axis.line=element_blank())
    }
    if (!border) {
        basic_theme <- basic_theme + theme(panel.border =  element_blank())
    } else {
        basic_theme <- basic_theme + theme(panel.border = element_rect(size=0.15, color="#cccccc"))
    }
    if (!ticks) {
        basic_theme <- basic_theme + theme(axis.ticks = element_blank())
        basic_theme <- basic_theme + theme(axis.ticks.x = element_blank())
        basic_theme <- basic_theme + theme(axis.ticks.y = element_blank())
    } else {
        basic_theme <- basic_theme + theme(axis.ticks = element_line(size=0.15))
        basic_theme <- basic_theme + theme(axis.ticks.x = element_line(size=0.15))
        basic_theme <- basic_theme + theme(axis.ticks.y = element_line(size=0.15))
        basic_theme <- basic_theme + theme(axis.ticks.length = grid::unit(5, "pt"))
    }
    basic_theme
}


#' USSC theme with Neo Sans Pro font
#' @description
#' theme_bw() with Neo Sans Pro font
#'  @section Building upon `theme_ussc_d`:
#' The function is setup in such a way that you can customize your own one by just
#' wrapping the call and changing the parameters. See source for examples.
#' @usage 
#' theme_ussc_neosanspro_light_d()
#' @param 
#' grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param 
#' axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param 
#' ticks ticks if `TRUE` add ticks
#' @param 
#' border border if `TRUE` add plot border
#' @examples 
#' Create ggplot theme using Neo Sans Pro USSC theme
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) + geom_point(size = 4, alpha=0.4) +  theme_ussc_neosanspro_light_d(grid="XY", border=FALSE) + labs(title="Neo Sans Pro Header", x="Sepal Width", y="Sepal Length") + scale_colour_ussc()
#' @author 
#' Zoe Meers

theme_ussc_neosanspro_light_d <- function(grid = TRUE, axis = FALSE, ticks = FALSE, border=TRUE){
    basic_theme <- theme_bw(base_family="neosansproLight") + 
        theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
              text = element_text(colour="#444444"),
              panel.border = element_rect(colour = "#cccccc"),
              axis.ticks = element_line(colour = "#444444"),
              axis.title.x = element_text(size=9),
              axis.title.y = element_text(size=9),
              axis.text.x = element_text(size = 9),
              axis.text.y = element_text(size = 7),
              legend.text = element_text(size=7),
              legend.title = element_text(size=9),
              legend.key.size = unit(x = 9,units = "pt"),
              plot.subtitle=element_text(size=10),
              plot.title= element_text(size=14),
              plot.caption =element_text(size=7),
              plot.background = element_blank(),
              strip.background = element_blank(),
              legend.background = element_blank()
        )
    
    if (inherits(grid, "character") | grid == TRUE) {
        
        basic_theme <- basic_theme + theme(panel.grid=element_line(color="#cccccc", size=0.2))
        basic_theme <- basic_theme + theme(panel.grid.major=element_line(color="#cccccc", size=0.2))
        basic_theme <- basic_theme + theme(panel.grid.minor=element_line(color="#cccccc", size=0.15))
        
        if (inherits(grid, "character")) {
            if (regexpr("X", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.major.x=element_blank())
            if (regexpr("Y", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.major.y=element_blank())
            if (regexpr("x", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.minor.x=element_blank())
            if (regexpr("y", grid)[1] < 0) basic_theme <- basic_theme + theme(panel.grid.minor.y=element_blank())
        }
        
    } else {
        basic_theme <- basic_theme + theme(panel.grid=element_blank())
    }
    
    if (inherits(axis, "character") | axis == TRUE) {
        basic_theme <- basic_theme + theme(axis.line=element_line(color="#cccccc", size=0.15))
        if (inherits(axis, "character")) {
            axis <- tolower(axis)
            if (regexpr("x", axis)[1] < 0) {
                basic_theme <- basic_theme + theme(axis.line.x=element_blank())
            } else {
                basic_theme <- basic_theme + theme(axis.line.x=element_line(color="#cccccc", size=0.15))
            }
            if (regexpr("y", axis)[1] < 0) {
                basic_theme <- basic_theme + theme(axis.line.y=element_blank())
            } else {
                basic_theme <- basic_theme + theme(axis.line.y=element_line(color="#cccccc", size=0.15))
            }
        } else {
            basic_theme <- basic_theme + theme(axis.line.x=element_line(color="#cccccc", size=0.15))
            basic_theme <- basic_theme + theme(axis.line.y=element_line(color="#cccccc", size=0.15))
        }
    } else {
        basic_theme <- basic_theme + theme(axis.line=element_blank())
    }
    if (!border) {
        basic_theme <- basic_theme + theme(panel.border =  element_blank())
    } else {
        basic_theme <- basic_theme + theme(panel.border = element_rect(size=0.15, color="#cccccc"))
    }
    if (!ticks) {
        basic_theme <- basic_theme + theme(axis.ticks = element_blank())
        basic_theme <- basic_theme + theme(axis.ticks.x = element_blank())
        basic_theme <- basic_theme + theme(axis.ticks.y = element_blank())
    } else {
        basic_theme <- basic_theme + theme(axis.ticks = element_line(size=0.15))
        basic_theme <- basic_theme + theme(axis.ticks.x = element_line(size=0.15))
        basic_theme <- basic_theme + theme(axis.ticks.y = element_line(size=0.15))
        basic_theme <- basic_theme + theme(axis.ticks.length = grid::unit(5, "pt"))
    }
    basic_theme
}
