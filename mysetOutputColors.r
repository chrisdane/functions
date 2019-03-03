## R
# if colorout package is used, load my default colors
my_setOutputColors <- function(normal=0, stderror=0,
                               number=20, negnum=20, zero=20,
                               date=39, string=39, const=35, false=203, true=40,
                               infinite=39, index=30, warn=202, error=c(160, 231),
                               zero.limit=NA, verbose=F) {
        # c(formating, background color, foreground color)
        # message(): stderror; cat(): normal; print("asd"): string
        # check defaults with my_setOutputColors(verbose=T)
        # colors: show256Colors()
        # attributes:
        # 0 No formating       Default or black 
        # 1 Bold or bright     Red          
        # 2 Faint              Green        
        # 3 Italic or inverse  Yellow       
        # 4 Underline          Blue         
        # 5 Blink slowly       Magenta      
        # 6 Blink quickly      Cyan     
        # 7 Invert             White
        setOutputColors(normal, negnum, zero, number, date, string,
                        const, false, true, infinite, index, stderror,
                        warn, error, zero.limit=zero.limit, verbose)
} # my_setOutputColors function

