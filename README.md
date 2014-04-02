# Extraction

This project is meant to contain glue code that integrates our various extraction processes into a single pipeline.
                                                                                                    
## Project layout                                                                                   
                                                                                                    
There are three subprojects:
                                                                                                    
1. interface: A definition of models used as output for extractions (extraction targets)            
2. demo: A webapp running an extraction demo 
3. ermine: An extraction manager to handle workflows

"ermine" is under construction.                                                                     

                                                                                                    
## Running ermine

ermine is not ready for consumption.

### Prolog
                                                                                                    
ermine currently depends on having swipl Prolog installed with the JPL library.                     
                                                                                                    
You can install this easily on OS X with [homebrew](http://brew.sh/):                               
                                                                                                    
`brew install swi-prolog --with-jpl`

Don't forget the `--with-jpl` flag! The root project's `Build.scala` will check for a valid swipl install at load time, and will print out a warning if one is missing.
