#!/bin/sh

Rscript -e 'shiny::runApp(port=7678, launch.browser=FALSE, host="0.0.0.0")'

