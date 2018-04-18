#!/bin/sh

Rscript -e 'shiny::runApp(port=7677, launch.browser=FALSE, host="0.0.0.0")'

