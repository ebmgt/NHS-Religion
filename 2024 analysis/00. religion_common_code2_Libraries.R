# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author:rbadgett@kumc.edu and zyang.uconn@gmail.com
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2025-06-14

# Libraries and packages -----
function_progress(0,'Libraries')

packages_essential <- c('stringr','grid','dplyr','readr', 'crayon')
function_libraries_install(packages_essential)
function_progress(20,'Libraries')

packages_open_office <- c('openxlsx','htmltools')
function_libraries_install(packages_open_office)
function_progress(30,'Libraries')