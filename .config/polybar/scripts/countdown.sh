#!/bin/bash

curr_year=$(date +'%y')
curr_day=$(date +'%j')
curr_hour=$(date +'%-H')

hours_passed=$((($curr_year - 2) * 365 * 24))
hours_passed=$(($hours_passed + ($curr_day * 24)))
hours_passed=$(($hours_passed + $curr_hour))

hours_total=700800

hours_left=$(($hours_total - $hours_passed))

echo $hours_left
