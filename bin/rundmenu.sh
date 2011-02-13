#!/bin/sh

font='-dejavu-dejavu-medium-r-normal-*-*-120-100-100-m-0-iso8859-*'
background='#FFEA85'
foreground='#000000'
selectedBackground='#A8C6FA'
selectedForeground='#FF00FF'

$(dmenu_path | dmenu -i -fn $font -nb $background -nf $foreground -sb $selectedBackground -sf $selectedForeground)  

