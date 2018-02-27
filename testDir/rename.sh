#!/bin/bash

if [ $# -eq 2 ]
then
	mv "$1" "$2"
	echo "rename successful"
else
	if [ $# -lt 2 ]
	then 
		echo "too few args"
	else
		echo "too many args"
	fi
fi
