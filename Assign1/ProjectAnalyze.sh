#!/bin/bash

#output a list of possible commands
echo "Commands List:"
echo "status"
echo "changes"
echo "todo"
echo "errors"
echo "searchExt"
echo "searchString"
echo " "

#get the user input for the command
read -p "Input Command: " cmd

#check if the repo is up to date
if [ "$cmd" = "status" ]
then
	git status

#put uncommitted changes in a changes.log file
elif [ "$cmd" = "changes" ]
then
	git diff > changes.log
	echo "Changes added to 'changes.log'"
	
	#prompt to open changes.log
        read -p "open changes.log? (Y/N) " open
        if [ "$open" = "Y" ]
        then
                vim changes.log
        fi

#put all todo files into a todo.log file
elif [ "$cmd" = "todo" ]
then
	grep -r "#TODO" --exclude=\todo.log  ~/CS1XA3/Assign1/ &> todo.log
	
	#prompt to open todo.log
	read -p "open todo.log? (Y/N) " open
        if [ "$open" = "Y" ]
        then
                vim todo.log
        fi

#find errors in haskell files and move them to a error.log file
elif [ "$cmd" = "errors" ]
then
	find ~ -iname "*.hs" -exec ghc -fno-code "{}" \; &> error.log
	echo "errors have been moved to 'error.log'"
	
	#prompt to open error.log
	read -p "open error.log? (Y/N) " open
	if [ "$open" = "Y" ]
	then
		vim error.log
	fi

#search file extension feature
elif [ "$cmd" = "searchExt" ]
then
	read -p "Enter the name of the extension (without the '.') : " file
	find ~ -iname "*.$file"

#find all files that contain a certain string
elif [ "$cmd" = "searchString" ]
then
	read -p "Enter word or phrase to search for: " string
	grep -rl "$string" ~
fi
