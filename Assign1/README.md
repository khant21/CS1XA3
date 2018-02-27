## Assignment 1
### README for ProjectAnalyze.sh
Upon execution of the script, a menu bar with possible commands will be outputted. The user will be prompted to input a command from the list. Each command is described below.

Note: the commands are case sensitive.

#### Base Commands
1. "status" - Informs you if you're local repo is up to date with the remote repository.
2. "changes" - Puts all uncommited changes into a file called changes.log.
3. "todo" - Puts each line from every file of your project with the tag #TODO into a file todo.log
4. "errors" - Checks all haskell files for syntax errors and puts the results into error.log

#### Additional Features
1. "searchExt" - Allows the user to input a file extension and see where all of the files with this extension are located. This is useful when one is having difficulty finding certain files on their server or computer.
2. "searchString" - Allows the user to enter a string of any type. The script searches all files from the home directory that contain this string pattern and outputs to the terminal.
3. Selective execution - The user is prompted to enter one command from the list rather than have the entire script run with all commands being executed at once. [citation: https://github.com/elwazana]
4. Open file prompt - If 'Y' has been entered, the contents that have been transferred into a file can be viewed for certain commands. For example, after all errors have been found and moved to error.log with the 'errors' command, the user is prompted to open this error.log file.
5. The script opens up with a vertical menu bar that contains a list of commands that the user can enter. This is useful because the user will not have to keep going back to the README file to see command syntax.
