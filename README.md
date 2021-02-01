# ECS713U - Group Project
Group 7 - William Heron and Brandon Taylor

This project is a simple Riot Games API project written in Haskell, which aims to retrieve data from the API endpoints, store it locally in an SQLite database, and execute queries on all of the information currently stored. Operations include retrieving new Summoners, their Matches, and querying the total number of kills, deaths or assists across their games. There is also the ability to dump the database as JSON, which utilises the JSON1 SQLite extension.

## Building and Running Application
In order to build and run this project, you will require a Stack installation.

If stack is installed, you can simply move the project files to their own directory, and within it, 
execute the following in the terminal to compile the project: `stack build`.

Once the project successfully builds, you can execute the project using `stack exec group-project-exe`, or to view
the documentation, use `stack haddock --open` in order to generate the documentation from the source-code and once
generated, it will open in your browser.

