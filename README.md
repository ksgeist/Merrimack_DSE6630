# Merrimack_DSE6630
This is the repository for DSE6630 Healthcare and Life Sciences Analytics at Merrimack College.

You will find four subdirectories, one for each of the project modules. 
* I. Biomedical & Clinical Informatics
* II. Public Health & Epidemiology Informatics
* III. Bioinformatics
* IV. Data Integrity & Patient Protection
* V. Final Project

__Think of these as _template_ directories__. Your task will be to copy the templates into your team's directory (under `Team_Directories`) and you will do your work on the copy.

# Branch-specific Instructions
Each team is working on their own branch. Remember, branches run in _parallel_ to the main branch. This means that you should be working from and pushing to your team's branch, which I've already made for you. Once you do, you will automatically see a `Pull Request` generated; that means that I will need to __merge__ your changes to your branch with the `main` branch. So, team alpha will not be able to see what team omega has done, and vice versa, until Dr. Geist (acting as your administrator) has merged your changes with `main`. 

But your changes still exist on the remote repo! They just exist on _your team's branch_! To see it on GitHub, simply change your branch from `main` to your team's branch! You may need to refresh your browser.

## GitHub Desktop Instructions:
1. Clone our course repo:
`File` -> `Clone Repository`
It should come up automatically once you are a collaborator. It will not be listed under your repos, however.

2. Open your local repo on your machine. This is now your working directory.

3. Back in GitHub Desktop, switch to your team branch. You will do this with the drop down in the top-middle of the GUI window. 
__NOTE__: Your team name will be in lower case, so the branch name is also lowercase.
 
4. Work in your assigned folder!

5. Add changes to the staging area, commit, then push:
 - a. Fetch changes from GitHub (pull + merge). Push the `Fetch origin` button on the top-right of the GUI window. 
 - b. Changes will add to staging area if the checkbox next to them is selected.
 - c. Commit changes with message.
 - c. Push your changes to your branch on the remote repo by pressing the button.

## Command-line Instructions:

1. Clone our course repo:
`git clone https://github.com/ksgeist/Merrimack_DSE6630`

2. Change directory to the local repo:
`cd Merrimack_DSE6630`

3. Switch to your team branch:
`git checkout team_name`

__NOTE__: Your team name will be in lower case, so the branch name is also lowercase.

4. Work in your assigned folder!

5. Add changes to the staging area, commit, then push:
 - a. Add to staging area
`git add .`
 - b. Commit changes with message
`git commit -m "My Commit Message"`
 - c. Push your changes to your branch on the remote repo
`git push`

__NOTE__: The file `sync_branches.sh` is NOT for student use. This is for course instructior/admin use only.



