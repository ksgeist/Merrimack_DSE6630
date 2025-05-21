#!/bin/bash

##################################
## WARNING: NOT FOR STUDENT USE ##
##################################

##################################
## Author: KS Geist ##############

## Usage: run from repo root
##   bash .sync_branches.sh 

## If you need to make executable:
## on the CL from repo root dir:
##   chmod +x sync_branches.sh
##################################
## Pre-reqs: #####################
## Assumes main is a protected branch, thus direct pushes to main are not allowed.
## Assumes all changes must come through pull requests (PR).
## Pull requests can be generated using the GitHub CLI https://cli.github.com

## My setup was as follows:
## brew install gh
## gh auth login
## set up SSH protocol (autoSync on ED25519)
## followed authentication 
##################################
## PURPOSE: ######################
## Streamline iterative update process for more efficient class management
## 1. Merges each team branch into main.
## 2. Then merges updated main back into each team branch.

## Logic is as follows - For each team branch:
## a. Fetch the branch
## b. Create a temporary integration branch (e.g., merge-alpha-to-main).
## c. Merge the team branch into that temp branch.
## d. Push the temp branch to GitHub.
## e. Create a PR into main.
## f. Automatically merge the PR (if possible) using gh pr merge.
###################################

## Team branches
#teams=("alpha" "delta" "gamma" "lambda" "sigma" "epsilon" "omega" "kappa" "rho" "theta")
teams=("alpha" "rho") #"gamma" "lambda" "sigma" "epsilon" "omega" "kappa" "rho" "theta")

## Make sure working directory is clean first - will fail if not!
if ! git diff-index --quiet HEAD --; then
    echo "Working dir not clean; commit or stash changes before running"
	## Returns the status
    git status
    exit 1
fi

################
## Start task 1:
echo "Starting merge of each team branch into main..."

## Checkout main branch & pull:
echo "Checking out main branch..."
git checkout main || { echo "main checkout failed!!"; exit 1; }
echo "Pulling latest on main..."
git pull origin main || { echo "main pull failed!!"; exit 1; }

## Loop through each of the team branches:
for branch in "${teams[@]}"; do
    echo "###################################################"
    echo "Processing $branch branch..."

    ## Fetch latest:
    git fetch origin

    ## Make sure local team branch exists or create it from remote
    if git show-ref --verify --quiet refs/heads/"$branch"; then
        git checkout "$branch"
    else
        git checkout -b "$branch" origin/"$branch" || { echo "$branch checkout failed!!"; continue; }
    fi

	## Create and checkout temp branch off main; each team branch will get it's own
    temp="merge-${branch}2main"
    git checkout -b "$temp" main || { echo "Temporary branch $temp creation failed!!"; continue; }

    echo "Merging $branch --> main via $temp..."

    ## Merge team branch into temp branch
    if ! git merge "$branch" -m "Merge $branch into main via $temp"; then
        echo "Merge conflict found with $branch --> $temp. Skipping."
        git merge --abort
        git checkout main
        git branch -D "$temp"
        continue
    fi

    ## Push temp branch to main
    git push -u origin "$temp" || { echo "Push $temp --> main failed!!"; continue; }

    ## Create a PR into main
    echo "Creating pull request $temp --> main..."
#     pr_url=$(gh pr create --base main --head "$temp" --title "Merge $branch into main" --body "Auto pull request to merge $branch into main" --json url -q ".url")
# 
#     if [[ -z "$pr_url" ]]; then
#         echo "Pull request creation for $branch --> main failed. Merge skipped."
#         continue
#     fi
	
	## Create PR from temp branch into main
	#echo "Creating pull request $temp --> main..."
	if ! gh pr create --base main --head "$temp" --title "Merge $branch into main" --body "Auto pull request to merge $branch into main"; then
         echo "Pull request creation for $branch --> main failed. Merge skipped."
	else
		echo "Pull request created: $branch --> main."
	fi

    ## Auto-merge the PR
    echo "Attempting to auto-merge pull request..."
    if ! gh pr merge "$pr_url" --merge --delete-branch --yes; then
        echo "Auto-merge for $pr_url failed!!"
        echo "Merge manually."
    else
        echo "Pull request auto-merged and $temp deleted."
    fi

# ###### Merge only way for an unprotected main branch with no PR needed.
# 	## Merge team branch into main
#     echo "Merging $branch into main..."
#     git checkout main
#     ## Attempt merge:
#     if ! git merge "$branch" -m "Merge $branch into main"; then
#         echo "Merge conflict found. Skipping."
#         ## Abort the merge
#         git merge --abort
#         continue
#     fi
#     ## Attempt push back onto main:
#     git push origin main || { echo "main push failed!!"; continue; }
done
echo "Task 1 completed: All team branches merged into main."
echo "######"

################
## Start task 2:
echo "Now updating each team branch from main..."

for branch in "${teams[@]}"; do
  echo "Merging main --> $branch"
  git checkout "$branch"
  git pull origin "$branch"

  ## Check for merge conflicts; abort if needed
  if ! git merge origin/main -m "Sync main into $branch"; then
    echo "Merge conflict found with main --> $branch. Skipping."
    git merge --abort
    continue
  fi

  ## Attempt push back onto main:
  git push origin $branch || { echo "$branch push failed!!"; continue; }
done

echo "###################################################"
echo "###################################################"
echo "###################################################"
echo "Synchronization finished."
