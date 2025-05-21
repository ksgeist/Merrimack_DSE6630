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

For each team:

Checkout the team branch.
Merge that team branch into main.
Push updated main.
Then merge main back into the team branch.
Push the updated team branch.

## PURPOSE: Streamlines iterative update process
## 1. Merges each team branch into main.
## 2. Then merges updated main back into each team branch.

## List of team branches
teams=("alpha" "delta" "gamma" "lambda" "sigma" "epsilon" "omega" "kappa" "rho" "theta")

## Ensure working directory is clean first - will fail if not!
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
    echo "Processing $branch branch..."

    ## Fetch latest:
    git fetch origin

    ## Make sure local team branch exists or create it from remote
    if git show-ref --verify --quiet refs/heads/"$branch"; then
        git checkout "$branch"
    else
        git checkout -b "$branch" origin/"$branch" || { echo "$branch checkout failed!!"; continue; }
    fi

	## Merge team branch into main
    echo "Merging $branch into main..."
    git checkout main
    ## Attempt merge:
    if ! git merge "$branch" -m "Merge $branch into main"; then
        echo "Merge conflict found. Skipping."
        ## Abort the merge
        git merge --abort
        continue
    fi
    ## Attempt push back onto main:
    git push origin main || { echo "main push failed!!"; continue; }

done

echo "Synchronization finished."
