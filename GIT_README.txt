To make a new commit to an existing repository, do the following in Git Bash:
 1. git add .
 2. git commit -m "<Describe commit here>"
 3. git push

This should be done after any directories or files are added anywhere in the repository.

If you want to make changes to an existing file, do the following:
 1. In Git Bash, enter the command "git checkout -b <branch_name>"
 2. Verify the new branch using the Git Bash command "git branch"
 3. Push the new branch when ready using the Git Bash command "git push --set-upstream origin <branch_name>"
 4. Go to the GitHub repository page at https://github.com/charlesrclark1243/ECO322Spring2022Project1
    and you should see a "Compare & pull request" button in a yellow bar on the top of the page.
 5. Click the "Compare & pull request" button.
 6. Write a brief description of what you changed in the "Open a pull request" page.
 7. In the "Open a pull request" page, select someone to review the change using the "Reviewers" tab.
 8. When done, click "Create pull request".

If you are the team's designated pull request "Reviewer," then you should check notifications
whenever you log into GitHub as there may be an alert that someone asked you to be their reviewer.
Follow the following steps:
 1. You should notice a yellow bar indicated a teammate "requested your review of this pull request".
 2. Click the "Add your review" button; this will take you to the Pull Request page.
 3. Click the "Review changes" drop-down menu: write a review summary, choose an action ("Comment,"
    "Approve," and "Request changes"), and click "Submit review" when ready.
 4. When satisfied with the pull request, go to the bottom of the pull request and click "Merge pull request".
 5. You should then see the message "Pull request successfully merged and closed" and a button to "Delete branch"
    which you should click.

Source: https://medium.com/@jonathanmines/the-ultimate-github-collaboration-guide-df816e98fb67