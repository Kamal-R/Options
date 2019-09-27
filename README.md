[![Build Status](https://travis-ci.com/apreshill/bookdown-travis-branch-deploy.svg?branch=master)](https://travis-ci.com/apreshill/bookdown-travis-branch-deploy) [![Netlify Status](https://api.netlify.com/api/v1/badges/813c7f3f-1a86-4608-846d-4779bcc25919/deploy-status)](https://app.netlify.com/sites/bookdown-travis-branch-deploy/deploys) 
[![CC BY SA 4.0](https://img.shields.io/badge/License-CC%20BY%20SA%204.0-green.svg)](https://creativecommons.org/licenses/by-sa/4.0/)

To follow along:

1. [Make a repo on GitHub](https://happygitwithr.com/new-github-first.html#make-a-repo-on-github-2)

1. [Create a new RStudio Project via git clone](https://happygitwithr.com/new-github-first.html#new-rstudio-project-via-git-clone)

1. In your R console, enter:

```
bookdown:::bookdown_skeleton(getwd())
```

**Follow my lead!**

+ [Travis + GitHub Pages](https://musing-aryabhata-b16338.netlify.com/travis-ghpages.html)

+ [Travis + Netlify](https://musing-aryabhata-b16338.netlify.com/travis-netlify.html)

**My Travis build logs:**

+ [Build #1: Build the book only](https://travis-ci.com/apreshill/bookdown-travis-branch-deploy/builds/98667100)

  ```
  Output created: _book/index.html
  [1] "/home/travis/build/apreshill/bookdown-travis-branch-deploy/_book/index.html"
  The command "Rscript -e 'bookdown::render_book("index.Rmd")'" exited with 0.
  
  store build cache
  
  Done. Your build exited with 0.
  ```

+ [Build #2: Build the book and deploy to gh-pages branch](https://travis-ci.com/apreshill/bookdown-travis-branch-deploy/builds/98668547)

  ```
  Output created: _book/index.html
  [1] "/home/travis/build/apreshill/bookdown-travis-branch-deploy/_book/index.html"
  The command "Rscript -e 'bookdown::render_book("index.Rmd")'" exited with 0.
  
  store build cache
  
  Installing deploy dependencies
  Logged in as @apreshill (Alison Presmanes Hill)
  
  Preparing deploy
  Deploying application
  Done. Your build exited with 0.
  ```
  
  + Deployed! https://apreshill.github.io/bookdown-travis-branch-deploy/
  + Deployed! https://bookdown-travis-branch-deploy.netlify.com/
