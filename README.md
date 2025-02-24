# CV plot operator with shiny interface for Bionavigator


# Deploy an app using git

```
bntools::createApp(tags=c('Visualization Components'), mainCategory = 'Visualization')
```

Push a new verion to git origin, and set a new tag

```
git add -A && git commit -m "++" && git push && git tag -a 2.1 -m "test" && git push --tags
```

Deploy the package on Pamgene CRAN and the app in Pamgene App Store

```
bntools::deployGitApp('https://bitbucket.org/bnoperator/cvplotoperator.git', '2.1')
```

# Publish a package on pamagene R repository

This is not necessary unless you want to update the package only

```
bntools::deployGitPackage('https://bitbucket.org/bnoperator/cvplotoperator.git', '2.1')
```
