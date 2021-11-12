# Haskell-GitHub-Trending  

Final project for UCSD CSE230 Fa21

Collaborators:

- Wenzhe Zhang (A59002032)
- Linfang He   (A59002470)
- Xinhao Luo (A59012359)


### Goal

#### Description

GitHub Trending include recent most popular GitHub repositories, and users could click in to see the details of these repositories. Here is a screenshot of the UI of GitHub Trending:

![TrendingUI](./img/TrendingUI.png)

Our goal is to implement a Haskell application to display the trending and repositories, and users could filter these trends by different categories like language, date, etc. and freely `click in` these repositories to see detailed contents.


#### Useful Deps

- https://github.com/phadej/github Github API for fetching repo stat
- https://github.com/haskell/aeson JSON parser
- https://hackage.haskell.org/package/req Network Request