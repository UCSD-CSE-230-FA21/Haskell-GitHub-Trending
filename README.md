# Haskell-GitHub-Trending  

Final project for UCSD CSE230 Fa21

Collaborators:

- Wenzhe Zhang (A59002032)
- Linfang He   (A59002470)
- Xinhao Luo (A59012359)
- Aiwei YIN (A59004363)


### Goal

#### Description

GitHub Trending include recent most popular GitHub repositories, and users could click in to see the details of these repositories. Here is a screenshot of the UI of GitHub Trending:

![TrendingUI](./img/TrendingUI.png)

Our goal is to implement a Haskell application to display the trending and repositories, and users could filter these trends by different categories like language, date, etc. and freely `click in` these repositories to see detailed contents.

#### Tredning fetch

Our application would fetch current trending from GitHub, and then display the trending in commandline GUI. 

#### Filters

User could user different filters to filter repositories in trending. These filters should include `Spoken Language`, `Development Language` and `Date Range`. These filters could be used together or seperately. For example, `Spoken Language = Chinese, Development Language = Java, Date Range = this week` is a valid filter, and `Spoken Language = Japanese` is also a valid filter.

#### Repo display

User could `click in` a certain repository in the trending to view its detailed information. User should type in the unique identifier of the repo, like its index in the trending. (Deatils need confirmed)

#### Repo mark

User could mark a repository as `read`, and this mark would be stored in file locally. Once the user exits our application, the mark record would be stored in disk. The next time the user runs the application, the record file should be loaded and reflected correctly when displaying the trending.


#### Useful Deps

- https://github.com/phadej/github Github API for fetching repo stat
- https://github.com/haskell/aeson JSON parser
- https://hackage.haskell.org/package/req Network Request
