# Haskell-GitHub-Trending  

Final project for UCSD CSE230 Fa21

Collaborators:

- Wenzhe Zhang (A59002032)
- Linfang He   (A59002470)
- Xinhao Luo (A59012359)
- Aiwei YIN (A59004363)


### Goal

#### Description

GitHub Trending include recent most popular public GitHub repositories, and users could click in to see the details of these repositories. Here is a screenshot of the UI of GitHub Trending:

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

#### Public APIs for GitHub Repo Search

> ```sh
> curl -G https://api.github.com/search/repositories --data-urlencode "sort=stars" --data-urlencode "order=desc" --data-urlencode "q=language:java"  --data-urlencode "q=created:>`date -v-7d '+%Y-%m-%d'`"
> ```

Postman example: (Get)

```shell
https://api.github.com/search/repositories?sort=stars&order=desc&q=language:java&q=created:>2021-11-26
```

The result looks like:

```
{
    "total_count": 609,
    "incomplete_results": false,
    "items": [
        {
            "id": 432357535,
            "node_id": "R_kgDOGcVAnw",
            "name": "ShadyAddons",
            "full_name": "4wl/ShadyAddons",
            "private": false,
            "owner": {
                "login": "4wl",
                "id": 69080903,
            },
            "description": "Not fixed.",
            "fork": false,
            "url": "https://api.github.com/repos/4wl/ShadyAddons",
            "size": 514,
            "created_at": "2021-11-27T03:22:33Z",
            "updated_at": "2021-11-27T04:42:37Z",
            "pushed_at": "2021-11-27T03:33:55Z",
            "stargazers_count": 3,
            "watchers_count": 3,
            "language": "Java",
            "forks_count": 1,
            "archived": false,
            "disabled": false,
            "open_issues_count": 0,
            "license": null,
            "visibility": "public",
            "default_branch": "main",
            "score": 1.0
        },
        ...
    ]
}     
```

Only parameters could be used in this project have been displayed; Others are omitted.

## Setup

To Run the project, simply do

```bash
stack build
stack run
```

be sure you have stack installed.