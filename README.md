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

#### Trending fetch

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



## Milestone 2 Update

### Architecture

![architecture](./img/architecture.png)

There would be three main modules in our application.

1. Network module: it is used to communicate with GitHub via Http calls. Query conditions should be provided from end user when making a call. The retrieved responses would be interpreted into data type which is pre-defined.
2. Storage module: it is used to read and write the filing storing flag (e.g. user marks a repo) information on disk.
3. Display module: it is used to receive data from Network module, and query whether a repo has been marked through Storage module. Then this module would integrate the two parts and organize data to display for end users. Also it should receive commands from end users, and pass these commands to relevant downstream modules.

### Challenges

There are mainly two challenges:

1. There are no public GitHub trending APIs.
2. How to deal with API call exceptions.

For the first one, there are two ways to solve it:

1. There is a public server providing request-forwarding-like function, which could receive our requests and send the trending results to us.
2. We could assemble query conditions and make use of public GitHub repo query APIs, and we should process the returned data to retrieve trending results.

The first one is much easier to operate, however, the second is much more stable and reliable. So in the end, we decided to move forward with the second solution.

For the second challenge, which did not come to our minds until Nov 27, 2021, when GitHub was down for a long while. We then realized we must handle the exceptions from API calls. In this way, we decided to display the error message directly to end users, but we may change this solution to a wrapped and user-friendly error message.

### Schedule

By far, we could finish all the goals we set up before deadline.

## Setup

To Run the project, simply do

```bash
stack build
stack run
```

be sure you have stack installed.