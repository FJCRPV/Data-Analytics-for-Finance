<p>Assignment done by:</p>
<p>Francisco Perestrello, 39001</p>
<p>Nuno Afonso, 39116 </p>
<p>Jorge Gouveia, 39215</p>
#r "nuget:FSharp.Data"
open FSharp.Data
let [<Literal>] CsvPath = __SOURCE_DIRECTORY__ + "/FootballPlayers.csv"
type FootballPlayersCsv = CsvProvider<CsvPath>
let playerStatsTable = 
    FootballPlayersCsv.GetSample().Rows
    |> Seq.toList
playerStatsTable
|> List.truncate 5
* Transform each element of the `playerStatsTable` List into a tuple with the player and team ( `Player`, `Team`)
playerStatsTable
|> List.map( fun x -> x.Player, x.Team )
|> List.truncate 5
* Transform each element of the `playerStatsTable` List into a tuple with the player and league/competiton ( `Player`, `League`)
playerStatsTable
|> List.map( fun x -> x.Player, x.League )
|> List.truncate 5
* Transform each element of the `playerStatsTable` List into a tuple with the player and age ( `Player`, `Age`)
playerStatsTable
|> List.map( fun x -> x.Player, x.Age )
|> List.truncate 5
* Transform each element of the `playerStatsTable` List into a tuple with the player and matches played ( `Player`, `MatchesPlayed`)
playerStatsTable
|> List.map( fun x -> x.Player, x.MatchesPlayed )
|> List.truncate 5
* Transform each element of the `playerStatsTable` List into a tuple with the player and goals scored ( `Player`, `GoalsScored`)
playerStatsTable
|> List.map( fun x -> x.Player, x.GoalsScored)
|> List.truncate 5
* Define a record named `PlayerAndTeam` with a field named `Player` that is a `string` and `Team` that is a `string`. 
Then transform each element of the `playerStatsTable` List into a `PlayerAndTeam` record.
type PlayerAndTeam =
    { Player: string
      Team: string }

playerStatsTable
|> List.map (fun x ->
    { Player = x.Player
      Team = x.Team } )
|> List.truncate 5
* Define a record named `PlayerAndLeague` with a field named `Player` that is a `string` and `League` that is a `string`. 
Then transform each element of the `playerStatsTable` List into a `PlayerAndLeague` record.
type PlayerAndLeague = 
    { Player: string
      League: string }

playerStatsTable
|> List.map ( fun x -> 
    { Player = x.Player
      League = x.League } )
|> List.truncate 5
* Define a record named `PlayerAndAge` with a field named `Player` that is a `string` and `Age` that is a integer(`int`). 
Then transform each element of the `playerStatsTable` List into a `PlayerAndAge` record.
type PlayerAndAge =
    { Player: string 
      Age: int }

playerStatsTable
|> List.map( fun x ->
    { Player = x.Player
      Age = x.Age } )
|> List.truncate 5
* Define a record named `PlayerAndMatchesPlayed` with a field named `Player` that is a `string` and `MatchesPlayed` that is a integer(`int`). 
Then transform each element of the `playerStatsTable` List into a `PlayerAndMatchesPlayed` record.
type PlayerAndMatchesPlayed = 
    { Player: string
      MatchesPlayed: int }

playerStatsTable
|> List.map( fun x -> 
    { Player = x.Player
      MatchesPlayed = x.MatchesPlayed } )
|> List.truncate 5
* Define a record named `PlayerAndGoalsScored` with a field named `Player` that is a `string` and `GoalsScored` that is a integer(`int`). 
Then transform each element of the `playerStatsTable` List into a `PlayerAndGoalsScored` record.
type PlayerAndGoalsScored = 
    { Player: string
      GoalsScored: int }

playerStatsTable
|> List.map( fun x ->
    { Player = x.Player
      GoalsScored = x.GoalsScored } )
|> List.truncate 5
* Transform each element of the `playerStatsTable` List into an anonymous record with a `Player` field that is a `string` and a `Team` field that is a `string`.
playerStatsTable
|> List.map( fun x ->
    {| Player = x.Player
       Team = x.Team |} )
|> List.truncate 5

* Transform each element of the `playerStatsTable` List into an anonymous record with a `Player` field that is a `string` and a `League` field that is a `string`.
playerStatsTable
|> List.map( fun x ->
    {| Player = x.Player
       League = x.League |} )
|> List.truncate 5

* Transform each element of the `playerStatsTable` List into an anonymous record with a `Player` field that is a `string` and a `Age` field that is a integer(`int`).
playerStatsTable
|> List.map( fun x -> 
    {| Player = x.Player
       Age =  x.Age |} )
|> List.truncate 5 
* Transform each element of the `playerStatsTable` List into an anonymous record with a `Player` field that is a `string` and a `MatchesPlayed` field that is a integer(`int`).
playerStatsTable
|> List.map( fun x -> 
    {| Player = x.Player
       MatchesPlayed = x.MatchesPlayed |} )
|> List.truncate 5
* Transform each element of the `playerStatsTable` List into an anonymous record with a `Player` field that is a `string` and a `GoalsScored` field that is a integer(`int`).
playerStatsTable
|> List.map( fun x ->
    {| Player = x.Player
       GoalsScored = x.GoalsScored |})
|> List.truncate 5
* map the `playerStatsTable` to a tuple of player and goals scored, but multiply goals scored by 10. ( `Player`, `GoalsScored * 10`)
playerStatsTable
|> List.map( fun x -> x.Player, x.GoalsScored * 10 )
|> List.truncate 5
* map the `playerStatsTable` to a tuple of player and goals scored, but divide GoalsScored by 2. ( `Player`, `GoalsScored / 2`)
playerStatsTable
|> List.map( fun x -> x.Player, float x.GoalsScored / 2.0)
|> List.truncate 5
* map the `playerStatsTable` to a tuple of player and goals scored, but convert goalsScored to float. ( `Player`, `float GoalsScored`)
playerStatsTable
|> List.map( fun x -> x.Player, float x.GoalsScored)
|> List.truncate 5
* map the `playerStatsTable` to a tuple of player and goals scored, but divide goalsScored by 2.0. ( `Player`, `GoalsScored / 2.0`)
playerStatsTable
|> List.map( fun x -> x.Player, float x.GoalsScored / 2.0)
|> List.truncate 5
* map the `playerStatsTable` to a record of player and goals scored, but multiply goals scored by 10. ( `Player`, `GoalsScored * 10`)
type PlayerAndGoalsTimes10 =
    { Player: string
      GoalsScoredTimes10: int }

playerStatsTable
|> List.map( fun x ->
    { Player = x.Player
      GoalsScoredTimes10 = x.GoalsScored * 10})
|> List.truncate 5
* map the `playerStatsTable` to a record of player and goals scored, but divide goals scored by 2.0. ( `Player`, `float GoalsScored  / 2.0`)
type PlayerAndGoalsDividedBy2 =
    { Player: string
      GoalsDividedBy2: float }

playerStatsTable
|> List.map( fun x ->
    { Player = x.Player
      GoalsDividedBy2 = float x.GoalsScored / 2.0} )
|> List.truncate 5
* map the `playerStatsTable` to an anonymous record of player and goals scored, but multiply goals scored by 10. ( `Player`, `GoalsScored * 10`)
playerStatsTable
|>List.map( fun x ->
    {| Player = x.Player
       GoalsScored = x.GoalsScored * 10 |})
|> List.truncate 3
* map the `playerStatsTable` to an anonymous record of player and goals scored, but divide goals scored by 2.0. ( `Player`, `float GoalsScored  / 2.0`)
playerStatsTable
|> List.map( fun x ->
    {| Player = x.Player
       GoalsScored = float x.GoalsScored / 2.0|})
|> List. truncate 3
Now that you are used to work with records and perform simple Transformations, map `playerStatsTable` to a record type that includes:

* Player (`Player`) - type `string`

* Nation (`Nation`) - type `string`

* League (`League`) - type `string`

* AgeNextYear (`Age + 1`) - type `int`

* HalfGoalsScored (`GoalsScored / 2.0`) - type `float`
type AllChanged =
    { Player: string
      Nation: string
      League: string
      AgeNextYear: int
      HalfGoalsScored: float}

playerStatsTable
|> List.map( fun x ->
    { Player = x.Player
      Nation = x.Nation
      League = x.League
      AgeNextYear = x.Age + 1
      HalfGoalsScored = float x.GoalsScored / 2.0 } )
|> List.truncate 30