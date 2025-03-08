[![Binder](img/badge-binder.svg)](https://mybinder.org/v2/gh/nhirschey/teaching/gh-pages?filepath=football-collection-functions.ipynb)&emsp;
[![Script](img/badge-script.svg)](/Teaching//football-collection-functions.fsx)&emsp;
[![Notebook](img/badge-notebook.svg)](/Teaching//football-collection-functions.ipynb)

# Using List collection functions and calculating summary statistics.

> Developed with [Davide Costa](https://github.com/DavideGCosta)
> 

<p>Assignment done by:</p>
<p>Francisco Perestrello, 39001</p>
<p>Nuno Afonso, 39116 </p>
<p>Jorge Gouveia, 39215</p>

You should now feel comfortable with the footballer dataset and how to work with
tuples, records, anonymous records. You should also know how to perform simple transformations.
With a large and heterogeneous dataset, it's useful to understand how to sort, group,
and filter the data, and also many other interesting List functions.

It is a good idea to browse the documentation for lists at the [F# language reference](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/Lists)
and the [F# core library](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-listmodule.html) documentation sites before you start.
For further discussion of collection functions, the related [F# for fun and profit](https://fsharpforfunandprofit.com/posts/list-module-functions/)
page is also useful.

### Reference needed nuget packages and open namespaces


#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"

open FSharp.Data
open FSharp.Stats
open FSharp.Stats.Correlation

### Load the Csv file.


let [<Literal>] CsvPath = __SOURCE_DIRECTORY__ + "/FootballPlayers.csv"
type FootballPlayersCsv = CsvProvider<CsvPath>

let playerStatsTable = 
    FootballPlayersCsv.GetSample().Rows
    |> Seq.toList

## EXERCISES - PART 2

* [List Functions.](#List-Functions)
  

  0 [List.take](#1-List-take)
    
  
  1 [List.truncate](#2-List-truncate)
    
  
  2 [List.distinct](#3-List-distinct)
    
  
  3 [List.countBy](#4-List-countBy)
    
  
  4 [List.filter](#5-List-filter)
    
  
  5 [List.sort and List.sortDescending](#6-List-sort-and-List-sortDescending)
    
  
  6 [List.sortBy and List.sortByDescending](#7-List-sortBy-and-List-sortByDescending)
    
  
  7 [List.splitInto](#8-List-splitInto)
    
  
  8 [List.groupBy](#9-List-groupBy)
    
  

* [Statistics List Functions.](#Statistics-List-Functions)
  

  0 [List.max](#1-List-max)
    
  
  1 [List.min](#2-List-min)
    
  
  2 [List.maxBy](#3-List-maxBy)
    
  
  3 [List.minBy](#4-List-minBy)
    
  
  4 [List.sum](#5-List-sum)
    
  
  5 [List.sumBy](#6-List-sumBy)
    
  
  6 [List.average](#7-List-average)
    
  
  7 [List.averageBy](#8-List-averageBy)
    
  
  8 [Seq.stDev](#9-Seq-stDev)
    
  
  9 [Seq.pearsonOfPairs](#10-Seq-pearsonOfPairs)
    
  

* [Further Statistics practice.](#Further-Statistics-practice)
  

  0 [List.countBy, List.filter and List.averageBy](#1-List-countBy-List-filter-and-List-averageBy)
    
  
  1 [List.groupBy, List.map and transformations](#2-List-groupBy-List-map-and-transformations)
    
  
  2 [List.sortDescending, List.splitInto, List.map and Seq.stDev](#3-List-sortDescending-List-splitInto-List-map-and-Seq-stDev)
    
  

## List Functions.

### 1 List.take

`List.take 5` takes the first 5 rows.
`List.take 2` takes the first 2 rows

Example: Take the first 4 rows from `playerStatsTable` with `List.take`.


playerStatsTable
|> List.take 4

* Take the first 7 rows from `playerStatsTable` with `List.take`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.take 7
### 2 List.truncate

`List.truncate 5` takes the first 5 rows.
`List.truncate 2` takes the first 2 rows

You must have noted that `List.take` and `List.truncate` return similar outputs, but these are not exactly the same.
`List.take` gives you the exact number of items that you specify in the parameters,
while `List.truncate` takes at maximum the number of items you specified in the parameters.
Thus, in most cases both give you the exact same output, except if you ask for more items then the ones available in the List (List length).
In this particular scenario `List.truncate` returns the maximum number of elements (all the elements in the List),
while `List.take` returns an error, since it is supposed to take the exact number of elements you asked for, which is impossible in this particular case.

Example: Take the first 4 rows from `playerStatsTable` with `List.truncate`.


playerStatsTable
|> List.truncate 4

* Take the first 7 rows from `playerStatsTable` with `List.truncate`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.truncate 7

### 3 List.distinct

`List.distinct` returns the unique elements from the List.
`["hello"; "world"; "hello"; "hi"] |> List.distinct` returns `["hello"; "world"; "hi"]`

Example: From `playerStatsTable` `Nation` field find the unique elements with `List.distinct`.


playerStatsTable
|> List.map(fun x -> x.Nation)
|> List.distinct

* From `playerStatsTable` `League` field find the unique elements with `List.distinct`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,22)-(2,30) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.map(fun x -> x.League)
|> List.distinct

### 4 List.countBy

`List.countBy` returns a list of paired tuples with the unique elements and their counts.

Example: From `playerStatsTable` `Team` field find the unique elements and their counts with `List.countBy`.


playerStatsTable
|> List.countBy(fun x -> x.Team)
|> List.truncate 5 //just to observe the first 5 rows, not a part of the exercise.

* From `playerStatsTable` `League` field find the unique elements and their counts with `List.countBy`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,26)-(2,34) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.countBy (fun x -> x.League)

### 5 List.filter

`List.filter` allows you to extract a subset of the dataset based on one or multiple conditions.

Example: `Filter` the `playerStatsTable` to get only portuguese players. (`Nation = "pt POR"`).
Remember that we have to look to the dataset to find the string correspondent to portuguese players,
which in this case is `"pt POR"`


playerStatsTable
|> List.filter(fun x -> x.Nation = "pt POR")
|> List.truncate 5 //just to observe the first 5 rows, not a part of the exercise.

* `Filter` the `playerStatsTable` to get only 16 year-old players. (`Age = 16`).

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,25)-(2,30) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.filter (fun x -> x.Age = 16)
### 6 List.sort and List.sortDescending

* `[1; 4; 5; 3; 6] |> List.sort` returns `[1; 3; 4; 5; 6]` (ascending sort).

* `[1; 4; 5; 3; 6] |> List.sortDescending` returns `[6; 5; 4; 3; 1]` (descending sort).

Example: map `playerStatsTable` to get a list of `Age` and sort it (ascending).

Since we want to sort the age List we first use `List.map` to get only that List.
Then we use `List.sort` to sort it.


playerStatsTable
|> List.map(fun x -> x.Age)
|> List.sort
|> List.truncate 60 //just to observe the first 60 values, not a part of the exercise.

* map `playerStatsTable` to get a list of `GoalsScored` and sort it (ascending).
Hint:
To sort the GoalsScored List you first need to use `List.map` to get only that List.
Then use `List.sort` to sort it.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,22)-(2,35) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.map (fun x -> x.GoalsScored)
|> List.sort
|> List.truncate 60

Example: Map `playerStatsTable` to get a list of `Age` and sort it (descending).

Since we want to sort the age List we first use `List.map` to get only that List.
Then we use `List.sortDescending` to sort it.


playerStatsTable
|> List.map(fun x -> x.Age)
|> List.sortDescending
|> List.truncate 60 //just to observe the first 60 values, not a part of the exercise.

* Map `playerStatsTable` to get a list of `GoalsScored` and sort it (descending).
Hint:
To sort the GoalsScored List you first need to use `List.map` to get only that List.
Then use `List.sortDescending` to sort it.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,22)-(2,35) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.map (fun x -> x.GoalsScored)
|> List.sortDescending
|> List.truncate 60
### 7 List.sortBy and List.sortByDescending

`List.sortBy` is very usefull to sort the dataset accordingly to a certain dataset field.

Example: sort (ascending) `playerStatsTable` by `Age` (`List.sortBy`).


playerStatsTable
|> List.sortBy(fun x -> x.Age)
|> List.truncate 5 //just to observe the first 5 rows, not a part of the exercise.

* sort (ascending) `playerStatsTable` by `GoalsScored` (`List.sortBy`).

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,25)-(2,38) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.sortBy(fun x -> x.GoalsScored)
|> List.truncate 5

Example: sort (descending) `playerStatsTable` by `Age` (`List.sortByDescending`).


playerStatsTable
|> List.sortByDescending(fun x -> x.Age)
|> List.truncate 5 //just to observe the first 5 rows, not a part of the exercise.

* sort (descending) `playerStatsTable` by `GoalsScored` (`List.sortByDescending`).

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,35)-(2,48) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.sortByDescending (fun x -> x.GoalsScored)
|> List.truncate 5

### 8 List.splitInto

`List.splitInto` is very usefull to split your dataset into multiple subsets.
This function is commonly used to generate quantiles by splitting a sorted List.
For instance, for investment strategies financial assets are usually sorted by a certain signal
and then splitted into quantiles. If the signal has a positive sign, it means that the long strategy consists of going long
on the first quantile stocks, and the long-short strategy consists of going long on the first quantile stocks and short on the last quantile stocks.

Note: `List.splitInto` receives one parameter which refers to the number of groups you want to create out of the dataset.

Example: Sort the `playerStatsTable` by `GoalsScored` and then split the dataset into 4 groups using `List.sortBy` and `List.splitInto`.


playerStatsTable
|> List.sortBy(fun x -> x.GoalsScored)
|> List.splitInto 4
|> List.truncate 2 //just to observe the first 2 groups Lists, not a part of the exercise.
|> List.map(fun x -> x |> List.truncate 5) //just to observe the first 5 rows of each group List, not a part of the exercise.

* Sort the `playerStatsTable` by `Age` and then split the dataset into 5 groups using `List.sortBy` and `List.splitInto`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,25)-(2,30) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.sortBy (fun x -> x.Age)
|> List.splitInto 5
|> List.truncate 2
|> List.map (fun x -> x |> List.truncate 5)

### 9 List.groupBy

`List.groupBy` allows you to group elements of a list.
It takes a key-generating function and a list as inputs.
The function is executed on each element of the List, returning a list of tuples
where the first element of each tuple is the key and the second is a list of the elements for which the function produced that key.

Example: Group the `playerStatsTable` by `Nation` using `List.groupBy`.


playerStatsTable
|> List.groupBy(fun x -> x.Nation)
|> List.truncate 2 //just to observe the first 2 groups Lists, not a part of the exercise.
|> List.map(fun (x, xs) -> x, xs |> List.truncate 5) //just to observe the first 5 rows of each group List, not a part of the exercise.

* Group the `playerStatsTable` by `Age` using `List.groupBy`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,26)-(2,31) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.groupBy (fun x -> x.Age)
|> List.truncate 2
|> List.map (fun (x, xs) -> x, xs |> List.truncate 5)

## Statistics List Functions

### 1 List.max

`[1; 4; 5; 3; 6] |> List.max` returns `6` (the highest value in the List).

Example: Map `playerStatsTable` to get the `Age` List, and find the maximum (`List.max`).


playerStatsTable
|> List.map(fun x -> x.Age)
|> List.max

* Map `playerStatsTable` to get the `GoalsScored` List, and find the maximum (`List.max`).

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,22)-(2,35) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.map(fun x -> x.GoalsScored)
|> List.max
### 2 List.min

`[1; 4; 5; 3; 6] |> List.min` returns `1` (the lowest value in the List).

Example: Map `playerStatsTable` to get the `Age` List, and find the minimum (`List.min`).


playerStatsTable
|> List.map(fun x -> x.Age)
|> List.min

* Map `playerStatsTable` to get the `GoalsScored` List, and find the minimum (`List.min`).

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,22)-(2,35) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.map(fun x -> x.GoalsScored)
|> List.min
### 3 List.maxBy

Sometimes you want the element with the "maximum y" where "y" is the result of applying a particular function to a list element. This is what `List.maxBy` is for. This function is best understood by seeing an example.

Example: Find the player in `playerStatsTable` with the maximum `Age` using `maxBy`. What we need to do then is write a function that takes a player as input and outputs the players age. `List.maxBy` will then find the player that is the maxiumum after transforming it using this function.


playerStatsTable
|> List.maxBy(fun x -> x.Age)

* Find the maximum `playerStatsTable` row by `GoalsScored` using `maxBy`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,24)-(2,37) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.maxBy(fun x -> x.GoalsScored)
### 4 List.minBy

Sometimes you want the element with the "minimum y" where "y" is the result of applying a particular function to a list element. This is what `List.minBy` is for.

Example: Find the player in `playerStatsTable` with the minimum `Age` using `minBy`.


playerStatsTable
|> List.minBy(fun x -> x.Age)

* Find the minimum `playerStatsTable` row by `GoalsScored` using `minBy`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,24)-(2,37) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.minBy(fun x -> x.GoalsScored)

### 5 List.sum

`[1; 4; 5; 3; 6] |> List.sum` returns `19` (sum of the List elements).

Example: Calculate the total number of years lived by all players. Hint: transform (`List.map`) each element of `playerStatsTable` into an integer representing the player's `Age` and then get the sum (`List.sum`) of all the players' ages (the result should be an `int`).


playerStatsTable
|> List.map(fun x -> x.Age)
|> List.sum

* Calculate the total goals scored (`GoalsScored`) by all players in `playerStatsTable`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,22)-(2,35) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.map(fun x -> x.GoalsScored)
|> List.sum
### 6 List.sumBy

We are using a dataset that has multiple fields per List element. If you want to get the sum for particular fields it convenient to use `List.sumBy`.
It takes a function and transforms each element using that function and afterward sums all the transformed elements. It is like an `List.map` and `List.sum` combined into one function.

Example: Use `List.sumBy` to calculate the total number of years lived by all players in `playerStatsTable`. Remember that each player has lived `Age` years.


playerStatsTable
|> List.sumBy(fun x -> x.Age)

* Find the sum of the `GoalsScored` by all players in `playerStatsTable` using `List.sumBy`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,24)-(2,37) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.sumBy(fun x -> x.GoalsScored)
### 7 List.average

`[1.0; 2.0; 5.0; 2.0] |> List.average` returns `2.5` (the average of all the List elements).

Example: Transform `playerStatsTable` into a list of the players' ages (`Age`) and find the average `Age` (`List.average`).
The field `x.Age` needs to be transformed from `int` to `float` because `List.average` only works with `floats` or `decimals`.


playerStatsTable
|> List.map(fun x -> float x.Age)
|> List.average

* Use `List.map` to transform `playerStatsTable` into a list of the players' `GoalsScored` and find the average `GoalsScored` (`List.average`).
Hint: The variable `x.GoalsScored` needs to be transformed from `int` to `float` since `List.average` only works with `floats` or `decimals`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,28)-(2,41) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.map( fun x -> float x.GoalsScored)
|> List.average
### 8 List.averageBy

We are using a dataset that has multiple fields per List element. If you want to get the average for particular fields it convenient to use `List.averageBy`.
It takes a function and transforms each element using that function and afterward averages all the transformed elements. It is like an `List.map` and `List.average` combined into one function.

Example: Find the average `Age` using `List.averageBy`.
The `Age` needs to be transformed from `int` to `float` since `List.averageBy` only works with `floats` or `decimals`.


playerStatsTable
|> List.averageBy(fun x -> float x.Age)

* Find the average `GoalsScored` using `List.averageBy`.
Hint: The `GoalsScored` needs to be transformed from `int` to `float` since `List.averageBy` only works with `floats` or `decimals`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,34)-(2,47) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.averageBy(fun x -> float x.GoalsScored)
### 9 Seq.stDev

For `Seq.stDev` to work, we loaded the `FSharp.Stats nuget` (`#r "nuget: FSharp.Stats"`).
This nuget contains the standard deviation function.
Besides this we also opened the module `FSharp.Stats` (`open FSharp.Stats`).
[FSharp.Stats documentation](https://fslab.org/FSharp.Stats/)

Example: Use `List.map` to transform `playerStatsTable` by `GoalsScored` and find the standard deviation. (`Seq.stDev`).
Note that for `Seq.stDev` to work the values need to be `floats` or `decimals`, so we need to transform the `GoalsScored` from `int` to `float`.


playerStatsTable
|> List.map(fun x -> float x.GoalsScored)
|> Seq.stDev

* Transform `playerStatsTable` into a list of the players' `Age`'s and find the standard deviation. (`Seq.stDev`).
Hint: You need to transform `Age` values from `int` to `floats`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,28)-(2,33) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.map(fun x -> float x.Age)
|> Seq.stDev
### 10 Seq.pearsonOfPairs

In order to perform correlations we have to load and open the namespace `FSharp.Stats`.
Also, we `open FSharpe.Stats.Correlation` to allow a easier access to the correlation functions.

It will be helpfull to check the [FSharp.Stats.Correlation Documentation](https://fslab.org/FSharp.Stats/reference/fsharp-stats-correlation-seq.html#pearson) before starting the exercises.

Example: Test the correlation between `MatchesPlayed` and `GoalsScored` using `pearsonOfPairs`.

`Seq.pearsonOfPairs` expects a list of tuples (x1 * x2), computing the correlation between x1 and x2.
So we use `List.map` to get a list of tuples with (`MatchesPlayed`, `GoalsScored`).
Then we only need to pipe (`|>`) to `Seq.pearsonOfPairs`.


playerStatsTable
|> List.map(fun x -> x.MatchesPlayed, x.GoalsScored)
|> Seq.pearsonOfPairs

* Test the correlation between `MatchesPlayed` and `Age` using `pearsonOfPairs`.
Hints:
`Seq.pearsonOfPairs` expects a list of tuples (x1 * x2). Use `List.map` to get a list of tuples with (`MatchesPlayed`, `Age`).
Then you only need to pipe (`|>`) to `Seq.pearsonOfPairs`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,22)-(2,37) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
input.fsx (2,39)-(2,44) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.map(fun x -> x.MatchesPlayed, x.Age )
|> Seq.pearsonOfPairs
* Test the correlation between `GoalsScored` and `Age` using `pearsonOfPairs`.
Hints:
`Seq.pearsonOfPairs` expects a list of tuples (x1 * x2). Use `List.map` to get a list of tuples with (`GoalsScored`, `Age`).
Then you only need to pipe (`|>`) to `Seq.pearsonOfPairs`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,22)-(2,35) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
input.fsx (2,37)-(2,42) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.map(fun x -> x.GoalsScored, x.Age )
|> Seq.pearsonOfPairs
## Further Statistics practice

Now that you should feel confortable with `List.filter`, `List.groupBy`, `List.splitInto`
and also some f# statistics functions, let's combine those concepts together.

### 1 List.countBy, List.filter and List.averageBy

Example: Find the average goals scored by portuguese players.

In order to find the average goals for portuguese players we know that we need to use `List.filter`.
But we need to know what is the string correspondent to portuguese players!
Using `List.distinct` or `List.countBy` we can observe all the `Nation` strings, which allow us to see that portuguese Nation string is `"pt POR"`.


playerStatsTable
|> List.countBy(fun x -> x.Nation)

Now that we know what is the Portuguese string we can filter `x.Nation = "pt POR"` in order to only get portuguese players' rows!
Then we can easily pipe it (`|>`) to `List.averageBy (fun x -> float x.Age)` to get the average age of portuguese players.


playerStatsTable
|> List.filter(fun x -> x.Nation = "pt POR")
|> List.averageBy(fun x -> float x.Age)

* Find the average age for players playing on the Premier League  .
Hint:
You'll first need to use `List.filter` to get only players from the Premier League (`x.League = "engPremier League"`).
Then use averageBy to compute the average by age, don't forget to use `float x.Age` to transform age values to float type.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,25)-(2,33) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
input.fsx (3,34)-(3,39) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.filter(fun x -> x.League = "engPremier League")
|> List.averageBy(fun x -> float x.Age)
### 2. List.groupBy, List.map and transformations.

Example: Group `playerStatsTable` by `Team` and compute the average number of `GoalsScored`.


//example using record:
type TeamAndAvgGls =
    { Team : string
      AvgGoalsScored : float }

playerStatsTable
|> List.groupBy(fun x -> x.Team)
|> List.map(fun (team, playerStats) -> 
    { Team = team
      AvgGoalsScored = playerStats |> List.averageBy(fun playerStats -> float playerStats.GoalsScored)})
|> List.truncate 5 //just to observe the first 5 rows, not a part of the exercise.

or


//example using tuple:
playerStatsTable
|> List.groupBy(fun x -> x.Team)
|> List.map(fun (team, playerStats) -> team, playerStats |> List.averageBy(fun playerStats -> float playerStats.GoalsScored))
|> List.truncate 5 //just to observe the first 5 rows, not a part of the exercise.

* Group `playerStatsTable` by `League` and then compute the Average `Age` by group.
Hint: Use `groupBy` to group by league (`League`).
Then use `averageBy` to compute the average by age (`Age`) and pipe it
(`|>`) to `List.map` to organize the data in a record or tuple with League (`League`) and Average Age.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (6,1)-(6,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (10,87)-(10,90) typecheck error The type 'LeagueAndAvgAge' does not define the field, constructor or member 'Age'.
input.fsx (13,1)-(13,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (17,72)-(17,75) typecheck error The type 'LeagueAndAvgAge' does not define the field, constructor or member 'Age'.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.groupBy(fun x -> x.League)
|> List.map(fun (League, x) -> League, x |> List.averageBy(fun x -> float x.Age))
|> List.truncate 5
### 3 List.sortDescending, List.splitInto, List.map and Seq.stDev

* From `playerStatsTable` sort the players' `Age` (descending), split the dataset into quartiles (4-quantiles) and compute the standard deviation for each quantile.
Hint: You only need the `Age` field from the dataset, so you can use `map` straight away to get the `Age` List.
Sort that List with `List.sortDescending`, and then split it into 4 parts using `List.splitInto`.
Finally use `List.map` to iterate through each quantile and apply the function `Seq.stDev`.

<div style="padding-left: 40px;">
<p> 
<span>
<details>
<summary><p style="display:inline">answer</p></summary>

```
input.fsx (1,1)-(1,17) typecheck error The value or constructor 'playerStatsTable' is not defined. Maybe you want one of the following:
   playersByPosition
input.fsx (2,28)-(2,33) typecheck error Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

</details>
</span>
</p>
</div>


playerStatsTable
|> List.map(fun x -> float x.Age)
|> List.sortDescending
|> List.splitInto 4
|> List.map(fun x -> Seq.stDev x)