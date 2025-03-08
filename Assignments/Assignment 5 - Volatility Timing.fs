[![Binder](img/badge-binder.svg)](https://mybinder.org/v2/gh/nhirschey/teaching/gh-pages?filepath=assignment-volatility-timing.ipynb)&emsp;
[![Script](img/badge-script.svg)](/Teaching//assignment-volatility-timing.fsx)&emsp;
[![Notebook](img/badge-notebook.svg)](/Teaching//assignment-volatility-timing.ipynb)

Student Name | Student Number
--- | ---
**1: Francisco Perestrello** | 39001


This is an assignment. You may work in pairs (two students).  You will find sections labeled **Task** asking you to do each piece of analysis. Please make sure that you complete all of these tasks. I included some tests to help you see if you are calculating the solution correctly, but if you cannot get the test to pass submit your best attempt and you may recieve partial credit.

All work that you submit should be your own. Make use of the course resources and example code on the course website. It should be possible to complete all the requested tasks using information given below or somewhere on the course website.


#r "nuget:FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET, 2.0.0-preview.17"

#r "nuget: Plotly.NET.Interactive, 2.0.0-preview.17"

open System
open FSharp.Data
open Plotly.NET
open FSharp.Stats

for testing.


#r "nuget: FsUnit.Xunit"
#r "nuget: xunit, 2.*"
open Xunit
open FsUnit.Xunit
open FsUnitTyped

// Set dotnet interactive formatter to plaintext
Formatter.Register(fun (x:obj) (writer: TextWriter) -> fprintfn writer "%120A" x )
Formatter.SetPreferredMimeTypesFor(typeof<obj>, "text/plain")
// Make plotly graphs work with interactive plaintext formatter
Formatter.SetPreferredMimeTypesFor(typeof<GenericChart.GenericChart>,"text/html")

## Load Data

First, make sure that you're referencing the correct files.

Here I'm assuming that you have a class folder with this
notebook and these files in it. The folder hierarchy would
look like below where you have the below files and folders accessible.

* `Common.fsx` is on the course website.

* `notebook.ipynb` is this notebook.

```code
/class
    Common.fsx
    notebook.ipynb                
```

let [<Literal>] ResolutionFolder = __SOURCE_DIRECTORY__
Environment.CurrentDirectory <- ResolutionFolder

#load "Common.fsx"
open Common

We get the Fama-French 3-Factor asset pricing model data.


let ff3 = 
    French.getFF3 Frequency.Daily
    |> Seq.toList
    |> List.filter (fun x -> x.Date < DateTime(2022,3,1))

let annualizeDailyStdDev dailyStdDev = sqrt(252.0) * dailyStdDev

As an example, I'll first calculate the standard deviation of the `MktRf` factor and assign it to a value named `stdDevMkt`.


let stdDevMkt =
    ff3
    |> Seq.stDevBy (fun x -> x.MktRf)

The following test will pass if I calculate it correctly.


// Test.
stdDevMkt 
|> should (equalWithin 0.005) 0.01

The test following test will fail if I calculate it incorrectly. In this failing example I report an annualized standard deviation instead of a daily standard deviation.


let stdDevMktFAIL =
    ff3
    |> Seq.stDevBy (fun x -> x.MktRf)
    |> annualizeDailyStdDev

// Test
if false then // make this `if true` to run the test.
    stdDevMktFAIL
    |> should (equalWithin 0.005) 0.01

## Start of the assignment

> **Task:** Calculate the standard deviation of the `Hml` factor's daily returns. Assign it to a value named `stdDevHml`.
> 

Write your solution in the cell below.


let stdDevHml =
    ff3
    |> Seq.stDevBy (fun x -> x.Hml)

// Test
stdDevHml 
|> should (equalWithin 0.005) 0.006

> **Task:** Calculate the annualized standard deviation of the `Hml` factor's daily returns. Assign it to a value named `stdDevHmlAnnualized`.
> 

Write your solution in the cell below.


let stdDevHmlAnnualized = annualizeDailyStdDev stdDevHml

// Test
stdDevHmlAnnualized
|> should (equalWithin 0.005) 0.098

> **Task:** Assign the daily returns of the `Hml` factor to a value named `hml` that is a `list` of `ReturnObs`.
> 


type ReturnObs = 
    { 
        Name: string
        Date: DateTime 
        Return: float 
    }
Write your solution in the cell below.


let hml =
    ff3
    |> List.map (fun x ->
        { Name = "hml"
          Date = x.Date
          Return = x.Hml})


// Tests
hml[..1] |> should be ofExactType<list<ReturnObs>>

hml[0].Name |> shouldEqual "hml"

hml |> shouldHaveLength 25187

hml
|> List.averageBy (fun x -> x.Return)
|> should (equalWithin 0.0001) 0.00015

> **Task:** Calculate the daily returns of the `Hml` factor with `2x` leverage applied to the portfolio every day. Assign it to a value named `hml2x` that is a `list` of `ReturnObs`.
> 

Write your solution in the cell below.


let leverage = 2.0
let hml2x =
    ff3
    |> List.map (fun x ->
        { Name = "hml2x"
          Date = x.Date
          Return = x.Hml * leverage})

// Tests
hml2x[..1] |> should be ofExactType<list<ReturnObs>>

hml2x[0].Name |> shouldEqual "hml2x"

hml2x |> shouldHaveLength 25187

hml2x
|> List.averageBy (fun x -> x.Return)
|> should (equalWithin 0.0001) 0.0003

> **Task:** Calculate the average annualized daily return of the `Hml` factor and assign it to a value name `hmlAvg`.
> 

Write your solution in the cell below.


let hmlAvg =
    hml
    |> List.averageBy (fun x -> x.Return)
    |> (fun avg -> 252.0*avg)

// tests
hmlAvg |> should (equalWithin 0.005) 0.04

> **Task:** Calculate the average annualized daily return of the 2x leveraged `Hml` factor and assign it to a value name `hml2xAvg`.
> 

Write your solution in the cell below.


let hml2xAvg =
    hml2x
    |> List.averageBy (fun x -> x.Return)
    |> (fun avg -> 252.0*avg)
    
// tests
hml2xAvg |> should (equalWithin 0.005) 0.075

Here is some code that you will use for the next task.


type GrowthObs =
    { 
        Name: string
        Date: DateTime
        Growth: float
    }

let cumulativeGrowth (xs: list<ReturnObs>) =
    let sorted = xs |> List.sortBy (fun x -> x.Date)
    let calcGrowth (prior: GrowthObs) (current: ReturnObs) =
        { Name = current.Name 
          Date = current.Date
          Growth = prior.Growth * (1.0 + current.Return) }        
    match sorted with
    | [] -> []
    | h::t ->
        let firstOb = 
            { Name = h.Name 
              Date = h.Date
              Growth = 1.0 + h.Return }
        (firstOb, t) ||> List.scan calcGrowth

> **Task:** Calculate the cumulative growth of $1 invested in HML at the start of the sample. Assign it to a value named `hmlGrowth` that is a `list` of `GrowthObs`.
> 

Write your solution in the cell below.


let hmlGrowth =
    cumulativeGrowth hml

// Tests
hmlGrowth[..1] |> should be ofExactType<list<GrowthObs>>

hmlGrowth
|> List.map (fun x -> x.Growth)
|> List.last
|> should (equalWithin 1.0) 27.0

> **Task:** Calculate the cumulative growth of $1 invested in 2x levered HML at the start of the sample. Assign it to a value named `hml2xGrowth` that is a `list` of `GrowthObs`.
> 

Write your solution in the cell below.


let hml2xGrowth  =
    cumulativeGrowth hml2x

// Tests
hml2xGrowth[..1] |> should be ofExactType<list<GrowthObs>>

hml2xGrowth
|> List.map (fun x -> x.Growth)
|> List.last
|> should (equalWithin 1.0) 286.0

Here is an example of a plot of the cumulative growth of $1 invested in the market.


let mkt = 
    [ for x in ff3 do
        { Name = "market"
          Date = x.Date
          Return = x.MktRf } ]

let marketGrowthChart = 
    mkt
    |> cumulativeGrowth
    |> List.map (fun x -> x.Date, x.Growth)
    |> Chart.Line
    |> Chart.withTraceInfo (Name = "MKT")  // I added this line to add a legend to the plot
    |> Chart.withYAxisStyle (AxisType = StyleParam.AxisType.Log)

> **Task:** Plot the cumulative growth of $1 invested in HML and $1 invested in the Market as a combined line chart using a log scale for the y-axis.
> 

Write your solution in the cell below.


let hmlGrowthChart = 
    hml
    |> cumulativeGrowth
    |> List.map (fun x -> x.Date, x.Growth)
    |> Chart.Line
    |> Chart.withTraceInfo (Name = "HML")
    |> Chart.withYAxisStyle (AxisType = StyleParam.AxisType.Log)


let listOfCharts = [marketGrowthChart; hmlGrowthChart]

listOfCharts 
|> Chart.combine
|> Chart.withXAxisStyle (TitleText="Date")
|> Chart.withYAxisStyle (TitleText="Log Cumulative Growth")
|> Chart.withTitle ("Cummulative Growth of HML and MKT")
> **Task:** Apply a constant levarage to the HML and market factors for the full sample such that the daily returns have a full-sample annualized standard deviation equal to 10%. Assign the results to values named `hml10` and `mkt10` that are lists of `ReturnObs`.
> 

Write your solution in the cell below.


let stdDevMktAnnualized = annualizeDailyStdDev stdDevMkt

let target = 0.1

let leverageHml = target/stdDevHmlAnnualized
let leverageMkt = target/stdDevMktAnnualized

let hml10 =
    ff3
    |> List.map (fun x ->
        { Name = "hml10"
          Date = x.Date
          Return = x.Hml * leverageHml})
    
let mkt10 =
    ff3
    |> List.map (fun x ->
        { Name = "mkt10"
          Date = x.Date
          Return = x.MktRf * leverageMkt})


// Tests
hml10[..1] |> should be ofExactType<list<ReturnObs>>
mkt10[..1] |> should be ofExactType<list<ReturnObs>>

hml10 
|> stDevBy (fun x -> x.Return) 
|> annualizeDailyStdDev
|> should (equalWithin 1e-6) 0.1

mkt10
|> stDevBy (fun x -> x.Return) 
|> annualizeDailyStdDev
|> should (equalWithin 1e-6) 0.1

> **Task:** Plot the cumulative growth of $1 invested in `hml10` and `mkt10` as a combined line chart using a log scale for the y-axis.
> 

Write your solution in the cell below.


let mkt10GrowthChart = 
    mkt10
    |> cumulativeGrowth
    |> List.map (fun x -> x.Date, x.Growth)
    |> Chart.Line
    |> Chart.withTraceInfo (Name = "MKT 10%")
    |> Chart.withYAxisStyle (AxisType = StyleParam.AxisType.Log)

let hml10GrowthChart = 
    hml10
    |> cumulativeGrowth
    |> List.map (fun x -> x.Date, x.Growth)
    |> Chart.Line
    |> Chart.withTraceInfo (Name = "HML 10%")
    |> Chart.withYAxisStyle (AxisType = StyleParam.AxisType.Log)


let listOfCharts = [mkt10GrowthChart; hml10GrowthChart]

listOfCharts 
|> Chart.combine
|> Chart.withXAxisStyle (TitleText="Date")
|> Chart.withYAxisStyle (TitleText="Log Cumulative Growth")
|> Chart.withTitle ("Cummulative Growth of HML and MKT with Target Volatility 10%")

> **Task:** Explain how to intepret the dramatic difference between the two plots that you have just created. Why is the plot of $1 invested in unlevered HML and Market factors so different from the plot of $1 invested in the versions that are levered to have a 10% annualized standard deviation?
> 

The dramatic difference between the two plots was more than expected given the unlevered annualized standard deviations of the HML and MKT factors. Prior to leveraging, the HML factor had an annualized standard deviation of 9,8%, which is very close to our target of 10% when leveraging. Thus, as expected, the plot for the HML factor doesn't change much when we leverage to target volatility. Not only the behavior of the curve is the same, but its highest peak and biggest fluke are somewhat the same as before. The unlevered MKT factor, however, had an annualized standard deviation of 17,1%, which is drastically different from our target of 10%. Thus, as in the second plot we are dramatically reducing its volatility, we already expected such drastic changes. Although the behavior of the curve is similar, the reduced volatility makes it so that its peak will be smaller, and its biggest fluke will be higher (i.e. doesn't drop to such low values). In the unlevered plot, we can see that the peak of the MKT factor cummulative return was 552 in 2021, and its lowest value was 0,35 in 1932, while when we look at the levered plot for the MKT factor, we see that the peak cummulative return was 57 in 2021, and its lowest value was 0.56 in 1932.

