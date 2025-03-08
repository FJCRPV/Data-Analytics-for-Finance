[![Binder](img/badge-binder.svg)](https://mybinder.org/v2/gh/nhirschey/teaching/gh-pages?filepath=assignment-signal-portfolio.ipynb)&emsp;
[![Script](img/badge-script.svg)](/Teaching//assignment-signal-portfolio.fsx)&emsp;
[![Notebook](img/badge-notebook.svg)](/Teaching//assignment-signal-portfolio.ipynb)

Student Name | Student Number

**1** | &#32; | &#32; Francisco Perestrello

**2** | &#32; | &#32; 39001


**Signal Name: Highest 5 Days of Return**

**Signal Code: rmax5_21d**

This is an assignment. You may work in pairs (two students) using either student's signal to answer the below questions.  You will find sections labeled **Task** asking you to do each piece of analysis. Please make sure that you complete all of these tasks. Make use of the course resources and example code on the course website. It should be possible to complete all the requested tasks using information given below or somewhere on the course website.


#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET, 2.0.0-preview.17"

#r "nuget: Plotly.NET.Interactive, 2.0.0-preview.17"

open System
open FSharp.Data
open Plotly.NET
open FSharp.Stats

// Set dotnet interactive formatter to plaintext
Formatter.Register(fun (x:obj) (writer: TextWriter) -> fprintfn writer "%120A" x )
Formatter.SetPreferredMimeTypesFor(typeof<obj>, "text/plain")
// Make plotly graphs work with interactive plaintext formatter
Formatter.SetPreferredMimeTypesFor(typeof<GenericChart.GenericChart>,"text/html")

## Load Data

First, make sure that you're referencing the correct files.

Here I'm assuming that you have a class folder with this notebook and a `data` folder inside of it. The folder hierarchy would look like below where you
have the below files and folders accessible:

```code
/class
    Portfolio.fsx
    Common.fsx
    notebook.ipynb
    /data
        id_and_return_data.csv
        zero_trades_252d.csv
    
```

let [<Literal>] ResolutionFolder = __SOURCE_DIRECTORY__
Environment.CurrentDirectory <- ResolutionFolder

#load "Portfolio.fsx"
open Portfolio

#load "Common.fsx"
open Common

### Data files

We assume the `id_and_return_data.csv` file and the signal csv file  are in the `data` folder. In this example the signal file is `zero_trades_252d.csv`. You should replace that file name with your signal file name.


let [<Literal>] IdAndReturnsFilePath = "data/id_and_return_data.csv"
let [<Literal>] MySignalFilePath = "data/rmax5_21d.csv"
let strategyName = "Highest 5 Days of Return"

If my paths are correct, then this code should read the first few lines of the files.
If it doesn't show the first few lines, fix the above file paths.


IO.File.ReadLines(IdAndReturnsFilePath) |> Seq.truncate 5

IO.File.ReadLines(MySignalFilePath) |> Seq.truncate 5

Ok, now assuming those paths were correct the below code will work.
I will put all this prep code in one block so that it is easy to run.


/// Type for Id and returns csv data
type IdAndReturnsType = 
    CsvProvider<Sample=IdAndReturnsFilePath,
                // The schema parameter is not required,
                // but I am using it to override some column types
                // to make filtering easier.
                // If I didn't do this these particular columns 
                // would have strings of "1" or "0", but explicit boolean is nicer.
                Schema="obsMain(string)->obsMain=bool,exchMain(string)->exchMain=bool",
                ResolutionFolder=ResolutionFolder>

/// Type for the signal data
type MySignalType = 
    CsvProvider<MySignalFilePath,
                ResolutionFolder=ResolutionFolder>

/// Id and returns data indexed by security id and month
let msfBySecurityIdAndMonth =
    IdAndReturnsType.GetSample().Rows
    |> Seq.map(fun row -> 
        let id = Other row.Id
        let month = DateTime(row.Eom.Year,row.Eom.Month,1)
        let key = id, month
        key, row)
    |> Map    

/// Signal data indexed by security id and month
let signalBySecurityIdAndMonth =
    MySignalType.GetSample().Rows
    |> Seq.choose(fun row -> 
        // we'll use choose to drop the security if the signal is None.
        // The signal is None when it is missing.
        match row.Signal with
        | None -> None // choose will drop these None observations
        | Some signal ->
            let id = Other row.Id
            let month = DateTime(row.Eom.Year,row.Eom.Month,1)
            let key = id, month
            // choose will convert Some(key,signal) into
            // (key,signal) and keep that.
            Some (key, signal))
    |> Map    

/// Investment universe for each month, indexed by month.
let securitiesByFormationMonth =
    msfBySecurityIdAndMonth
    |> Map.values
    |> Seq.groupBy(fun x -> DateTime(x.Eom.Year, x.Eom.Month,1))
    |> Seq.map(fun (ym, obsThisMonth) -> 
        let idsThisMonth = [ for x in obsThisMonth do Other x.Id ]
        ym, idsThisMonth)
    |> Map

/// Function to get investment universe for a month.
let getInvestmentUniverse formationMonth =
    match Map.tryFind formationMonth securitiesByFormationMonth with
    | Some securities -> 
        { FormationMonth = formationMonth 
          Securities = securities }
    | None -> failwith $"{formationMonth} is not in the date range"
   
/// Function to get signal for entire investment universe.
let getMySignals (investmentUniverse: InvestmentUniverse) =
    let getMySignal (securityId, formationMonth) =
        match Map.tryFind (securityId, formationMonth) signalBySecurityIdAndMonth with
        | None -> None
        | Some signal ->
            Some { SecurityId = securityId 
                   // if a high signal means low returns,
                   // use `-signal` here instead of `signal`
                   Signal = signal }
    let listOfSecuritySignals =
        investmentUniverse.Securities
        |> List.choose(fun security -> 
            getMySignal (security, investmentUniverse.FormationMonth))    
    
    { FormationMonth = investmentUniverse.FormationMonth 
      Signals = listOfSecuritySignals }


/// Function to get market cap for security in a month.
let getMarketCap (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> None
    | Some row -> 
        match row.MarketEquity with
        | None -> None
        | Some me -> Some (security, me)

/// Function to get returns for a security in a month.
let getSecurityReturn (security, formationMonth) =
    // If the security has a missing return, assume that we got 0.0.
    // Note: If we were doing excess returns, we would need 0.0 - rf.
    let missingReturn = 0.0
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> security, missingReturn
    | Some x ->  
        match x.Ret with 
        | None -> security, missingReturn
        | Some r -> security, r

let isObsMain (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.ObsMain

let isPrimarySecurity (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.PrimarySec

let isCommonStock (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.Common

let isExchMain (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.ExchMain

let hasMarketEquity (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.MarketEquity.IsSome

/// Data filters
let myFilters securityAndFormationMonth =
    isObsMain securityAndFormationMonth &&
    isPrimarySecurity securityAndFormationMonth &&
    isCommonStock securityAndFormationMonth &&
    isExchMain securityAndFormationMonth &&
    isExchMain securityAndFormationMonth &&
    hasMarketEquity securityAndFormationMonth

/// Function to filter investment universe by filters.
let doMyFilters (universe:InvestmentUniverse) =
    let filtered = 
        universe.Securities
        // my filters expect security, formationMonth
        |> List.map(fun security -> security, universe.FormationMonth)
        // do the filters
        |> List.filter myFilters
        // now convert back from security, formationMonth -> security
        |> List.map fst
    { universe with Securities = filtered }

/// Months in the sample where we can form portfolios.
let sampleMonths = 
    let startSample = 
        msfBySecurityIdAndMonth.Keys
        |> Seq.map(fun (id, dt) -> dt)
        |> Seq.min
    let endSample = 
        let lastMonthWithData = 
            msfBySecurityIdAndMonth.Keys
            |> Seq.map(fun (id, dt) -> dt)
            |> Seq.max
        // The end of sample is the last month when we have returns.
        // So the last month when we can form portfolios is one month
        // before that.
        lastMonthWithData.AddMonths(-1) 
    getSampleMonths (startSample, endSample)

## Start of assignment

> **Task:** Complete the below function. It should take a month, a strategy name, and a number `n` of portfolios as input. The output should be a list of assigned portfolios, where stocks are assigned to `n` portfolios by sorts on the signal. I've included type signatures to constrain the output to the correct type.
> 


let formAssignedPortfolios (ym: DateTime) (strategyName: string) (n: int) : list<AssignedPortfolio> =
    ym
    |> getInvestmentUniverse
    |> doMyFilters 
    |> getMySignals
    |> assignSignalSort strategyName n
> **Task:** Using your `formAssignedPortfolios` function, calculate quintile portfolios (5 portfolios) for your signal for August 2017. Assign the result to `aug2017Assignments5`. I've assigned type signatures to constrain the output to the correct type.
> 


let aug2017Assignments5: list<AssignedPortfolio> =
    formAssignedPortfolios (DateTime(2017, 8, 1)) strategyName 5
> **Task:** Assign quintile 5 to a value named `quintile5`. I've assigned type constraints to make sure the output is the correct type.
> 


let quintile5: AssignedPortfolio = 
    aug2017Assignments5[4]

> **Task:** How many stocks are in quintile 5?
> 
let stocksInQuintile =
    quintile5.Signals.Length

stocksInQuintile
> **Task:** Calculate value-weights for quintile 5. The result should have type `Portfolio`.
> 


let quintile5VW: Portfolio =
    quintile5
    |> giveValueWeights getMarketCap
> **Task:** Plot a histogram of the position weights for the stocks in quintile 5.
> 
let weightHistogram =
    quintile5VW.Positions
    |> List.map (fun x -> x.Weight)
    |> Chart.Histogram

weightHistogram

> **Task:** Calculate the minimum, 5th percentile, 50th percentile, 95th percentile, and maxium of the position weights for quintile 5.
> 
let quintile5WeightsSorted = quintile5VW.Positions |> List.sortBy (fun x -> x.Weight) |> List.map (fun x -> x.Weight)

let pctl = [0.05; 0.5; 0.95]

let signalMin = quintile5WeightsSorted |> Seq.min
let signalP5 = Quantile.compute pctl[0] (quintile5WeightsSorted |> List.toArray)
let signalP50 = Quantile.compute pctl[1] (quintile5WeightsSorted |> List.toArray)
let signalP95 = Quantile.compute pctl[2] (quintile5WeightsSorted |> List.toArray)
let signalMax = quintile5WeightsSorted |> Seq.max
> **Task:** Calculate the total weight put in quintile 5's top 10 positions when using value weights. How does this compare to the total weight of the top 10 positions if you used equal weights instead of value weights?
> 
let quintile5WeightsSortedReverse = quintile5VW.Positions |> List.sortBy (fun x -> -x.Weight) |> List.map (fun x -> x.Weight)

let weightsEqualWeight = 1.0/float(stocksInQuintile)

let totalWeightTop10ValueWeight =
    quintile5WeightsSortedReverse
    |> List.take 10
    |> List.sum

let totalWeightTop10EqualWeight =
    weightsEqualWeight * 10.0

// Comparing the total weight from the two strategies (value-weight vs equal-weight), we see that the value-weight strategy gives a substantially larger weight to its top 10 stocks - around 31% of the investment -, while the equal-weight strategy only attributes around 1.2% of the investment to its top 10 stocks. This happens because the value-weighted strategy takes into account the market-cap of each stock and attributes a larger weight to "more valuable" stocks, that is, the ones with higher market cap.