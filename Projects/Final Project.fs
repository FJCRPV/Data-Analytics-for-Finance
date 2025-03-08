## **Data Analytics for Finance Final Project**
#### Professor Nicholas Hirshey
Student Name | Student Number | Signal
--- | --- | ---
Francisco Perestrello | 39001 | Highest 5-days of Return
Let us start by loading the necessary modules, defining our current directory and loading the data.
#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET,2.0.0-preview.17"
#r "nuget: Plotly.NET.Interactive,2.0.0-preview.17"
#r "nuget: MathNet.Numerics"
#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: Accord"
#r "nuget: Accord.Statistics"
#r "nuget: DiffSharp-lite"

open System
open FSharp.Data
open FSharp.Stats
open Plotly.NET
open MathNet.Numerics.Statistics
open Accord
open Accord.Statistics.Models.Regression.Linear
open DiffSharp
#load "Portfolio.fsx"
open Portfolio
#load "Common.fsx"
open Common
#load "YahooFinance.fsx"
open YahooFinance
let [<Literal>] ResolutionFolder = __SOURCE_DIRECTORY__
Environment.CurrentDirectory <- ResolutionFolder

let [<Literal>] IdAndReturnsFilePath = "Data/id_and_return_data.csv"
let [<Literal>] MySignalFilePath = "Data/rmax5_21d.csv"
// Set dotnet interactive formatter to plaintext
Formatter.Register(fun (x:obj) (writer: TextWriter) -> fprintfn writer "%120A" x )
Formatter.SetPreferredMimeTypesFor(typeof<obj>, "text/plain")
// Make plotly graphs work with interactive plaintext formatter
Formatter.SetPreferredMimeTypesFor(typeof<GenericChart.GenericChart>,"text/html")
type IdAndReturnsType = 
    CsvProvider<Sample=IdAndReturnsFilePath,
                // The schema parameter is not required,
                // but I am using it to override some column types
                // to make filtering easier.
                // If I didn't do this these particular columns 
                // would have strings of "1" or "0", but explicit boolean is nicer.
                Schema="obsMain(string)->obsMain=bool,exchMain(string)->exchMain=bool",
                ResolutionFolder=ResolutionFolder>

type MySignalType = 
    CsvProvider<MySignalFilePath,
                ResolutionFolder=ResolutionFolder>
let idAndReturnsCsv = IdAndReturnsType.GetSample()
let mySignalCsv = MySignalType.GetSample()
let idAndReturnsRows = idAndReturnsCsv.Rows |> Seq.toList
let mySignalRows = mySignalCsv.Rows |> Seq.toList
Some useful functions we will need.
let msfBySecurityIdAndMonth =
    idAndReturnsRows
    |> List.map(fun row -> 
        let id = Other row.Id
        let month = DateTime(row.Eom.Year,row.Eom.Month,1)
        let key = id, month
        key, row)
    |> Map    

let signalBySecurityIdAndMonth =
    mySignalRows
    |> List.choose(fun row -> 
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

let securitiesByFormationMonth =
    idAndReturnsRows
    |> List.groupBy(fun x -> DateTime(x.Eom.Year, x.Eom.Month,1))
    |> List.map(fun (ym, obsThisMonth) -> 
        let idsThisMonth = [ for x in obsThisMonth do Other x.Id ]
        ym, idsThisMonth)
    |> Map

let getInvestmentUniverse formationMonth =
    match Map.tryFind formationMonth securitiesByFormationMonth with
    | Some securities -> 
        { FormationMonth = formationMonth 
          Securities = securities }
    | None -> failwith $"{formationMonth} is not in the date range"

let getMySignal (securityId, formationMonth) =
    match Map.tryFind (securityId, formationMonth) signalBySecurityIdAndMonth with
    | None -> None
    | Some signal ->
        Some { SecurityId = securityId 
               // if a high signal means low returns,
               // use `-signal` here instead of `signal`
               Signal = signal }

let getMySignals (investmentUniverse: InvestmentUniverse) =
    let listOfSecuritySignals =
        investmentUniverse.Securities
        |> List.choose(fun security -> 
            getMySignal (security, investmentUniverse.FormationMonth))    
    
    { FormationMonth = investmentUniverse.FormationMonth 
      Signals = listOfSecuritySignals }

let getMarketCap (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> None
    | Some row -> 
        match row.MarketEquity with
        | None -> None
        | Some me -> Some (security, me)

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

let myFilters securityAndFormationMonth =
    isObsMain securityAndFormationMonth &&
    isPrimarySecurity securityAndFormationMonth &&
    isCommonStock securityAndFormationMonth &&
    isExchMain securityAndFormationMonth &&
    hasMarketEquity securityAndFormationMonth

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

let startSample = 
    idAndReturnsRows
    |> List.map(fun row -> DateTime(row.Eom.Year,row.Eom.Month,1))
    |> List.min

let endSample = 
    let lastMonthWithData = 
        idAndReturnsRows
        |> Seq.map(fun row -> DateTime(row.Eom.Year,row.Eom.Month,1))
        |> Seq.max
    // The end of sample is the last month when we have returns.
    // So the last month when we can form portfolios is one month
    // before that.
    lastMonthWithData.AddMonths(-1) 

let sampleMonths = getSampleMonths (startSample, endSample)
### **Strategy Analysis**
#### **Constructing the strategy.**
Sorting stocks monthly and into terciles based on the signal, and then forming value-weighted portfolios for each tercile.
let formStrategy ym =
    ym
    |> getInvestmentUniverse
    |> doMyFilters
    |> getMySignals
    |> assignSignalSort "Highest 5 Days of Return" 3
    |> List.map (giveValueWeights getMarketCap)
    |> List.map (getPortfolioReturn getSecurityReturn)

let doParallel = true
let portfolios =
    if doParallel then
        sampleMonths
        |> List.toArray
        |> Array.Parallel.map formStrategy
        |> Array.toList
        |> List.collect id
    else
        sampleMonths
        |> List.collect formStrategy
Turning returns of these terciles into excess returns, i.e. in excess of the risk-free rate.
let ff3 = 
    French.getFF3 Frequency.Monthly

let monthlyRiskFreeRate =
    [ for obs in ff3 do 
        let key = DateTime(obs.Date.Year,obs.Date.Month,1)
        key, obs.Rf ]
    |> Map

let portfolioExcessReturns =
    portfolios
    |> List.map(fun x -> 
        match Map.tryFind x.YearMonth monthlyRiskFreeRate with 
        | None -> failwith $"Can't find risk-free rate for {x.YearMonth}"
        | Some rf -> { x with Return = x.Return - rf })
Forming our useful portfolios, with the Long-Only portfolio being the top tercile, and the Long-Short portfolio representing a strategy that is long on the top portfolio and short on the bottom portfolio.
let long = 
    portfolioExcessReturns 
    |> List.filter(fun x -> 
        x.PortfolioId = Indexed {| Name = "Highest 5 Days of Return"; Index = 3 |})
    |> List.map(fun x -> 
        { PortfolioId = Named "Long-Only"
          YearMonth = x.YearMonth
          Return = x.Return})

let short = 
    portfolioExcessReturns 
    |> List.filter(fun x -> 
        x.PortfolioId = Indexed {| Name = "Highest 5 Days of Return"; Index = 1 |})
    
let longShort = 
    // We'll loop through the long portfolio observations,
    // looking for the short portfolio observation for that month.
    // For efficiently looking up the short portfolio by month,
    // we're putting it in a Map collection indexed by month.
    let shortByYearMonthMap = 
        short 
        |> List.map(fun row -> row.YearMonth, row) 
        |> Map
    
    [ for longObs in long do
        match Map.tryFind longObs.YearMonth shortByYearMonthMap with
        | None -> failwith "probably your date variables are not aligned for a weird reason"
        | Some shortObs ->
            { PortfolioId = Named "Long-Short"
              YearMonth = longObs.YearMonth
              Return = longObs.Return - shortObs.Return } ] 
#### **Analyzing the performance of the portfolios**
Defining a useful function to cumulate returns, a function for plotting returns, and defining the value-weighted stock market portfolio.
let cumulateSimpleReturn (xs: PortfolioReturn list) =
    let accumulator (priorObs:PortfolioReturn) (thisObs:PortfolioReturn) =
        let asOfNow = (1.0 + priorObs.Return)*(1.0 + thisObs.Return) - 1.0
        { thisObs with Return = asOfNow}
    // remember to make sure that your sort order is correct.
    match xs |> List.sortBy(fun x -> x.YearMonth) with
    | [] -> []      // return empty list if the input list is empty
    | head::tail -> // if there are observations do the calculation
        (head, tail) 
        ||> List.scan accumulator

let portfolioReturnPlot (xs:PortfolioReturn list) =
    xs
    |> List.map(fun x -> x.YearMonth, x.Return)
    |> Chart.Line 

let vwMktRf = 
    let portfolioMonths = 
        portfolioExcessReturns 
        |> List.map(fun x -> x.YearMonth)
    let minYm = portfolioMonths |> List.min
    let maxYm = portfolioMonths |> List.max
    
    [ for x in ff3 do
        if x.Date >= minYm && x.Date <= maxYm then
            { PortfolioId = Named("Value-Weighted Market Portfolio")
              YearMonth = x.Date
              Return = x.MktRf } ]
Plotting the cumulative returns for all portfolios.
let combinedChart =
    List.concat [long; longShort; vwMktRf]
    |> List.groupBy(fun x -> x.PortfolioId)
    |> List.map(fun (portId, xs) ->
        xs
        |> cumulateSimpleReturn
        |> portfolioReturnPlot
        |> Chart.withTraceInfo (Name=portId.ToString()))
    |> Chart.combine
    |> Chart.withTitle "Growth of 1 Euro Invested in Each Portfolio"

combinedChart
Finding the necessary constant leverage for each portfolio for it to have an annualized volatility of 10% over the full sample and defining these new portfolios.
let stdDevLong = long |> Seq.stDevBy (fun x -> x.Return)
let stdDevLongShort = longShort |> Seq.stDevBy (fun x -> x.Return)
let stdDevVwMktRf = vwMktRf |> Seq.stDevBy (fun x -> x.Return)

let annualizeMonthlyStdDev monthlyStdDev = sqrt(12.0) * monthlyStdDev

let stdDevLongAnnualized = annualizeMonthlyStdDev stdDevLong
let stdDevLongShortAnnualized = annualizeMonthlyStdDev stdDevLongShort
let stdDevLongVwMktRfAnnualized = annualizeMonthlyStdDev stdDevVwMktRf

let target = 0.1

let leverageLong = target/stdDevLongAnnualized
let leverageLongShort = target/stdDevLongShortAnnualized
let leverageVwMktRf = target/stdDevLongVwMktRfAnnualized

let long10 =
    long
    |> List.map (fun x ->
        { PortfolioId = x.PortfolioId
          YearMonth = x.YearMonth
          Return = x.Return * leverageLong})
    
let longShort10 =
    longShort
    |> List.map (fun x ->
        { PortfolioId = x.PortfolioId
          YearMonth = x.YearMonth
          Return = x.Return * leverageLongShort})

let vwMktRf10 =
    vwMktRf
    |> List.map (fun x ->
        { PortfolioId = x.PortfolioId
          YearMonth = x.YearMonth
          Return = x.Return * leverageVwMktRf})
Plotting the cumulative returns for all portfolios with applied leverage to reach 10% volatility over the full sample.
let combinedChart =
    List.concat [long10; longShort10; vwMktRf10]
    |> List.groupBy(fun x -> x.PortfolioId)
    |> List.map(fun (portId, xs) ->
        xs
        |> cumulateSimpleReturn
        |> portfolioReturnPlot
        |> Chart.withTraceInfo (Name=portId.ToString()))
    |> Chart.combine
    |> Chart.withTitle "Growth of 1 Euro Invested in Each Portfolio with Volatility 10%"

combinedChart
To report performance measures, let us first do the calculations for the full sample for all three portfolios.
let annualizedExcessReturn (xs: float seq) = 
    (Seq.mean xs) * 12.0

let longAnnExcessReturn = long |> List.map (fun x -> x.Return) |> annualizedExcessReturn
let longShortAnnExcessReturn = longShort |> List.map (fun x -> x.Return) |> annualizedExcessReturn
let vwMktRfAnnExcessReturn = vwMktRf |> List.map (fun x -> x.Return) |> annualizedExcessReturn
let annualizedSharpe (xs: float seq) =
    ((Seq.mean xs) / (Seq.stDev xs)) * sqrt(12.0)

let longAnnSharpe = long |> List.map (fun x -> x.Return) |> annualizedSharpe
let longShortAnnSharpe = longShort |> List.map (fun x -> x.Return) |> annualizedSharpe
let vwMktRfAnnSharpe = vwMktRf |> List.map (fun x -> x.Return) |> annualizedSharpe
type RegData =
    { Date : DateTime
      Portfolio : float
      MktRf : float 
      Hml : float 
      Smb : float }

let ff3ByMonth = 
    ff3
    |> Array.map(fun x -> DateTime(x.Date.Year, x.Date.Month,1), x)
    |> Map

let regData (x: list<PortfolioReturn>) =
    x
    |> List.map(fun x ->
        let monthToFind = DateTime(x.YearMonth.Year,x.YearMonth.Month,1)
        match Map.tryFind monthToFind ff3ByMonth with
        | None -> failwith "probably you messed up your days of months"
        | Some ff3 -> 
            { Date = monthToFind
              Portfolio = x.Return
              MktRf = ff3.MktRf 
              Hml = ff3.Hml 
              Smb = ff3.Smb })
    |> List.toArray

let longRegData = regData long
let longShortRegData = regData longShort
type RegressionOutput =
    { Model : MultipleLinearRegression 
      TValuesWeights : float array
      TValuesIntercept : float 
      R2: float }
 
type XY = (float array) array * float array

let fitModel (x: (float array) array, y: float array) =
    let ols = new OrdinaryLeastSquares(UseIntercept=true)
    let estimate = ols.Learn(x,y)
    let mse = estimate.GetStandardError(x,y)
    let se = estimate.GetStandardErrors(mse, ols.GetInformationMatrix())
    let tvaluesWeights = 
        estimate.Weights
        |> Array.mapi(fun i w -> w / se.[i])
    let tvalueIntercept = estimate.Intercept / (se |> Array.last)
    let r2 = estimate.CoefficientOfDetermination(x,y)
    { Model = estimate
      TValuesWeights = tvaluesWeights
      TValuesIntercept = tvalueIntercept  
      R2 = r2 }

let capmModelData (x: RegData array) =
    x
    |> Array.map(fun obs -> [|obs.MktRf|], obs.Portfolio)
    |> Array.unzip

let ff3ModelData (x: RegData array) =
    x
    |> Array.map(fun obs -> [|obs.MktRf; obs.Hml; obs.Smb |], obs.Portfolio)
    |> Array.unzip

let longCapmModelData = capmModelData longRegData
let longFf3ModelData = ff3ModelData longRegData

let longShortCapmModelData = capmModelData longShortRegData
let longShortFf3ModelData = ff3ModelData longShortRegData
let longCapmEstimate = longCapmModelData |> fitModel
let longFf3Estimate = longFf3ModelData |> fitModel

let longShortCapmEstimate = longShortCapmModelData |> fitModel
let longShortFf3Estimate = longShortFf3ModelData |> fitModel
// Get predicted values and residuals

type Prediction = { Label : float; Score : float}

let makePredictions 
    (estimate:MultipleLinearRegression) 
    (x: (float array) array, y: float array) =
    (estimate.Transform(x), y)
    ||> Array.zip
    |> Array.map(fun (score, label) -> { Score = score; Label = label })

let residuals (xs: Prediction array) = xs |> Array.map(fun x -> x.Label - x.Score)


let longCapmPredictions = makePredictions longCapmEstimate.Model longCapmModelData
let longCapmResiduals = residuals longCapmPredictions

let longShortCapmPredictions = makePredictions longShortCapmEstimate.Model longShortCapmModelData
let longShortCapmResiduals = residuals longShortCapmPredictions

// Information Ratio Function

let informationRatio monthlyAlpha (monthlyResiduals: float array) =
    let annualAlpha = 12.0 * monthlyAlpha
    let annualStDev = sqrt(12.0) * (Seq.stDev monthlyResiduals)
    annualAlpha / annualStDev 

let longInformationRatio = informationRatio longCapmEstimate.Model.Intercept longCapmResiduals
let longShortInformationRatio = informationRatio longShortCapmEstimate.Model.Intercept longShortCapmResiduals
Table for the performance analysis of the full sample.
let round (x:float) =
    System.Math.Round(x,2)

let headers = ["Portfolio"; "Avg. Ann. Excess Return"; "Ann. Sharpe Ratio"; "CAPM Alpha"; "CAPM Alpha T-Stat"; "FF3 Alpha"; "FF3 Alpha T-Stat"; "Info. Ratio"]
let rows = 
    [
        [1.0; longAnnExcessReturn |> round; longAnnSharpe |> round; longCapmEstimate.Model.Intercept |> round; longCapmEstimate.TValuesIntercept |> round; longFf3Estimate.Model.Intercept |> round; longFf3Estimate.TValuesIntercept |> round; longInformationRatio |> round]
        [2.0; longShortAnnExcessReturn |> round; longShortAnnSharpe |> round; longShortCapmEstimate.Model.Intercept |> round; longShortCapmEstimate.TValuesIntercept |> round; longShortFf3Estimate.Model.Intercept |> round; longShortFf3Estimate.TValuesIntercept |> round; longShortInformationRatio |> round]
        [3.0; vwMktRfAnnExcessReturn |> round; vwMktRfAnnSharpe |> round]
    ]
Chart.Table(headers, rows)
Calculating the same measures for our first and second half of the sample.
let longSplit = long |> List.splitInto 2
let longShortSplit = longShort |> List.splitInto 2
let vwMktRfSplit = vwMktRf |> List.splitInto 2

let long1stHalf = longSplit[0]
let long2ndHalf = longSplit[1]
let longShort1stHalf = longShortSplit[0]
let longShort2ndHalf = longShortSplit[1]
let vwMktRf1stHalf = vwMktRfSplit[0]
let vwMktRf2ndHalf = vwMktRfSplit[1]

let long1stHalfAnnExcessReturn = long1stHalf |> List.map (fun x -> x.Return) |> annualizedExcessReturn
let long2ndHalfAnnExcessReturn = long2ndHalf |> List.map (fun x -> x.Return) |> annualizedExcessReturn
let longShort1stHalfAnnExcessReturn = longShort1stHalf |> List.map (fun x -> x.Return) |> annualizedExcessReturn
let longShort2ndHalfAnnExcessReturn = longShort2ndHalf |> List.map (fun x -> x.Return) |> annualizedExcessReturn
let vwMktRf1stHalfAnnExcessReturn = vwMktRf1stHalf |> List.map (fun x -> x.Return) |> annualizedExcessReturn
let vwMktRf2ndHalfAnnExcessReturn = vwMktRf2ndHalf |> List.map (fun x -> x.Return) |> annualizedExcessReturn


let long1stHalfAnnSharpe = long1stHalf |> List.map (fun x -> x.Return) |> annualizedSharpe
let long2ndHalfAnnSharpe = long2ndHalf |> List.map (fun x -> x.Return) |> annualizedSharpe
let longShort1stHalfAnnSharpe = longShort1stHalf |> List.map (fun x -> x.Return) |> annualizedSharpe
let longShort2ndHalfAnnSharpe = longShort2ndHalf |> List.map (fun x -> x.Return) |> annualizedSharpe
let vwMktRf1stHalfAnnSharpe = vwMktRf1stHalf |> List.map (fun x -> x.Return) |> annualizedSharpe
let vwMktRf2ndHalfAnnSharpe = vwMktRf2ndHalf |> List.map (fun x -> x.Return) |> annualizedSharpe

let long1stHalfRegData = regData long1stHalf
let long2ndHalfRegData = regData long2ndHalf
let longShort1stHalfRegData = regData longShort1stHalf
let longShort2ndHalfRegData = regData longShort2ndHalf


let long1stHalfCapmModelData = capmModelData long1stHalfRegData
let long1stHalfFf3ModelData = ff3ModelData long1stHalfRegData
let long2ndHalfCapmModelData = capmModelData long2ndHalfRegData
let long2ndHalfFf3ModelData = ff3ModelData long2ndHalfRegData

let longShort1stHalfCapmModelData = capmModelData longShort1stHalfRegData
let longShort1stHalfFf3ModelData = ff3ModelData longShort1stHalfRegData
let longShort2ndHalfCapmModelData = capmModelData longShort2ndHalfRegData 
let longShort2ndHalfFf3ModelData = ff3ModelData longShort2ndHalfRegData


let long1stHalfCapmEstimate = long1stHalfCapmModelData |> fitModel
let long1stHalfFf3Estimate = long1stHalfFf3ModelData |> fitModel
let long2ndHalfCapmEstimate = long2ndHalfCapmModelData |> fitModel
let long2ndHalfFf3Estimate = long2ndHalfFf3ModelData |> fitModel

let longShort1stHalfCapmEstimate = longShort1stHalfCapmModelData |> fitModel
let longShort1stHalfFf3Estimate = longShort1stHalfFf3ModelData |> fitModel
let longShort2ndHalfCapmEstimate = longShort2ndHalfCapmModelData |> fitModel
let longShort2ndHalfFf3Estimate = longShort2ndHalfFf3ModelData |> fitModel


let long1stHalfCapmPredictions = makePredictions long1stHalfCapmEstimate.Model long1stHalfCapmModelData
let long2ndHalfCapmPredictions = makePredictions long2ndHalfCapmEstimate.Model long2ndHalfCapmModelData
let long1stHalfCapmResiduals = residuals long1stHalfCapmPredictions
let long2ndHalfCapmResiduals = residuals long2ndHalfCapmPredictions

let longShort1stHalfCapmPredictions = makePredictions longShort1stHalfCapmEstimate.Model longShort1stHalfCapmModelData
let longShort2ndHalfCapmPredictions = makePredictions longShort2ndHalfCapmEstimate.Model longShort2ndHalfCapmModelData
let longShort1stHalfCapmResiduals = residuals longShort1stHalfCapmPredictions
let longShort2ndHalfCapmResiduals = residuals longShort2ndHalfCapmPredictions


let long1stHalfInformationRatio = informationRatio long1stHalfCapmEstimate.Model.Intercept long1stHalfCapmResiduals
let long2ndHalfInformationRatio = informationRatio long2ndHalfCapmEstimate.Model.Intercept long2ndHalfCapmResiduals
let longShort1stHalfInformationRatio = informationRatio longShort1stHalfCapmEstimate.Model.Intercept longShort1stHalfCapmResiduals
let longShort2ndHalfInformationRatio = informationRatio longShort2ndHalfCapmEstimate.Model.Intercept longShort2ndHalfCapmResiduals
Table for the performance analysis of the second half of the sample.
let headers = ["Portfolio"; "Avg. Ann. Excess Return"; "Ann. Sharpe Ratio"; "CAPM Alpha"; "CAPM Alpha T-Stat"; "FF3 Alpha"; "FF3 Alpha T-Stat"; "Info. Ratio"]
let rows = 
    [
        [1.0; long1stHalfAnnExcessReturn |> round; long1stHalfAnnSharpe |> round; long1stHalfCapmEstimate.Model.Intercept |> round; long1stHalfCapmEstimate.TValuesIntercept |> round; long1stHalfFf3Estimate.Model.Intercept |> round; long1stHalfFf3Estimate.TValuesIntercept |> round; long1stHalfInformationRatio |> round]
        [2.0; longShort1stHalfAnnExcessReturn |> round; longShort1stHalfAnnSharpe |> round; longShort1stHalfCapmEstimate.Model.Intercept |> round; longShort1stHalfCapmEstimate.TValuesIntercept |> round; longShort1stHalfFf3Estimate.Model.Intercept |> round; longShort1stHalfFf3Estimate.TValuesIntercept |> round; longShort1stHalfInformationRatio |> round]
        [3.0; vwMktRf1stHalfAnnExcessReturn |> round; vwMktRf1stHalfAnnSharpe |> round]
    ]

Chart.Table(headers, rows)
Table for the performance analysis of the second half of the sample.
let headers = ["Portfolio"; "Avg. Ann. Excess Return"; "Ann. Sharpe Ratio"; "CAPM Alpha"; "CAPM Alpha T-Stat"; "FF3 Alpha"; "FF3 Alpha T-Stat"; "Info. Ratio"]
let rows = 
    [
        [1.0; long2ndHalfAnnExcessReturn |> round; long2ndHalfAnnSharpe |> round; long2ndHalfCapmEstimate.Model.Intercept |> round; long2ndHalfCapmEstimate.TValuesIntercept |> round; long2ndHalfFf3Estimate.Model.Intercept |> round; long2ndHalfFf3Estimate.TValuesIntercept |> round; long2ndHalfInformationRatio |> round]
        [2.0; longShort2ndHalfAnnExcessReturn |> round; longShort2ndHalfAnnSharpe |> round; longShort2ndHalfCapmEstimate.Model.Intercept |> round; longShort2ndHalfCapmEstimate.TValuesIntercept |> round; longShort2ndHalfFf3Estimate.Model.Intercept |> round; longShort2ndHalfFf3Estimate.TValuesIntercept |> round; longShort2ndHalfInformationRatio |> round]
        [3.0; vwMktRf2ndHalfAnnExcessReturn |> round; vwMktRf2ndHalfAnnSharpe |> round]
    ]

Chart.Table(headers, rows)
### **Strategy as a part of a diversified portfolio.**
Defining some useful functions, records and variables.
type StockData =
    { Symbol : string 
      Date : DateTime
      Return : float }

let ff3List = French.getFF3 Frequency.Monthly |> Array.toList

let longStockData =
       long 
       |> List.map(fun x -> {Symbol="Long"; Date=x.YearMonth; Return=x.Return})

let longShortStockData =
       longShort
       |> List.map(fun x -> {Symbol="Long-Short"; Date=x.YearMonth; Return=x.Return})

let tickers = 
    [ 
        "VBR" // Vanguard Small-cap Value ETF
        "VUG" // Vanguard Growth ETF
        "VTI" // Vanguard Total Stock Market ETF
        "BND" // Vanguard Total Bond Market ETF
    ]

let tickPrices = 
    YahooFinance.PriceHistory(
        tickers,
        startDate = DateTime(2000,2,1),
        interval = Monthly)

let pricesToReturns (symbol, adjPrices: list<PriceObs>) =
    adjPrices
    |> List.sortBy (fun x -> x.Date)
    |> List.pairwise
    |> List.map (fun (day0, day1) ->
        let r = day1.AdjustedClose / day0.AdjustedClose - 1.0 
        { Symbol = symbol 
          Date = day1.Date 
          Return = r })

let tickReturns =
    tickPrices
    |> List.groupBy (fun x -> x.Symbol)
    |> List.collect pricesToReturns

let standardInvestmentsExcess = // Making sure the ETFs present excess returns
    let maxff3Date = ff3List |> List.map(fun x -> x.Date) |> List.max
    tickReturns
    |> List.filter(fun x -> x.Date <= maxff3Date)
    |> List.map(fun x -> 
        match Map.tryFind x.Date monthlyRiskFreeRate with 
        | None -> failwith $"why isn't there a rf for {x.Date}"
        | Some rf -> { x with Return = x.Return - rf })

let stockDataWith (x: list<StockData>) (y: list<StockData>) =
    [x;y]
    |> List.concat
    |> List.groupBy(fun x -> x.Symbol)
    |> Map

let stockDataWithLong = stockDataWith standardInvestmentsExcess longStockData
let stockDataWithLongShort = stockDataWith standardInvestmentsExcess longShortStockData

let getCov xId yId (stockData: Map<string,StockData list>) =
    let xRet = 
        stockData[xId] 
        |> List.map (fun x -> x.Date,x.Return) 
        |> Map
    let yRet = 
        stockData[yId]
        |> List.map (fun y -> y.Date, y.Return)
        |> Map
    let overlappingDates =
        [ xRet.Keys |> set
          yRet.Keys |> set]
        |> Set.intersectMany
    [ for date in overlappingDates do xRet[date], yRet[date]]
    |> Seq.covOfPairs

let tickersLong = 
    [ 
        "VBR" // Vanguard Small-cap Value ETF
        "VUG" // Vanguard Growth ETF
        "VTI" // Vanguard Total Stock Market ETF
        "BND" // Vanguard Total Bond Market ETF
        "Long" // My Long-Only Portfolio
    ]

let tickersLongShort = 
    [ 
        "VBR" // Vanguard Small-cap Value ETF
        "VUG" // Vanguard Growth ETF
        "VTI" // Vanguard Total Stock Market ETF
        "BND" // Vanguard Total Bond Market ETF
        "Long-Short" // My Long-Short Portfolio
    ]

let covariances (xTickers: list<string>) (yStockData: Map<string,StockData list>) =
    [ for rowTick in xTickers do 
        [ for colTick in xTickers do
            getCov rowTick colTick yStockData ]]
    |> dsharp.tensor

let means (xTickers: list<string>) (yStockData: Map<string,StockData list>) =
    [ for ticker in xTickers do
        yStockData[ticker]
        |> List.averageBy (fun x -> x.Return)]
    |> dsharp.tensor

let covariancesLong = covariances tickersLong stockDataWithLong
let meansLong = means tickersLong stockDataWithLong

let covariancesLongShort = covariances tickersLongShort stockDataWithLongShort
let meansLongShort = means tickersLongShort stockDataWithLongShort
Finding the two versions of the tangency portfolio, one using the long-only portfolio combined with other assets, and the second version using the long-short portfolio combined with other assets.
let w' (covariances: Tensor) (means: Tensor) = dsharp.solve(covariances,means)
let w (w': Tensor)= w' / w'.sum()
let portVariance (w: Tensor) (covariances: Tensor)= w.matmul(covariances).matmul(w)
let portStDev (portVariance: Tensor) = portVariance.sqrt()
let portMean (w: Tensor) (means: Tensor) = dsharp.matmul(w,means)
let portAnnSharpe (portMean: Tensor) (portStDev: Tensor)= sqrt(12.0)*(portMean/portStDev)
let wLong' = w' covariancesLong meansLong
let wLong = w wLong'
let longPortVariance = portVariance wLong covariancesLong
let longPortStDev = portStDev longPortVariance
let longPortMean = portMean wLong meansLong
let longPortAnnSharpe = portAnnSharpe longPortMean longPortStDev


let wLongShort' = w' covariancesLongShort meansLongShort
let wLongShort = w wLongShort'
let longShortPortVariance = portVariance wLongShort covariancesLongShort
let longShortPortStDev = portStDev longShortPortVariance
let longShortPortMean = portMean wLongShort meansLongShort
let longShortPortAnnSharpe = portAnnSharpe longShortPortMean longShortPortStDev
Defining some useful functions to find the tangency portfolio.
let weights (tickers: list<string>) (w: Tensor) =
    Seq.zip tickers (w.toArray1D<float>())
    |> Map.ofSeq

let stockDataByDateWith (stockDataWith: Map<string,list<StockData>>) =
    stockDataWith.Values
    |> Seq.toList
    |> List.collect id // combine all different StockDataWithLong symbols into one list.
    |> List.groupBy(fun x -> x.Date) // group all symbols on the same date together.
    |> List.sortBy fst // sort by the grouping variable, which here is Date.

let allAssetsStart (stockData: list<DateTime * list<StockData>>) (tickers: list<string>)=
    stockData
    // find the first array element where there are as many stocks as you have symbols
    |> List.find(fun (month, stocks) -> stocks.Length = tickers.Length)
    |> fst // convert (month, stocks) to month

let allAssetsEnd (stockData: list<DateTime * list<StockData>>) (tickers: list<string>) =
    stockData
    // find the last array element where there are as many stocks as you have symbols
    |> List.findBack(fun (month, stocks) -> stocks.Length = tickers.Length)
    |> fst // convert (month, stocks) to month

// filter our data between the above defined dates
let stockDataWithByDateComplete (stockData: list<DateTime * list<StockData>>) (startDate: DateTime) (endDate: DateTime) =
    stockData
    |> List.filter(fun (date, stocks) -> 
        date >= startDate &&
        date <= endDate)

let checkOfCompleteData (stockData: list<DateTime * list<StockData>>) (tickers: list<string>) =
    stockData
    |> List.map snd
    |> List.filter(fun x -> x.Length <> tickers.Length)

let portfolioMonthReturn weights monthData =
    weights
    |> Map.toList
    |> List.map(fun (symbol, weight) ->
        let symbolData = 
            // we're going to be more safe and use tryFind here so
            // that our function is more reusable
            match monthData |> List.tryFind(fun x -> x.Symbol = symbol) with
            | None -> failwith $"You tried to find {symbol} in the data but it was not there"
            | Some data -> data
        symbolData.Return*weight)
    |> List.sum   
let weightsLong = weights tickersLong wLong
let weightsLongShort = weights tickersLongShort wLongShort


let stockDataWithLongByDate = stockDataByDateWith stockDataWithLong
let stockDataWithLongShortByDate = stockDataByDateWith stockDataWithLongShort


let allAssetsStartLong = allAssetsStart stockDataWithLongByDate tickersLong
let allAssetsStartLongShort = allAssetsStart stockDataWithLongShortByDate tickersLongShort


let allAssetsEndLong = allAssetsEnd stockDataWithLongByDate tickersLong
let allAssetsEndLongShort = allAssetsEnd stockDataWithLongShortByDate tickersLongShort

//Ok, let's filter our data between those dates.
let stockDataWithLongByDateComplete = stockDataWithByDateComplete stockDataWithLongByDate allAssetsStartLong allAssetsEndLong
let stockDataWithLongShortByDateComplete = stockDataWithByDateComplete stockDataWithLongShortByDate allAssetsStartLongShort allAssetsEndLongShort


let checkOfCompleteDataLong =  checkOfCompleteData stockDataWithLongByDateComplete tickersLong
let checkOfCompleteDataLongShort =  checkOfCompleteData stockDataWithLongShortByDateComplete tickersLongShort
 
Our final two versions of the tangency portfolio.
let portMve (stockData: list<DateTime * list<StockData>>) (weights: Map<string,float>) =
  stockData
  |> List.map(fun (date, data) -> 
        { Symbol = "MVE"
          Date = date
          Return = portfolioMonthReturn weights data })

let portMveLong = portMve stockDataWithLongByDateComplete weightsLong
let portMveLongShort = portMve stockDataWithLongShortByDateComplete weightsLongShort
#### **Forming comparison diversified portfolios.**
Forming a 60/40 Portfolio and an Equal-Weighted Portfolio.
let weights6040 = Map [("VTI",0.6);("BND",0.4)]
let equalWeights = Map [("VTI",0.25);("BND",0.25);("VBR",0.25);("VUG",0.25)]

let port6040 = 
    stockDataWithLongByDateComplete  //this stock data includes all necessary data for the four ETFS, so we dont need to create another stock data for the ETFS alone
    |> List.map(fun (date, data) -> 
        { Symbol = "60/40"
          Date = date 
          Return = portfolioMonthReturn weights6040 data} )

let portEqualWeight = 
    stockDataWithLongByDateComplete //this stock data includes all necessary data for the four ETFS, so we dont need to create another stock data for the ETFS alone
    |> List.map(fun (date, data) -> 
        { Symbol = "EW"
          Date = date 
          Return = portfolioMonthReturn equalWeights data} )
#### **Analyzing the performance of the mean-variance efficient portfolios and the comparison portfolios.**
Plotting cumulative returns for all portfolios.
let cumulateReturns (xs: list<StockData>) =
    let folder (prev: StockData) (current: StockData) =
        let newReturn = prev.Return * (1.0+current.Return)
        { current with Return = newReturn}
    
    match xs |> List.sortBy (fun x -> x.Date) with
    | [] -> []
    | h::t ->
        ({ h with Return = 1.0+h.Return}, t) 
        ||> List.scan folder

let portMveLongCumulative = portMveLong |> cumulateReturns
let portMveLongShortCumulative = portMveLongShort |> cumulateReturns
let port6040Cumulative = port6040|> cumulateReturns
let portEqualWeightCumulative = portEqualWeight |> cumulateReturns


let chart (data: list<StockData>) =
    data
    |> List.map(fun x -> x.Date, x.Return)
    |> Chart.Line

let chartMveLong = (chart portMveLongCumulative) |> Chart.withTraceInfo(Name="MVE With Long-Only Portfolio")
let chartMveLongShort = (chart portMveLongShortCumulative) |> Chart.withTraceInfo(Name="MVE with Long-Short Portfolio")
let chart6040 = (chart port6040Cumulative) |> Chart.withTraceInfo(Name="60/40")
let chartEqualWeight = (chart portEqualWeightCumulative) |> Chart.withTraceInfo(Name="EW")

let chartCombined =
    [ chartMveLong; chartMveLongShort; chart6040; chartEqualWeight]
    |> Chart.combine
    |> Chart.withTitle "Growth of 1 Euro Invested in Each Portfolio"

chartCombined
Plotting the cumulative returns of the portfolios with a constant leverage applied so they present an annualized volatility of 10% over the full sample, to have a better sense for which is better per unit of volatility.
// The method applied is the same as before.
let stdDevLongPortMve = portMveLong |> Seq.stDevBy (fun x -> x.Return)
let stdDevLongShortPortMve = portMveLongShort |> Seq.stDevBy (fun x -> x.Return)
let stdDevPort6040 = port6040 |> Seq.stDevBy (fun x -> x.Return)
let stdDevPortEqualWeight = portEqualWeight |> Seq.stDevBy (fun x -> x.Return)

let stdDevLongPortMveAnnualized = annualizeMonthlyStdDev stdDevLongPortMve
let stdDevLongShortPortMveAnnualized = annualizeMonthlyStdDev stdDevLongShortPortMve
let stdDevPort6040Annualized = annualizeMonthlyStdDev stdDevPort6040
let stdDevPortEqualWeightAnnualized = annualizeMonthlyStdDev stdDevPortEqualWeight

let leverageLongPortMve = target/stdDevLongPortMveAnnualized
let leverageLongShortPortMve = target/stdDevLongShortPortMveAnnualized
let leveragePort6040 = target/stdDevPort6040Annualized
let leveragePortEqualWeight = target/stdDevPortEqualWeightAnnualized


let portMveLeverage (stockData: list<StockData>) (leverage: float) =
    stockData
    |> List.map (fun x ->
        { Symbol = x.Symbol
          Date = x.Date
          Return = x.Return * leverage})

let portLongMve10 = portMveLeverage portMveLong leverageLongPortMve
let portLongShortMve10 = portMveLeverage portMveLongShort leverageLongShortPortMve
let port604010 = portMveLeverage port6040 leveragePort6040 
let portEqualWeight10 = portMveLeverage portEqualWeight leveragePortEqualWeight


let longPortMve10Cumulative = portLongMve10 |> cumulateReturns
let longShortPortMve10Cumulative = portLongShortMve10 |> cumulateReturns
let port604010Cumulative = port604010 |> cumulateReturns
let portEqualWeight10Cumulative = portEqualWeight10 |> cumulateReturns


let chartMveLong = (chart portMveLongCumulative) |> Chart.withTraceInfo(Name="MVE With Long-Only Portfolio")

let chartLongMve10 = (chart longPortMve10Cumulative) |> Chart.withTraceInfo(Name="MVE with Long-Only Portfolio")
let chartLongShortMve10 = (chart longShortPortMve10Cumulative) |> Chart.withTraceInfo(Name="MVE with Long-Short Portfolio")
let chart604010 = (chart port604010Cumulative) |> Chart.withTraceInfo(Name="60/40")
let chartEqualWeight10 = (chart portEqualWeight10Cumulative) |> Chart.withTraceInfo(Name="EW")

let chartCombined =
    [chartLongMve10; chartLongShortMve10; chart604010; chartEqualWeight10]
    |> Chart.combine
    |> Chart.withTitle "Growth of 1 Euro Invested in Each Portfolio with Volatility 10%"

chartCombined
To report performance measures, let us first do the calculations for all four portfolios and then present a table for the performance analysis of the full sample.
let portMveLongAnnExcessReturn = portMveLong |> List.map (fun x -> x.Return) |> annualizedExcessReturn
let portMveLongShortAnnExcessReturn = portMveLongShort |> List.map (fun x -> x.Return) |> annualizedExcessReturn
let port6040AnnExcessReturn = port6040 |> List.map (fun x -> x.Return) |> annualizedExcessReturn
let portEqualWeightAnnExcessReturn = portEqualWeight |> List.map (fun x -> x.Return) |> annualizedExcessReturn

let portMveLongAnnSharpe = portMveLong |> List.map (fun x -> x.Return) |> annualizedSharpe
let portMveLongShortAnnSharpe = portMveLongShort |> List.map (fun x -> x.Return) |> annualizedSharpe
let port6040AnnSharpe = port6040 |> List.map (fun x -> x.Return) |> annualizedSharpe
let portEqualWeightAnnSharpe = portEqualWeight |> List.map (fun x -> x.Return) |> annualizedSharpe

let headers = ["Portfolio"; "Avg. Ann. Excess Return"; "Ann. Sharpe Ratio"]
let rows = 
    [
        [1.0; portMveLongAnnExcessReturn |> round; portMveLongAnnSharpe |> round]
        [2.0; portMveLongShortAnnExcessReturn |> round; portMveLongShortAnnSharpe |> round]
        [3.0; port6040AnnExcessReturn |> round; port6040AnnSharpe |> round]
        [4.0; portEqualWeightAnnExcessReturn |> round; portEqualWeightAnnSharpe |> round]
    ]
Chart.Table(headers, rows)
Let's take a look at the same performance analysis table, but for our four portfolios noramlized to 10% volatility.
let portMveLong10AnnExcessReturn = portLongMve10 |> List.map (fun x -> x.Return) |> annualizedExcessReturn
let portMveLongShort10AnnExcessReturn = portLongShortMve10 |> List.map (fun x -> x.Return) |> annualizedExcessReturn
let port604010AnnExcessReturn = port604010 |> List.map (fun x -> x.Return) |> annualizedExcessReturn
let portEqualWeight10AnnExcessReturn = portEqualWeight10 |> List.map (fun x -> x.Return) |> annualizedExcessReturn

let portMveLong10AnnSharpe = portLongMve10 |> List.map (fun x -> x.Return) |> annualizedSharpe
let portMveLongShort10AnnSharpe = portLongShortMve10 |> List.map (fun x -> x.Return) |> annualizedSharpe
let port604010AnnSharpe = port604010 |> List.map (fun x -> x.Return) |> annualizedSharpe
let portEqualWeight10AnnSharpe = portEqualWeight10 |> List.map (fun x -> x.Return) |> annualizedSharpe

let headers = ["Portfolio"; "Avg. Ann. Excess Return"; "Ann. Sharpe Ratio"]
let rows = 
    [
        [1.0; portMveLong10AnnExcessReturn |> round; portMveLong10AnnSharpe |> round]
        [2.0; portMveLongShort10AnnExcessReturn |> round; portMveLongShort10AnnSharpe |> round]
        [3.0; port604010AnnExcessReturn |> round; port604010AnnSharpe |> round]
        [4.0; portEqualWeight10AnnExcessReturn |> round; portEqualWeight10AnnSharpe |> round]
    ]

Chart.Table(headers, rows)