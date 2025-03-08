[![Binder](../img/badge-binder.svg)](https://mybinder.org/v2/gh/nhirschey/teaching/gh-pages?filepath=project/signal-exploration.ipynb)&emsp;
[![Script](../img/badge-script.svg)](/Teaching//project/signal-exploration.fsx)&emsp;
[![Notebook](../img/badge-notebook.svg)](/Teaching//project/signal-exploration.ipynb)

**Student Name: Francisco Perestrello**

**Student Number: 39001**

**Signal Name (e.g., Book to Market): Highest 5 days of return**

**Signal Code (e.g., be_me): rmax5_21d**

This is the mid-term project. It is an individual project. You may use course resources and ask for help on general programming questions, but you may not give or ask for solutions to these specific assigned tasks. You must solve these tasks on your own, and all work that you submit should be your own.

This is an exercise in exploratory data analysis, using your assigned trading signal as the data. There is a written component called **Signal Background** and a data analysis component called **Signal Analysis**. The written component will be graded based on the completeness, accuracy, and clarity of the writing. The data analysis component will be graded based on the completeness, accuracy, and clarity of the code (small, easy to read functions are better). Partial credit may be awarded for partially correct answers.

## Signal Background

The signal I chose was the **Highest 5 Days of Return – rmax5_21d**. As its name mentions, it is constructed in a rather simple way. Each month, the highest 5 days of return are selected and an average is computed, giving us our signal.

To explain the economic motivation for why this may or may not be a useful predictor of security returns, I will take a look at *T. I. Jensen, B. Kelly, and L. H. Pedersen (2021). Is There a Replication Crisis in Finance?*.

In this paper, the authors observe at a set of economic factors, which they group into clusters based on hierarchical agglomerative clustering (Murtagh and Legendre, 2014). They compute the correlation between the factors based on the CAPM-residual returns of the factors. This algorithm assigned my signal – Highest 5 days of return – to a theme cluster called Low Risk.

The authors looked at the replication rate for US factors when those factors are constructed from subsamples based on stock size, and we see that my signal’s theme cluster is replicable with a rate of around 10%. Additionally, if we distinguish the replication rate across different regions, such as the USA, Developed regions and Emerging regions, we see that my signal keeps this 100% replication rate in both the USA and in the Developed markets, but it does drop to somewhere between 75% and 100% in Emerging markets. An analysis on the replication rate for different stock sizes was also conducted by the authors, and they conclude that my signal’s theme cluster has a replication rate of 100% in all stock sizes except nano stocks, for which it drops to around 75%.

To find out which factors are the most impactful anomalies in economic terms, the authors looked at which factors matter most from an investment performance standpoint.  We see that the highest 5 days of return has a factor alfa of around 0.5% per month, and is highly significant. This could contribute to increase this signal’s usefulness in working as a predictor of security returns.

The authors then evaluate which factor themes are most economically important across global regions and across stock size groups, taking the equal-weighted average alpha of factors within each cluster. Here, I was able to see that my signal’s Theme Cluster shows great results, with Low Risk showing great importance in all size groups except Nano stocks, for which Low Risk seems to lose a bit of importance. Then, when evaluating by regions, I see that this Theme Cluster is again amongst the most important clusters in both Developed and Emerging makers, as well as in the United States.

As the CAPM alpha does not control for duplicate behavior other than through the market factor, the authors tried to understand how clusters contribute to their alpha while controlling for all other clusters. Thus, they estimated cluster weights in a tangency portfolio that invests jointly in all cluster-level portfolios. They also tested the significance of the estimated weights. A high weight in the tangency portfolio means that the cluster matters for an investor, even when controlling for all other factors. Looking at the results, however, we see that when controlling for all other clusters, my signal’s theme cluster – low risk – does seem to lose importance, falling among the bottom 6 themes. If we were to discriminate between regions, however, we see that Low-Risk is among the top weights in the tangency portfolio for the USA, while being amongst the bottom in both the rest of the Developed Regions as well as in Emerging Markets, both with weights around 0%. If this discrimination was made through stock sizes, we see that Low Risk has one of the largest weights in Mega stocks, it loses a bit of weight for Large stocks, and becomes rather irrelevant for both Small, Micro and Nano stocks.

To conclude, I believe that, based on economic motivation, my signal – Highest 5 days of return – could work as a somewhat fine predictor of security returns. The paper analyzed exposed some interesting statistics about my signal, which contribute for its usefulness as a predictor. However, when controlling for duplicate behavior, they exposed some limitations on my signal. Thus, I believe it could work as a useful predictor, but only under the right circumstances, such as when analyzing Mega or Large stocks in the USA, for example.


## Signal Analysis 

This section involves analysis of your signal. I guide you through a series of programming tasks to complete. You will find sections labeled **Task** asking you to do each piece of analysis. Please make sure that you complete all of these tasks. Make use of the course resources and example code on the course website. It should be possible to complete all the requested tasks using information given below or somewhere on the course website.

Some tasks ask for a written response. You may write your response to the written question in the same cell that that question is asked in. **Please do not** delete the task question text. We need it to locate your answers to specific tasks when grading.

Load libraries.


#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET,2.0.0-preview.17"
#r "nuget: Plotly.NET.Interactive,2.0.0-preview.17"

open System
open FSharp.Data
open FSharp.Stats
open Plotly.NET

// Set dotnet interactive formatter to plaintext
Formatter.Register(fun (x:obj) (writer: TextWriter) -> fprintfn writer "%120A" x )
Formatter.SetPreferredMimeTypesFor(typeof<obj>, "text/plain")
// Make plotly graphs work with interactive plaintext formatter
Formatter.SetPreferredMimeTypesFor(typeof<GenericChart.GenericChart>,"text/html")

### First, make sure that you're referencing the correct files.

Here I'm assuming that you have a class folder with this `signal-exploration.ipynb` notebook and a `data` folder inside of it. The folder hierarchy would look like below where you
have the below files and folders accessible:

```code
/class
    signal-exploration.ipynb
    /data
        id_and_return_data.csv
        zero_trades_252d.csv
    
```

First, make sure that our working directory is the source file directory.


let [<Literal>] ResolutionFolder = __SOURCE_DIRECTORY__
Environment.CurrentDirectory <- ResolutionFolder

We assume the `id_and_return_data.csv` file and the signal csv file  are in the `data` folder. In this example the signal file is `zero_trades_252d.csv`. You should replace that file name with your signal file name.


let [<Literal>] IdAndReturnsFilePath = "data/id_and_return_data.csv"
let [<Literal>] MySignalFilePath = "data/rmax5_21d.csv"

If my paths are correct, then this code should read the first few lines of the files.
If it doesn't show the first few lines, fix the above file paths.


IO.File.ReadLines(IdAndReturnsFilePath) |> Seq.truncate 5

IO.File.ReadLines(MySignalFilePath) |> Seq.truncate 5

Assuming the paths are defined correctly and you saw the first 5 rows above,
we can now read the data using the CSV provider that parses the fields in the file.

First define the Csv types from the sample files:


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

Now read in the data.


let idAndReturnsCsv = IdAndReturnsType.GetSample()

let mySignalCsv = MySignalType.GetSample()
    

Columns in the `idAndReturnsCsv` are:


idAndReturnsCsv.Headers

Columns in the `mySignalCsv` are:


mySignalCsv.Headers

There are a lot of columns in the id and returns csv. You can look at the data documentation to figure out what they are.

Put the rows into a list (we're more familiar with lists).


let idAndReturnsRows = idAndReturnsCsv.Rows |> Seq.toList
let mySignalRows = mySignalCsv.Rows |> Seq.toList

### Distribution of unique stocks in the id and returns data

To get you started, I will walk you through some simple analysis of the id and returns data.

Count the total number of stocks.

First, look at a few ids


idAndReturnsRows
|> List.map (fun row -> row.Id)
|> List.truncate 5

Now count all of them.


idAndReturnsRows
|> List.map (fun row -> row.Id)
|> List.distinct
|> List.length

Number of stocks each month.

First look at the date column


idAndReturnsRows
|> List.map (fun row -> row.Eom)
|> List.truncate 5

Group by month, then count per month.


let idAndReturnStocksPerMonth =
    let byMonth =
        idAndReturnsRows
        |> List.groupBy (fun row -> row.Eom)
        |> List.sortBy (fun (month, rows) -> month)
    [ for (month, rows) in byMonth do
        let nStocks = 
            rows
            |> List.map (fun row -> row.Id)
            |> List.distinct
            |> List.length
        month, nStocks ]

Look at a first few months.


idAndReturnStocksPerMonth
|> List.sortBy (fun (month, nStocks) -> month) 
|> List.truncate 5

Look at the last few.


idAndReturnStocksPerMonth
|> List.sortByDescending (fun (month, nStocks) -> month)
|> List.truncate 5

Create a column chart showing the number of stocks per month (Plotly.net column chart [docs](https://plotly.net/02_1_bar-and-column-charts.html)).


idAndReturnStocksPerMonth
|> Chart.Column

Add some lables to the axes (Plotly.net axis styling [docs](https://plotly.net/01_0_axis-styling.html)).


idAndReturnStocksPerMonth
|> List.sortBy (fun (month, nStocks) -> month)
|> Chart.Column
|> Chart.withXAxisStyle (TitleText="Month")
|> Chart.withYAxisStyle (TitleText="Number of Stocks")

We have some different size groups already assigned in the data:


idAndReturnsRows
|> List.countBy (fun row -> row.SizeGrp)

Let's make a plot with separate bars for each group in 2015. You can read more about multiple charts in the Plotly.net [docs](https://plotly.net/01_2_multiple-charts.html).

We'll write a function. We need to give a type hint so that
it knows the type of the input data. If we didn't include the type hint, we'd get an error saying 'Lookup of indeterminate type ..' because it doesn't know the data type of the 'rows' input. The type hint the  `: list<IdAndReturnsType.Row>` part of the function definition.
This is saying we have a list of rows from the CsvProvider type that we defined earlier for this csv file data.


let countIdAndReturnsRows (rows: list<IdAndReturnsType.Row>) =
    let byMonth =
        rows
        |> List.groupBy (fun row -> row.Eom)
        |> List.sortBy (fun (month, rows) -> month)
    [ for (month, rows) in byMonth do
        let nStocks = 
            rows
            |> List.map (fun row -> row.Id)
            |> List.distinct
            |> List.length
        month, nStocks ]

Look at the function output. It is a list of tuples where each tuple is a pair of month (`DateTime`) and the count (`int`).


idAndReturnsRows
|> countIdAndReturnsRows
|> List.truncate 3

Just for large caps.


let stockCountsLarge =
    let toPlot = 
        idAndReturnsRows
        |> List.filter (fun row -> 
            row.SizeGrp = "large" && 
            row.Eom.Year = 2015)
        |> countIdAndReturnsRows
    Chart.Column(toPlot, Name = "Large caps")

stockCountsLarge

Just for small caps.


let stockCountsSmall =
    let toPlot = 
        idAndReturnsRows
        |> List.filter (fun row -> 
            row.SizeGrp = "small" &&
            row.Eom.Year = 2015)
        |> countIdAndReturnsRows
    Chart.Column(toPlot, Name = "Small caps")

stockCountsSmall

combined:


[ stockCountsLarge; stockCountsSmall ]
|> Chart.combine

Now all groups


let stockCountsAllSizes =
    idAndReturnsRows
    |> List.filter (fun row -> row.Eom.Year = 2015)
    |> List.groupBy (fun row -> row.SizeGrp)
    |> List.map (fun (sizeGrp, rows) -> 
        let toPlot = countIdAndReturnsRows rows
        sizeGrp, toPlot)

// first few observations of all size Groups
stockCountsAllSizes
|> List.map (fun (sizeGroup, xs) ->
    sizeGroup, xs |> List.truncate 3)

A combined chart.


stockCountsAllSizes
|> List.map (fun (sizeGrp, toPlot) -> 
    Chart.Column(toPlot, Name = sizeGrp))
|> Chart.combine

(** Same, but stacking each chart on top of eachother. *)   

stockCountsAllSizes
|> List.map (fun (sizeGrp, toPlot) -> 
    Chart.Column(toPlot, Name = sizeGrp))
|> Chart.SingleStack()

You should now have some a good idea of how to work with this data.

### Distribution of unique stocks in the your signal data

Do similar analysis as above, but for the your signal data.

> **Task:** Complete this function. It takes a list of `MySignalType.Row` as input and should return a list of the month and the integer count of unique stock ids that month (`list<DateTime * int>`).
> 


// solution here
let countMySignalRows (rows: list<MySignalType.Row>) =
    let byMonth =
        rows
        |> List.groupBy (fun row -> row.Eom)
        |> List.sortBy (fun (month, rows) -> month)
    [ for (month, rows) in byMonth do
        let nStocks =
            rows
            |> List.map (fun row -> row.Id)
            |> List.distinct
            |> List.length
        month, nStocks]

> **Task:** Create a column chart showing the number of stocks per month in your signal data csv file.
> 


// solution here

// First, group by month, then count per month.
let mySignalStocksPerMonth =
    let byMonth =
        mySignalRows
        |> List.groupBy (fun row -> row.Eom.Month) //mudei aqui de row.Eom para row.Eom.Month
        |> List.sortBy (fun (month, rows) -> month)
    [ for (month, rows) in byMonth do
        let nStocks = 
            rows
            |> List.map (fun row -> row.Id)
            |> List.distinct
            |> List.length
        month, nStocks ]

// Then, create the column chart and plot it
mySignalStocksPerMonth
|> Chart.Column
You may have some stocks with missing data. If you have some stocks with missing signal data, the below code will return the first 3 observations.
If you do not have missing data it will return an empty list.


mySignalRows
|> List.choose (fun row -> 
    // Choose the rows where row.Signal is None.
    match row.Signal with
    | None -> Some row
    | Some signal -> None )
|> List.truncate 3

We can create a list that only contains stocks with non-missing signals. We define a record type to hold this data. The main change is making signal have `float` type instead of `Option<float>` because we're removing missing data.


type NonMissingSignal =
    {
        Id: string
        Eom: DateTime
        Signal: float
    }

let myNonMissingSignals =
    mySignalRows
    |> List.choose (fun row -> 
        match row.Signal with
        | None -> None
        | Some signal -> 
            Some { Id = row.Id; Eom = row.Eom; Signal = signal })

> **Task:** Complete this function. It takes a list of `NonMissingSignal` records as input and should return a list of the month and the integer count of unique stock ids that month (`list<DateTime * int>`).
> 


// solution here
let countMyNonMissingSignalRows (rows: list<NonMissingSignal>) =
    let byMonth =
        rows
        |> List.groupBy (fun row -> row.Eom)
        |> List.sortBy (fun (month, rows) -> month)
    [ for (month, rows) in byMonth do
        let nStocks =
            rows
            |> List.map (fun row -> row.Id)
            |> List.distinct
            |> List.length
        month, nStocks]
> **Task:** Create a column chart showing the number of stocks per month in your signal data that **do not** have missing signals.
> 


// solution here
// First, group by month, then count per month.
let mySignalNonMissingStocksPerMonth =
    let byMonth =
        myNonMissingSignals
        |> List.groupBy (fun row -> row.Eom.Month) // Fiz aqui uma alteração de row.Eom para row.Eom.Month
        |> List.sortBy (fun (month, rows) -> month)
    [ for (month, rows) in byMonth do
        let nStocks = 
            rows
            |> List.map (fun row -> row.Id)
            |> List.distinct
            |> List.length
        month, nStocks ]

// Then, create the column chart and plot it
mySignalNonMissingStocksPerMonth
|> Chart.Column
> **Task:** Create a column chart showing the number of stocks per month in your signal data that **do** have missing signals.
> 


// solution here - Confirmar
let mySignalMissingStocksPerMonth =
    let byMonth =
        mySignalRows
        |> List.groupBy (fun row -> row.Eom.Month) // Fiz aqui uma alteração de row.Eom para row.Eom.Month
        |> List.sortBy (fun (month, rows) -> month)
    [ for (month, rows) in byMonth do
        let nStocks = 
            rows
            |> List.filter (fun row -> row.Signal = None)
            |> List.map (fun row -> row.Id)
            |> List.distinct
            |> List.length
        month, nStocks ]

// Then, create the column chart and plot it
mySignalMissingStocksPerMonth
|> Chart.Column
### Distribution of the signal

> **Task:** Compute the minimum, maximum, median, standard deviation, and average of the non-missing signals in your dataset.
> 


// solution here.
// let us first create 
let mySignalValues = myNonMissingSignals |> List.map (fun row -> row.Signal)

let mySignalMin = mySignalValues |> Seq.min
let mySignalMax = mySignalValues |> Seq.max
let mySignalMedian = mySignalValues |> Seq.median
let mySignalStDev = mySignalValues |> Seq.stDevPopulation
let mySignalMean = mySignalValues |> Seq.mean
It can also be useful to compute percentiles of the signal. You can calculate percentils using `FSharp.Stats` quantile module.


// 10th, 50th, and 90th percentiles
let pctlExamples = [0.1; 0.5; 0.9]

// you must have an array of values
let pctlExamplesData = 
    [ 10.0; -20.0; 0.1; -5.0; 7.0; 4.0]
    |> List.toArray 

Compute the percentiles.


let pctlExamplesComputed =    
    [ for pctl in pctlExamples do
        Quantile.compute pctl pctlExamplesData ]
pctlExamplesComputed

> **Task:** Compute the 1st, 10th, 50th, 90th, and 99th percentiles of the non-missing signals in your dataset. Once these percentiles are calculated them, assign the signals to the values below. Explain what you learn about the distribution. Is it uniformly distributed, a skewed distribution, are there outliers, etc.?
> 


// solution here
let pctl = [0.01; 0.1; 0.5; 0.9; 0.99]

let signalP01 = Quantile.compute pctl[0] (mySignalValues |> List.toArray)
let signalP10 = Quantile.compute pctl[1] (mySignalValues |> List.toArray)
let signalP50 = Quantile.compute pctl[2] (mySignalValues |> List.toArray)
let signalP90 = Quantile.compute pctl[3] (mySignalValues |> List.toArray)
let signalP99 = Quantile.compute pctl[4] (mySignalValues |> List.toArray)

// By taking a look at the percentiles, we see that the distribution is definitely not uniformly distributed, as we see the values changing significantly from one percentile to another. However, the distribution does seem skewed to the left, as we can see that 90% of signals are very low - below 0.084, while the top 10% take a big jump, with the top 1% of signals being above 0.189. The presence of outliers seems true.
> **Task:** Create a [histogram](https://plotly.net/04_0_histograms.html) showing the distribution of the signal in for all stocks in your dataset that have non-missing signals. Limit the data to 2015 to make it easier to plot. Explain what you learn about the distribution. Is it uniformly distributed, are there outliers, etc. How do you see this in the plot, and is there anything new that you learned relative to the percentiles?
> 


// solution here.
let mySignalHistogram =
    let toPlot =
        myNonMissingSignals
        |> List.filter (fun row ->
            row.Eom.Year = 2015)
        |> List.map (fun row -> row.Signal)
        |> List.toArray
    Chart.Histogram(toPlot)

mySignalHistogram

// The plot seems to confirm our previous suspicions - the distribution is not uniformly distributed, it is skewed to the left. Additionally, we can confirm the presence of a few outliers. We see this in the plot as the vast majority of signals are concentrated around a bit above zero. The plot gives us the information that most signals are relatively low, and large signals are rare.
[Winsorizing](https://en.wikipedia.org/wiki/Winsorizing) is a technique to remove the influence of outliers from a dataset. Let's create a winsorized version of your data.

Assuming that you have defined the percentile above correctly, this will create a winsorized version of your signal dataset. It is winsorized at the 1st and 99th percentiles.


let winsorizeSignals (signalOb: NonMissingSignal) =
    let newSignal =
        if signalOb.Signal < signalP01 then 
            signalP01
        elif signalOb.Signal > signalP99 then
            signalP99
        else
            signalOb.Signal
    // copy and update the observation with the
    // winsorized signal.
    { signalOb with Signal = newSignal }

Test on a random signal


winsorizeSignals myNonMissingSignals[99]

do for all


let myWinsorizedSignals =
    myNonMissingSignals
    |> List.map winsorizeSignals

> **Task:** Create a [histogram](https://plotly.net/04_0_histograms.html) showing the distribution of the **winsorized signals** for all stocks in your dataset. Limit the data to 2015 to make it easier to plot. Explain what you learn about the distribution. Is it uniformly distributed, are there outliers, etc. How do you see this in the plot, and is there anything new that you learned relative to the percentiles and non-winsorized histogram?
> 


// solution here.
let myWinsorizedHistogram =
    let toPlot =
        myWinsorizedSignals
        |> List.filter (fun row ->
            row.Eom.Year = 2015)
        |> List.map (fun row -> row.Signal)
        |> List.toArray
    Chart.Histogram(toPlot)

myWinsorizedHistogram

// The winsorized distribution helped eliminate the presence of outliers. Now, we see a much more concise distribution. As expected, it is still skewed to the left, and the mean is still a bit above zero. However, there are no longer outliers present in the distribution.
> **Task:** Create a map collection called `byStockMonthIdAndReturnMap` where the key is a tuple of stock id as string and month as DateTime (`string * DateTime`) and the value is an `IdAndReturnsType.Row`.
> 

**Note:** I have added a type constraint of `: Map<(string * DateTime), IdAndReturnsType.Row>` to make sure that the type of the map is correct. If you fill in code below, you will get a type mismatch error until your code is correct. You don't generally need these type constraints, but I am putting it here to make the compiler check that you produce the output that I am asking for.

**Hint:** we did things like this in the momentum signal lecture. There's also a practice quiz on map collections.


// solution here
let byStockMonthIdAndReturnMap: Map<string * DateTime, IdAndReturnsType.Row> =
    idAndReturnsRows
    |> List.map(fun x ->
        let ym = DateTime(x.Eom.Year, x.Eom.Month, x.Eom.Day) 
        let key = x.Id, ym
        key, x)
    |> Map
> **Task:** Create a [histogram](https://plotly.net/04_0_histograms.html) showing the distribution of the **winsorized signals** for only **small-cap stocks** in your dataset. Limit the data to 2015 to make it easier to plot.
> 

**Hint:** if you have a stock and it's signal in a particular month, the `byStockMonthIdAndReturnMap` is useful for looking up thinks about the stock that month.)


// solution here
let smallWinsorizedHistogram =
    let toPlot =
        myWinsorizedSignals
        |> List.filter (fun x -> x.Eom.Year = 2015)
        |> List.filter (fun x -> byStockMonthIdAndReturnMap[x.Id, x.Eom].SizeGrp = "small")
        |> List.map (fun x -> x.Signal)
    Chart.Histogram(toPlot)

smallWinsorizedHistogram
> **Task:** Create a [histogram](https://plotly.net/04_0_histograms.html) showing the distribution of the **winsorized signals** for only **large-cap stocks** in your dataset. Limit the data to 2015 to make it easier to plot.
> 


// solution here
let largeWinsorizedHistogram =
    let toPlot =
        myWinsorizedSignals
        |> List.filter (fun x -> x.Eom.Year = 2015)
        |> List.filter (fun x -> byStockMonthIdAndReturnMap[x.Id, x.Eom].SizeGrp = "large")
        |> List.map (fun x -> x.Signal)
    Chart.Histogram(toPlot)

largeWinsorizedHistogram
> **Task:** Compare and contrast the histograms for the **small-cap** and **large-cap** stocks. Are there any differences? If we wanted to sort stocks based on the signal, do you think that we would end up with stocks that have different average sizes in the low and high signal portfolios?
> 
// solution here
// By comparing the histograms for the small-cap and large-cap stocks, we see that they have very similar distributions. Both are skewed to the left and seem to be concentrated around the same signal value. There seem to be more observations with these values in the histogram of the small-cap stocks, but the difference is rather negligable. These histograms suggest that if we were to sort stocks based on the signal, we would most likely end up with stocks that have different average sizes in the low and high signal portfolios.
### Towards portfolios.

> **Task:** Using your winsorized list of signals, group your stocks by month. Assign this result to a value named `byStockMonthSignals` that is a list of `DateTime * list<NonMissingSignal>` tuples. The first thing in the tuple is the month and the second thing is a list of `NonMissingSignal` records for all stocks in that month.
> 


// solution here
let byStockMonthSignals: list<DateTime * list<NonMissingSignal>> =
    myWinsorizedSignals
    |> List.groupBy (fun x -> x.Eom)

Now assuming `byStockMonthSignals` is correct, we'll sort the stocks each month from smallest to largest based on the signal that month. Then split the stocks into 3 equal-sized portfolios (aka terciles) based on the sorted signal. We'll create a `SortedPort` record for each portfolio and assign the list to a value named `terciles`.


type SortedPort =
    { Portfolio: int
      Eom: DateTime
      Stocks: list<NonMissingSignal> }

let terciles: list<SortedPort> =
    byStockMonthSignals
    |> List.collect (fun (eom, signals) ->
        let sortedSignals =
            signals
            |> List.sortBy (fun signalOb -> signalOb.Signal)
            |> List.splitInto 3
        sortedSignals
        |> List.mapi (fun i p -> 
            { Portfolio = i + 1
              Eom = eom
              Stocks = p }))

look at the first portfolio


terciles[0]

look at the last portfolio


terciles |> List.last

> **Task:** Using `terciles`, compute the average signal in each tercile portfolio each month. Plot a combined (`Chart.combine`) line chart (`Chart.line`) showing the average signal for each tercile portfolio from the start to the end of the sample. What do you learn? Is the average signal in each tercile constant throughout the sample, or does it vary over time?
> 


// solution here
let returnAverageTercile (rows: list<SortedPort>) =
    let byPortfolioMonth =
        rows
        |> List.groupBy (fun x -> (x.Eom))
        |> List.sortBy (fun (month, row) -> month)
    [for (month, row) in byPortfolioMonth do
        let average = 
            row
            |> List.collect (fun x -> x.Stocks)
            |> List.map (fun x -> x.Signal)
            |> Seq.mean
        month, average]

let averageTercile =
    terciles
    |> List.groupBy (fun x -> x.Portfolio)
    |> List.map (fun (portfolio,x) ->
        let toPlot = returnAverageTercile x
        portfolio, toPlot)

averageTercile
|> List.map (fun (portfolio, toPlot) -> Chart.Line(toPlot, Name = portfolio.ToString()))
|> Chart.combine

// The average signal for each tercile is definitely not constant over time. We see that they all constantly spike, and all three terciles' average signal spike during all known crisis - 2008, 2012, 2021.
> **Task:** Using `byStockMonthSignals`, sort the stocks each month from smallest to largest based on the signal that month. Then split the stocks into 5 equal-sized portfolios (aka quintiles) based on the sorted signal. Create a `SortedPort` record for each portfolio and assign the list to a value named `quintiles`.
> 
// solution here
type SortedPort =
    { Portfolio: int
      Eom: DateTime
      Stocks: list<NonMissingSignal> }

let quintiles: list<SortedPort> =
    byStockMonthSignals
    |> List.collect (fun (eom, signals) ->
        let sortedSignals =
            signals
            |> List.sortBy (fun signalOb -> signalOb.Signal)
            |> List.splitInto 5
        sortedSignals
        |> List.mapi (fun i p -> 
            { Portfolio = i + 1
              Eom = eom
              Stocks = p }))

> **Task:** Filter `quintiles` to the quintile portfolio of stocks each month that has the lowest signal value. This should be stocks where `SortedPort.Portfolio = 1`. Assign the filtered list to a value named `bottomQuintile`.
> 


// solution here 
let bottomQuintile: list<SortedPort> =
    quintiles
    |> List.filter (fun row -> row.Portfolio = 1)

> **Task:** Create a list named `bottomQuintileReturn` that contains the return of the bottom quintile portfolio each month. The portfolio return for a given month should be calculated using equal weights on every stock in the portfolio that month. The result should be given as a list of `SortedPortfolioReturn` records. **Additionally,** the month of the return should be lagged one month relative to the portfolio formation month. That means that if you formed a portfolio based on a signal known as of the end of February 2022 (Eom = DateTime(2022,02,28)), the portfolio return during the first month that you hold it will be calculated using stock returns during March 2022 (MonthOfReturn = DateTime(2022,03,31)).
> 

Quick example getting end of month additon:


let endOfFebruary = DateTime(2022,02,28)

let addOneEom (eom: DateTime) =
    DateTime(eom.Year, eom.Month, 1).AddMonths(2).AddDays(-1.0)

addOneEom endOfFebruary

That will give you the end of March. So in summary, if the signal that you use to form portfolios comes from February 2022 (signal EOM = DateTime(2022,2,28)), make sure that you get returns from March 2022 (return EOM = DateTime(2022,3,31)).


// solution here
type SortedPortfolioReturn =
    { 
        Portfolio: int
        MonthOfReturn: DateTime
        AvgReturn: float
    }

let bottomQuintileReturn: list<SortedPortfolioReturn> =
    let missingReturn = 0.0
    [for port in bottomQuintile do
        let date = port.Eom
        let stocks =
            port.Stocks
            |> List.map (fun x -> x.Id)
            
        let AvgReturn =
            [for stock in stocks do
                match Map.tryFind (stock, addOneEom date) byStockMonthIdAndReturnMap with
                | None -> missingReturn
                | Some x ->
                    match x.Ret with
                        | None -> missingReturn
                        | Some r -> r]
            |> List.average
        {Portfolio = port.Portfolio
         MonthOfReturn = port.Eom
         AvgReturn = AvgReturn}]
> **Task:** Plot a line chart of the cumulative return of the bottom quintile portfolio during the sample. For reference you will find the [plotting returns](https://nhirschey.github.io/Teaching/Momentum-Class.html#Plotting-returns) section of the momentum class lecture useful. It provides an example of calculating a portfolio's cumulative returns using `List.scan`.
> 


let sortedBottomQuintileReturn =
    bottomQuintileReturn
    |> List.sortBy (fun x -> x.MonthOfReturn)

let logReturns =
    [for month in sortedBottomQuintileReturn do
        {month with AvgReturn = log(1.0+month.AvgReturn)}]

let cumulativeReturns =
    let h::t = logReturns
    (h,t)
    ||> List.scan (fun priorMonth thisMonth ->
        {thisMonth with AvgReturn = thisMonth.AvgReturn + priorMonth.AvgReturn})

[for month in cumulativeReturns do
    month.MonthOfReturn, month.AvgReturn]
|> Chart.Line

