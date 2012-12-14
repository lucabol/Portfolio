(**
% LPort: looking at portfolios and trades as processes
% Luca Bolognese
% 22/11/2012
**)

(**
Main Ideas
==========

A portfolio is just a snapshot of a process in motion the application of a set of trades to an initial monetary value.
You can add or subtract a trade to a portfolio, producing a new portfolio. You can add and subtract two portfolios,
which just means applying the operation to each trade in the second portfolio to the first.
**)

#if INTERACTIVE
#else
module LPort
#endif

open System

let declare<'a>  = ref Unchecked.defaultof<'a>

[<Measure>]
type mo

[<Measure>]
type sh

type Ticker = string

type Trade =    // time,      ticker,  shares,     price,      commission  
| Buy           of DateTime * Ticker * float<sh> * float<mo> * float<mo> 
| Sell          of DateTime * Ticker * float<sh> * float<mo> * float<mo>
//                 time,      shares,     price
| Deposit       of DateTime * float<sh> * float<mo>
| Withdrawl     of DateTime * float<sh> * float<mo>     

type Position = Position of string * float<sh>

type Portfolio = seq<Position>

(**
Cash is managed by identifying a ticker as the 'cash' ticker. The price of such ticker needs to be provided
for the sake of knowing how many shares of cash to sell/buy to pay for the trade.
This is achieved by a Pricer, which is a function that gives you the price of a ticker at a particular DateTime. 
**)

type Pricer = Ticker -> DateTime -> float<mo>

let emptyPort: Portfolio = Seq.empty<Position>

//                              cash,
let addTrade = declare<Trade -> Ticker -> Pricer -> Portfolio -> Portfolio>

(**
You create a test portfolio by writing the below code
**)

let today                  = DateTime.Today
let tPricer ticker time    = 1.<mo>

let testPort () =
    emptyPort
    |> !addTrade (Deposit(  today,         100000.<sh>,1.<mo>))             "CASH" tPricer
    |> !addTrade (Buy(      today, "MSFT", 300.<sh>,   25.<mo>, 2.5<mo>))   "CASH" tPricer
    |> !addTrade (Buy(      today, "ORCL", 300.<sh>,   15.<mo>, 2.5<mo>))   "CASH" tPricer

(**
A Pricer is also needed to calc the total value of a portfolio.
**)

let calcPortValue = declare<Pricer -> DateTime -> Portfolio -> float<mo>>

(**
Implementation
==============

Now we can go and define the various functions.

First we add a trade.

The cases are:

* A ticker doesn't exist in the portfolio -> we insert it with the number of shares
* A ticker does exist in the portfolio    -> we add/removes the shares to the existing ones

We need to do the above for both the ticker in the trade and cash, but invert the sign for cash.

We don't consider the cash going negative as an issue (assumes infinite free borrowing)

TODO: addOrReplace could be done with one pass by using fold or mutable var (but it will be less readable)
**)

let addOrReplace test change el l =
    match Seq.exists test l with
    | false     -> Seq.append (Seq.singleton el) l
    | true      -> l |> Seq.map (fun e -> if test e then change e else e)

let getDate = function
    | Buy(date, _, _, _, _)     -> date
    | Sell(date, _, _, _, _)    -> date
    | Deposit(date, _, _)       -> date
    | Withdrawl(date, _, _)     -> date

addTrade := fun trade cash pricer port ->
    let test tick (Position(t, _))  = t = tick
    let change s1 (Position(t, s))  = Position(t, s + s1)

    let processPort op date ticker shares price comm =
        let cashShares = shares * price / (pricer cash date) + (op (comm * 1.<sh / mo>))
        port
        |> addOrReplace (test ticker) (change (op shares)) (Position(ticker, (op shares)))
        |> addOrReplace (test cash) (change (- (op cashShares))) (Position(cash, - (op cashShares))) 

    let port' =
        match trade with
        | Buy(date, ticker, shares, price, comm)    -> processPort (~+) date ticker shares price comm 
        | Sell(date, ticker, shares, price, comm)   -> processPort (~-) date ticker shares price comm
        | Deposit(date,   shares, price)            ->
            port |> addOrReplace (test cash) (change ((~+) shares)) (Position(cash, ((~+) shares)))
        | Withdrawl(date, shares, price)            -> 
            port |> addOrReplace (test cash) (change ((~-) shares)) (Position(cash, ((~-) shares)))

    // remove zeros positions
    port' |> Seq.filter (fun (Position(_, s)) -> s <> 0.<sh>)

let testAddTrade () =
    let data = [
        [Deposit(today, 10000.<sh>, 1.<mo>)], [Position("CASH", 10000.<sh>)]
        [Deposit(today, 10000.<sh>, 1.<mo>); Deposit(today, 10000.<sh>, 1.<mo>)],
            [Position("CASH", 20000.<sh>)]
        [Deposit(today, 10000.<sh>, 1.<mo>); Buy(today, "MSFT", 100.<sh>, 2.<mo>, 4.<mo>)],
            [Position("MSFT", 100.<sh>); Position("CASH", (10000.<sh> - 100.<sh> * 2. - 4.<sh>))]
        [Deposit(today, 10000.<sh>, 1.<mo>); Sell(today, "MSFT", 100.<sh>, 2.<mo>, 4.<mo>)],
            [Position("MSFT", - 100.<sh>); Position("CASH", (10000.<sh> + 100.<sh> * 2. - 4.<sh>))]
        [Deposit(today, 10000.<sh>, 1.<mo>); Buy(today, "MSFT", 100.<sh>, 2.<mo>, 4.<mo>);
            Sell(today, "MSFT", 50.<sh>, 2.<mo>, 4.<mo>);],
            [Position("MSFT", 50.<sh>); Position("CASH", (10000.<sh> - 50.<sh> * 2. - 8.<sh>))]
        [Deposit(today, 10000.<sh>, 1.<mo>); Buy(today, "MSFT", 50.<sh>, 2.<mo>, 4.<mo>);
            Buy(today, "ORCL", 50.<sh>, 2.<mo>, 4.<mo>);],
            [Position("MSFT", 50.<sh>); Position("ORCL", 50.<sh>);
             Position("CASH", (10000.<sh> - 100.<sh> * 2. - 8.<sh>))]
        [Deposit(today, 10000.<sh>, 1.<mo>); Buy(today, "MSFT", 100.<sh>, 2.<mo>, 4.<mo>);
            Sell(today, "MSFT", 100.<sh>, 2.<mo>, 4.<mo>);],
            [Position("CASH", (10000.<sh> - 8.<sh>))]
    ]
    let construct = List.fold (fun st tr -> st |> !addTrade tr "CASH" tPricer) emptyPort
    data |> List.forall (fun (l1, p1) ->
        let p0 = construct l1
        (Set.ofSeq p0) = Set.ofList p1)

assert(testAddTrade())

(**
Calculating the value of a portfolio is just a simple multiplication
**)

calcPortValue := fun pricer date port ->
    port |> Seq.map (fun (Position(t, sh)) ->
                        let price   = pricer t date 
                        price * sh)
        |> Seq.fold (+) 0.<mo>