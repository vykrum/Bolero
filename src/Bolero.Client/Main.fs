module Bolero.Client.Main
open Clusters
open Elmish
open Bolero
open Bolero.Html
open Bolero.Templating.Client
open Microsoft.FSharp.Collections


//let dd = Hexel.cls 15 [] (5.0,5.0)
//let ff = Hexel.mcs 10 dd |> List.concat
let cv l m = (Hexel.cls l [] (10.0,5.0)) |> Hexel.mcs m |> List.concat

let x1 lst = List.map (fun x -> fst x)lst |> List.map (fun x -> x*20.0)|> List.map (fun x -> Attr("cx", x))
let y1 lst = List.map (fun y -> snd y)lst |> List.map (fun y -> y*20.0) |> List.map (fun y -> Attr("cy", y))

let cr a b = Elt("circle", [a;b;Attr("r",10);Attr("stroke","green");Attr("fill","yellow")],[])
//let fg = List.map2 (fun x y -> cr x y) (x1 ff) (y1 ff)
//let r = svg[Attr("width", 500);Attr("height", 500)] fg
 
let vg (l,m) = svg[Attr("width", 500);Attr("height", 200)] (List.map2 (fun x y -> cr x y) (x1 (cv l m)) (y1 (cv l m)))

type Model = { 
    cll: string
    hst: string
    }
let initModel = { 
    cll = "5"
    hst = "10"
    }

type Message = SetCll of string| SetHst of string

let update message model =
    match message with
    | SetCll n -> { model with cll = n }
    | SetHst n -> { model with hst = n }

let viewInput model setValue =
    input [
        attr.value model
        on.change (fun e -> setValue (unbox e.Value))
    ]

let view model dispatch=
    div [Attr("width",300)] [
        div[Attr("style","background-color:lightblue")][
            text (" Sub-Cluster Count : ")
            viewInput model.cll (fun n -> dispatch (SetCll n))
            ]
        div[Attr("style","background-color:lightblue")][
            text (" Host Cluster Count: ")
            viewInput model.hst (fun n -> dispatch (SetHst n))
            ]
        div[Attr("style","background-color:darkblue")][
            vg ((model.hst |> int),(model.cll |> int))
            ]
        div[Attr("style","background-color:lightblue")][
            text (sprintf " SCC: %s HCS:  %s" (model.cll) (model.hst))
            ]
        ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
#if DEBUG
        |> Program.withHotReload
#endif
