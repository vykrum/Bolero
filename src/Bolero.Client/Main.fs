module Bolero.Client.Main
open Clusters
open Elmish
open Bolero
open Bolero.Html
open Bolero.Templating.Client
open Microsoft.FSharp.Collections

//let dd = Hexel.cls 15 [] (5.0,5.0)
//let ff = Hexel.mcs 10 dd |> List.concat
//let cv l m = (Hexel.cls l [] (15,15)) |> Hexel.mcs m |> List.concat
//let cv l m = (Hexel.cls l (Hexel.cls m [] (20,20)) (15,15))

let cv l m =
    (Hexel.cl m [] ((Hexel.cl l [] [(20,20)])
    |>List.concat))

//Position
let x1 lst = 
    List.map (fun x -> fst x)lst 
    |> List.map (fun x -> x*10)
    |> List.map (fun x -> Attr("cx", x))

let y1 lst = 
    List.map (fun y -> snd y)lst 
    |> List.map (fun y -> y*10) 
    |> List.map (fun y -> Attr("cy", y))

//Geometry
let cr a b = 
    Elt("circle",
        [a;b;Attr("r",10);
        Attr("stroke","green");
        Attr("fill","yellow")],
        [])

let cl a b = 
    Elt("circle", [a;b;Attr("r",10);
    Attr("stroke","yellow");
    Attr("fill","green")],[])
 
 //Render
let vg (l,m) = 
    svg[Attr("width", 500);Attr("height", 500)] 
        ([(List.map2 (fun x y -> cr x y) (x1 (cv l m).[0]) (y1 (cv l m).[0]));
        (List.map2 (fun x y -> cl x y) (x1 (cv l m).[1]) (y1 (cv l m).[1]))]
        |>List.concat)



/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home

/// The Elmish application's model.
type Model =
    {
        page: Page
        cll: string
        hst: string
    }

let initModel =
    {
        page = Home
        cll = "5"
        hst = "5"
    }
/// The Elmish application's update messages.
type Message =
    | SetPage of Page | SetCll of string| SetHst of string

/// Connects the routing system to the Elmish application.
let router = 
    Router.infer SetPage (fun model -> model.page)

type Main =
    Template<"wwwroot/index.html">

let update message model =
    match message with
    | SetPage page -> { model with page = page }
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
