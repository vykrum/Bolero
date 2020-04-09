module Bolero.Client.Main
open Clusters
open Elmish
open Bolero
open Bolero.Html
open Bolero.Templating.Client
open Microsoft.FSharp.Collections

//type Model =
//    {
//        x: string
//    }

//let initModel =
//    {
//        x = ""
//    }

//type Message =
//    | Ping

//let update message model =
//    match message with
//    | Ping -> model

//let view model dispatch =
//    text "Hello, world!"

//type MyApp() =
//    inherit ProgramComponent<Model, Message>()

//    override this.Program =
//        Program.mkSimple (fun _ -> initModel) update view
//#if DEBUG
//        |> Program.withHotReload
//#endif



//type Model = { firstName: string; lastName: string }
//let initModel = { firstName = ""; lastName = "" }

//type Message = SetFirstName of string | SetLastName of string
//let update message model =
//    match message with
//    | SetFirstName n -> { model with firstName = n }
//    | SetLastName n -> { model with lastName = n }

//let viewInput model setValue =
//    input [
//        attr.value model
//        on.change (fun e -> setValue (unbox e.Value))
//    ]

//let view model dispatch =
//    div [] [
//        viewInput model.firstName (fun n -> dispatch (SetFirstName n))
//        viewInput model.lastName (fun n -> dispatch (SetLastName n))
//        text (sprintf "Hello, %s %s!" model.firstName model.lastName)
//    ]

////let program =
////    Program.mkSimple (fun _ -> initModel) update view

//type MyApp() =
//    inherit ProgramComponent<Model, Message>()

//    override this.Program =
//        Program.mkSimple (fun _ -> initModel) update view
//#if DEBUG
//        |> Program.withHotReload
//#endif

let dd = HexShapes.cls 15 [] (5.0,5.0)
let ff = HexShapes.mcs 10 dd |> List.concat
let cv l m = (HexShapes.cls l [] (5.0,5.0)) |> HexShapes.mcs m |> List.concat

let x1 lst = List.map (fun x -> fst x)lst |> List.map (fun x -> x*20.0)|> List.map (fun x -> Attr("cx", x))
let y1 lst = List.map (fun x -> snd x)lst |> List.map (fun x -> x*20.0) |> List.map (fun x -> Attr("cy", x))

let cr a b = Elt("circle", [a;b;Attr("r",10);Attr("stroke","green");Attr("fill","yellow")],[])
let fg = List.map2 (fun x y -> cr x y) (x1 ff) (y1 ff)
let r = svg[Attr("width", 500);Attr("height", 500)] fg
 
let vg (l,m) = svg[Attr("width", 500);Attr("height", 500)] (List.map2 (fun x y -> cr x y) (x1 (cv l m)) (y1 (cv l m)))

type Model = { 
    cll: int
    hst: int
    clt: Node
    }
let initModel = { 
    cll = 5
    hst = 10
    clt = vg (5, 10)
    }

type Message = IncCll | IncHst | DecCll | DecHst |SetClt

let update message model =
    match message with
    | IncCll -> { model with cll = model.cll + 1 }
    | IncHst -> { model with hst = model.hst + 1 }
    | DecCll -> { model with cll = model.cll - 1 }
    | DecHst -> { model with hst = model.hst - 1 }
    | SetClt -> { model with clt = vg (model.cll,model.hst)}


let viewInput model setValue =
    input [
        attr.value model
        on.change (fun e -> setValue (unbox e.Value))
    ]

let view model dispatch=
    div [] [
        //viewInput model.clt (fun _ -> dispatch SetClt)
        vg (model.cll,model.cll)
        button [on.click (fun _ -> dispatch DecCll)] [text "-"]
        text (string model.cll)
        button [on.click (fun _ -> dispatch IncCll)] [text "+"]
        button [on.click (fun _ -> dispatch DecHst)] [text "-"]
        text (string model.hst)
        button [on.click (fun _ -> dispatch IncHst)] [text "+"]
        ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
#if DEBUG
        |> Program.withHotReload
#endif