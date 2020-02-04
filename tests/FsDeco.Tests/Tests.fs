module Tests
open Xunit
open FsDeco
open FsDeco.BuhlmanTables

let tables = Resources.loadBundledTables ()

[<Fact>]
let ``Schreiner example from decotengu`` () =
    //https://wrobell.dcmod.org/decotengu/model.html
    let table = tables |> List.find (fun x -> x.Name = "ZH-L16B")
    let dive = [
        {Breathing = GasMix.EAN32; Depth = {Time = 1.5<min>; CurrentAbsolutePressure = 1.0<bar>; AscentRate = 2.0<bar/min> }}
        {Breathing = GasMix.EAN32; Depth = {Time = 20.<min>; CurrentAbsolutePressure = 4.0<bar>; AscentRate = 0.<bar/min> }}
        {Breathing = GasMix.EAN32; Depth = {Time = 2.<min>; CurrentAbsolutePressure = 4.0<bar>; AscentRate = -1.<bar/min> }}
    ]
    let iter0 = List.zip table.Compartments (BuhlmanTables.init table 1.) |> Map.ofList 
    
    let firstCalculatedN2s = 
        List.scan schreinerMultiCompartment iter0 dive
        |> List.map (fun x -> x |> Map.toList |> List.map snd |> List.head)
        |> List.map (fun x -> x.N2)
    
    let firstN2Knowns = [
        0.74065446;
        0.9193966739893478;
        2.5674910421243347;
        2.421840492164397]
    firstCalculatedN2s |> List.zip firstN2Knowns |> List.iter (fun (a,b) -> Assert.Equal(a,b))