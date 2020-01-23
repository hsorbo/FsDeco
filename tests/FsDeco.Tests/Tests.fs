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
    let a = List.fold schreinerMultiCompartment iter0 dive
    let bb =(a |> Map.toList |> List.map snd |> List.head)
    Assert.Equal(2.421840492164397, bb.N2)