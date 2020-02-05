module Tests
open Xunit
open FsDeco
open FsDeco.Decompression
open Swensen.Unquote

let tables = Resources.loadBundledTables ()

module DecotenguExamples =
    //https://wrobell.dcmod.org/decotengu/model.html
    let exampleDive = [
        {Breathing = GasMix.EAN32; Time = 1.5<min>; CurrentAbsolutePressure = 1.0<bar>; AscentRate = 2.0<bar/min> }
        {Breathing = GasMix.EAN32; Time = 20.<min>; CurrentAbsolutePressure = 4.0<bar>; AscentRate = 0.<bar/min> }
        {Breathing = GasMix.EAN32; Time = 2.<min>; CurrentAbsolutePressure = 4.0<bar>; AscentRate = -1.<bar/min> }]
    
    let exampleTable = tables |> List.find (fun x -> x.Name = "ZH-L16B")

    [<Fact>]
    let ``Schreiner Equation example from decotengu`` () =
        let states = schreinerCacluate exampleTable exampleDive
        //printfn "%A" (states)
        let firstCalculatedN2s = 
            states             
            |> List.map (fun x -> x.Nitrogen |> List.head)
            |> List.map (fun x -> x.Current)
        
        let expected = [0.74065446<bar>;0.9193966739893478<bar>;2.5674910421243347<bar>;2.421840492164397<bar>]
        test <@ firstCalculatedN2s = expected @>


    [<Fact>]
    let ``Buhlmann Equation example from decotengu`` () =
        let states = schreinerCacluate exampleTable exampleDive
        let calculatedLimits = 
            states 
            |> List.map (fun x -> x.Nitrogen |> List.head)  
            |> List.map (fun x -> TypedCalculations.buhlmann 0.3 x.CompartmentDetails.A x.CompartmentDetails.B x.Current)
        //0.314886, 0.4592862, 1.7907266, 1.6730607
        let expected = [0.314886009020073631<bar>; 0.459285984345593145<bar>; 1.79072739810699244<bar>; 1.6730610933715213<bar>]
        test <@ calculatedLimits = expected @>
