module Tests
open System
open Xunit
open FsDeco
open FsDeco.Decompression
open Swensen.Unquote
open Gas
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


module GfDecoTests = 
    open GfDeco
    let private onePresison numbers = numbers |> List.map (fun (x:float) -> Math.Round(x, 1))

    (*
                                  DECOMPRESSION CALCULATION PROGRAM
                                Developed in FORTRAN by Erik C. Baker
         
         
        Program Run:    02-05-2020 at 08:46 pm                       Model: ZH-L16B/GF
         
        Description:    SAMPLE DIVE TO 90 METERS OF SEAWATER GAUGE (MSWG) FOR 20 MINUTES      
         
        Gasmix Summary:                        FO2    FHe    FN2
                                  Gasmix # 1  0.130  0.500  0.370
                                  Gasmix # 2  0.360  0.000  0.640
                                  Gasmix # 3  0.500  0.000  0.500
                                  Gasmix # 4  0.800  0.000  0.200
         
         
                                            DIVE PROFILE
         
        Seg-  Segm.  Run   | Gasmix | Ascent    From     To      Rate    | Constant
        ment  Time   Time  |  Used  |   or     Depth   Depth    +Dn/-Up  |  Depth
          #   (min)  (min) |    #   | Descent  (mswg)  (mswg)  (msw/min) |  (mswg)
        ----- -----  ----- | ------ | -------  ------  ------  --------- | --------
          1     3.9    3.9 |    1   | Descent     0.     90.      23.0   |
          2    16.1   20.0 |    1   |                                    |    90.
         
         
                                       DECOMPRESSION PROFILE
         
                  Leading compartment enters the decompression zone at   71.0 mswg
                         Deepest possible decompression stop is   69.0 mswg
         
        Seg-  Segm.  Run   | Gasmix | Ascent   Ascent         |  DECO   STOP   RUN
        ment  Time   Time  |  Used  |   To      Rate    Grad. |  STOP   TIME   TIME
          #   (min)  (min) |    #   | (mswg) (msw/min) Factor | (mswg)  (min)  (min)
        ----- -----  ----- | ------ | ------ --------- ------ | ------  -----  -----
          3     3.6   23.6 |    1   |   54.    -10.0          |
          4     0.4   24.0 |    1   |                   0.30  |    54      2     24
          5     0.3   24.3 |    1   |   51.    -10.0          |
          6     1.7   26.0 |    1   |                   0.33  |    51      2     26
          7     0.3   26.3 |    1   |   48.    -10.0          |
          8     1.7   28.0 |    1   |                   0.35  |    48      2     28
          9     0.3   28.3 |    1   |   45.    -10.0          |
         10     1.7   30.0 |    1   |                   0.38  |    45      2     30
         11     0.3   30.3 |    1   |   42.    -10.0          |
         12     1.7   32.0 |    1   |                   0.40  |    42      2     32
         13     0.3   32.3 |    1   |   39.    -10.0          |
         14     1.7   34.0 |    1   |                   0.43  |    39      2     34
         15     0.3   34.3 |    1   |   36.    -10.0          |
         16     1.7   36.0 |    1   |                   0.45  |    36      2     36
         17     0.3   36.3 |    1   |   33.    -10.0          |
         18     1.7   38.0 |    2   |                   0.48  |    33      2     38
         19     0.3   38.3 |    2   |   30.    -10.0          |
         20     1.7   40.0 |    2   |                   0.50  |    30      2     40
         21     0.3   40.3 |    2   |   27.    -10.0          |
         22     1.7   42.0 |    2   |                   0.52  |    27      2     42
         23     0.3   42.3 |    2   |   24.    -10.0          |
         24     1.7   44.0 |    2   |                   0.55  |    24      2     44
         25     0.3   44.3 |    2   |   21.    -10.0          |
         26     3.7   48.0 |    3   |                   0.58  |    21      4     48
         27     0.3   48.3 |    3   |   18.    -10.0          |
         28     3.7   52.0 |    3   |                   0.60  |    18      4     52
         29     0.3   52.3 |    3   |   15.    -10.0          |
         30     5.7   58.0 |    3   |                   0.62  |    15      6     58
         31     0.3   58.3 |    3   |   12.    -10.0          |
         32     7.7   66.0 |    3   |                   0.65  |    12      8     66
         33     0.3   66.3 |    3   |    9.    -10.0          |
         34     9.7   76.0 |    4   |                   0.68  |     9     10     76
         35     0.3   76.3 |    4   |    6.    -10.0          |
         36    17.7   94.0 |    4   |                   0.70  |     6     18     94
         37     0.3   94.3 |    4   |    3.    -10.0          |
         38    33.7  128.0 |    4   |                   0.73  |     3     34    128
         39     0.3  128.3 |    4   |    0.    -10.0          |
                                                        0.75  |    
    *)
    [<Fact>]
    let ``Example Dive 90m`` () =
        // https://github.com/hsorbo/gfdeco/blob/master/examples/90m_dive/GFDECO.OUT
        let settings = {
            GradientFactorLo=0.3
            GradientFactorHi=0.75
            MinimumDecoStopTime=2.
        }
        
        let diveplan = {
            Gasses = [ 
                { O2 = 0.13; Inert = {He = 0.50; N2 = 0.37 }} 
                { O2 = 0.36; Inert = {He = 0.0;  N2 = 0.64 }}
                { O2 = 0.50; Inert = {He = 0.0;  N2 = 0.50 }}
                { O2 = 0.80; Inert = {He = 0.0;  N2 = 0.20 }} ]
            DivePlanSegments = [
            AscentDescent(0.,90.,23.,0)
            ConstantDepth(90.,20.,0)]
            Change = [
                {Depth = 90.; Rate = -10.; StepSize = 3.}
                {Depth = 33.; Rate = -10.; StepSize = 3.}
                {Depth = 21.; Rate = -10.; StepSize = 3.}
                {Depth =  9.; Rate = -10.; StepSize = 3.}
            ]
            // Repetitive_Dive_Flag = 0
            }
        let segmentTime = [3.6;0.4;0.3;1.7;0.3;1.7;0.3;1.7;0.3;1.7;0.3;1.7;0.3;1.7;0.3;1.7;0.3;1.7;0.3;1.7;0.3;1.7;0.3;3.7;0.3;3.7;0.3;5.7;0.3;7.7;0.3;9.7;0.3;17.7;0.3;33.7;0.3]
        let runtime = [23.6;24.0;24.3;26.0;26.3;28.0;28.3;30.0;30.3;32.0;32.3;34.0;34.3;36.0;36.3;38.0;38.3;40.0;40.3;42.0;42.3;44.0;44.3;48.0;48.3;52.0;52.3;58.0;58.3;66.0;66.3;76.0;76.3;94.0;94.3;128.0;128.3]
        let gasNumber = [1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;2;2;2;2;2;2;2;3;3;3;3;3;3;3;3;4;4;4;4;4;4]
        let calculatedDeco = GfDeco.calciveplanDeco settings diveplan
        //printfn "%A" segmentTime
        //printfn "%A" (calculatedDeco |> List.map (fun x -> x.SegmentTime) |> onePresison)
        test <@ calculatedDeco |> List.map (fun x -> x.SegmentTime) |> onePresison = segmentTime @>
        test <@ calculatedDeco |> List.map (fun x -> x.RunTime) = runtime @>
        test <@ calculatedDeco |> List.map (fun x -> x.MixNumber + 1) = gasNumber @>
        test <@ (calculatedDeco |> List.last).GradientFactor = 0.75 @>


    (*
                                          DIVE PROFILE
      
      Seg-  Segm.  Run   | Gasmix | Ascent    From     To      Rate    | Constant
      ment  Time   Time  |  Used  |   or     Depth   Depth    +Dn/-Up  |  Depth
        #   (min)  (min) |    #   | Descent  (mswg)  (mswg)  (msw/min) |  (mswg)
      ----- -----  ----- | ------ | -------  ------  ------  --------- | --------
        1     5.0    5.0 |    1   | Descent     0.     50.      10.0   |
        2    20.0   25.0 |    1   |                                    |    50.
      
      
                                     DECOMPRESSION PROFILE
      
                Leading compartment enters the decompression zone at   34.2 mswg
                       Deepest possible decompression stop is   33.0 mswg
      
      Seg-  Segm.  Run   | Gasmix | Ascent   Ascent         |  DECO   STOP   RUN
      ment  Time   Time  |  Used  |   To      Rate    Grad. |  STOP   TIME   TIME
        #   (min)  (min) |    #   | (mswg) (msw/min) Factor | (mswg)  (min)  (min)
      ----- -----  ----- | ------ | ------ --------- ------ | ------  -----  -----
        3     1.7   26.7 |    1   |   33.    -10.0          |
        4     0.3   27.0 |    1   |                   0.10  |    33      1     27
        5     0.3   27.3 |    1   |   30.    -10.0          |
        6     0.7   28.0 |    1   |                   0.17  |    30      1     28
        7     0.3   28.3 |    1   |   27.    -10.0          |
        8     0.7   29.0 |    1   |                   0.25  |    27      1     29
        9     0.3   29.3 |    1   |   24.    -10.0          |
       10     0.7   30.0 |    1   |                   0.32  |    24      1     30
       11     0.3   30.3 |    1   |   21.    -10.0          |
       12     0.7   31.0 |    1   |                   0.39  |    21      1     31
       13     0.3   31.3 |    1   |   18.    -10.0          |
       14     0.7   32.0 |    1   |                   0.46  |    18      1     32
       15     0.3   32.3 |    1   |   15.    -10.0          |
       16     1.7   34.0 |    1   |                   0.54  |    15      2     34
       17     0.3   34.3 |    1   |   12.    -10.0          |
       18     3.7   38.0 |    1   |                   0.61  |    12      4     38
       19     0.3   38.3 |    1   |    9.    -10.0          |
       20     6.7   45.0 |    1   |                   0.68  |     9      7     45
       21     0.3   45.3 |    1   |    6.    -10.0          |
       22    10.7   56.0 |    1   |                   0.75  |     6     11     56
       23     0.3   56.3 |    1   |    3.    -10.0          |
       24    23.7   80.0 |    1   |                   0.83  |     3     24     80
       25     0.3   80.3 |    1   |    0.    -10.0          |
                                                      0.90  |
    *)

    [<Fact>]
    let ``Example Dive 50m`` () =
        //https://github.com/lilvinz/deco/blob/6e55de4d286ad5c45dd015f03284f14233aaac27/zhl16b_validate.ipynb
        let settings = {
            GradientFactorLo=0.1
            GradientFactorHi=0.9
            MinimumDecoStopTime=1.}
        let runtime =[26.7;27.0;27.3;28.0;28.3;29.0;29.3;30.0;30.3;31.0;31.3;32.0;32.3;34.0;34.3;38.0;38.3;45.0;45.3;56.0;56.3;80.0;80.3]
        let segmentTime=[1.7;0.3;0.3;0.7;0.3;0.7;0.3;0.7;0.3;0.7;0.3;0.7;0.3;1.7;0.3;3.7;0.3;6.7;0.3;10.7;0.3;23.7;0.3];
        let diveplan = {
            Gasses = [ { O2 = 0.21; Inert = {He = 0.35; N2 = 0.44 }} ]
            DivePlanSegments = [
                AscentDescent(0.,50.,10.,0)
                ConstantDepth(50.,25.,0)]
            Change = [
                {Depth = 50.; Rate = -10.; StepSize = 3.}]}
        let calculatedDeco = GfDeco.calciveplanDeco settings diveplan
        test <@ calculatedDeco |> List.map (fun x -> x.SegmentTime) |> onePresison = segmentTime @>
        test <@ calculatedDeco |> List.map (fun x -> x.RunTime) = runtime @>
        test <@ (calculatedDeco |> List.last).GradientFactor = 0.90 @>
