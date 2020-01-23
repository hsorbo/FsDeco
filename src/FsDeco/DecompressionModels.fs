namespace FsDeco
    
//https://wrobell.dcmod.org/decotengu/_modules/decotengu/model.html
//https://wrobell.dcmod.org/decotengu/model.html#schreiner-equation
//https://www.shearwater.com/wp-content/uploads/2012/08/Introductory-Deco-Lessons.pdf
    
[<Measure>] type min
[<Measure>] type bar

module BuhlmanTables =
    type GasValue = {HalfTime:float; A:float; B:float}
    type Depth = {Time:float<min>; AscentRate:float<bar/min>; CurrentAbsolutePressure:float<bar>}
    type Compartment = {N2:GasValue;He:GasValue}
    type Table = {Name:string;Source:string;Compartments:Compartment list}



    type GasMix = {He:float;N2:float}
        with static member EAN32 = {N2 = 0.68; He=0.}
    let waterVaporPressure =  0.0627 //0.627??
    let nitrogenAtSeaLevel = 0.7902
    type GradientFactor = {Hi:float;Low:float}
        with static member Default = {Hi=0.85;Low=0.3}

    type CurrentPressure = {He:float;N2:float}
        with static member Init surfacePressure = {He=0.0;N2=nitrogenAtSeaLevel * (surfacePressure - waterVaporPressure)}

    let init profile surfacePressure =
        profile.Compartments |> List.map (fun _  -> CurrentPressure.Init surfacePressure)

    type Diving = {Breathing:GasMix; Depth:Depth}

    /// Schreiner equation: P = Pio + R(t - 1/k) - [Pio - Po - (R/k)]e^-kt
    let private schreiner po pio r t k =
        //printfn "P = %f + %f * (%f - 1. / %f) - (%f - %f - %f / %f)e^%f*%f" pio r t k pio po r k -k t
        pio + r * (t - 1. / k) - (pio - po - r / k) * exp(-k * t)
    
    //Buhlmann + Erik Baker GF: Pl=(P−A∗gf)/(gf/B+1.0−gf)
    let buhlmann gf (comp : Compartment) pressure = {
        N2 = ((pressure.N2 - comp.N2.A * gf)/(gf/comp.N2.B + 1.0 - gf))
        He = ((pressure.He - comp.He.A * gf)/(gf/comp.He.B + 1.0 - gf))}
    
    type SchreinerGasDetails = {InitialInertGasPressure:float;InertGasFraction:float;Halftime:float}
    
    let private schreiner2 waterWaporPressure depth details =
        let palv = details.InertGasFraction * ((float depth.CurrentAbsolutePressure) - waterWaporPressure)
        let r = details.InertGasFraction  * (float depth.AscentRate)
        let gasDecayConstant = log(2.) / details.Halftime
        schreiner details.InitialInertGasPressure palv r (float depth.Time) gasDecayConstant

    let schreinerSingleCompartment (compartment:Compartment) current diving = 
        let he = {InitialInertGasPressure=current.He; InertGasFraction=diving.Breathing.He; Halftime = compartment.He.HalfTime }
        let n2 = {InitialInertGasPressure=current.N2; InertGasFraction=diving.Breathing.N2; Halftime = compartment.N2.HalfTime }
        let sh = schreiner2 waterVaporPressure diving.Depth
        {CurrentPressure.He = sh he; CurrentPressure.N2 = sh n2}

    let schreinerMultiCompartment (state:Map<Compartment,CurrentPressure>) (diving:Diving) = 
        state |> Map.toList
              |> List.map (fun (comp, press) -> (comp, schreinerSingleCompartment comp press diving))
              |> Map.ofList