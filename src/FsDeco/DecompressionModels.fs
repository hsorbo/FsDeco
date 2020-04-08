namespace FsDeco
open Microsoft.FSharp.Core
    
//https://wrobell.dcmod.org/decotengu/_modules/decotengu/model.html
//https://wrobell.dcmod.org/decotengu/model.html#schreiner-equation
//https://www.shearwater.com/wp-content/uploads/2012/08/Introductory-Deco-Lessons.pdf
    
[<Measure>] type min
[<Measure>] type bar
[<Measure>] type gasFraction

module Constants =
    let waterVaporPressure = 0.0627<bar>
    let surfacePressure = 1.0<bar>

module Calculations =
    //Palv Pressure of inspired inert gas
    let pAlv fGas pAbs pWvp = fGas * (pAbs - pWvp)
    /// Schreiner equation: P = Pio + R(t - 1/k) - [Pio - Po - (R/k)]e^-kt
    let schreiner po pAlv r t k = pAlv + r * (t - 1. / k) - (pAlv - po - r / k) * exp(-k * t)
    
    let shreinerFull po fGas pAbs pWvp pRate t hT = 
        schreiner po (pAlv fGas pAbs pWvp) (fGas * pRate) t (log(2.) / hT)
    
    //Buhlmann + Erik Baker GF: Pl=(P-A*gf)/(gf/B+1.0−gf)
    let buhlmann gf a b p = (p - a * gf)/(gf/b + 1.0 - gf)
    
module TypedCalculations =
    let pressureOfInspiredInertGas waterVaporPressure (absolutePressure:float<bar>) (inertGasFraction:float<gasFraction>)  =
        Calculations.pAlv (float inertGasFraction) (float absolutePressure) (float waterVaporPressure) * 1.<bar>

    let schreiner 
        (initialInertGasInCompartment:float<bar>)
        (pressureOfInspiredInertGas:float<bar>)
        (rateOfChangeOfInertGasPressure:float<gasFraction*bar/min>)
        (timeOfExposure:float<min>) 
        gasDecayConstantForTissueCompartment  = 
            Calculations.schreiner 
                (float initialInertGasInCompartment) 
                (float pressureOfInspiredInertGas) 
                (float rateOfChangeOfInertGasPressure)
                (float timeOfExposure)
                gasDecayConstantForTissueCompartment * 1.<bar>
    
    let schreinerFull 
        initialInertGasInCompartment
        inertGasFraction
        absolutePressure
        ascentRate
        timeOfExposure
        waterVaporPressure
        halftimeForTissueCompartment = 
        schreiner initialInertGasInCompartment 
            (pressureOfInspiredInertGas waterVaporPressure absolutePressure inertGasFraction  ) 
            (inertGasFraction * ascentRate)
            timeOfExposure 
            (log(2.) / halftimeForTissueCompartment)

    let buhlmann gradientFactor a b (pressure:float<bar>) = 
        Calculations.buhlmann gradientFactor a b (float pressure) * 1.<bar>



module Decompression =
    open Tables
    
    type GasMix = {He:float<gasFraction>;N2:float<gasFraction>} with 
        static member EAN32 = {N2 = 0.68<gasFraction>; He=0.<gasFraction>}
        static member Air = {N2 = 0.7902<gasFraction>; He=0.<gasFraction>}
    
    type DiveStep = {Breathing:GasMix; Time:float<min>; AscentRate:float<bar/min>; CurrentAbsolutePressure:float<bar>}

    // type GradientFactor = {Hi:float;Low:float}
    //     with static member Default = {Hi=0.85;Low=0.3}

    let private calcSurfacePressure = TypedCalculations.pressureOfInspiredInertGas Constants.waterVaporPressure Constants.surfacePressure

    type TissuePressure = {Current:float<bar>; CompartmentDetails:GasValue}

    type DiveState = {Helium: TissuePressure list;Nitrogen: TissuePressure list} with
        static member Init (table:Table) = {
            Helium   = table.Compartments |> List.map (fun x -> {Current = calcSurfacePressure GasMix.Air.He; CompartmentDetails = x.He })
            Nitrogen = table.Compartments |> List.map (fun x -> {Current = calcSurfacePressure GasMix.Air.N2; CompartmentDetails = x.N2 })}
        
    let schreinerNewState (state:DiveState) (diving:DiveStep) = 
        let calc inertGasFraction state = { 
            state with 
                Current = TypedCalculations.schreinerFull 
                    state.Current
                    inertGasFraction
                    diving.CurrentAbsolutePressure 
                    diving.AscentRate 
                    diving.Time 
                    Constants.waterVaporPressure
                    state.CompartmentDetails.HalfTime }
        { Helium = state.Helium |> List.map (calc diving.Breathing.He)
          Nitrogen = state.Nitrogen |> List.map (calc diving.Breathing.N2)}
    
    let schreinerCacluate table dives = List.scan schreinerNewState (DiveState.Init table) dives 