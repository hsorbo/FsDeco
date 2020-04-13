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
    let waterVaporPressureEricBaker = 0.567  // for feet 1.848

module Calculations =
    //Palv Pressure of inspired inert gas
    let pAlv fGas pAbs pWvp = fGas * (pAbs - pWvp)
    /// Schreiner equation: P = Pio + R(t - 1/k) - [Pio - Po - (R/k)]e^-kt
    let schreiner po pAlv r t k = pAlv + r * (t - 1. / k) - (pAlv - po - r / k) * exp(-k * t)
    
    let shreinerFull po fGas pAbs pWvp pRate t hT = 
        schreiner po (pAlv fGas pAbs pWvp) (fGas * pRate) t (log(2.) / hT)

    let schreinerEquation initialInspiredGasPressure rateChangeInspGasPressure intervalTime gasTimeConstant initialGasPressure = 
        initialInspiredGasPressure + rateChangeInspGasPressure * (intervalTime - 1.0/gasTimeConstant) 
        - (initialInspiredGasPressure - initialGasPressure - rateChangeInspGasPressure/gasTimeConstant) * exp(-gasTimeConstant*intervalTime)
    
    
    //Buhlmann + Erik Baker GF: Pl=(P-A*gf)/(gf/B+1.0−gf)
    let buhlmann gf a b p = (p - a * gf)/(gf/b + 1.0 - gf)

    let haldaneEquation initialGasPressure inspiredGasPressure gasTimeConstant intervalTime =
        initialGasPressure + (inspiredGasPressure - initialGasPressure) * (1.0 - exp(-gasTimeConstant * intervalTime))

    
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

module Gas =
    type InertGasses = {He:float;N2:float}
    type GasMix = {O2:float;Inert:InertGasses} with 
        member x.Mod (ppO2:float<bar>) : float<bar> = ppO2 / (float x.O2)
        static member EAN32      = {O2=0.32;Inert = {N2=0.68;  He=0.0}}
        static member Air        = {O2=0.21;Inert = {N2=0.7902;He=0.00}}
        static member Trimix2135 = {O2=0.21;Inert = {N2=0.56;  He=0.35}}
        static member Trimix1845 = {O2=0.18;Inert = {N2=0.37;  He=0.45}}
        static member Trimix1555 = {O2=0.15;Inert = {N2=0.30;  He=0.55}}
        static member Trimix1070 = {O2=0.10;Inert = {N2=0.20;  He=0.70}}
        

module Decompression =
    open Tables
    open Gas
    type DiveStep = {Breathing:GasMix; Time:float<min>; AscentRate:float<bar/min>; CurrentAbsolutePressure:float<bar>}

    // type GradientFactor = {Hi:float;Low:float}
    //     with static member Default = {Hi=0.85;Low=0.3}

    let private calcSurfacePressure = TypedCalculations.pressureOfInspiredInertGas Constants.waterVaporPressure Constants.surfacePressure

    type TissuePressure = {Current:float<bar>; CompartmentDetails:GasValue}

    type DiveState = {Helium: TissuePressure list;Nitrogen: TissuePressure list} with
        static member Init (table:Table) = {
            Helium   = table.Compartments |> List.map (fun x -> {Current = calcSurfacePressure (LanguagePrimitives.FloatWithMeasure<gasFraction> GasMix.Air.Inert.He); CompartmentDetails = x.He })
            Nitrogen = table.Compartments |> List.map (fun x -> {Current = calcSurfacePressure (LanguagePrimitives.FloatWithMeasure<gasFraction> GasMix.Air.Inert.N2); CompartmentDetails = x.N2 })}
        
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
        { Helium = state.Helium |> List.map (calc (LanguagePrimitives.FloatWithMeasure<gasFraction> diving.Breathing.Inert.He))
          Nitrogen = state.Nitrogen |> List.map (calc (LanguagePrimitives.FloatWithMeasure<gasFraction> diving.Breathing.Inert.N2))}
    
    let schreinerCacluate table dives = List.scan schreinerNewState (DiveState.Init table) dives 