//Decompression after eric backers gdfeco
namespace FsDeco

module GfDeco =
    open System
    open Gas

    type Coefficients<'t> = {A:'t; B:'t}

    type TissuePressures = {
        Current:InertGasses;
        HalfTimes:InertGasses
        CompartmentDetails:Tables.Compartment}
        with 
            member x.ToCoeff () = {
                A = { He = x.CompartmentDetails.He.A; N2 = x.CompartmentDetails.N2.A}
                B = { He = x.CompartmentDetails.He.B; N2 = x.CompartmentDetails.N2.B}}
    type State = { 
        RunTime:float
        MixNumber: int
        SegmentTime:float
        GradientFactor:float
        TissuePressure:TissuePressures list}
    
    type DivePlanSegment =
    | AscentDescent of startingDepth:float * endingDepth:float * rate:float * mixNumber:int
    | ConstantDepth of depth:float * runTimeEndOfSegment:float * mixNumber:int

    type ChangeSet = { Depth:float; Rate:float; StepSize: float }
    type Dive = {
        DivePlanSegments: DivePlanSegment list
        Change: ChangeSet list
        Gasses:GasMix list}

    type Settings = {
        GradientFactorLo:float
        GradientFactorHi:float
        MinimumDecoStopTime:float}

    let private haldaneEquationBoth inspiredGasPressure intervalTime tissuePressure  = {
        He = Calculations.haldaneEquation tissuePressure.Current.He inspiredGasPressure.He tissuePressure.HalfTimes.He intervalTime
        N2 = Calculations.haldaneEquation tissuePressure.Current.N2 inspiredGasPressure.N2 tissuePressure.HalfTimes.N2 intervalTime }

    let private schreinerEquationBoth initialInspiredGasPressure rateChangeInspGasPressure intervalTime tissuePressure  = {
        He = Calculations.schreinerEquation initialInspiredGasPressure.He rateChangeInspGasPressure.He intervalTime tissuePressure.HalfTimes.He tissuePressure.Current.He
        N2 = Calculations.schreinerEquation initialInspiredGasPressure.N2 rateChangeInspGasPressure.N2 intervalTime tissuePressure.HalfTimes.N2 tissuePressure.Current.N2 }

    let private calcInspiredPressure ambientPressure intertFraction = {
        He = (ambientPressure - Constants.waterVaporPressureEricBaker)*intertFraction.He
        N2 = (ambientPressure - Constants.waterVaporPressureEricBaker)*intertFraction.N2}        
        
    let private calcRate rate intertFraction = {He = rate*intertFraction.He; N2= rate*intertFraction.N2}      

    // applies the Schreiner equation to update the gas loadings (partial pressures of helium and nitrogen) 
    // in the half-time compartments due to a linear ascent or descent segment at a constant rate.
    let private gasLoadingsAscentDescent startingDepth endingDepth rate barometricPressure divegas state = // OK
        let segmentTime = (endingDepth - startingDepth)/rate //segtime = feil
        let ambientPressure = startingDepth + barometricPressure //startingAmbientPressure
        let inspiredPressure = calcInspiredPressure ambientPressure divegas
        let calc = schreinerEquationBoth inspiredPressure (calcRate rate divegas) segmentTime
        let tissuePressure = state.TissuePressure |> List.map (fun x -> {x with Current = calc x})
        {state with RunTime = state.RunTime + segmentTime; SegmentTime = segmentTime; TissuePressure = tissuePressure}

    //hsorbo: remark: the runtime of this is total runtime (if runtime of previous segment is longer this is invalid)
    let private gasLoadingsConstantDepth depth runTimeEndOfSegment barometricPressure divegas state = // OK
        let segmentTime = runTimeEndOfSegment - state.RunTime
        let ambientPressure = depth + barometricPressure
        let inspiredPressure = calcInspiredPressure ambientPressure divegas
        let calc = haldaneEquationBoth inspiredPressure segmentTime
        let tissuePressure = state.TissuePressure |> List.map (fun x -> {x with Current = calc x})
        {state with RunTime = runTimeEndOfSegment; SegmentTime = segmentTime; TissuePressure = tissuePressure}

    let gasLoading barometricPressure diveplan state x =
        match x with
            | AscentDescent (startDepth, endDepth, rate, mixNumber) -> 
                gasLoadingsAscentDescent startDepth endDepth rate barometricPressure diveplan.Gasses.[mixNumber].Inert state
            | ConstantDepth (depth, runtimeEndOfSegment, mixNumber) -> 
                gasLoadingsConstantDepth depth runtimeEndOfSegment barometricPressure diveplan.Gasses.[mixNumber].Inert state

    let calcDecoCelinngComp barometricPressure gradientFactor comp = 
        let gasload = comp.Current.He + comp.Current.N2
        let a = (comp.Current.He * comp.CompartmentDetails.He.A + comp.Current.N2 * comp.CompartmentDetails.N2.A) / gasload
        let b = (comp.Current.He * comp.CompartmentDetails.He.B + comp.Current.N2 * comp.CompartmentDetails.N2.B) / gasload
        let l = (gasload - a*gradientFactor)/(gradientFactor/b - gradientFactor + 1.0)
        (max 0.0 l) - barometricPressure

    let calcDecoCeilings barometricPressure state = // OK
        state.TissuePressure |> List.map (fun x -> calcDecoCelinngComp barometricPressure state.GradientFactor x)

    // This calculates the deco ceiling (the safe ascent depth) in each compartment, 
    // based on M-values modifed by gradient factors,and then finds the deepest deco 
    // ceiling across all compartments. This deepest value (Deco Ceiling Depth) is then 
    // used by the Decompression Stop subroutine to determine the actual deco schedule.
    let calcDecoCeiling barometricPressure state = calcDecoCeilings barometricPressure state |> List.max
       

    // Finds the depth at which the leading compartment just enters the decompression zone.
    let calcStartOfDecoZone startingDepth rate inertGas barometricPressure state =
        let startingAmbientPressure = startingDepth + barometricPressure
        let inspiredPressure = calcInspiredPressure startingAmbientPressure inertGas
        let rates = calcRate rate inertGas
        // ESTABLISH THE BOUNDS FOR THE ROOT SEARCH USING THE BISECTION METHOD
        // AND CHECK TO MAKE SURE THAT THE ROOT WILL BE WITHIN BOUNDS. PROCESS
        // EACH COMPARTMENT INDIVIDUALLY AND FIND THE MAXIMUM DEPTH ACROSS ALL
        // COMPARTMENTS (LEADING COMPARTMENT)
        // In this case, we are solving for time - the time when the gas tension in
        // the compartment will be equal to ambient pressure. The low bound for time
        // is set at zero and the high bound is set at the time it would take to
        // ascend to zero ambient pressure (absolute). Since the ascent rate is
        // negative, a multiplier of -1.0 is used to make the time positive. The
        // desired point when gas tension equals ambient pressure is found at a time
        // somewhere between these endpoints. The algorithm checks to make sure that
        // the solution lies in between these bounds by first computing the low bound
        // and high bound function values.
        let lowBound = 0.0
        let highBound = -1.0*(startingAmbientPressure/rate)

        let bisect x =
            let functionAtLowBound = x.Current.He + x.Current.N2 - startingAmbientPressure
            let highBoundPressure = schreinerEquationBoth inspiredPressure rates highBound x
            let functionAtHighBound = highBoundPressure.He + highBoundPressure.N2
            if functionAtHighBound * functionAtLowBound >= 0.0 then failwith "ERROR! ROOT IS NOT WITHIN BRACKETS"
            // APPLY THE BISECTION METHOD IN SEVERAL ITERATIONS UNTIL A SOLUTION WITH
            // THE DESIRED ACCURACY IS FOUND
            // Note: the program allows for up to 100 iterations. Normally an exit will
            // be made from the loop well before that number.
            let timeToStartOfDecoZone =
                //if x = 100 then failwith "ERROR! ROOT SEARCH EXCEEDED MAXIMUM ITERATIONS"
                let rec timeToStartOfDecoZoneLoop timeToStartOfDecoZone differentialChange =
                    let midRangeTime = timeToStartOfDecoZone + differentialChange
                    let midRangePressure = schreinerEquationBoth inspiredPressure rates midRangeTime x
                    let functionAtMidRange = midRangePressure.He + midRangePressure.N2 - (startingAmbientPressure + rate*midRangeTime)
                    let timeToStartOfDecoZone2 = if functionAtMidRange <= 0.0 then midRangeTime else timeToStartOfDecoZone
                    if abs differentialChange < 1.0E-3 || functionAtMidRange = 0.0 then timeToStartOfDecoZone2
                    else timeToStartOfDecoZoneLoop timeToStartOfDecoZone2 (differentialChange*0.5)
                let initialTime = if functionAtLowBound < 0.0 then lowBound else highBound
                let initialDiffChange = if functionAtLowBound < 0.0 then highBound - lowBound else lowBound - highBound
                timeToStartOfDecoZoneLoop initialTime initialDiffChange
            (startingAmbientPressure + rate*timeToStartOfDecoZone) - barometricPressure

        state.TissuePressure 
            |> List.map bisect
            |> List.max
    
    let private calcCoefficient inspiredPressure coeff = {
            A = (inspiredPressure.He*coeff.A.He + inspiredPressure.N2*coeff.A.N2)/(inspiredPressure.He+inspiredPressure.N2)
            B = (inspiredPressure.He*coeff.B.He + inspiredPressure.N2*coeff.B.N2)/(inspiredPressure.He+inspiredPressure.N2)}    

    let projectedAscent startingDepth rate decoStopDepth stepSize barometricPressure divegas state = 
        let startingAmbientPressure = startingDepth + barometricPressure
        let inspiredPressure = calcInspiredPressure startingAmbientPressure divegas
        let rates = calcRate rate divegas

        let rec calcDecoStopDepth decoStopDepth endingAmbientPressure =
            let segmentTime = (endingAmbientPressure - startingAmbientPressure)/rate
            let inner = 
                seq {
                    for x in state.TissuePressure do
                        let tempPressure = schreinerEquationBoth inspiredPressure rates segmentTime x
                        let tempGasLoading = tempPressure.He + tempPressure.N2
                        let coefficient = calcCoefficient tempPressure (x.ToCoeff())
                        let allowableGasLoading = endingAmbientPressure *(state.GradientFactor/coefficient.B - state.GradientFactor+ 1.0) + state.GradientFactor*coefficient.A
                        yield tempGasLoading > allowableGasLoading }
            if inner |> Seq.exists id
                then calcDecoStopDepth (decoStopDepth + stepSize) (endingAmbientPressure + stepSize)
                else decoStopDepth
        calcDecoStopDepth decoStopDepth (decoStopDepth + barometricPressure)
            
        

    // calculates the required time at each decompression stop.
    let decompressionStop decoStopDepth stepSize minimumDecoStopTime barometricPressure divegas state =
        let ambientPressure = decoStopDepth + barometricPressure
        let nextStop = decoStopDepth - stepSize
        let inspiredPressure = calcInspiredPressure ambientPressure divegas

        // Check to make sure that program won't lock up if unable to decompress
        // to the next stop. If so, write error message and terminate program.
        for x in state.TissuePressure do
            if inspiredPressure.He + inspiredPressure.N2 > 0.0 then
                let coefficient = calcCoefficient inspiredPressure (x.ToCoeff())
                let allowableGasLoading = (nextStop + barometricPressure) * (state.GradientFactor/coefficient.B - state.GradientFactor + 1.0) + state.GradientFactor*coefficient.A
                if inspiredPressure.He + inspiredPressure.N2 > allowableGasLoading then
                    failwith (sprintf "ERROR! OFF-GASSING GRADIENT IS TOO SMALL TO DECOMPRESS AT THE %f STOP" decoStopDepth)

        let roundUpOperation = (round((state.RunTime/minimumDecoStopTime) + 0.5)) * minimumDecoStopTime

        let rec calc tempSegmentTime tempState =
            let nextTissue = tempState.TissuePressure |> List.map (fun x -> {x with Current = haldaneEquationBoth inspiredPressure tempState.SegmentTime x})
            let nextState = { tempState with TissuePressure = nextTissue }
            if calcDecoCeiling barometricPressure nextState > nextStop then
                calc (tempSegmentTime + minimumDecoStopTime) { nextState with SegmentTime = minimumDecoStopTime; RunTime = tempState.RunTime + minimumDecoStopTime}
            else {nextState with SegmentTime = tempSegmentTime}  
        let tempState = {state with SegmentTime = roundUpOperation - state.RunTime; RunTime = roundUpOperation} 
        calc tempState.SegmentTime tempState
        
              

    // This calculates barometric pressure at altitude based on the
    // publication "U.S. Standard Atmosphere, 1976", U.S. Government Printing
    // Office, Washington, D.C. The source for this code is a Fortran 90 program
    // written by Ralph L. Carmichael (retired NASA researcher) and endorsed by
    // the National Geophysical Data Center of the National Oceanic and
    // Atmospheric Administration. It is available for download free from
    // Public Domain Aeronautical Software at: http://www.pdas.com/atmos.htm
    let calcBarometricPressure altitude = 
        let radiusOfEarth = 6369.0  //kilometers
        let accellerationOfGravity = 9.80665 //meters/second^2
        let molecularWeightOfAir = 28.9644 //mols
        let gasConstantR = 8.31432 //Joules/mol*deg Kelvin
        let tempAtSeaLevel = 288.15 //#degrees Kelvin
        //feet of seawater based on 101325 Pa at sea level (Standard Atmosphere)
        //let pressureAtSeaLevelFsw = 33.0 //Fsw = 33
        //meters of seawater based on 100000 Pa at sea level (European System)
        let pressureAtSeaLevel = 10.0 
        // Change in Temp deg Kelvin with change in geopotential altitude, 
        // valid for first layer of atmosphere up to 11 kilometers or 36,000 feet
        let tempGradient = -6.5
        let gmrFactor = accellerationOfGravity * molecularWeightOfAir / gasConstantR
        let altitudeKilometers = altitude / 1000.0
        let geopotentialAltitude = (altitudeKilometers * radiusOfEarth) / (altitudeKilometers + radiusOfEarth)
        let tempAtGeopotentialAltitude = tempAtSeaLevel + tempGradient * geopotentialAltitude
        pressureAtSeaLevel * exp (Math.Log(tempAtSeaLevel / tempAtGeopotentialAltitude) * gmrFactor / tempGradient)


///    let divedataAscentDescent Starting_Depth Ending_Depth Rate Respiratory_Minute_Volume =

    let printDeepestPossibleStopDepth depthStartOfDecoZone stepSize =
        let deepestPossibleStopDepth = 
            if stepSize < 3. 
                then (round ((depthStartOfDecoZone/stepSize) - 0.5))* stepSize 
                else (round ((depthStartOfDecoZone/3.) - 0.5)) * 3. 
        printfn "deepestPossibleStopDepth %A" deepestPossibleStopDepth
   
    let private hackCoeff (comp:Tables.Compartment) = 
        {comp with 
            He = {comp.He with A = Math.Round(comp.He.A * 10., 3)}
            N2 = {comp.N2 with A = Math.Round(comp.N2.A * 10., 3)}}
        
    let private createInitialDiveState barometricPressure gradientFactor = 
        let table =  Resources.loadBundledTables () |> List.find (fun x -> x.Name =  "ZH-L16B")
        let  halftime x = Math.Log(2.0)/x
        {   MixNumber = 0
            RunTime = 0.
            SegmentTime = 0. //toodo
            GradientFactor = gradientFactor
            TissuePressure = 
                table.Compartments 
                |> List.map (fun x -> { 
                    CompartmentDetails = hackCoeff x 
                    HalfTimes = { He = halftime x.He.HalfTime; N2 = halftime x.N2.HalfTime}
                    Current = { He = 0.; N2 = (barometricPressure - Constants.waterVaporPressureEricBaker)*0.79 }})}
    
    let decompressionStop2 decoStopDepth stepSize minimumDecoStopTime barometricPressure diveplan state =
        decompressionStop decoStopDepth stepSize minimumDecoStopTime barometricPressure diveplan.Gasses.[state.MixNumber].Inert state
    
    let private findGasForDepth depth diveplan  =
        diveplan.Change |> List.filter (fun x -> x.Depth >= depth)
                        |> List.minBy (fun x -> x.Depth)

    let private switchDecoGasIfNeccecary diveplan decoStopDepth state =
        let k = diveplan |> findGasForDepth decoStopDepth
        {state with MixNumber = diveplan.Change |> List.findIndex (fun x -> x = k)}

    let private recalcGradientFactors settings factorSlope nextStop state =
        {state with GradientFactor = nextStop * factorSlope + settings.GradientFactorHi}
    
    let private calcFactorSlope settings decoStopDepth = 
        if decoStopDepth > 0.0 
            then (settings.GradientFactorHi - settings.GradientFactorLo)/(0.0 - decoStopDepth)
            else 0.0 
    
    let calcDeco dybdeDings barometricPressure settings diveplan state = 
        let depthStartOfDecoZone = calcStartOfDecoZone dybdeDings.Depth dybdeDings.Rate (diveplan.Gasses.[state.MixNumber].Inert) barometricPressure state
        let decoCeilingDepth = (calcDecoCeiling barometricPressure state)
        
        let decoStopDepth = 
            if decoCeilingDepth < 0.0 
            then 0.0 
            else (round ((decoCeilingDepth/dybdeDings.StepSize) + 0.5) * dybdeDings.StepSize)
        
        if decoStopDepth > depthStartOfDecoZone then failwith "STEP SIZE IS TOO LARGE TO DECOMPRESS"
        let decoStopDepth = projectedAscent dybdeDings.Depth dybdeDings.Rate decoStopDepth dybdeDings.StepSize barometricPressure (diveplan.Gasses.[state.MixNumber].Inert) state
        if decoStopDepth > depthStartOfDecoZone then failwith "STEP SIZE IS TOO LARGE TO DECOMPRESS"
        
        let factorSlope = calcFactorSlope settings decoStopDepth

        let rec decoLoop depth decoStopDepth stepSize rate prevstates =
            let oldState = prevstates |> List.last
            let ascendState = gasLoadingsAscentDescent depth decoStopDepth rate barometricPressure (diveplan.Gasses.[oldState.MixNumber].Inert) oldState
            if decoStopDepth <= 0.0 then prevstates @ [ ascendState ]
            else
                let dybdings = findGasForDepth decoStopDepth diveplan
                let nextStop = decoStopDepth - dybdings.StepSize
                let decoState =
                    ascendState
                        |> recalcGradientFactors settings factorSlope nextStop
                        |> switchDecoGasIfNeccecary diveplan decoStopDepth
                        |> decompressionStop2 decoStopDepth stepSize settings.MinimumDecoStopTime barometricPressure diveplan
                decoLoop decoStopDepth nextStop stepSize rate (prevstates @ [ ascendState; decoState ])
        decoLoop dybdeDings.Depth decoStopDepth dybdeDings.StepSize dybdeDings.Rate [state]

    
    let calcDiveplan settings diveplan  =
        let barometricPressure = calcBarometricPressure 0.0
        diveplan.DivePlanSegments |> List.scan (gasLoading barometricPressure diveplan) (createInitialDiveState barometricPressure settings.GradientFactorLo)

    let calciveplanDeco settings diveplan =
        let barometricPressure = calcBarometricPressure 0.0
        let state = calcDiveplan settings diveplan |> List.last
        calcDeco diveplan.Change.[0] barometricPressure settings diveplan state |> List.skip 1
