namespace FsDeco
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open System.IO
open System.Reflection

module Resources = 
    open Tables
    let private deserializeJsonStream (stream:Stream) = 
        let serializerSettings =
            let resolver = DefaultContractResolver(NamingStrategy = SnakeCaseNamingStrategy())
            JsonSerializerSettings(ContractResolver = resolver, Formatting = Formatting.Indented)
        let serializer = JsonSerializer.Create serializerSettings
        use textReader = new StreamReader(stream)
        use jsonReader = new JsonTextReader(textReader)
        serializer.Deserialize<Table list>(jsonReader)

    let private deserializeJson str = 
        let serializerSettings =
            let resolver = DefaultContractResolver(NamingStrategy = SnakeCaseNamingStrategy())
            JsonSerializerSettings(ContractResolver = resolver, Formatting = Formatting.Indented)
        JsonConvert.DeserializeObject<Table list>(str, serializerSettings)
        
    let loadTables filename = File.ReadAllText(filename) |> deserializeJson
    let loadBundledTables () =
        let asm = typedefof<FsDeco.Tables.Table>.GetTypeInfo().Assembly
        //printfn "NINJA %A" (asm.GetManifestResourceNames())
        asm.GetManifestResourceStream("FsDeco.buhlman_tables.json") |> deserializeJsonStream
