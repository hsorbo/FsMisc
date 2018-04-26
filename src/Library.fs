namespace FsMisc
open System

module GimmeProxy =
    open Newtonsoft.Json
    open System.Net
    open System.Net.Http

    type Protocol = | Http | Socks4 | Socks5
    type ContryCode = string
    type Anonymity = | NotAnonymous | Anonymous
    type Args =
    | ApiKey of string
    | Get of bool
    | Post of bool
    | Cookies of bool
    | Referer of bool 
    | UserAgent of bool //user-agent header support
    | SupportsHttps of bool //HTTPS support
    | AnonymityLevel of Anonymity
    | Protocol of Protocol
    | Port of int
    | Country of ContryCode list
    | MaxCheckPeriod of TimeSpan
    //| Websites of list
    | MinSpeed of float //kbps
    | NotCountry of string

    type Response = {
        [<JsonProperty(PropertyName = "supportsHttps")>] SupportsHttps : bool
        [<JsonProperty(PropertyName = "protocol")>] Protcol : string // Enum?
        [<JsonProperty(PropertyName = "ip")>] Ip : string
        [<JsonProperty(PropertyName = "port")>] Port : int
        [<JsonProperty(PropertyName = "get")>] Get : bool
        [<JsonProperty(PropertyName = "post")>] Post : bool
        [<JsonProperty(PropertyName = "cookies")>] Cookies : bool
        [<JsonProperty(PropertyName = "referer")>] Referer : bool
        [<JsonProperty(PropertyName = "user-agent")>] UserAgent : bool
        //[<JsonProperty(PropertyName = "anonymityLevel")>] AnonymityLevel : Anonymity
        [<JsonProperty(PropertyName = "websites")>] Websites : Map<string, bool>
        [<JsonProperty(PropertyName = "country")>] Country : string
        [<JsonProperty(PropertyName = "tsChecked")>] TsChecked : int
        [<JsonProperty(PropertyName = "curl")>] Curl : string
        [<JsonProperty(PropertyName = "ipPort")>] IpPort : string
        [<JsonProperty(PropertyName = "type")>] Type : string //Enum?
        [<JsonProperty(PropertyName = "speed")>] Speed : float
        //[<JsonProperty(PropertyName = "otherProtocols")>] OtherProtocols : Response array
    }

    let private boolString = function
    | true -> "true"
    | false -> "false"

    let private argGenerator = function
    | ApiKey s -> ("api_key", s)
    | Get x -> ("get", boolString x)
    | Post x -> ("post", boolString x)
    | Cookies x -> ("cookies", boolString x)
    | Referer x -> ("referer", boolString x)
    | UserAgent x -> ("user-agent", boolString x)
    | SupportsHttps x -> ("supportsHttps", boolString x)
    | AnonymityLevel x -> ("anonymityLevel", match x with | Anonymous -> "1" | NotAnonymous -> "0")
    | Protocol x -> ("protocol", match x with | Socks4 -> "socks4"| Socks5 -> "socks5" | Http -> "http")
    | Port x -> ("port", x.ToString())
    | Country x -> ("country", String.Join(",", x))
    | MaxCheckPeriod x -> ("maxCheckPeriod", x.TotalSeconds.ToString())
    | MinSpeed x -> ("minSpeed", x.ToString())
    | NotCountry x -> ("notCountry", x)

    let private toQueryParam (args:Args seq) =
         args |> Seq.map argGenerator
              |> Seq.map (fun (k,v) -> sprintf "%s=%s" (WebUtility.UrlEncode(k)) (WebUtility.UrlEncode(v)))
    let private BaseUrl = "https://gimmeproxy.com/api/getProxy"
    let generateUri args = 
        let builder = UriBuilder(BaseUrl, Query = String.Join("&", toQueryParam args))
        builder.Uri

    let getProxy args = async {
        use http = new HttpClient()
        let uri = generateUri args
        let! resp = http.GetStringAsync(uri) |> Async.AwaitTask
        return JsonConvert.DeserializeObject<Response>(resp)
    }

//https://www.gnu.org/software/inetutils/manual/html_node/The-_002enetrc-file.html
module NetRc =
    type Identity =
    | Default
    | Machine of string

    type Entry = {
        Machine : Identity
        Login : string option
        Password : string option
        Account : string option
        Macdef : string option
    }
    with 
         static member Create x = {Machine = x; Login = None; Password = None; Account=None; Macdef = None}
         static member Default = Entry.Create Default
         static member ForMachine x = Entry.Create (x |> Machine)
    


    //no space https://stackoverflow.com/questions/12674888/can-netrc-handle-passphrases-with-spaces
    let private split (s:string) = 
        s.Split([|" ";"\r";"\n"|], StringSplitOptions.RemoveEmptyEntries) 
        |> Array.map (fun x -> x.Trim())
    
    type Element = 
    | Id of Identity
    | Login of string
    | Password of string
    | Account of string
    | Macdef of string

    let private parse input = 
        printfn "%A" input
        let rec parseInner output input =
            match input with
            | [] -> output |> List.rev
            | input ->  let (x, y) =
                            match input with
                            | "machine"  :: v :: t -> t, Machine(v) |> Id 
                            | "default"  :: t      -> t, Default |> Id
                            | "login"    :: v :: t -> t, Login(v)
                            | "password" :: v :: t -> t, Password(v)
                            | "account"  :: v :: t -> t, Account(v)
                            | "macdef"   :: v :: t -> t, Macdef(v)
                            | x :: _ -> failwith (sprintf "invalid token '%s'" x)
                        parseInner (y :: output) x
        parseInner [] input
    
    let private isId = function | Id _ -> true  | _ -> false

    let rec group (a:Element list) = seq {
        match a with
        | [] -> ()
        | Id x :: tail 
            -> yield (x, tail |> List.takeWhile (isId >> not))
               yield! group (tail |> List.skipWhile (isId >> not))
        | _ -> failwith "Expected identity" 
    }
    let buildState (x,z) =
        z |> List.fold (
            fun state x
                -> match x with
                   | Login x -> {state with Login = Some x}
                   | Password x -> {state with Password = Some x}
                   | Account x -> {state with Account = Some x}
                   | Macdef x -> {state with Macdef = Some x}
                   | _ -> failwith "No id here!"
        ) (Entry.Create x)
        

    let parseString str = 
        str
        |> split 
        |> Array.toList 
        |> parse
        |> group
        |> Seq.map buildState
        |> Seq.toList

    let parseFile filename =
        System.IO.File.ReadAllText(filename) |> parseString
        
        
       
