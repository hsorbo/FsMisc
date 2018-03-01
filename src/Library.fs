namespace FsMisc

module SortedSeqDiff =
    type SeqDiff<'a> =
    | Left of 'a
    | Right of 'a
    | Both of 'a * 'a

    let private tryHeadAndTail s = s |> Seq.tryHead |> Option.map (fun h -> h, s |> Seq.skip 1)

    let rec seqDiff chooser (left : seq<_>) (right : seq<_>) = seq {
        let (l,r) = (left |> tryHeadAndTail, right |> tryHeadAndTail)
        match (l,r) with
        | (Some (lh, lt), Some (rh, _)) when chooser lh < chooser rh 
            -> yield Left(lh)
               yield! seqDiff chooser lt right
        | (Some (lh, _), Some (rh, rt)) when chooser lh > chooser rh 
            -> yield Right(rh)
               yield! seqDiff chooser left rt
        | (Some (lh, lt), Some (rh, rt))
            -> yield Both(lh,rh)
               yield! seqDiff chooser lt rt
        | (Some (lh, lt), None)   
            -> yield Left(lh)
               yield! seqDiff chooser lt right
        | (None, Some  (rh, rt))
            -> yield Right(rh)
               yield! seqDiff chooser left rt
        | (None, None) -> ()
    }

module GimmeProxy =
    open System
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
