module Crawler.Base
open System.Net.Http
open System.Text.RegularExpressions
open HtmlAgilityPack
open System.Linq

let base_url = "https://timetable.spbu.ru" 

let client = new HttpClient()

type StudyDirection = {
    name : string;
    data : list<string * string> // Year and href
}

type StudyProgram = {
    name : string;
    programs : list<StudyDirection>
}

let processGetRequest (url : string) =
    async {
        try
            let! response = client.GetAsync(url) |> Async.AwaitTask
            response.EnsureSuccessStatusCode () |> ignore
            let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
            return Some content
        with
            | _ ->
                printfn "Error: failed to access %s" url
                return None
    }

let ind = ref 0

let getTt html =
    let helper (el : HtmlNode) =
        let doc = new HtmlDocument()
        doc.LoadHtml el.InnerHtml
        let date1 = 
          doc.DocumentNode.SelectNodes("//div[@class='col-sm-2 studyevent-datetime']")

        // let group = 

        let date = 
          date1.Select(fun t -> [for matches in (Regex(">([^#<>]+)</span>", RegexOptions.Compiled)
                                .Matches(t.InnerHtml) : MatchCollection) -> matches.Groups.[1].Value]
                      ) |> Seq.toList
        let subject = 
          doc.DocumentNode.SelectNodes("//div[@class='col-sm-4 studyevent-subject']")
              .Select(fun t -> [for matches in (Regex(">([^#<>]+)</span>", RegexOptions.Compiled)
                                .Matches(t.InnerHtml) : MatchCollection) -> matches.Groups.[1].Value]
              ) |> Seq.toList
        let loc = 
          doc.DocumentNode.SelectNodes("//div[@class='col-sm-3 studyevent-locations']")
             .Select(fun t -> [for matches in (Regex("<span class=\"hoverable-link\">([^#<>]+)</span>", RegexOptions.Compiled)
                                .Matches(t.InnerHtml) : MatchCollection) -> matches.Groups.[1].Value]
             ) |> Seq.toList
        let edu = 
          doc.DocumentNode.SelectNodes("//div[@class='col-sm-3 studyevent-educators']")
             .Select(fun t -> [for matches in (Regex("<a href=\"/EducatorEvents/[0-9]{4}\">([^#<>]+)</a>", RegexOptions.Compiled)
                                .Matches(t.InnerHtml) : MatchCollection) -> matches.Groups.[1].Value]
             ) |> Seq.toList
        let len = List.length 
        let hd = List.head
        match (date, subject, edu) with
        | ([ d ], [ s ], [ e ]) when len d = 1 && len s = 1 && len e = 1 -> Some (hd d, hd s, hd e)
        | _ -> None
        
    let doc = new HtmlDocument()
    doc.LoadHtml html
    try
      let l = doc.DocumentNode
                 .SelectNodes("//li[@class='common-list-item row']")
                 .Select(helper)
                 .ToList()
                 |> Seq.toList
                 |> List.fold (fun acc x -> match x with | Some x -> acc @ [x] | None -> acc) []
      l
    with
    | _ -> []
    

// Get faculty names and their url's.
let get_tags html =
    [for matches in (Regex("<a href\s*=\s*\"?([^\"]+)\"?\s*>([^\"]+)</a>", RegexOptions.Compiled)
    .Matches(html) : MatchCollection) -> (matches.Groups.[1].Value, matches.Groups.[2].Value)]

let getSpecInfo html =
    let helper (el : HtmlNode) =
        let names = [for matches in (Regex("<div class\s*=\s*\"col-sm-5\">([^#<>]+)</div>", RegexOptions.Compiled)
                     .Matches(el.InnerHtml) : MatchCollection) -> matches.Groups.[1].Value]
        
        let years = [for matches in (Regex("[^_]20([0-9])([0-9])", RegexOptions.Compiled)
                      .Matches(el.InnerHtml) : MatchCollection) -> matches.Groups.[0].Value] 
        
        let href = [for matches in (Regex("<a href\s*=\s*\"?([^\"]+)\"?", RegexOptions.Compiled)
                     .Matches(el.InnerHtml) : MatchCollection) -> matches.Groups.[1].Value]
        match (names, years, href) with
        | [ n ], y, h when List.length y = List.length h ->
          let yh = List.zip y h
          Some {
            name = n
            data = yh
          }
        | _ -> None

    let getName h = 
        [for matches in (Regex("href=\"#studyProgramLevel[0-9]\">([^0-9]+)</a>", RegexOptions.Compiled)
                     .Matches(h) : MatchCollection) -> matches.Groups.[1].Value]

    let doc = new HtmlDocument()
    doc.LoadHtml html
    let name = getName html

    try
      doc.DocumentNode.SelectNodes("//li[@class='common-list-item row']")
        .Select(helper)
        .ToList()
      |> Seq.toList
      |> List.fold (fun acc x -> match x with | Some x -> acc @ [x] | None -> acc) []
      |> (fun l -> 
          match name with
          | [ n ] ->
            Some { 
              name = n
              programs = l
            }
          | _ -> None
        )  
    with
    | _ -> None

// Get base elements on the chosen faculty page.
let getAllSpecs url =
    async {
        let! response = processGetRequest url
        match response with
        | Some html ->
          let doc = new HtmlDocument()
          doc.LoadHtml html
          let studyPrograms = 
            doc.DocumentNode.SelectNodes("//div[@class='panel panel-default']")
              .Select(fun t -> t.InnerHtml)
              .ToList()
          let result =
            studyPrograms |> Seq.toList
            |> List.map getSpecInfo
            |> List.fold (fun acc x -> match x with | Some x -> acc @ [x] | None -> acc) []
          return result
        | _ -> return []
    } |> Async.RunSynchronously

let ex =
    async {
        let! data = processGetRequest base_url
        let hrefs = 
            match data with 
            | Some x -> 
              let a = x
              get_tags x 
              |> List.filter (fun (s, _) -> String.length s > 0 && s.[0] = '/')
              |> List.map (fun (s, name) -> (String.concat "" [base_url; s]), name)
            | _ -> []
        let new_data =
            hrefs
            |> List.map 
                (fun (s, name) -> 
                    let html = processGetRequest s |> Async.RunSynchronously
                    (html, name))
            |> List.fold (fun acc -> function | Some h, name -> (name, h) :: acc | _ -> acc) []
            |> List.rev
        return new_data
    }

// Get list of faculties
let get_faculty_list () =
    async {
        let! data = processGetRequest base_url
        return 
            match data with 
            | Some x -> 
              get_tags x 
              |> List.filter (fun (s, _) -> String.length s > 0 && s.[0] = '/')
              |> List.map (fun (s, n) -> (String.concat "" [base_url; s], n))
            | _ -> []
    } |> Async.RunSynchronously


let get_group_page_info url =
    async {
        let! data = processGetRequest <| String.concat "" [base_url; url]
        return data
    } |> Async.RunSynchronously

let get_group_hrefs html =
    [for matches in (Regex("onclick=\"window.location.href=([^\"]+)\">", RegexOptions.Compiled)
    .Matches(html) : MatchCollection) -> matches.Groups.[1].Value]

let downloadTimeTableForAllGroups html =
    let result = 
      html 
      |> get_group_hrefs 
      |> List.map
        (fun u -> 
            let fullAddr =
                u 
                |> String.filter (fun c -> c <> ''')
                |> (fun s -> String.concat "" [base_url; s])
            async {
              let! newHtml = processGetRequest fullAddr
              return newHtml 
            } |> Async.RunSynchronously
        )
      |> List.fold (fun acc x -> match x with Some h -> h :: acc | _ -> acc) []
      // |> List.map getTt
    let tt = result |> List.map getTt
    tt

(*
[<EntryPoint>]
let main argv =
   // let p = getTt d
    let hrefs = ex |> Async.RunSynchronously
    let a = getAllSpecs "https://timetable.spbu.ru/MATH"
    let l = get_faculty_list ()
    // let hrefs = ex |> Async.RunSynchronously
    0
*)