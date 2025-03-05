module web.Views

open web.Models
open Giraffe.ViewEngine

let layout (content: XmlNode list) =
    html [] [
        head [] [
            title []  [ encodedText "web" ]
            link [ 
                _rel "stylesheet"
                _type "text/css"
                _href "/main.css" 
            ]
        ]
        body [] content
    ]

let partial () =
    h1 [] [ encodedText "Spbu Timetable" ]

let partialWithArg data =
    h1 [] [ encodedText <| String.concat ". " ["Spbu Timetable"; data] ]


let flist fl =
    form [ _action "/selectFaculty"; _method "POST"] [
        select [ _name "Faculty" ] [ 
            for i in fl -> option [] [ str i ]
        ]
        input [ _type "submit" ]
    ]

let faculties facList =
    [
        partial ()
        p [] [ encodedText "Input your data" ]
        // inputData ()
        flist facList
    ] |> layout

let studyProgram data h =
    [
        partialWithArg h
        form [ _action "/studyProgram"; _method "POST"] [
          select [ _name "StudyProgram" ] [ 
              for i in data -> option [] [ str i ]
          ]
          input [ _type "submit" ]
      ]
    ]
    |> layout

let studyDirection data h =
    [
        partialWithArg h  
        form [ _action "/studyDirection"; _method "POST"] [
          select [ _name "StudyDirection" ] [ 
              for i in data -> option [] [ str i ]
          ]
          input [ _type "submit" ]
      ]
    ]
    |> layout

let years data h =
    [
        partialWithArg h  
        form [ _action "/yearOfAdmission"; _method "POST"] [
          select [ _name "Year" ] [ 
              for i in data -> option [] [ str i ]
          ]
          input [ _type "submit" ]
      ]
    ]
    |> layout

let formOneTable data = 
    table [] [
        for (f, s, t) in data ->
                tr [] [ 
                    td [] [ str f ]
                    td [] [ str s ]
                    td [] [ str t ]
                ]
    ]

let timetable data =
    [
        for i in data -> 
            formOneTable i
    ]
    |> layout