module web.Models

[<CLIMutable>]
type Faculty = { Faculty : string }

[<CLIMutable>]
type Input =
    {
        Faculty : string
        Speciality : string
        Year : string
    }

[<CLIMutable>]
type StudyProgram = { StudyProgram : string }

[<CLIMutable>]
type StudyDirection = { StudyDirection : string }

[<CLIMutable>]
type Year = { Year : string }