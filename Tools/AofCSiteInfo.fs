namespace Tools

open System.Net.Http
open System.Text.RegularExpressions

type HttpStatic private () =
    static let _client = new HttpClient()
    static member Client = _client


module AofCSiteInfo =
    type DayInfo = { Day: int; Title: string; Url: string; }

    let main year day =
        let url = $"https://adventofcode.com/{year}/day/{day}"
        async {
            let! result = HttpStatic.Client.GetStringAsync(url) |> Async.AwaitTask |> Async.Catch
            let response = match result with
                            | Choice.Choice1Of2 v -> (
                                let m = Regex.Match(v, @": (.+?)?(?=\s*-{2,})")
                                if m.Success then m.Value else "N/A"
                              )
                            | Choice.Choice2Of2 ex -> (
                                if ex.Message.Contains("404") then "Not yet available!" else ex.Message
                              )
                                    
            let result = {
                Day = day;
                Title = response;
                Url = url;
            }
            return result
        }
