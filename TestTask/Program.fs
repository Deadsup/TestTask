
let transitionTable =
    [
        ("CLOSED", "APP_PASSIVE_OPEN", "LISTEN");
        ("CLOSED", "APP_ACTIVE_OPEN", "SYN_SENT");
        ("LISTEN", "RCV_SYN", "SYN_RCVD");
        ("LISTEN", "APP_SEND", "SYN_SENT");
        ("LISTEN", "APP_CLOSE", "CLOSED");
        ("SYN_RCVD", "APP_CLOSE", "FIN_WAIT_1");
        ("SYN_RCVD", "RCV_ACK", "ESTABLISHED");
        ("SYN_SENT", "RCV_SYN", "SYN_RCVD");
        ("SYN_SENT", "RCV_SYN_ACK", "ESTABLISHED");
        ("SYN_SENT", "APP_CLOSE", "CLOSED");
        ("ESTABLISHED", "APP_CLOSE", "FIN_WAIT_1");
        ("ESTABLISHED", "RCV_FIN", "CLOSE_WAI");
        ("FIN_WAIT_1", "RCV_FIN", "CLOSING");
        ("FIN_WAIT_1", "RCV_FIN_ACK", "TIME_WAIT");
        ("FIN_WAIT_1", "RCV_ACK", "FIN_WAIT_2");
        ("CLOSING", "RCV_ACK", "TIME_WAIT");
        ("FIN_WAIT_2", "RCV_FIN", "TIME_WAIT");
        ("TIME_WAIT", "APP_TIMEOUT", "CLOSED");
        ("CLOSE_WAIT", "APP_CLOSE", "LAST_ACK");
        ("LAST_ACK", "RCV_ACK", "CLOSED");
    ]

let processEvent currentState event =
    match List.tryFind (fun (s, e, _) -> s = currentState && e = event) transitionTable with
    | Some (_, _, nextState) -> nextState
    | None -> "ERROR"

let processEvents initialState events =
      Array.fold (fun state event -> processEvent state event) initialState events

let printResult result =
    printfn $"{result}"

let main() =
    printfn "Введите события через . или , или ; или /"
    let line = System.Console.ReadLine()
    let initialState = "CLOSED"
    let example = line.Split([|'.';',';' ';'/'|])
    let example1 = [|"APP_PASSIVE_OPEN"; "APP_SEND";"RCV_SYN_ACK"|]
    let example2 = [|"APP_ACTIVE_OPEN"|]
    let example3 = [|"APP_ACTIVE_OPEN"; "RCV_SYN_ACK"; "APP_CLOSE"; "RCV_FIN_ACK"; "RCV_ACK"|]
    let example4 = [|"RCV_SYN_ACK"; "APP_CLOSE"; "RCV_FIN_ACK"; "RCV_ACK"|]

    let result = processEvents initialState example
    let result1 = processEvents initialState example1
    let result2 = processEvents initialState example2
    let result3 = processEvents initialState example3
    let result4 = processEvents initialState example4

    printResult result
    printResult result1
    printResult result2
    printResult result3
    printResult result4

main()
