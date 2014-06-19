namespace NabfAgentLogic
module Logging =
    open JSLibrary.Logging
    open System
    open System.IO

    let debugLevel = DebugLevel.Important

    let sw = new StreamWriter (System.Console.OpenStandardOutput());
    sw.AutoFlush <- true;
    System.Console.SetOut (sw);
    let mutable Enabled = true

    type DebugFlag =
        | Planning
        | ActionSpecifications
        | Intentions
        | Perception
        | Agent
        | Parsing

    let allowFlags = 
        [ 
            Planning
            ActionSpecifications
            Intentions
            Perception
            Agent
            Parsing
        ] |> Set.ofList

    let logger = new Logger (sw, debugLevel);
    let logLock = new Object ();

    let logPrefix debugLevel prefix (debugFlag : DebugFlag) str =
        if Set.exists ((=) debugFlag) allowFlags then
            let prefixWithDebugLevel = sprintf "%s : [%A] " prefix debugLevel
            lock logLock (fun () -> logger.LogStringWithPrefix (str, prefixWithDebugLevel, debugLevel))

    let log debugLevel (debugFlag : DebugFlag) str = 
        if Set.exists ((=) debugFlag) allowFlags then
            lock logLock (fun () -> logger.LogStringWithTimeStamp (str, debugLevel))

    let logPrefixCritical = logPrefix DebugLevel.Critical
    let logPrefixError = logPrefix DebugLevel.Error
    let logPrefixWarning = logPrefix DebugLevel.Warning
    let logPrefixImportant = logPrefix DebugLevel.Important
    let logPrefixInfo = logPrefix DebugLevel.Info
    let logPrefixAll = logPrefix DebugLevel.All

    let logCritical = log DebugLevel.Critical
    let logError = log DebugLevel.Error
    let logWarning = log DebugLevel.Warning
    let logImportant = log DebugLevel.Important
    let logInfo = log DebugLevel.Info
    let logAll = log DebugLevel.All