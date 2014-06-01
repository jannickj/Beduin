namespace NabfAgentLogic
module Logging =
    open JSLibrary.Logging
    open System
    open System.IO

    let debugLevel = DebugLevel.None

    let sw = new StreamWriter (System.Console.OpenStandardOutput());
    sw.AutoFlush <- true;
    System.Console.SetOut (sw);

    let logger = new Logger (sw, debugLevel);
    let logLock = new Object ();
    let mutable Enabled = true;
    let log debugLevel str = if Enabled then lock logLock (fun () -> logger.LogStringWithTimeStamp (str, debugLevel))

    let logCritical = log DebugLevel.Critical
    let logError = log DebugLevel.Error
    let logWarning = log DebugLevel.Warning
    let logImportant = log DebugLevel.Important
    let logInfo = log DebugLevel.Info
    let logAll = log DebugLevel.All


    
