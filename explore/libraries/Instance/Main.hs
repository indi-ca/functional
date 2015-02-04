module Main where


import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter, close)
import System.Log.Formatter


 -- By default, all messages of level WARNING and above are sent to stderr.
 -- Everything else is ignored.

 -- "MyApp.Component" is an arbitrary string; you can tune
 -- logging behavior based on it later.
main :: IO ()
main = do
    debugM "MyApp.Component"  "This is a debug message -- never to be seen"
    warningM "MyApp.Component2" "Something Bad is about to happen."

    -- Copy everything to syslog from here on out.
    s <- openlog "SyslogStuff" [PID] USER DEBUG

    f <- fileHandler "/Users/indika/temp.log" DEBUG

    --TODO: Figure out how to appropriate close and open this file
    --updateGlobalLogger rootLoggerName (addHandler f)
    updateGlobalLogger rootLoggerName (addHandler s)


    errorM "MyApp.Component" "This is going to stderr and syslog."

    -- Now we'd like to see everything from BuggyComponent
    -- at DEBUG or higher go to syslog and stderr.
    -- Also, we'd like to still ignore things less than
    -- WARNING in other areas.
    --
    -- So, we adjust the Logger for MyApp.Component.

    updateGlobalLogger "MyApp.BuggyComponent"
                       (setLevel DEBUG)

    -- This message will go to syslog and stderr
    debugM "MyApp.BuggyComponent" "This buggy component is buggy"

    -- This message will go to syslog and stderr too.
    warningM "MyApp.BuggyComponent" "Still Buggy"

    -- This message goes nowhere.
    debugM "MyApp.WorkingComponent" "Hello"

    --close f
