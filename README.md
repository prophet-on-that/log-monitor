# log-monitor
Monitoring hslogger logs

A Haskell executable to monitor
[hslogger](http://hackage.haskell.org/package/hslogger) logs, emailing
a list of recipients a summary report. The program reads a
[YAML](http://www.yaml.org) configuration file on startup
(*log-monitor.yaml* by default), which must specify lists of *admins*
and *recipients* and a list of *sources*. Sources represent log files
to monitor, and must specify the *sourcePath*, a *sourceName*, the
hslogger *formatString* and the minimum priority (*minPriority*) to
report. The format of the logs require `$msg`, `$loggername`,
`$utcTime` and `$prio` fields. Here's an example of a
*log-monitor.yaml* file.
  
```  
admins: 
- admin@provider.com
recipients: 
- recipient@provider.com
sources: 
  - sourceName: web-server
  sourcePath: /home/user/web-server/log
  formatString: "[$utcTime $loggername $prio] $msg"
  minPriority: WARNING
  - sourceName: email-server
  sourcePath: /home/user/email-server/log
  formatString: "[$utcTime $loggername $prio] $msg"
  minPriority: INFO
```

