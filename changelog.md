# 1.1 - 24-Oct-2016

## Breaking changes 

* Emailing parse errors to given admin addresses, configurable with
  `--exception-rate` command line parameter. This is a breaking change
  as it requires a new `admin` field in the YAML configuration file.
  
## Other changes

* No longer failing to parse logs if individual lines fail to parse.
