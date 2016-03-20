# haskell-db-fahrplan-cli
Command Line Interface for the Deutsche Bahn Fahrplan API.

```bash
DeutscheBahn CLI - Retrieve schedules via the command line

Usage: haskell-db-fahrplan-cli (-k|--key AUTHKEY) [-t|--time TIME]
                               [-d|--day Day] [-b|--board BOARD]
                               [-l|--language LANG] LOCATION

Available options:
  -h,--help                Show this help text
  -k,--key AUTHKEY         API Authentication Key provided by Deutsche Bahn
  -t,--time TIME           Time of day e.g. 20:22 or 05:20. Defaults to now.
  -d,--day Day             Day e.g. 30.12.2016. Defaults to today.
  -b,--board BOARD         `arrival` || `departure` || `all`
  -l,--language LANG       Response language `de` or `en`, default is `en`
```

The example below demonstrates how to lookup all arrivals and departures for Stuttgart on the given date and time.
`???` is the API key provided by Deutsche Bahn.

```bash
$ ./haskell-db-fahrplan-cli Stuttgart -k "???" -t 10:00 -d 23.03.2016                                                                                                                              
Connections for stop: Sturovo
departs 2016-03-23 19:14:00     "EC 173"        track: ?        with: EC        to: Budapest-Keleti

departs 2016-03-23 21:24:00     "EN 476"        track: ?        with: EN        to: Berlin Hbf (tief)

departs 2016-03-24 07:13:00     "EN 477"        track: ?        with: EN        to: Budapest-Keleti

departs 2016-03-24 08:49:00     "EC 172"        track: ?        with: EC        to: Hamburg-Altona

arrives 2016-03-23 19:11:00     "EC 173"        track: ?        with: EC        from: Hamburg-Altona

arrives 2016-03-23 21:21:00     "EN 476"        track: ?        with: EN        from: Budapest-Keleti

arrives 2016-03-24 07:10:00     "EN 477"        track: ?        with: EN        from: Berlin Hbf (tief)

arrives 2016-03-24 08:46:00     "EC 172"        track: ?        with: EC        from: Budapest-Keleti


Connections for stop: Stuttgart Hbf
departs 2016-03-23 10:04:00     "IC 1296"       track: 9        with: IC        to: Frankfurt(Main)Hbf

departs 2016-03-23 10:07:00     "IC 2065"       track: 16       with: IC        to: München Hbf

departs 2016-03-23 10:13:00     "ICE 513"       track: 15       with: ICE       to: München Hbf

departs 2016-03-23 10:29:00     "IC 183"        track: 3        with: IC        to: Zürich HB

departs 2016-03-23 10:41:00     "ICE 714"       track: 11       with: ICE       to: Düsseldorf Hbf

departs 2016-03-23 10:51:00     "ICE 690"       track: 10       with: ICE       to: Berlin Ostbahnhof

departs 2016-03-23 11:11:00     "IC 1268"       track: 11       with: IC        to: Karlsruhe Hbf

departs 2016-03-23 11:13:00     "ICE 1091"      track: 16       with: ICE       to: München Hbf

departs 2016-03-23 11:25:00     "ICE 578"       track: 5        with: ICE       to: Hamburg-Altona

departs 2016-03-23 11:29:00     "IC 2312"       track: 6        with: IC        to: Hamburg-Altona
```
