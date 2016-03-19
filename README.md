# haskell-db-fahrplan-cli
Command Line Interface for the Deutsche Bahn Fahrplan API.

E.g. `/haskell-db-fahrplan-cli Berlin -k "???"` looksup all current arrivals and departures at the stop Berlin.
Where ??? is the API key provided by Deutsche Bahn.

```
2016-03-19 04:27:00     "ICE 1583"      track: 2        with: ICE       to: München Hbf

2016-03-19 05:27:00     "ICE 593"       track: ?        with: ICE       to: München Hbf

2016-03-19 05:33:00     "ICE 1038"      track: 8        with: ICE       to: Hamburg-Altona

2016-03-19 05:38:00     "ICE 946"       track: 2 D - G  with: ICE       to: Düsseldorf Hbf

2016-03-19 05:38:00     "ICE 956"       track: 2 A - D  with: ICE       to: Köln Hbf

2016-03-19 06:27:00     "ICE 1585"      track: 2        with: ICE       to: München Hbf

2016-03-19 06:34:00     "ICE 277"       track: ?        with: ICE       to: Basel SBB

2016-03-19 06:38:00     "ICE 1618"      track: 7        with: ICE       to: Hamburg-Altona

2016-03-19 06:41:00     "IC 240"        track: ?        with: IC        to: Bad Bentheim

2016-03-19 06:52:00     "ICE 644"       track: 2 D - G  with: ICE       to: Düsseldorf Hbf
```
