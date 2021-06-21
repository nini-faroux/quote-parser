<h2>Quote Parser</h2>

A program to parse and print quote messages from a market data feed. See 'problem.txt' for a full description of the problem.

Run with Docker:

```bash
 docker run ninifaroux/quote-parser
```
To sort the output based on the quote accept time:

```bash
 docker run -e sort=-r ninifaroux/quote-parser
```

Build and run with Stack:

```bash
 stack build
 stack exec -- stock-parser-exe "./data/mdf-kospi200.20110216-0.pcap"
```

To sort the output based on the quote accept time append the '-r' flag after the filepath:

```bash
stack exec -- stock-parser-exe "./data/mdf-kospi200.20110216-0.pcap" -r
```

More generally:

```bash
stack exec -- stock-parser-exe <path-to-file> [-r]
```
