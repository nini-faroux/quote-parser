# Build stage
FROM fpco/stack-build:lts-17 as build

RUN export PATH=$(stack path --local-bin):$PATH
WORKDIR /opt/build

COPY stack.yaml .
COPY stock-parser.cabal .
RUN stack build --only-dependencies --system-ghc --fast

COPY . .
RUN stack install

# Run stage
FROM ubuntu

WORKDIR /app
COPY --from=build /root/.local/bin/stock-parser-exe .
COPY --from=build /opt/build/data ./data

ENV inputfile="/app/data/mdf-kospi200.20110216-0.pcap"
ENV sort="" 

CMD "/app/stock-parser-exe" ${inputfile} ${sort}
