advent-exe:=".stack-work/dist/x86_64-linux/ghc-9.6.6/build/advent-exe/advent-exe"
advent-test:=".stack-work/dist/x86_64-linux/ghc-9.6.6/build/advent-test/advent-test"

default: build

build:
    @stack build --profile --ghc-options -fprint-potential-instances

profile year day: build
    stack exec --profile -- advent-exe test {{year}} {{day}} +RTS -p -hc

run year day:
    @stack run solve {{year}} {{day}}

solve year day: build
    @{{advent-exe}} solve {{year}} {{day}}

test year day: build
    @stack test
    @{{advent-exe}} test {{year}} {{day}}

start year day:
    @stack run start {{year}} {{day}}

create year:
    mkdir -p src/Challenges/Y{{year}}
    echo {01..25} | xargs -n1 just start {{year}}
